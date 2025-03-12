;;; lk-org.el --- -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(require 'orgfold)

(setq fold-directory-alist '(("." . "~/roam/.fold")))

;; Functionality for obtaining todo items from my projects and todo list
;; from the org-roam database.  This code will query the index that org-roam
;; keeps to find files that are tagged with the '@project' or '@todo' tag.

;; Adapted from the following blog post:
;; https://www.d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5
;; I'm not doing any automatic tagging, however.
;;
;; I still need to customize the agenda view

;; Define these toplevel tags (@project/@todo), useful for finding the files
;; associated with todo items to be collected. We can narrow these down further
;; by adding tags either to the toplevel of the file or to the todo items
;; themselves
(defun todo-files ()
  "Return a list of files with todo items (@project/@todo tags)."
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select
      [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where
      ;; (defun blabla (a) `(or ,@(seq-map (lambda (b) (= b a)) '(1 2 3 4 5)))
      ;; `(or (like tag (seq-map (lambda (t) '(format "%%\"%s\"%%" t) '("@project" "@todo")))))]))))
      (or (like tag ' "%\"@project\"%") (like tag ' "%\"@todo\"%"))]))))

(defun todo-files-update (&rest _)
  "Update the value of `variable:org-agenda-files'."
  (setq org-agenda-files (todo-files)))

(advice-add 'org-agenda :before #'todo-files-update)
(advice-add 'org-todo-list :before #'todo-files-update)

(defun org-file-map-headlines (file func)
  "Read an org FILE and map a FUNC to its headlines."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (org-element-map (org-element-parse-buffer 'headline) 'headline func)))

(defun timestamp-in-p (org-ts lower upper)
  "Test if the org timestamp, ORG-TS is in LOWER and UPPER."
  (let ((diff-days
         (time-to-number-of-days
          (time-subtract (org-timestamp-to-time org-ts) (current-time)))))
    (and (<= diff-days upper) (> diff-days lower))))

(defun get-timestamp-plist (h t-props)
  "Get timestamp values from heading H, given T-PROPS."
  (seq-mapcat
   (lambda (p)
     (let ((v (org-element-property p h)))
       (if v
           `(,p ,v)
         v)))
   t-props))

;; (defun headline-in-time-range (h range t-props)
;;   "Test if headline H has a timestamp in RANGE.
;; RANGE is specified units of days.  Negative values must be specified by the
;; caller.  T-PROPS are the properties in the plist that store the timestamps"
;;   (seq-some
;;    (lambda (p)
;;      (timestamp-in-p (org-element-property p h) (car range) (cadr range)))
;;    t-props))

(defun any-in-time-range-p (plist range t-props)
  "Test if any member of PLIST has a timestamp in RANGE.
RANGE is specified units of days.  Negative values must be specified by the
caller.  T-PROPS are the properties in the plist that store the timestamps"
  (seq-some
   (lambda (p)
     (timestamp-in-p (plist-get plist p) (car range) (cadr range)))
   t-props))

(setq todos-dblock-time-props '(:closed :deadline :scheduled))

(defun get-todo-items (file &optional todo-states todo-tags time-range)
  "Given the input FILE, parse todo headlines and their inherited tags.
If TODO-STATES is specified, filter based on the state.  If TODO-TAGS is
specified, we should keep only todo items with these tags.  If TIME-RANGE
is specified, then if the timestamps of one of 'todos-dblock-time-props'
are in the range, then the todos will be kept."
  (let ((states (or todo-states (car org-todo-keywords)))
        (t-props todos-dblock-time-props))
    (org-file-map-headlines
     file
     (lambda (h)
       (let* ((todo (org-element-property :todo-keyword h))
              (title (org-element-property :title h))
              (tags (org-get-tags h))
              (has-tags (or (null todo-tags) (seq-intersection todo-tags tags)))
              (t-stamps (get-timestamp-plist h t-props))
              (in-time-range
               (or (null time-range)
                   (any-in-time-range-p t-stamps time-range t-props)))
              (has-state (member todo states)))
         (when (and has-tags has-state in-time-range)
           (append
            `(:state ,todo :title ,title :tags ,tags :file ,file)
            t-stamps)))))))

;; TODO: perhaps it's possible to generate these with some sort of
;;       constructor?
;; https://orgmode.org/worg/dev/org-element-api.html
(defun format-todo-entries (todos level)
  "Create a newline separated string containing all todos.
TODOS is a list of plists with keys ':state' and ':level'.
New headlines are created with the specified LEVEL."
  (let ((entry-fmt
         (lambda (s)
           (let ((ts-kw (car (seq-intersection s todos-dblock-time-props))))
             (format "%s %s [[file:%s::*%s][%s]] (%s:%s)"
                     (make-string level ?*)
                     (plist-get s :state)
                     (plist-get s :file)
                     (plist-get s :title)
                     (plist-get s :title)
                     ts-kw
                     (org-element-property :raw-value (plist-get s ts-kw)))))))
    (string-join (seq-map entry-fmt todos) "\n")))

(defun sort-todos (todos)
  "Sort TODOS first by todo state, then by title."
  (seq-sort
   (lambda (l r)
     (let ((state-l (plist-get l :state))
           (state-r (plist-get r :state))
           (title-l (plist-get l :title))
           (title-r (plist-get r :title)))
       (or (string< state-l state-r)
           (and (string= state-l state-r) (string< title-l title-r)))))
   todos))

(defun format-todo-sections (todos level)
  "Given a list of TODOS, sort and group by tags, then format.
This will return a list of blocks of text to insert.  The LEVEL argument
will describe the level at which headlines should be generated."
  (seq-map
   (lambda (group)
     (format "%s %s\n%s"
             (make-string level ?*)
             (string-join (car group) ":")
             (format-todo-entries (cdr group) (+ 1 level))))
   (seq-group-by (lambda (t) (plist-get t :tags)) (sort-todos todos))))

(defun get-current-headline-level ()
  "Get the headline level of the current element under cursor."
  (let ((curr-hline (org-element-lineage (org-element-at-point) '(headline) t)))
    (org-element-property :level curr-hline)))

(defun org-dblock-write:todos (params)
  "Insert all exported todo items from the specified files.
PARAMS is a plist with ':files' to find todos in (default is 'org-agenda-files',
':states' for todo states to consider (default all), and ':tags' specifying
the todo tags to consider (among all inherited tags).

Here's an example of a dynamic block with all of the options
#+BEGIN: todos :files (\"todo.org\") :tags (\"@todo\") :states (\"DONE\") \
:time (-7 14)

But, if the defaults are desired, the dynamic block is simply:
#+BEGIN: todos"
  (message "RUNNING TODOS DBLOCK")
  (let* ((files (or (plist-get params :files) org-agenda-files))
         (states (or (plist-get params :states) (car org-todo-keywords)))
         (tags (plist-get params :tags))
         (level (+ 1 (get-current-headline-level)))
         (time-range (plist-get params :time))
         (todos
          (seq-mapcat
           (lambda (f) (get-todo-items f states tags time-range)) files)))
    (insert (string-join (format-todo-sections todos level) "\n"))))
:deadline
:scheduled
:closed
(advice-add 'org-dblock-write:todos :before #'todo-files-update)

(use-package
 org
 :init
 (defun org-zotero-open (url)
   "Visit the zotero link to a pdf referenced by the url.
    The link should look like: zotero://open-pdf/library/items/3A2XZNUW"
   (async-shell-command
    ;; NB org strips off zotero:
    (concat "xdg-open zotero:" (shell-quote-argument url) " & disown")))

 ;; :bind ("C-c C-p" . org-present)
 :custom
 ;; LaTeX preview settings
 (org-preview-latex-default-process 'dvisvgm)
 (org-latex-create-formula-image-program 'dvisvgm)
 (org-log-done 'time)
 (org-startup-with-inline-images t)
 (org-highlight-latex-and-related '(latex))
 (org-hide-macro-markers 1)
 (org-src-tab-acts-natively nil)
 (org-return-follows-link t)
 (org-todo-keywords
  '((sequence "TODO" "IN_PROGRESS" "REVIEW" "|" "DONE" "CANCELLED")))
 (org-todo-keyword-faces
  '(("TODO" . (:foreground "lightgrey" :weight bold))
    ("IN_PROGRESS" . (:foreground "darkturquoise" :weight bold))
    ("REVIEW" . (:foreground "lightslateblue" :weight bold))
    ;; hm, org-done invalid, just leave unset
    ;; ("DONE" . (:foreground org-done :weight bold))
    ("CANCELLED" . (:foreground "grey50" :weight bold))))
 :config
 (plist-put org-format-latex-options :scale .8)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((shell . t) (python . t) (julia . t) (C . t) (latex . t)))
 (org-add-link-type "zotero" #'org-zotero-open)
 :hook
 (org-mode . auto-fill-mode)
 (org-mode . orgfold-activate)
 (org-mode . org-fold-hide-drawer-all)) ;; always hide drawer
;; (org-mode . org-beamer-export-to-pdf))

(use-package
 org-roam
 :after org
 :init
 (unless (file-exists-p "~/roam")
   (make-directory "~/roam"))
 :hook (org-capture-mode-hook . org-fold-hide-drawer-all)
 :custom
 (org-roam-directory "~/roam")
 (org-roam-dailies-directory "~/roam/daily")
 (org-roam-graph-link-hidden-types '("file" "http" "https" "zotero"))
 (org-roam-complete-everywhere t)
 (org-roam-dailies-capture-templates
  '(("d"
     "default"
     entry
     "* %?"
     :target (file+head "%<%Y-%m-%d>.org" :head "#+title: %<%Y-%m-%d>\n"))
    ("w" "weekly" plain ""
     :target
     (file+head
      "weekly/%<%Y-%V>.org"
      "\
#+title: Weekly Meeting %<%Y-%V>
* Goals for last week\n
** \n
* Issues\n
** \n
* Goals for this week\n
** \n
* Tasks and Timeline

#+BEGIN: todos :time (-7 14) :tags (\"@tskit\" \"@weekly\")
#+END:
")
     :unnarrowed t)
    ("t"
     "todo"
     entry
     "* TODO %?"
     :target (file+datetree "todo.org" week)
     :empty-lines-before 1
     :unnarrowed t)
    ("l"
     "links"
     entry
     "* %?"
     :target (file "links.org")
     :empty-lines-before 1)))
 :config (org-roam-db-autosync-mode))

(use-package ob-async :after org)
(use-package org-fragtog :after org :hook (org-mode . org-fragtog-mode))
(use-package org-autolist :after org :hook (org-mode . org-autolist-mode))

(use-package
 org-download
 :after org
 :hook ((org-mode dired-mode) . org-download-enable)
 :custom
 (org-download-method 'attach)
 (org-download-screenshot-method "grim -g \"$(slurp)\" %s"))

(use-package
 org-attach
 :ensure nil
 :after org
 ;; use inheritance so that I can link attachments in any part of the file
 :custom (org-attach-use-inheritance t))

(use-package
 org-present
 :after org
 :hook
 (org-present-mode
  .
  (lambda ()
    (org-present-big)
    (org-display-inline-images)
    (org-present-hide-cursor)
    (org-present-read-only)))
 (org-present-mode-quit
  .
  (lambda ()
    (org-present-small)
    (org-remove-inline-images)
    (org-present-show-cursor)
    (org-present-read-write))))

;; (defun lk/presentation-setup ()
;;   (setq text-scale-mode-amount 3)
;;   (org-display-inline-images)
;;   (text-scale-mode 1))
;; (defun lk/presentation-end ()
;;   (text-scale-mode 0))

;; (use-package
;;  org-tree-slide
;;  ;; :hook
;;  ;; ((org-tree-slide-play . lk/presentation-setup)
;;  ;;  (org-tree-slide-stop . lk/presentation-end))
;;  :custom
;;  (org-tree-slide-skip-outline-level 2)
;;  (org-tree-slide-in-effect t)
;;  (org-tree-slide-activate-message "Presentation started!")
;;  (org-tree-slide-deactivate-message "Presentation finished!")
;;  (org-tree-slide-header t)
;;  (org-tree-slide-breadcrumbs " > ")
;;  (org-image-actual-width nil))


;; The main goal of this code is to identify files that are linked to a single
;; node. It needs some work. The biggest issue is that I don't have many
;; recursively linked pages, so I can't test this. Also -- What will happen when
;; we have circular links?

;; (seq-map
;; (lambda (s) (org-roam-db-query
;; [:select [links:dest] :from links
;;  :where (= links:type "id")
;;  :and (= links:source $s1)] s))
;; (org-roam-db-query
;; [:select [links:dest] :from files
;;  :inner-join nodes :on (= files:file nodes:file)
;;  :and (= files:title "Dissertation")
;;  :inner-join links :on (= links:source nodes:id)
;;  :and (= links:type "id")]))
;; (nil nil nil nil)

;; (org-roam-db-query
;; [:select [links:dest] :from files
;;  :inner-join nodes :on (= files:file nodes:file)
;;  :and (= files:title "Dissertation")
;;  :inner-join links :on (= links:source nodes:id)
;;  :and (= links:type "id")])
;; (("991ce73b-f1e5-44a1-abda-8f62cc81e034")
;;  ("5d1c6d3f-1e2c-4bbf-8de5-6c6cdddd4fa6")
;;  ("2688296a-8489-4f2c-b762-5786fba5c4a1")
;;  ("25f18ffb-734e-4321-83e6-17876eafc3db"))


(provide 'lk-org)
;;; lk-org.el ends here
