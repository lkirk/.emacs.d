;;; lk-org.el --- -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

(require 'orgfold)

(setq fold-directory-alist '(("." . "~/roam/.fold")))

(use-package
 org
 :init
 (defun org-zotero-open (url)
   "Visit the zotero link to a pdf referenced by the url.
    The link should look like: zotero://open-pdf/library/items/3A2XZNUW"
   (async-shell-command
    ;; NB org strips off zotero:
    (concat "xdg-open zotero:" (shell-quote-argument url) " & disown")))
 :custom

 ;; LaTeX preview settings
 (org-preview-latex-default-process 'dvisvgm)
 (org-latex-create-formula-image-program 'dvisvgm)

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
  '(("d" "default" entry "* %?"
     :target
     (file+head "%<%Y-%m-%d>.org" :head "#+title: %<%Y-%m-%d>\n"))
    ("w" "weekly" plain ""
     :target
     (file+head
      "weekly/%<%Y-%V>.org"
      "#+title: Weekly Meeting %<%Y-%V>\n* Goals for last week\n\n** \n\n* Issues\n\n** \n\n* Goals for this week\n\n** ")
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

(defun org-dblock-write:todos (params)
  "Insert TODO items from an external file dynamically.
PARAMS provide the filename to pull from."
  (let*
      ((file (plist-get params :file)) ;; Set default file
       (todo-states (or (plist-get params :todo) (car org-todo-keywords)))
       (todos "")
       (hline
        (make-string
         (+ (org-element-property
             :level
             (org-element-lineage (org-element-at-point) '(headline) t))
            1)
         ?*)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map
       (org-element-parse-buffer 'headline) 'headline
       (lambda (h)
         (let ((todo (org-element-property :todo-keyword h))
               (title (org-element-property :raw-value h)))
           (when (member todo todo-states)
             (setq
              todos
              (if (string-empty-p todos)
                  (format "%s %s %s" hline todo title) ;; First entry: no newline
                (concat todos (format "\n%s %s %s" hline todo title)))))))))
    (insert todos)))


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
