;;; lk-org-extensions.el --- -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;; TODO: this func is duplicated
(defun pjoin (&rest p)
  "Join file paths P."
  (apply #'concat
         (append (seq-map #'file-name-as-directory (butlast p)) (last p))))

;; Functionality for obtaining todo items from my projects and todo list
;; from the org-roam database.  This code will query the index that org-roam
;; keeps to find files that are tagged with the '@project' or '@todo' tag.

;; Adapted from the following blog post:
;; https://www.d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5
;; I'm not doing any automatic tagging, however.
;;
;; I still need to customize the agenda view

;; Define these toplevel tags (@project/@todo), useful for finding the files
;; associated with todo items to be collected.  We can narrow these down further
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


;; The main goal of this code is to identify files that are linked to a single
;; node, so that they can be exported to a new location.

(defun roam-get-files-from-ids (ids)
  "Given a list of IDS, get the associated files."
  (org-roam-db-query
   [:select
    :distinct [nodes:id files:file]
    :from nodes
    :inner-join files
    :on (= nodes:file files:file)
    :and (in nodes:id $v1)]
   (seq-into ids 'vector)))

(defun roam-get-id-from-title (title)
  "Given a file's TITLE, get the document id."
  (car ;; This should return exactly one value
   (org-roam-db-query
    [:select
     :distinct [links:source]
     :from files
     :inner-join nodes
     :on (= files:file nodes:file)
     :and (= files:title $s1)
     :inner-join links
     :on (= links:source nodes:id)
     :and (= links:type "id")]
    title)))

(defun roam-get-linked-from-ids (ids)
  "Given a list of IDS, get all linked files."
  (org-roam-db-query
   [:select
    :distinct [links:dest files:title]
    :from links
    :inner-join nodes
    :on (= links:source nodes:id)
    :and (in links:source $v1)
    :inner-join files
    :on (= files:file nodes:file)]
   (seq-into ids 'vector)))

(defun roam-get-linked-from-title (title)
  "Given a file's TITLE, get all linked files."
  (org-roam-db-query
   [:select
    :distinct [links:dest files:title]
    :from files
    :inner-join nodes
    :on (= files:file nodes:file)
    :and (= files:title $s1)
    :inner-join links
    :on (= links:source nodes:id)
    :and (= links:type "id")]
   title))

(defun roam-find-all-linked-files-from-title (title)
  "Given the TITLE of a document, find all linked files."
  (let ((query-result (roam-get-linked-from-title title))
        ;; we want to include the id of the toplevel file as well
        (ids (roam-get-id-from-title title))
        (seen '()))
    ;; to avoid an infinite loop, filter results we've already seen
    (while (< 0 (length (seq-difference (seq-map #'cadr query-result) seen)))
      (seq-map
       (lambda (row)
         (seq-let [id name] row
           (setq ids (seq-union ids `(,id)))
           (setq seen (seq-union seen `(,name)))))
       query-result)
      (setq query-result
            (roam-get-linked-from-ids (seq-map #'car query-result))))
    (seq-map
     (lambda (row)
       (seq-let [id path] row
         `(,id . ,path)))
     (roam-get-files-from-ids ids))))


(defun roam-rewrite-links (in-file out-file link-alist)
  "Rewrite links with name of file that the id in the roam link encodes.
Read from IN-FILE and write to OUT-FILE without modifying the original.
LINK-ALIST stores the mapping between roam ids and the out file."
  (with-temp-buffer
    (insert-file-contents in-file)
    (org-mode)
    (let ((links
           (reverse
            (org-element-map (org-element-parse-buffer) 'link #'identity))))
      (seq-do
       (lambda (link)
         (let ((type (org-element-property :type link))
               (path (org-element-property :path link))
               (link-txt
                (substring-no-properties
                 (org-element-interpret-data (org-element-contents link)))))
           (when-let* ((_ (string= type "id"))
                       (begin (org-element-property :begin link))
                       (end (min (point-max) (org-element-property :end link)))
                       (new-path (cdr (assoc path link-alist))))
             ;; (print (format "type=%s path=%s new-path=%s" type path new-path))
             (goto-char begin)
             (delete-region begin end)
             (insert (format "[[%s][%s]]" new-path link-txt)))))
       links))
    (write-region (point-min) (point-max) out-file)))

(defun non-roam-filename (path)
  "Remove preceeding timestamp preceeding file name in roam PATH."
  (replace-regexp-in-string "^[0-9]+-" "" (file-name-nondirectory path)))

(defun roam-rewrite-subtree-to-dir (title outdir)
  "Given a single node's TITLE, copy all linked files and their linked files.
Write the outputs to a given OUTDIR and modify links so that they're all
relative."
  (org-roam-db-sync)
  (make-directory outdir t)
  (let* ((linked-files (roam-find-all-linked-files-from-title title))
         (out-links
          (seq-map
           (pcase-lambda (`(,id . ,path))
             (cons id (non-roam-filename path)))
           linked-files)))
    (seq-map
     (lambda (file)
       (let ((outfile (pjoin outdir (non-roam-filename file))))
         (print (format "%s -> %s" file outfile))
         (roam-rewrite-links file outfile out-links)))
     (seq-map #'cdr linked-files))))

(provide 'lk-org-extensions)
;;; lk-org-extensions.el ends here
