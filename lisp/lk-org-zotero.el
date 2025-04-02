;;; lk-org-zotero.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Functionality for integrating zotero into org mode.  Provides the ability
;; to open links and complete by title for link insertion.  To integrate this
;; code into your org setup (must be run after org):
;;
;; (setq org-zotero-db-path "~/Zotero/zotero.sqlite")
;; (org-zotero-setup-links)
;; 

;;; Code:

(defcustom org-zotero-db-path nil
  "Path to Zotero sqlite database."
  :group 'org-zotero
  :type 'string)

(defun org-zotero-setup-links ()
  "Setup zotero links."
  (when (null org-zotero-db-path)
    (error "org-zotero-db-path must be non-nil to setup zotero links"))
  (org-link-set-parameters
   "zotero"
   :complete #'org-zotero-link-complete
   :insert-description #'org-zotero-link-insert-description
   :follow #'org-zotero-open))

(defmacro org-zotero-with-temp-db (connection-spec &rest body)
  "Create a copy of the zotero db, deleting after executing the BODY.
This is necessary because the zotero db is locked when zotero is running.
CONNECTION-SPEC establishes the db binding."
  (declare (indent 1))
  `(let* ((orig org-zotero-db-path)
          (org-zotero-db-path (make-temp-file "zotero" nil ".sqlite")))
     (unwind-protect
         (progn
           (copy-file orig org-zotero-db-path t)
           (emacsql-with-connection ,connection-spec ,@body))
       (delete-file org-zotero-db-path))))

(defun org-zotero-get-items ()
  "Query the zotero db and return an alist of (id . title) entries."
  (let
      ((items
        (org-zotero-with-temp-db (db (emacsql-sqlite-open org-zotero-db-path))
          (emacsql
           db
           [:select
            ;; format the title as a string so that it's not output as a lisp symbol
            [(funcall printf ' "\"%s\"" items:key)
             (funcall printf ' "\"%s\"" itemDataValues:value)]
            :from itemAttachments
            :inner-join itemData
            :on (= itemData:itemID itemAttachments:parentItemID)
            :inner-join itemDataValues
            :on (= itemDataValues:valueID itemData:valueID)
            :inner-join items
            :on (= items:itemID itemAttachments:itemID)
            :and (= itemData:fieldID 1)]))))
    (seq-map
     (lambda (row)
       (seq-let [id title] row
         `(,id . ,title)))
     items)))

;; for group links, see:
;; https://github.com/mhucka/zowie/blob/92a5bf6/zowie/zotero.py#L178C1-L189C1

(setq org-zotero-link-prefix "zotero://open-pdf/library/items/")

(defun org-zotero-title-from-link (path)
  "Get the zotero title from the link PATH and an alist of ZOT-ITEMS.
ZOT-ITEMS maps the zotero id to the article title."
  (let ((id (string-remove-prefix org-zotero-link-prefix path)))
    (cdr (assoc-string id (org-zotero-get-items)))))

(defun org-zotero-link-insert-description (path &optional description)
  "Automatically generate a description for a zotero link.
We take the PATH and use the key to generate the DESCRIPTION, which can
be overwritten."
  (if (not (null description))
      description
    (org-zotero-title-from-link path)))

(defun org-zotero-link-complete ()
  "Completion function for Zotero links."
  (let* ((zot-items (org-zotero-get-items))
         (sel-title (completing-read "Title: " (mapcar #'cdr zot-items) nil t))
         (sel-id (car (rassoc sel-title zot-items))))
    (concat org-zotero-link-prefix sel-id)))


;; still a parent process of Emacs if opened for the first time
;; this is a bit tricky for some reason. This works well for now.
(defun org-zotero-open (url)
  "Visit the zotero link to a pdf referenced by the URL.
The link should look like: zotero://open-pdf/library/items/3A2XZNUW"
  (start-process "zotero" nil "setsid" "xdg-open" (concat "zotero:" url)))

(provide 'lk-org-zotero)
;;; lk-org-zotero.el ends here
