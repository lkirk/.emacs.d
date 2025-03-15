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
;; "~/Zotero/zotero.sqlite"

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

(defun org-zotero-get-items ()
  "Query the zotero db and return an alist of (id . title) entries."
  (let
      ((items
        (emacsql-with-connection
         (db (emacsql-sqlite-open org-zotero-db-path))
         (emacsql db [:pragma (= query-only on)])
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

(defun org-zotero-open (url)
  "Visit the zotero link to a pdf referenced by the URL.
The link should look like: zotero://open-pdf/library/items/3A2XZNUW"
  (async-shell-command
   ;; NB org strips off zotero:
   (concat "xdg-open zotero:" (shell-quote-argument url) " & disown")))

(provide 'lk-org-zotero)
;;; lk-org-zotero.el ends here
