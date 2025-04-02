;;; lk-org-papis.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package
 papis
 :ensure
 (:host
  github
  :repo "papis/papis.el"
  :ref "a396a45e564c9ea51799301e9a9e072f8cbc3913")
 :custom (papis-library "papers-local")
 :config
 (defun papis-open (doc)
   (interactive (list (papis--read-doc)))
   (let* ((files (papis--get-file-paths doc))
          (file
           (pcase (length files)
             (1 (car files))
             (0 (error "Doc has no files"))
             (_ (completing-read "file: " files)))))
     (papis--cmd (format "open ref:%s" (papis--get-ref (papis--read-doc)))))))

(defun papis-org-ref-get-pdf-filename (key)
  "Get pdf filename from org-ref reference KEY."
  (interactive)
  (let* ((docs (papis-query (format "ref:'%s'" key)))
         (doc (car docs))
         (files (papis--get-file-paths doc)))
    (pcase (length files)
      (1 (car files))
      (_ (completing-read "" files)))))


(provide 'lk-org-papis)
;;; lk-org-papis.el ends here
