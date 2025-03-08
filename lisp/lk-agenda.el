;;; lk-agenda.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Functionality for obtaining todo items from my projects and todo list
;; from the org-roam database.  This code will query the index that org-roam
;; keeps to find files that are tagged with the '@project' or '@todo' tag.

;; Adapted from the following blog post:
;; https://www.d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5
;; I'm not doing any automatic tagging, however.
;;
;; I still need to customize the agenda view
;;

;;; Code:


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
      (or (like tag ' "%\"@project\"%") (like tag ' "%\"@todo\"%"))]))))


(defun todo-files-update (&rest _)
  "Update the value of `variable:org-agenda-files'."
  (setq org-agenda-files (todo-files)))

(advice-add 'org-agenda :before #'todo-files-update)
(advice-add 'org-todo-list :before #'todo-files-update)

(provide 'lk-agenda)
;;; lk-agenda.el ends here
