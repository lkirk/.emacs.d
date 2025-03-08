;;; lk-org.el --- -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

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

(use-package
 org-download
 :hook ((org-mode dired-mode) . org-download-enable)
 :custom
 (org-download-method 'attach)
 (org-download-screenshot-method "grim -g \"$(slurp)\" %s"))

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
    ("DONE" . (:foreground org-done :weight bold))
    ("CANCELLED" . (:foreground "grey50" :weight bold))))
 :config
 (plist-put org-format-latex-options :scale .8)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((shell . t) (python . t) (julia . t) (C . t) (latex . t)))
 (org-add-link-type "zotero" #'org-zotero-open)
 :hook (org-mode . auto-fill-mode)) ;; (org-mode . org-beamer-export-to-pdf))

(use-package ob-async :after org)
(use-package org-fragtog :after org :hook (org-mode . org-fragtog-mode))
(use-package org-autolist :after org :hook (org-mode . org-autolist-mode))
;; use inheritance so that I can link attachments in any part of the file
(use-package
 org-attach
 :ensure nil
 :after org
 :custom (org-attach-use-inheritance t))

(use-package
 evil-org
 :after org
 :hook (org-mode . (lambda () evil-org-mode))
 :config
 (require 'evil-org-agenda)
 (evil-org-agenda-set-keys))

(provide 'lk-org)
;;; lk-org.el ends here
