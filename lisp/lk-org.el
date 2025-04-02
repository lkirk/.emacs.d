;;; lk-org.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'orgfold)
(require 'lk-org-extensions)
;; (require 'lk-org-zotero)
(require 'lk-org-papis)

(defun pjoin (&rest p)
  "Join file paths P."
  (apply #'concat
         (append (seq-map #'file-name-as-directory (butlast p)) (last p))))

(setq org-roam-directory "~/roam")
(setq org-roam-db-location (pjoin org-roam-directory "org-roam.db"))
(setq org-roam-dailies-directory (pjoin org-roam-directory "daily"))
(setq fold-directory-alist `(("." . ,(pjoin org-roam-directory ".fold"))))

(use-package
 org
 :after julia-vterm
 ;; :bind ("C-c C-p" . org-present)
 :custom
 ;; LaTeX preview settings
 (org-preview-latex-default-process 'dvisvgm)
 (org-latex-create-formula-image-program 'dvisvgm)
 (org-log-done 'time)
 (org-startup-with-inline-images t)
 (org-highlight-latex-and-related '(latex))
 (org-hide-macro-markers 1)
 ;; (org-src-preserve-indentation t)
 ;; (org-src-tab-acts-natively t)
 (org-src-tab-fontify-natively t)
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
 ;; (org-zotero-db-path "~/Zotero/zotero.sqlite")
 (org-confirm-babel-evaluate nil)
 (org-babel-julia-command "julia --project=@.")
 (org-agenda-custom-commands
  '(("c" "Custom Agenda"
     ((agenda "" ((org-deadline-warning-days 0)))
      (tags "@project")
      (tags "@todo")))))
 :config
 (add-to-list 'org-src-lang-modes '("eidos" . eidos))
 (plist-put org-format-latex-options :scale .8)
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((shell . t)
    (python . t)
    ;; (julia . t)
    (C . t) (latex . t) (R . t) (julia-vterm . t)))
 ;; (org-zotero-setup-links)

 :hook
 (org-mode . auto-fill-mode)
 (org-mode . orgfold-activate)
 (org-mode . org-fold-hide-drawer-all)) ;; always hide drawer
(use-package
 org-roam
 :after org
 :init
 (unless (file-exists-p org-roam-directory)
   (make-directory org-roam-directory))
 :hook (org-capture-mode-hook . org-fold-hide-drawer-all)
 :custom
 ;; (org-roam-graph-link-hidden-types '("file" "http" "https" "zotero"))
 (org-roam-graph-link-hidden-types '("file" "http" "https" "papis"))
 (org-roam-complete-everywhere t)
 (org-roam-dailies-capture-templates
  '(("d"
     "default"
     entry
     "* %?"
     :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))
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
     :empty-lines-before 1)
    ("l"
     "links"
     entry
     "* %?"
     :target (file+head "links.org" "#+title: Links\n")
     :empty-lines-before 1)
    ("r"
     "reading"
     entry
     "* \n- [ ] %?"
     :target (file+head "reading-list.org" "#+title: Reading List\n")
     :empty-lines-before 1
     :unnarrowed t)))
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

(provide 'lk-org)
;;; lk-org.el ends here
