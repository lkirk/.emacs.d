;;; lk-org.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'orgfold)
(require 'lk-org-extensions)
(require 'lk-org-papis)

(defun pjoin (&rest p)
  "Join file paths P."
  (apply #'concat
         (append (seq-map #'file-name-as-directory (butlast p)) (last p))))

(setq org-roam-directory "~/roam")
(setq org-roam-db-location (pjoin org-roam-directory "org-roam.db"))
(setq org-roam-dailies-directory (pjoin org-roam-directory "daily"))
(setq fold-directory-alist `(("." . ,(pjoin org-roam-directory ".fold"))))

;; use font faces for org latex code embedding
(use-package engrave-faces :after org)

(use-package
 org
 :ensure nil
 :after engrave-faces
 :custom (org-use-property-inheritance t)
 ;; LaTeX export
 (org-latex-src-block-backend 'engraved)
 (org-latex-pdf-process
  (if (executable-find "latexmk")
      '("latexmk -shell-escape -f -pdf -%latex -interaction=nonstopmode -output-directory=%o %f")
    '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
      "%latex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 ;; LaTeX preview settings
 (org-log-done 'time)
 (org-startup-with-inline-images t)
 (org-startup-with-latex-preview t)
 (org-highlight-latex-and-related '(latex))
 (org-hide-macro-markers 1)
 (org-src-tab-fontify-natively t)
 (org-return-follows-link t)
 (org-todo-keywords
  '((sequence "TODO" "IN_PROGRESS" "REVIEW" "|" "DONE" "CANCELLED")))
 (org-todo-keyword-faces
  '(("TODO" . (:foreground "lightgrey" :weight bold))
    ("IN_PROGRESS" . (:foreground "darkturquoise" :weight bold))
    ("REVIEW" . (:foreground "lightslateblue" :weight bold))
    ("CANCELLED" . (:foreground "grey50" :weight bold))))
 (org-confirm-babel-evaluate nil)
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
    (C . t)
    (latex . t)
    (R . t)
    (julia . julia-snail)
    (gnuplot . t)))
 :hook
 (org-babel-after-execute . org-redisplay-inline-images)
 (org-mode . auto-fill-mode)
 (org-mode . orgfold-activate)
 (org-mode . org-fold-hide-drawer-all))
(use-package
 org-roam
 :after org
 :init
 (unless (file-exists-p org-roam-directory)
   (make-directory org-roam-directory))
 :hook (org-capture-mode-hook . org-fold-hide-drawer-all)
 :custom
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

(use-package org-fragtog :after org :hook (org-mode . org-fragtog-mode))
(use-package
 org-autolist
 :diminish
 :after org
 :hook (org-mode . org-autolist-mode))

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

(use-package org-nix-shell :hook (org-mode . org-nix-shell-mode))

(provide 'lk-org)
;;; lk-org.el ends here
