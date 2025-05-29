;;; lk-emacs.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; This is my general Emacs config, defining general behavior and
;; configuring desktop mode.

;;; Code:

(use-package
 desktop
 :defer t
 :ensure nil
 :init
 (defun save-desktop ()
   "Non-interactive version of `desktop-save-in-desktop-dir`.
     Prevents hangs when killing emacs from systemd"
   (desktop-save desktop-dirname)
   (message "Desktop saved in %s" (abbreviate-file-name desktop-dirname)))
 :custom
 (desktop-dirname "~/.emacs.d/desktop/")
 (desktop-path (list desktop-dirname))
 (desktop-base-file-name "emacs.desktop")
 (desktop-base-lock-name "emacs.desktop.lock")
 (desktop-save t)
 (desktop-load-locked-desktop nil)
 (desktop-auto-save-timeout 30)
 (desktop-save-mode 1)
 :hook (kill-emacs . save-desktop))

;; pseudo-package for configuring built-in emacs functionality
(use-package
 emacs
 ;; turns off elpaca-use-package-mode current declaration will not be deferred
 :ensure nil
 :init
 (defun select-next-window ()
   "Switch to the next window"
   (interactive)
   (select-window (next-window)))

 (defun light-theme ()
   "Activate prefered light theme (modus operandi)"
   (interactive)
   (disable-theme (car custom-enabled-themes))
   (load-theme 'modus-operandi t))

 (defun dark-theme ()
   "Activate prefered dark theme (zenburn)"
   (interactive)
   (disable-theme (car custom-enabled-themes))
   (load-theme 'zenburn t))

 ;; Add prompt indicator to `completing-read-multiple'.
 ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
 (defun crm-indicator (args)
   (cons
    (format "[CRM%s] %s"
            (replace-regexp-in-string
             "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
            (car args))
    (cdr args)))
 (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

 ;; Do not allow the cursor in the minibuffer prompt
 (setq minibuffer-prompt-properties
       '(read-only t cursor-intangible t face minibuffer-prompt))
 (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

 :bind
 ("C-x c" . comment-region)
 ("C-x C" . uncomment-region)
 ("M-`" . select-next-window)

 :hook (after-save . executable-make-buffer-file-executable-if-script-p)

 :config
 ;; Disable the menu bar
 (menu-bar-mode -1)
 ;; Disable the tool bar
 (tool-bar-mode -1)
 ;; Disable the scroll bars
 (scroll-bar-mode -1)
 ;; Save the history across sessions as much as possible
 (save-place-mode t)
 (savehist-mode t)
 (recentf-mode t)
 ;; y or n instead of typing yes or no
 (setopt use-short-answers t)
 ;; Turn off fringe
 (fringe-mode -1)
 ;; Until I make eidos-mode a proper package, load here
 (let ((eidos-mode-file "~/repo/eidos-mode/eidos-mode.el"))
   (if (file-exists-p eidos-mode-file)
       (progn
         (load eidos-mode-file)
         (add-to-list 'auto-mode-alist '("\\.slim\\'" . eidos-mode)))))

 (let ((theme-file (expand-file-name "~/.theme")))
   (when (file-exists-p theme-file)
     (require 's)
     (require 'f)
     (let ((theme (s-chomp (f-read-text theme-file 'utf-8))))
       (message theme)
       (cond
        ((string= theme "light")
         (message "loading light theme")
         (disable-theme (car custom-enabled-themes))
         (load-theme 'modus-operandi t))
        ((string= theme "dark")
         (message "loading dark theme")
         (disable-theme (car custom-enabled-themes))
         (load-theme 'zenburn t))))))

 ;; Wayland remove title bar
 (add-to-list 'default-frame-alist '(undecorated . t))
 :custom
 (inhibit-x-resources
  t "ignore Xresources, just to be safe, don't want any config clash")
 (make-backup-files nil "Do not make backup ~ files")
 (window-resize-pixelwise t "necessary for fractional scaling")
 (frame-resize-pixelwise t "necessary for fractional scaling")
 (column-number-mode t "Use column numbers in modeline")
 (inhibit-startup-screen t "Disable splash screen")
 (initial-scratch-message "" "No default scratch buffer message")
 (initial-major-mode
  'fundamental-mode "Initial major mode for the scratch buffer")
 (fill-column 80 "fill-paragraph number of columns")
 (native-comp-async-report-warnings-errors
  nil "do not warn when native compile encounters errors")
 (help-window-select t "Switch to help buffers automatically")

 ;; recommended by vertico
 ;; Support opening new minibuffers from inside existing minibuffers.
 (enable-recursive-minibuffers t)
 ;; Hide commands in M-x which do not work in the current mode.  Vertico
 ;; commands are hidden in normal buffers. This setting is useful beyond
 ;; Vertico.
 (read-extended-command-predicate #'command-completion-default-include-p)

 ;; from corfu readme
 (text-mode-ispell-word-completion nil)

 ;; Hide commands in M-x which do not apply to the current mode.  Corfu
 ;; commands are hidden, since they are not used via M-x. This setting is
 ;; useful beyond Corfu.
 (read-extended-command-predicate #'command-completion-default-include-p)

 ;; safe .dir-locals.el variables
 (safe-local-variable-values
  '((eval
     progn
     (require 'reformatter)
     (reformatter-define
      reorder-python-imports
      :program "uvx"
      :args
      (list
       "--from"
       "git+https://github.com/asottile/reorder_python_imports@v3.14.0"
       "reorder-python-imports"
       "--exit-zero-even-if-changed"
       (format
        "--application-directories=%s"
        (concat (expand-file-name (project-root (project-current))) "python"))
       "--unclassifiable-application-module=_tskit"
       "-")
      :lighter " RdrPyImp"
      :group 'reorder-python-imports)
     (reorder-python-imports-on-save-mode))
    (eval
     progn
     (require 'reformatter)
     (reformatter-define
      reorder-python-imports
      :program "uvx"
      :args
      (list
       "git+https://github.com/asottile/reorder_python_imports@v3.14.0"
       "--exit-zero-even-if-changed"
       (format
        "--application-directories=%s"
        (concat (expand-file-name (project-root (project-current))) "python"))
       "--unclassifiable-application-module=_tskit"
       "-")
      :lighter " RdrPyImp"
      :group 'reorder-python-imports)
     (reorder-python-imports-on-save-mode))
    (eval add-to-list
          'eglot-server-programs
          '(haskell-mode
            "nix"
            "develop"
            "--command"
            "haskell-language-server-wrapper"
            "--lsp"))
    (org-todo-keyword-faces
     ("TODO" :foreground "lightgrey" :weight bold)
     ("IN_PROGRESS" :foreground "greenyellow" :weight bold)
     ("REVIEW" :foreground "lightslateblue" :weight bold)
     ("DONE" :foreground org-done :weight bold)
     ("CANCELLED" :foreground "grey50" :weight bold))
    (org-log-into-drawer . t)
    (clang-format-executable . "clang-format-6")
    (eval add-to-list
          'eglot-server-programs
          '(c-mode "clangd" "-header-insertion=never"))
    (eval add-hook 'before-save-hook 'org-beamer-export-to-pdf nil t)
    (eval add-hook 'before-save-hook 'clang-format-buffer nil t))))


(provide 'lk-emacs)
;;; lk-emacs.el ends here
