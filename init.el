;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Refined version of my Emacs config, using elpaca and use-package.
;;
;; useful documents:
;; https://github.com/jwiegley/use-package/blob/master/README.md
;; https://github.com/progfolio/elpaca/blob/master/doc/manual.md
;;

;;; Code:

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'load-elpaca)

;; BEGIN Package Management

;; enable elpaca get-user integration
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; globally add :ensure to use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; END Package Management

;; BEGIN Look and Feel

(use-package zenburn-theme :config (load-theme 'zenburn t))

(use-package undo-tree :config (global-undo-tree-mode))

(use-package
 evil
 :after undo-tree
 :config
 (evil-set-undo-system 'undo-tree)
 (evil-mode t)
 :custom (evil-want-C-u-scroll t)
 ;; these next two are needed for evil-collection
 (evil-want-integration t) ;; optional, already t by default
 (evil-want-keybinding nil)
 (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))))

(use-package evil-collection :after evil :config (evil-collection-init))

;; preferred light theme
(use-package modus-themes)

;; TODO: debug diminish for some minor modes (eldoc, etc...)
(use-package diminish)

;; END Look and Feel

;; BEGIN: Dev Tools
(use-package
 magit
 :bind
 :commands
 magit-status
 magit-blame
 :hook (with-editor-mode evil-insert-state)
 :custom
 (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
;; END: Dev Tools

;; NB: Packages that are included in emacs are marked with :ensure nil
;; BEGIN: Programming Modes

(use-package
 cc-mode
 :ensure nil
 :hook
 (c-mode
  .
  (lambda ()
    (setq comment-start "//")
    (setq comment-end "")))
 :custom (indent-tabs-mode nil) (c-basic-offset 4))

;; Need this for tskit development. Uses an older version of clang-format
;; (set via dir-locals.el)
(use-package clang-format)

(use-package rust-mode :custom (rust-mode-treesitter-derive t))

(use-package
 julia-mode
 :mode ("\\.jl\\'" . julia-mode)
 :hook (julia-mode . eglot-jl-init)
 :custom (tab-width 4))

(use-package
 julia-repl
 :hook
 (julia-mode . julia-repl-mode)
 (julia-repl . undo-tree-mode)
 :config (julia-repl-set-terminal-backend 'eat))

(use-package
 eat
 :custom (eat-kill-buffer-on-exit t)
 :hook (eshell-load . eat-eshell-mode))

(use-package
 eglot-jl
 :after eglot
 :commands eglot-jl-init
 :custom (tab-width 4))

(use-package
 systemd
 :mode (("\\.service\\'" . systemd-mode) ("\\.target\\'" . systemd-mode)))

(use-package haskell-mode)

(use-package meson-mode :hook (meson-mode . company-mode))

(use-package eldoc :diminish eldoc-mode :ensure nil :config (global-eldoc-mode))

(use-package
 flymake
 :ensure nil
 :hook (prog-mode . flymake-mode)
 :bind
 (:map
  flymake-mode-map
  ("C-c n" . flymake-goto-next-error)
  ("C-c N" . flymake-goto-prev-error)))

(use-package
 eglot
 :init
 (defun my/format-on-save ()
   "Format buffer if not in `cc-mode`."
   ;; Disable so that I can use clang-format in tskit development
   (unless (string-match "repo/tskit" buffer-file-name)
     (eglot-format)))
 :ensure nil
 :hook (prog-mode . eglot-ensure) (before-save . my/format-on-save)
 :custom (eglot-autoshutdown 1) (eglot-report-progress nil)
 :config
 (let ((mode '(wolfram-mode :language-id "Wolfram Language"))
       ;; (wolfram-lsp-cmd '("wolframscript" "-code"
       ;;                    "Needs[\"LSPServer`\"];LSPServer`StartServer[]")))
       (wolfram-lsp-cmd
        '("WolframKernel"
          "-noinit"
          "-noprompt"
          "-nopaclet"
          "-noicon"
          "-nostartuppaclets"
          "-run"
          "Needs[\"LSPServer`\"];LSPServer`StartServer[]")))
   (add-to-list 'eglot-server-programs (cons mode wolfram-lsp-cmd)))
 :bind
 (:map
  eglot-mode-map
  ("C-c C-j" . xref-find-definitions)
  ("C-c C-k" . xref-find-references)))

(use-package
 ruff-format
 :diminish
 :hook (python-mode . ruff-format-on-save-mode))

;; TODO: until we can do this with ruff
(use-package isortify :diminish :hook (python-mode . isortify-mode))

(use-package company :diminish :init (global-company-mode t))

;; (use-package tex
;;   :ensure nil
;;   :hook
;;   (latex-mode . auto-fill-mode))

(use-package
 yaml-mode
 :mode (("\\.yml\\'" . yaml-mode) ("\\clang-format\\'" . yaml-mode)))

(use-package
 markdown-mode
 :mode ("\\.md\\'" . markdown-mode)
 :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
 :hook (markdown-mode . auto-fill-mode))

(use-package dockerfile-mode)

(use-package
 wolfram-mode
 :mode "\\.\\(wl\\|wls\\|m\\)\\'"
 :custom
 (tab-width 4)
 (wolfram-indent 4))

(use-package auctex :ensure nil :hook (LaTeX-mode-hook . auto-fill-mode))

(use-package preview-latex :ensure nil :after auctex)

(use-package sphinx-doc :hook (python-mode . sphinx-doc-mode))

;; Bind key sequence to the function
(use-package
 python
 :ensure nil
 :config
 (defun insert-ipython-debug ()
   "Insert an interactive debug at point."
   (interactive)
   (insert "__import__(\"IPython\").embed()"))
 :bind ("C-c p" . insert-ipython-debug))

(use-package
 auto-fill
 :ensure nil
 :mode (("\\.md\\'" . auto-fill-mode) ("\\.tex\\'" . auto-fill-mode)))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook
 (emacs-lisp-mode . elisp-autofmt-mode)
 (before-save . elisp-autofmt-buffer))

;; END Programming Modes

;; BEGIN Productivity Tools

(use-package
 org
 :custom (org-hide-macro-markers 1)
 :config
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((shell . t) (python . t) (julia . t) (C . t) (latex . t)))
 :hook (org-mode . auto-fill-mode))

(use-package ob-async :after org)

(use-package
 evil-org
 :after org
 :hook (org-mode . (lambda () evil-org-mode))
 :config
 (require 'evil-org-agenda)
 (evil-org-agenda-set-keys))

;; END Productivity Tools

;; Ensure that everything loads before loading desktop mode
;; this prevents opening buffers before their major modes are available
(elpaca (general :wait t))

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
 ;; turns off elpaca-use-package-mode current declaration
 ;; will not be deferred
 :ensure nil

 :init
 (defun select-next-window ()
   "Switch to the next window"
   (interactive)
   (select-window (next-window)))

 (defun light-theme ()
   "Activate prefered light theme (modus operandi)"
   (interactive)
   (load-theme 'modus-operandi t))

 (defun dark-theme ()
   "Activate prefered light theme (modus operandi)"
   (interactive)
   (load-theme 'zenburn t))

 :bind
 ("C-x c" . comment-region)
 ("C-x C" . uncomment-region)
 ("M-`" . select-next-window)

 :hook (after-save . executable-make-buffer-file-executable-if-script-p)

 :config
 ;; Set default font face
 (set-face-attribute 'default nil :font "Ubuntu Mono")
 ;; Disable the menu bar
 (menu-bar-mode -1)
 ;; Disable the tool bar
 (tool-bar-mode -1)
 ;; Disable the scroll bars
 (scroll-bar-mode -1)
 ;; Enable ido mode
 (ido-mode t)
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

 :custom
 ;; Do not make backup ~ files
 (make-backup-files nil)
 ;; From my orig emacs config.. not sure why
 (window-resize-pixelwise t)
 (frame-resize-pixelwise t)
 ;; Use column numbers in modeline
 (column-number-mode t)
 ;; Disable splash screen
 (inhibit-startup-screen t)
 ;; No default scratch buffer message
 (initial-scratch-message "")
 ;; Configure ido
 (ido-ignore-extensions t)
 (ido-ignore-files '("__pycache__" "\\.egg-info\\'" "\\#.*#\\'"))
 (ido-ignore-buffers '("\\` " "\\*EGLOT .+? events\\*" "\\*Flymake log\\*"))
 ;; Initial major mode for the scratch buffer
 (initial-major-mode 'fundamental-mode)
 ;; fill-paragraph number of columns
 (fill-column 80)

 (safe-local-variable-values
  '((org-todo-keyword-faces
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


(provide 'init)
;;; init.el ends here
