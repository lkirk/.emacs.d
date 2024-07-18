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

;; enable elpaca get-user integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; globally add :ensure to use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package evil
  :after undo-tree
  :config
  (evil-set-undo-system 'undo-tree)
  (evil-mode t)
  :custom
  (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))))

;; preferred light theme
(use-package modus-themes)

;; Packages that are included in emacs are marked with :ensure nil

(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode))

(use-package flymake
  ;; :diminish -- seems like a cool feature, need to look into it
  :ensure nil
  :hook
  (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
	      ("C-c n" . flymake-goto-next-error)
	      ("C-c N" . flymake-goto-prev-error)))

(use-package eglot
  :ensure nil
  :hook
  (prog-mode . eglot-ensure)
  (before-save . eglot-format)
  :custom
  (eglot-autoshutdown 1)
  :bind (:map eglot-mode-map
	      ("C-c C-j" . xref-find-definitions)
	      ("C-c C-k" . xref-find-references)))

(use-package flymake-ruff
  :hook
  (eglot-managed-mode . flymake-ruff-load))

(use-package ruff-format
  :hook
  (python-mode . ruff-format-on-save-mode))

(use-package company
  :init
  (global-company-mode t))

(use-package tex
  :ensure nil
  :hook
  (latex-mode . auto-fill-mode))

;; building is complicated
;; (use-package tex
;;   :ensure auctex)

;; Bind key sequence to the function
(use-package python
  :ensure nil
  :config
  (defun insert-ipython-debug()
    "Insert an interactive debug at point."
    (interactive)
    (insert "__import__(\"IPython\").embed()"))
  :bind
  ("C-c p" . insert-ipython-debug))

;; pseudo-package for configuring built-in emacs functionality
(use-package emacs
  ;; turns off elpaca-use-package-mode current declaration
  ;; will not be deferred
  :ensure nil

  :init
  (defun select-next-window()
    "Switch to the next window"
    (interactive)
    (select-window (next-window)))

  (defun light-theme()
    "Activate prefered light theme (modus operandi)"
    (interactive)
    (load-theme 'modus-operandi t))

  (defun dark-theme()
    "Activate prefered light theme (modus operandi)"
    (interactive)
    (load-theme 'zenburn t))

  :bind
  ("C-x c" . comment-region)
  ("C-x C" . uncomment-region)
  ("M-`" . select-next-window)

  :config
  ;; Set default font face
  (set-face-attribute 'default nil :font "Ubuntu Mono")
  ;; Disable the menu bar
  (menu-bar-mode -1)
  ;; Disable the tool bar
  (tool-bar-mode -1)
  ;; Disable the scroll bars
  (scroll-bar-mode -1)
  ;; Fix issue with backspace on remote servers over ssh
  (normal-erase-is-backspace-mode 1)
  ;; Enable ido mode
  (ido-mode t)
  ;; Save the history across sessions as much as possible
  (save-place-mode t)
  (savehist-mode t)
  (recentf-mode t)
  ;; y or n instead of typing yes or no
  (defalias 'yes-or-no #'y-or-n-p)
  ;; Turn off fringe
  (fringe-mode -1)
  (desktop-save-mode 1)
  ;; Until I make eidos-mode a proper package, load here
  (let ((eidos-mode-file "~/repo/eidos-mode/eidos-mode.el"))
    (if (file-exists-p eidos-mode-file)
	(progn
          (load eidos-mode-file)
          (add-to-list 'auto-mode-alist '("\\.slim\\'" . eidos-mode)))))

  :custom
  ;; desktop mode settings
  (desktop-dirname "~/.emacs.d/desktop/")
  (desktop-base-file-name "emacs.desktop")
  (desktop-base-lock-name "emacs.desktop.lock")
  (desktop-path (list desktop-dirname))
  (desktop-save t)
  (desktop-load-locked-desktop nil)
  (desktop-auto-save-timeout 30)
  (desktop-save-mode 1)

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
  (ido-ignore-directories '("__pycache__"))
  (ido-ignore-files '("__pycache__"))
  ;; Initial major mode for the scratch buffer
  (initial-major-mode 'fundamental-mode)
  ;; fill-paragraph number of columns
  (fill-column 80))

(provide 'init)
;;; init.el ends here
