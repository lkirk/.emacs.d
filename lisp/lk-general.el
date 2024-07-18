;;; lk-general.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;  General Emacs settings.  This uses the pseudo-package provided
;;  by elpaca to perform configuration.

;; pseudo-package for configuring built-in Emacs functionality
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

;;; Code:

(provide 'lk-general)
;;; lk-general.el ends here
