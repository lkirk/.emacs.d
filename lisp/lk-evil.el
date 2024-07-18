;;; lk-evil.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;  Evil mode settings

;;; Code:

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

(provide 'lk-evil)
;;; lk-evil.el ends here
