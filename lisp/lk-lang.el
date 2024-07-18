;;; lk-lang.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;  Language specific settings

;;; Code:

;; Python

(use-package flymake-ruff
  :hook
  (eglot-managed-mode . flymake-ruff-load))

(use-package ruff-format
  :hook
  (python-mode . ruff-format-on-save-mode))

(use-package python
  :ensure nil
  :config
  (defun insert-ipython-debug()
    "Insert an interactive debug at point."
    (interactive)
    (insert "__import__(\"IPython\").embed()"))
  :bind
  ("C-c p" . insert-ipython-debug))

;; LaTeX
(use-package tex
  :ensure nil
  :hook
  (latex-mode . auto-fill-mode))

;; building is complicated
;; (use-package tex
;;   :ensure auctex)

(provide 'lk-lang)
;;; lk-lang.el ends here
