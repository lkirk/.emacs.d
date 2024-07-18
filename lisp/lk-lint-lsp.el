;;; lk-lint-lsp.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;  Linting and lsp settings for general linting functionality
;;  and completion.  See lang.el for the language specific bits

;;; Code:
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

(use-package company
  :init
  (global-company-mode t))

(provide 'lk-lint-lsp)
;;; lk-lint-lsp.el ends here
