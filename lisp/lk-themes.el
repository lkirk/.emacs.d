;;; lk-themes.el --- -*- lexical-binding: t; -*-

;;; Commentary:
;;  Install custom themes

;;; Code:

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; preferred light theme
(use-package modus-themes)

(provide 'lk-themes)
;;; lk-themes.el ends here
