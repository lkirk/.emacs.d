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
(require 'lk-load-elpaca)

;; enable elpaca get-user integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; globally add :ensure to use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'lk-themes)
(require 'lk-evil)
(require 'lk-lint-lsp)
(require 'lk-lang)
(require 'lk-general)

(provide 'init)
;;; init.el ends here
