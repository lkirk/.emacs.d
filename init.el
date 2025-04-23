;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs config

;;; Code:

(add-to-list 'trusted-content (concat user-emacs-directory "early-init.el"))
(seq-map
 (lambda (f) (add-to-list 'trusted-content f))
 (file-expand-wildcards (concat user-emacs-directory "lisp/*.el")))
(add-to-list 'trusted-content (concat user-emacs-directory "lisp/"))
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'load-elpaca)

;; enable elpaca get-user integration
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; globally add :ensure to use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'lk-look-and-feel)
(require 'lk-dev)
(require 'lk-prog)
(require 'lk-org)

;; Ensure that everything loads before loading desktop mode
;; this prevents opening buffers before their major modes are available
(elpaca (general :wait t))
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
(require 'lk-emacs)

(provide 'init)
;;; init.el ends here
