;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs config

;;; Code:

;; (set-face-attribute 'default nil :family "Ubuntu Mono" :height 105)
(set-face-attribute 'default nil :font "Noto Sans Mono" :height 120)

;; trusted lisp content
(add-to-list 'trusted-content (concat user-emacs-directory "early-init.el"))
(seq-map
 (lambda (f) (add-to-list 'trusted-content f))
 (file-expand-wildcards (concat user-emacs-directory "lisp/*.el")))
(add-to-list 'trusted-content (concat user-emacs-directory "lisp/"))

;; add lisp modules to load path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; load the package manager
(require 'load-elpaca)

;; enable elpaca get-user integration
(elpaca elpaca-use-package (elpaca-use-package-mode))

;; globally add :ensure to use-package
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; load lisp modules
(require 'lk-look-and-feel)
(require 'lk-dev)
(require 'lk-prog)
(require 'lk-org)

;; Ensure that everything loads before loading desktop mode
;; this prevents opening buffers before their major modes are available
(elpaca (general :wait t))

;; globally prevent flymake from attempting to load legacy procs.
;; (leads to tons of warnings)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
(require 'lk-emacs)

(provide 'init)
;;; init.el ends here
