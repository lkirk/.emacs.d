;; I tried using lsp-mode with polymode, but it was slow and buggy

;; ;; may have to use this:
;; ;; https://github.com/dankessler/polymode/tree/develop
;; ;; https://github.com/polymode/polymode/issues/305#issuecomment-2453106460
;; (use-package
;;  polymode
;;  :ensure
;;  (:host
;;   github
;;   :repo "dankessler/polymode"
;;   :ref "c87ad4a9b3410cc04be4bd4afc39d93d85178d67"))
;; ;; :after eglot
;; ;; :config
;; ;; (add-to-list
;; ;;  'polymode-run-these-after-change-functions-in-other-buffers
;; ;;  'eglot--after-change)
;; ;; (add-to-list
;; ;;  'polymode-run-these-before-change-functions-in-other-buffers
;; ;;  'eglot--before-change))
;; (use-package poly-org)
;; (use-package poly-R)
;; (use-package poly-markdown)
;; (use-package poly-noweb)

;; (use-package
;;  flycheck
;;  :config (global-flycheck-mode)
;;  :bind
;;  (:map
;;   flycheck-mode-map
;;   ("C-c n" . flycheck-next-error)
;;   ("C-c N" . flycheck-previous-error)))

;; ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;; (use-package
;;  lsp-mode
;;  :commands (lsp lsp-deferred)
;;  :hook
;;  ((prog-mode . lsp-deferred) (lsp-mode . lsp-enable-which-key-integration))
;;  :custom
;;  (lsp-headerline-breadcrumb-enable nil)
;;  (lsp-enable-folding nil)
;;  (lsp-enable-links nil)
;;  (lsp-enable-snippet nil)
;;  (lsp-warn-no-matched-clients nil)
;;  (lsp-keep-workspace-alive nil)
;;  :bind
;;  (:map
;;   lsp-mode-map
;;   ("C-c C-j" . xref-find-definitions)
;;   ("C-c C-k" . xref-find-references))
;;  :hook
;;  (before-save . lsp-format-buffer)
;;  (before-save . lsp-organize-imports))

;; (use-package
;;  lsp-pyright
;;  :custom (lsp-pyright-langserver-command "basedpyright")
;;  :hook (python-mode . (lambda () (require 'lsp-pyright))))
