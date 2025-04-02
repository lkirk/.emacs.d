;;; lk-dev.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Miscellaneous development tools

;;; Code:

(use-package transient)
(use-package
 magit
 :after transient ;; magit requires a newer version of transient
 :commands magit-status magit-blame
 :hook (with-editor-mode evil-insert-state)
 :custom (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Vertico + Consult + Orderless + Embark + Marginalia + Corfu ???
(use-package prescient)
(use-package vertico-prescient)

(use-package
 corfu
 ;; Optional customizations
 ;; :custom
 ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
 ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
 ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
 ;; (corfu-preview-current nil)    ;; Disable current candidate preview
 ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
 ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

 ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
 ;; :hook ((prog-mode . corfu-mode)
 ;;        (shell-mode . corfu-mode)
 ;;        (eshell-mode . corfu-mode))

 ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
 ;; be used globally (M-/).  See also the customization variable
 ;; `global-corfu-modes' to exclude certain modes.
 :init (global-corfu-mode))

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

(use-package
 flycheck
 :config (global-flycheck-mode)
 :bind
 (:map
  flycheck-mode-map
  ("C-c n" . flycheck-next-error)
  ("C-c N" . flycheck-previous-error)))

;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
(use-package
 lsp-mode
 :commands (lsp lsp-deferred)
 :hook
 ((prog-mode . lsp-deferred) (lsp-mode . lsp-enable-which-key-integration))
 :custom
 (lsp-headerline-breadcrumb-enable nil)
 (lsp-enable-folding nil)
 (lsp-enable-links nil)
 (lsp-enable-snippet nil)
 (lsp-warn-no-matched-clients nil)
 (lsp-keep-workspace-alive nil)
 :bind
 (:map
  lsp-mode-map
  ("C-c C-j" . xref-find-definitions)
  ("C-c C-k" . xref-find-references))
 :hook
 (before-save . lsp-format-buffer)
 (before-save . lsp-organize-imports))

(use-package
 lsp-pyright
 :custom (lsp-pyright-langserver-command "basedpyright")
 :hook (python-mode . (lambda () (require 'lsp-pyright))))

(use-package rainbow-mode)

(provide 'lk-dev)
;;; lk-dev.el ends here
