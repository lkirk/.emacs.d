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
 :config
 :custom (corfu-auto t) (corfu-quit-no-match 'separator)
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
 :init (declare-function global-corfu-mode "corfu") (global-corfu-mode))

;; Add extensions
(use-package
 cape
 ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
 ;; Press C-c p ? to for help.
 ;; :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
 :bind ("C-<tab>" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+
 ;; Alternatively bind Cape commands individually.
 ;; :bind (("C-c p d" . cape-dabbrev)
 ;;        ("C-c p h" . cape-history)
 ;;        ("C-c p f" . cape-file)
 ;;        ...)
 :init
 ;; Add to the global default value of `completion-at-point-functions' which is
 ;; used by `completion-at-point'.  The order of the functions matters, the
 ;; first function returning a result wins.  Note that the list of buffer-local
 ;; completion functions takes precedence over the global list.
 (add-hook 'completion-at-point-functions #'cape-tex)
 (add-hook 'completion-at-point-functions #'cape-file)
 (add-hook 'completion-at-point-functions #'cape-keyword)
 (add-hook 'completion-at-point-functions #'cape-history))
;; (add-hook 'completion-at-point-functions #'cape-elisp-symbol)
;; (add-hook 'completion-at-point-functions #'cape-elisp-block))
;; (add-hook 'completion-at-point-functions #'cape-history)
;; ...

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

;; (use-package  ;; interacts with julia org babel
;;  eat
;;  :custom (eat-kill-buffer-on-exit t)
;;  :hook (eshell-load . eat-eshell-mode))


(use-package
 flymake
 :ensure nil
 :hook (prog-mode . flymake-mode)
 :custom (flymake-indicator-type 'margins) (flymake-autoresize-margins nil)
 (flymake-margin-indicators-string
  '((error "!" compilation-error)
    (warning "W" compilation-warning)
    (note "I" compilation-info)))
 :bind
 (:map
  flymake-mode-map
  ("C-c n" . flymake-goto-next-error)
  ("C-c N" . flymake-goto-prev-error))
 :config
 (put
  'flymake-error
  'flymake-margin-string
  (alist-get 'error flymake-margin-indicators-string))
 (put
  'flymake-warning
  'flymake-margin-string
  (alist-get 'warning flymake-margin-indicators-string))
 (put
  'flymake-note
  'flymake-margin-string
  (alist-get 'note flymake-margin-indicators-string)))

(defun lk/filter-plist (lst)
  "Filters all plist items from LST.

For example (a b :foo v1 :bar v2 c :baz v3 d) => (a b c d)"
  (let ((last-was-kw nil))
    (seq-mapcat
     (lambda (elm)
       (cond
        ((keywordp elm)
         (setq last-was-kw t)
         nil)
        (last-was-kw
         (setq last-was-kw nil)
         nil)
        (t
         (list elm))))
     lst)))

;; (declare-function eglot--guess-contact "eglot")
;; (defun lk/eglot-ensure ()
;;   (require 'eglot)
;;   (when (nth 3 (eglot--guess-contact))
;;     (eglot-ensure)))
;; (defun lk/eglot-ensure ()
;;   "Run eglot if the current major mode supports it."
;;   (let ((supported-modes
;;          (seq-mapcat
;;           (lambda (l)
;;             (if (listp l)
;;                 (lk/filter-plist l)
;;               (ensure-list l)))
;;           (seq-mapcat
;;            (lambda (l) (ensure-list (car l))) eglot-server-programs))))
;;     (when (seq-contains-p supported-modes `,major-mode)
;;       (eglot-ensure))))

;; (defun lk/eglot-supports (server-progs)
;;   "Run eglot if the current major mode supports it, as defined in SERVER-PROGS."
;;   (let ((supported-modes
;;          (seq-mapcat
;;           (lambda (l)
;;             (if (listp l)
;;                 (lk/filter-plist l)
;;               (ensure-list l)))
;;           (seq-mapcat (lambda (l) (ensure-list (car l))) server-progs))))
;;     ;; for some reason I can't match?
;;     (seq-contains-p supported-modes `,major-mode)))
;; (advice-add
;;  'eglot-ensure
;;  :around
;;  (lambda (orig &rest _args)
;;    (when (lk/eglot-supports eglot-server-programs)
;;      (funcall orig))))

(defun lk/format-on-save ()
  "Format buffer if not in `cc-mode`."
  ;; Disable so that I can use clang-format in tskit development
  (unless (or (string-match "repo/tskit" buffer-file-name)
              (derived-mode-p 'wolfram-mode))
    (when (eglot-managed-p) ;; prevent jsonrpc errors
      (eglot-format))))

(use-package
 eglot
 :init
 (declare-function eglot-managed-p "eglot")
 (declare-function eglot-format "eglot")
 :ensure t
 :hook (before-save . lk/format-on-save)
 ;; (prog-mode . lk/eglot-ensure)
 (prog-mode . eglot-ensure)
 :custom
 (eglot-autoshutdown 1)
 (eglot-report-progress nil)
 (eglot-extend-to-xref 1)
 :config
 (add-to-list 'eglot-server-programs '(fish-mode . ("fish-lsp" "start")))
 (add-to-list 'eglot-server-programs '(awk-mode . ("awk-language-server")))
 (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab")))

 (let ((mode '((wolfram-mode :language-id "Wolfram Language")))
       ;; (wolfram-lsp-cmd '("wolframscript" "-code"
       ;;                    "Needs[\"LSPServer`\"];LSPServer`StartServer[]")))
       (wolfram-lsp-cmd
        '("WolframKernel"
          "-noinit"
          "-noprompt"
          "-nopaclet"
          "-noicon"
          "-nostartuppaclets"
          "-run"
          "Needs[\"LSPServer`\"];LSPServer`StartServer[]")))
   (add-to-list 'eglot-server-programs (cons mode wolfram-lsp-cmd)))

 (setq-default eglot-workspace-configuration
               '(:texlab
                 (:experimental
                  (:verbatimEnvironments ["julia"])
                  :auxDirectory "./build"
                  :chktex (:onEdit t :onOpenAndSave t)
                  :formatterLineLength 80
                  :latexFormatter "latexindent"
                  :latexindent (:modifyLineBreaks t))))
 :bind
 (:map
  eglot-mode-map
  ("C-c C-j" . xref-find-definitions)
  ("C-c C-k" . xref-find-references)))

(use-package rainbow-mode)

(provide 'lk-dev)
;;; lk-dev.el ends here
