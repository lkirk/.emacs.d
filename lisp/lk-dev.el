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

;; (declare-function corfu-prescient-mode "corfu")
;; (declare-function global-corfu-mode "corfu")
;; (declare-function cape-wrap-buster "cape")

;; (use-package
;;  corfu
;;  ;; :after (eglot cape prescient)
;;  :after (eglot prescient)
;;  :custom (corfu-auto t)
;;  ;; (corfu-quit-no-match 'separator "Quit at separator if no match")
;;  (corfu-quit-no-match nil)
;;  (corfu-cycle t "Enable cycling for `corfu-next/previous'") ;; 
;;  (corfu-preselect nil "Do not preselect prompt")
;;  ;; :bind (:map corfu-mode-map ("TAB" . #'corfu-complete))
;;  (corfu-prescient-mode 1)
;;  :config (global-corfu-mode))

(use-package company :hook (after-init . global-company-mode))

;; (use-package
;;  cape
;;  :bind ("C-c p" . cape-prefix-map)
;;  :init
;;  (add-hook 'completion-at-point-functions #'cape-tex)
;;  (add-hook 'completion-at-point-functions #'cape-file)
;;  (add-hook 'completion-at-point-functions #'cape-keyword)
;;  (add-hook 'completion-at-point-functions #'cape-history))

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

(use-package
 eglot
 :init
 (declare-function eglot-managed-p "eglot")
 (declare-function eglot-format "eglot")
 (defun lk/format-on-save ()
   "Format buffer if not in `cc-mode`."
   ;; Disable so that I can use clang-format in tskit development
   (unless (or (string-match "repo/tskit" buffer-file-name)
               (derived-mode-p 'wolfram-mode))
     (when (eglot-managed-p) ;; prevent jsonrpc errors
       (eglot-format))))
 :ensure t
 :hook (before-save . lk/format-on-save)
 ;; (prog-mode . lk/eglot-ensure)
 (prog-mode . eglot-ensure)
 :custom
 (eglot-autoshutdown 1)
 (eglot-report-progress nil)
 (eglot-extend-to-xref 1)
 :config
 ;; Enable cache busting, depending on if your server returns
 ;; sufficiently many candidates in the first place.
 (with-eval-after-load 'eglot
   (setq completion-category-defaults nil))
 (add-to-list 'eglot-server-programs '(vala-mode . ("vala-language-server")))
 (add-to-list 'eglot-server-programs '(meson-mode . ("mesonlsp")))
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

(use-package eldoc :diminish eldoc-mode :ensure nil :config (global-eldoc-mode))

(provide 'lk-dev)
;;; lk-dev.el ends here
