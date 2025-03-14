;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Programming modes
;; NB: Packages that are included in Emacs are marked with :ensure nil

;;; Code:

(use-package
 elisp-mode
 :ensure nil
 :config
 (when (boundp 'elisp-flymake-byte-compile-load-path)
   (add-to-list 'elisp-flymake-byte-compile-load-path load-path)))

(use-package
 cc-mode
 :ensure nil
 :hook
 (c-mode
  .
  (lambda ()
    (setq comment-start "//")
    (setq comment-end "")))
 :custom (indent-tabs-mode nil) (c-basic-offset 4))

;; Need this for tskit development. Uses an older version of clang-format
;; (set via dir-locals.el)
(use-package clang-format)

(use-package rust-mode :custom (rust-mode-treesitter-derive t))

(use-package
 julia-mode
 :mode ("\\.jl\\'" . julia-mode)
 :hook (julia-mode . eglot-jl-init)
 :custom (tab-width 4))

(use-package
 julia-repl
 :hook
 (julia-mode . julia-repl-mode)
 (julia-repl . undo-tree-mode)
 :config (julia-repl-set-terminal-backend 'eat))

(use-package
 eat
 :custom (eat-kill-buffer-on-exit t)
 :hook (eshell-load . eat-eshell-mode))

(use-package
 eglot-jl
 :after eglot
 :commands eglot-jl-init
 :custom (tab-width 4))

(use-package
 systemd
 :mode (("\\.service\\'" . systemd-mode) ("\\.target\\'" . systemd-mode)))

(use-package haskell-mode)

(use-package meson-mode :hook (meson-mode . company-mode))
;; (use-package cmake-mode)

(use-package nix-mode :mode "\\.nix\\'")

(use-package eldoc :diminish eldoc-mode :ensure nil :config (global-eldoc-mode))

(use-package
 flymake
 :ensure nil
 :hook (prog-mode . flymake-mode)
 :bind
 (:map
  flymake-mode-map
  ("C-c n" . flymake-goto-next-error)
  ("C-c N" . flymake-goto-prev-error)))

(use-package
 eglot
 :init
 (defun my/format-on-save ()
   "Format buffer if not in `cc-mode`."
   ;; Disable so that I can use clang-format in tskit development
   (unless (or (string-match "repo/tskit" buffer-file-name)
               (derived-mode-p 'wolfram-mode))
     (eglot-format)))
 :ensure nil
 :hook
 (prog-mode . eglot-ensure)
 (before-save . my/format-on-save)
 :custom
 (eglot-autoshutdown 1)
 (eglot-report-progress nil)
 :config
 (add-to-list 'eglot-server-programs '(fish-mode . ("fish-lsp" "start")))
 (add-to-list 'eglot-server-programs '(awk-mode . ("awk-language-server")))
 (add-to-list 'eglot-server-programs '(LaTeX-mode . ("texlab")))
 (let ((mode '(wolfram-mode :language-id "Wolfram Language"))
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

(use-package
 ruff-format
 :diminish
 :hook (python-mode . ruff-format-on-save-mode))

;; TODO: until we can do this with ruff
(use-package isortify :diminish :hook (python-mode . isortify-mode))

(use-package company :diminish :init (global-company-mode t))

(use-package
 yaml-mode
 :mode (("\\.yml\\'" . yaml-mode) ("\\clang-format\\'" . yaml-mode)))

(use-package
 markdown-mode
 :mode ("\\.md\\'" . markdown-mode)
 :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
 :hook (markdown-mode . auto-fill-mode))

(use-package dockerfile-mode)

(use-package
 apptainer-mode
 :ensure (:host github :repo "jrgant/apptainer-mode" :tag "v0.3")
 :mode ("\\.def\\'" . apptainer-mode))

(use-package
 wolfram-mode
 :mode "\\.\\(wl\\|wls\\|m\\)\\'"
 :custom
 (tab-width 4)
 (wolfram-indent 4))

(use-package
 tex
 :ensure auctex
 :custom
 (TeX-parse-self t)
 (font-latex-fontify-script nil)
 ;; (TeX-electric-math (cons "\\(" "\\)"))
 :hook
 (LaTeX-mode . eglot-ensure)
 (LaTeX-mode . auto-fill-mode)
 (LaTeX-mode . flyspell-mode)
 (LaTeX-mode
  .
  (lambda () (set (make-local-variable 'TeX-electric-math) (cons "\\(" "\\)"))))
 (plain-TeX-mode
  . (lambda () (set (make-local-variable 'TeX-electric-math) (cons "$" "$")))))

(use-package sphinx-doc :hook (python-mode . sphinx-doc-mode))
;; (use-package gendoxy)

;; Bind key sequence to the function
(use-package
 python
 :ensure nil
 :config
 (defun insert-ipython-debug ()
   "Insert an interactive debug at point."
   (interactive)
   (insert "__import__(\"IPython\").embed()"))
 :bind ("C-c p" . insert-ipython-debug))

(use-package auto-fill :ensure nil :mode (("\\.md\\'" . auto-fill-mode)))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package typescript-mode)

(use-package ess :defer t :ensure t)

(use-package
 ess-r-mode
 :bind
 (:map ess-r-mode-map ("_" . ess-insert-assign))
 (:map inferior-ess-r-mode-map ("_" . ess-insert-assign))
 :ensure nil
 :custom (inferior-R-args "--no-restore-history --no-save "))

(use-package clojure-mode :defer t)
(use-package cider :defer t)

(use-package geiser)
(use-package geiser-chez :custom (geiser-chez-binary "chez"))
(use-package geiser-guile)

(use-package fish-mode)
(use-package zig-mode)

(provide 'lk-prog)
;;; lk-prog.el ends here
