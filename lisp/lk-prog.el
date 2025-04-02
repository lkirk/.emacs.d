;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Programming modes
;; NB: Packages that are included in Emacs are marked with :ensure nil

;;; Code:

;; (use-package
;;  elisp-mode
;;  :ensure nil
;;  :config
;;  (when (boundp 'elisp-flymake-byte-compile-load-path)
;;    (add-to-list 'elisp-flymake-byte-compile-load-path load-path)))

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

(use-package ess)

(use-package
 julia-mode
 :after ess
 :mode ("\\.jl\\'" . julia-mode)
 :hook (julia-mode . eglot-jl-init)
 :custom (tab-width 4))

;; (use-package
;;  julia-repl
;;  :hook
;;  (julia-mode . julia-repl-mode)
;;  (julia-repl . undo-tree-mode)
;;  :config (julia-repl-set-terminal-backend 'eat))

;; (use-package  ;; interacts with julia org babel
;;  eat
;;  :custom (eat-kill-buffer-on-exit t)
;;  :hook (eshell-load . eat-eshell-mode))

(use-package
 eglot-jl
 :after eglot
 :commands eglot-jl-init
 :custom (tab-width 4))

(use-package julia-vterm)
;; :custom (julia-vterm-repl-program "julia --project=@."))

(use-package
 ob-julia-vterm
 :after org
 :config (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
 (defalias
   'org-babel-variable-assignments:julia
   'org-babel-variable-assignments:julia-vterm))

;; (use-package
;;  ess-julia-mode
;;  :ensure nil
;;  :custom (inferior-julia-args "--project=@. "))

;; (use-package lsp-julia)

(use-package
 systemd
 :mode (("\\.service\\'" . systemd-mode) ("\\.target\\'" . systemd-mode)))

(use-package haskell-mode)

(use-package meson-mode :hook (meson-mode . company-mode))
;; (use-package cmake-mode)

(use-package nix-mode :mode "\\.nix\\'")

(use-package eldoc :diminish eldoc-mode :ensure nil :config (global-eldoc-mode))

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
 :custom (TeX-parse-self t) (font-latex-fontify-script nil)
 ;; (TeX-electric-math (cons "\\(" "\\)"))
 :hook
 ;; (LaTeX-mode . eglot-ensure)
 (LaTeX-mode . auto-fill-mode) (LaTeX-mode . flyspell-mode)
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
