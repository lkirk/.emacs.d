;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Programming modes
;; NB: Packages that are included in Emacs are marked with :ensure nil

;;; Code:

;; elisp  =================================================================

(use-package
 flycheck-elsa
 :after elsa
 :hook (emacs-lisp-mode . flycheck-elsa-setup))

(use-package
 elisp-mode
 :ensure nil
 :config
 (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)
 (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)
 (when (boundp 'elisp-flymake-byte-compile-load-path)
   (add-to-list 'elisp-flymake-byte-compile-load-path user-emacs-directory)
   (add-to-list
    'elisp-flymake-byte-compile-load-path
    (concat user-emacs-directory "lisp/"))))

(use-package
 elisp-autofmt
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode))

;; C ======================================================================

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

;; julia ==================================================================

(use-package
 julia-mode
 :mode ("\\.jl\\'" . julia-mode)
 :hook (julia-mode . eglot-jl-init)
 :custom (tab-width 4)
 (eglot-jl-julia-flags
  `(,(when-let* ((si (expand-file-name "~/.julia/sysimage/EglotJl.1.so"))
                 ((file-exists-p si)))
       (format "--sysimage=%s" si)))))

(use-package
 eglot-jl
 :after eglot
 :commands eglot-jl-init
 :custom (tab-width 4))

(use-package
 julia-snail
 :init
 (defun lk/julia-sysimage-args (files)
   (seq-map
    (lambda (s) (format "--sysimage=%s" s))
    (seq-filter #'file-exists-p (seq-map #'expand-file-name files))))
 :hook (julia-mode . julia-snail-mode)
 :ensure (:repo "/home/lkirk/repo/ob-julia-snail")
 :custom
 (julia-snail-terminal-type :eat)
 (julia-snail-use-emoji-mode-lighter nil)
 :config
 ;; snail-extensions is a buffer-local var, so must setq-default for global
 ;; (setq-default julia-snail-extensions '(ob-julia))
 (setq-default julia-snail-repl-display-eval-results t)
 (setq-default julia-snail-extra-args
               `("--threads=auto,auto" "-q" "--project=@." ,@
                 (lk/julia-sysimage-args
                  '("~/.julia/sysimage/JuliaSnail.1.so"
                    "~/.julia/sysimage/LkTools.1.so"))))

 ;; disable features that overlap eglot and corfu
 (remove-hook 'completion-at-point-functions #'julia-snail-company-capf t)
 (remove-hook
  'completion-at-point-functions #'julia-snail-repl-completion-at-point
  t)
 (remove-function (local 'eldoc-documentation-function) #'julia-snail-eldoc)
 (remove-hook 'xref-backend-functions #'julia-snail-xref-backend t)
 ;; ;; some hacky overrides for now.
 (load-file "~/repo/ob-julia-snail/ob-julia-snail/ob-julia-snail.el"))

;; R ======================================================================

(use-package
 ess-r-mode
 :bind
 (:map ess-r-mode-map ("_" . ess-insert-assign))
 (:map inferior-ess-r-mode-map ("_" . ess-insert-assign))
 :ensure nil
 :custom (inferior-R-args "--no-restore-history --no-save "))

;; haskell ================================================================

(defun lk/haskell-program-name-with-args-advice (orig-fn)
  "If `flake.nix' exists in project root, use nix develop ghci.
otherwise call ORIG-FN.  TODO: add a dir-locals flag to trigger this?"
  (if-let ((flake-nix (concat (project-root (project-current)) "flake.nix"))
           ((file-exists-p flake-nix)))
    '("nix" "develop" "-c" "ghci")
    (funcall orig-fn)))

(advice-add
 'haskell-program-name-with-args
 :around #'lk/haskell-program-name-with-args-advice)

(use-package haskell-mode)

;; systemd ================================================================

(use-package
 systemd
 :mode (("\\.service\\'" . systemd-mode) ("\\.target\\'" . systemd-mode)))

;; python =================================================================

(use-package
 python
 :ensure nil
 :config
 (defun insert-ipython-debug ()
   "Insert an interactive debug at point."
   (interactive)
   (insert "__import__(\"IPython\").embed()"))
 :bind (:map python-mode-map ("C-c p" . insert-ipython-debug)))

(use-package
 ruff-format
 :diminish
 :hook (python-mode . ruff-format-on-save-mode))
(use-package sphinx-doc :hook (python-mode . sphinx-doc-mode))

;; TODO: until we can do this with ruff
(use-package isortify :diminish :hook (python-mode . isortify-mode))

;; markdown formats =======================================================

(use-package
 yaml-mode
 :mode (("\\.yml\\'" . yaml-mode) ("\\clang-format\\'" . yaml-mode)))

(use-package
 markdown-mode
 :mode ("\\.md\\'" . markdown-mode)
 :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5"))
 :hook (markdown-mode . auto-fill-mode))

;; containers =============================================================

(use-package dockerfile-mode)

(use-package
 apptainer-mode
 :ensure (:host github :repo "jrgant/apptainer-mode" :tag "v0.3")
 :mode ("\\.def\\'" . apptainer-mode))

;; wolfram ================================================================

(use-package
 wolfram-mode
 :mode "\\.\\(wl\\|wls\\|m\\)\\'"
 :custom
 (tab-width 4)
 (wolfram-indent 4))

;; LaTeX ==================================================================

(use-package
 tex
 :ensure auctex
 :custom (TeX-parse-self t) (font-latex-fontify-script nil)
 ;; do not alter font size of sections
 (font-latex-fontify-sectioning 'color)
 :hook (LaTeX-mode . auto-fill-mode) (LaTeX-mode . flyspell-mode)
 (LaTeX-mode
  .
  (lambda () (set (make-local-variable 'TeX-electric-math) (cons "\\(" "\\)"))))
 (plain-TeX-mode
  . (lambda () (set (make-local-variable 'TeX-electric-math) (cons "$" "$")))))

(use-package auto-fill :ensure nil :mode (("\\.md\\'" . auto-fill-mode)))

(use-package typescript-mode)

(use-package meson-mode) ;; :hook (meson-mode . company-mode))
;; (use-package cmake-mode)
(use-package nix-mode :mode "\\.nix\\'")

(use-package clojure-mode :defer t)
(use-package cider :defer t)

(use-package geiser)
(use-package geiser-chez :custom (geiser-chez-binary "chez"))
(use-package geiser-guile)

(use-package fish-mode)
(use-package zig-mode)

(provide 'lk-prog)
;;; lk-prog.el ends here
