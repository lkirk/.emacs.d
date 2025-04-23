;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Programming modes
;; NB: Packages that are included in Emacs are marked with :ensure nil

;;; Code:

;; (when (boundp 'elisp-flymake-byte-compile-load-path)
;;   (add-to-list 'elisp-flymake-byte-compile-load-path load-path)))

(use-package
 flycheck-elsa
 :after elsa
 :hook (emacs-lisp-mode . flycheck-elsa-setup))

(use-package
 elisp-mode
 :ensure nil
 :config
 (when (boundp 'elisp-flymake-byte-compile-load-path)
   (add-to-list 'elisp-flymake-byte-compile-load-path user-emacs-directory)
   (add-to-list
    'elisp-flymake-byte-compile-load-path
    (concat user-emacs-directory "lisp/"))))

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

;; (use-package
;;  ess
;;  :custom (inferior-julia-args "--threads=auto,auto -q --project=@."))

(use-package
 julia-mode
 ;; :after ess
 :mode ("\\.jl\\'" . julia-mode)
 :hook (julia-mode . eglot-jl-init)
 :custom (tab-width 4)
 (eglot-jl-julia-flags
  `(,(when-let* ((si (expand-file-name "~/.julia/sysimage/EglotJl.1.so"))
                 ((file-exists-p si)))
       (format "--sysimage=%s" si)))))

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

;;(use-package
;; julia-vterm
;; :custom (julia-vterm-repl-program "julia --project=@."))

;; (use-package
;;  ob-julia-vterm
;;  :after org
;;  :config (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;;  (defalias
;;    'org-babel-variable-assignments:julia
;;    'org-babel-variable-assignments:julia-vterm))

(declare-function eat-self-input "eat")
(declare-function evil-insert-state "evil-states")
(declare-function evil-local-set-key "evil-core")
(defun lk/eat-input-kbd (str)
  "Send a kbd STR to eat."
  (seq-do (lambda (c) (eat-self-input 1 c)) (listify-key-sequence (kbd str))))

;; TODO: better names, reflecting the original evil funcs.
(defun lk/eat-forward-word ()
  "Vim w."
  (interactive)
  (lk/eat-input-kbd "M-f"))
(defun lk/eat-backward-word ()
  "Vim b."
  (interactive)
  (lk/eat-input-kbd "M-b"))
(defun lk/eat-edit-new-line ()
  "Vim o."
  (interactive)
  (lk/eat-input-kbd "C-e C-M-j")
  (evil-insert-state))
(defun lk/eat-kill-line ()
  "Vim D."
  (interactive)
  (lk/eat-input-kbd "C-k"))
(defun lk/eat-delete-line ()
  "Vim dd."
  (interactive)
  (lk/eat-input-kbd "C-a C-k"))
(defun lk/eat-undo ()
  "Vim u."
  (interactive)
  (lk/eat-input-kbd "C-_"))
(defun lk/eat-insert-at-end ()
  "Vim A."
  (interactive)
  (lk/eat-input-kbd "C-e")
  (evil-insert-state))
(defun lk/eat-up ()
  "Vim k."
  (interactive)
  (lk/eat-input-kbd "<up>"))
(defun lk/eat-down ()
  "Vim j."
  (interactive)
  (lk/eat-input-kbd "<down>"))
(defun lk/eat-left ()
  "Vim h."
  (interactive)
  (lk/eat-input-kbd "<left>"))
(defun lk/eat-right ()
  "Vim l."
  (interactive)
  (lk/eat-input-kbd "<right>"))
(defun lk/eat-yank ()
  "Vim p."
  (interactive)
  (lk/eat-input-kbd "C-y"))

(use-package
 eat
 :after evil
 :custom (eat-kill-buffer-on-exit t)
 :hook (eshell-load . eat-eshell-mode)
 (eat-mode
  .
  (lambda ()
    ;; TODO: only define these for julia-snail-repl-mode-map
    (evil-local-set-key 'normal (kbd "k") 'lk/eat-up)
    (evil-local-set-key 'normal (kbd "j") 'lk/eat-down)
    (evil-local-set-key 'normal (kbd "h") 'lk/eat-left)
    (evil-local-set-key 'normal (kbd "l") 'lk/eat-right)
    (evil-local-set-key 'normal (kbd "w") 'lk/eat-forward-word)
    (evil-local-set-key 'normal (kbd "b") 'lk/eat-backward-word)
    (evil-local-set-key 'normal (kbd "o") 'lk/eat-edit-new-line)
    (evil-local-set-key 'normal (kbd "D") 'lk/eat-kill-line)
    (evil-local-set-key 'normal (kbd "dd") 'lk/eat-delete-line)
    (evil-local-set-key 'normal (kbd "p") 'lk/eat-yank)
    (evil-local-set-key 'normal (kbd "u") 'lk/eat-undo)
    (evil-local-set-key 'normal (kbd "A") 'lk/eat-insert-at-end))))
;; :config (declare-function evil-collection-define-key "evil-collection"))
;; (evil-define-local-key
;;  'normal
;;  'eat-mode-map
;;  (kbd "w")
;;  'lk/eat-forward-word
;;  (kbd "b")
;;  'lk/eat-backward-word
;;  (kbd "o")
;;  'lk/eat-edit-new-line
;;  (kbd "D")
;;  'lk/eat-kill-line
;;  (kbd "dd")
;;  'lk/eat-delete-line
;;  (kbd "u")
;;  'lk/eat-undo))
;; (declare-function evil-define-key "evil-core")


;; first attempt
;; :config
;; (defun lk/eat-input-kbd (str)
;;   (seq-do (lambda (c) (eat-self-input 1 c)) (kbd str)))
;; (evil-define-key 'normal 'eat-mode-map (kbd "b") (lk/eat-input-kbd "M-b"))
;; (evil-define-key 'normal 'eat-mode-map (kbd "w") (lk/eat-input-kbd "M-f"))
;; (evil-define-key
;;  'normal 'eat-mode-map (kbd "o") (lk/eat-input-kbd "C-e C-M-j"))
;; (evil-define-key 'normal 'eat-mode-map (kbd "D") (lk/eat-input-kbd "C-k"))
;; (evil-define-key
;;  'normal 'eat-mode-map (kbd "dd") (lk/eat-input-kbd "C-a C-k")))
;; ;; (evil-define-key 'normal 'eat-mode-map (kbd "A") (progn (lk/eat-input-kbd "C-e") (evil-insert-state)))
;; (evil-define-key 'normal 'eat-mode-map (kbd "u") (lk/eat-input-kbd "C-_"))

;; (delete [?\C-u] eat-semi-char-non-bound-keys) ; make C-u work in Eat terminals like in normal terminals
;; (delete [?\C-g] eat-semi-char-non-bound-keys) ; ditto for C-g
;; (eat-update-semi-char-mode-map))
;; XXX: Awkward workaround for the need to call eat-reload after changing Eat's keymaps,
;; but reloading from :config section causes infinite recursion because :config wraps with-eval-after-load.
;; (defvar eat--prevent-use-package-config-recursion nil)
;; (unless eat--prevent-use-package-config-recursion
;;   (setq eat--prevent-use-package-config-recursion t)
;;   (eat-reload))
;; (makunbound 'eat--prevent-use-package-config-recursion))

;; (use-package
;;  julia-snail
;;  :hook (julia-mode . julia-snail-mode)
;;  :custom (julia-snail-terminal-type :eat)
;;  :config
;;  ;; snail-extensions is a buffer-local var, so must setq-default for global
;;  (setq-default julia-snail-extensions '(ob-julia))
;;  (setq-default julia-snail-extra-args
;;                '("--threads=auto,auto" "-q" "--project=@."))
;;  ;; disable features that overlap eglot and corfu
;;  (remove-hook 'completion-at-point-functions #'julia-snail-company-capf t)
;;  (remove-hook
;;   'completion-at-point-functions #'julia-snail-repl-completion-at-point
;;   t)
;;  (remove-function (local 'eldoc-documentation-function) #'julia-snail-eldoc)
;;  (remove-hook 'xref-backend-functions #'julia-snail-xref-backend t))

;; :custom (julia-snail-use-emoji-mode-lighter nil)

;; (defun lk/org-redisplay-babel-result ()
;;   "Redisplay plot after running org-babel."
;;   (save-excursion
;;     (condition-case err
;;         (when-let* ((beg (org-babel-where-is-src-block-result))
;;                     (elem (and (goto-char beg) (org-element-context)))
;;                     (end
;;                      (- (org-element-property :end elem)
;;                         (org-element-property :post-blank elem))))
;;           (funcall (cond
;;                     ((fboundp 'org-link-preview)
;;                      #'org-link-preview-region)
;;                     ((featurep 'org-image-preview)
;;                      #'org-image-preview--in-region)
;;                     (t
;;                      #'org-display-inline-images))
;;                    nil 'refresh beg end))
;;       (error (message "Could not display images: %S" err)))))

(use-package
 julia-snail
 :init
 (defun lk/julia-sysimage-args (files)
   (seq-map
    (lambda (s) (format "--sysimage=%s" s))
    (seq-filter #'file-exists-p (seq-map #'expand-file-name files))))
 :hook (julia-mode . julia-snail-mode)
 ;; (org-babel-julia-after-async-execute . lk/org-redisplay-babel-result)
 :ensure (:repo "/home/lkirk/repo/ob-julia-snail")
 :custom
 (julia-snail-terminal-type :eat)
 (julia-snail-use-emoji-mode-lighter nil)
 :config
 ;; snail-extensions is a buffer-local var, so must setq-default for global
 (setq-default julia-snail-repl-display-eval-results t)
 ;; (setq-default julia-snail-extensions '(ob-julia))
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

;; (use-package
;;  ess-julia-mode
;;  :ensure nil
;;  :custom (inferior-julia-args "--project=@. "))


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

(use-package
 systemd
 :mode (("\\.service\\'" . systemd-mode) ("\\.target\\'" . systemd-mode)))

(use-package haskell-mode)
;; :custom
;; (haskell-process-path-ghci
;;  (if (file-exists-p (concat (project-root (project-current)) "flake.nix"))
;;      '("nix" "develop" "-c" "ghci"))))

(use-package meson-mode) ;; :hook (meson-mode . company-mode))
;; (use-package cmake-mode)

(use-package nix-mode :mode "\\.nix\\'")

(use-package eldoc :diminish eldoc-mode :ensure nil :config (global-eldoc-mode))

(use-package
 ruff-format
 :diminish
 :hook (python-mode . ruff-format-on-save-mode))

;; TODO: until we can do this with ruff
(use-package isortify :diminish :hook (python-mode . isortify-mode))
;; (use-package company :diminish :init (global-company-mode t))

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
 ;; do not alter font size of sections
 (font-latex-fontify-sectioning 'color)
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
 :bind (:map python-mode-map ("C-c p" . insert-ipython-debug)))

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
