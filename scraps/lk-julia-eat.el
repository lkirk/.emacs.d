;; some old code that may be useful if I ever decide to revisit this...


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

;; add as hook to julia-snail?
;; (org-babel-julia-after-async-execute . lk/org-redisplay-babel-result)

;; (use-package
;;  ess-julia-mode
;;  :ensure nil
;;  :custom (inferior-julia-args "--project=@. "))


;; (use-package  ;; interacts with julia org babel
;;  eat
;;  :custom (eat-kill-buffer-on-exit t)
;;  :hook (eshell-load . eat-eshell-mode))

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


;; (use-package
;;  ess
;;  :custom (inferior-julia-args "--threads=auto,auto -q --project=@."))


;; (defun lk/org-redisplay-babel-result ()
;;   "Display images after running async blocks."
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

;; (use-package
;;  ob-julia
;;  :ensure (:host github :repo "karthink/ob-julia")
;;  :after (ob org julia-snail)
;;  ;; :hook (org-babel-julia-after-async-execute . lk/org-redisplay-babel-result)
;;  :custom (org-babel-julia-backend 'julia-snail)
;;  :config
;;  ;; ob-julia is overwriting functions in this package.
;;  ;; this works for now
;;  (with-eval-after-load 'julia-snail/ob-julia
;;    (progn
;;      (load-file "~/repos/ob-julia-snail/ob-julia-snail.el"))))
;; ;; :config
;; ;; (require 'ob-julia)
;; ;; (require 'ob-julia-snail))
