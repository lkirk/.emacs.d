;;; lk-look-and-feel.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Settings that define the look and feel, specifically themes, completion
;; and evil mode.
;; Note that there are some vertico settings in `file:emacs-general.el'.

;;; Code:

; themes
(use-package zenburn-theme)
(use-package solarized-theme)
(use-package
 modus-themes
 :custom (modus-operandi-palette-overrides '((bg-main "#EDEDED")))
 :config
 (progn
   (defvar after-load-theme-hook nil
     "Hook run after a color theme is loaded using `load-theme'.")
   (defadvice load-theme (after run-after-load-theme-hook activate)
     "Run `after-load-theme-hook'."
     (run-hooks 'after-load-theme-hook))
   (defun customize-modus-operandi ()
     "Customize modus operandi theme"
     (if (member 'modus-operandi custom-enabled-themes)
         (custom-theme-set-faces 'modus-operandi
                                 '(mode-line-buffer-id
                                   ((t (:foreground "#5a5a5a")))))))
   (add-hook 'after-load-theme-hook 'customize-modus-operandi)))

; undo tree + evil
(use-package undo-tree :diminish :config (global-undo-tree-mode))

(use-package
 evil
 :after undo-tree
 :config
 (declare-function evil-set-undo-system "evil-vars")
 (declare-function evil-mode "evil-core")
 (evil-set-undo-system 'undo-tree)
 (evil-mode t)
 :custom (evil-want-C-u-scroll t)
 ;; these next two are needed for evil-collection
 (evil-want-integration t) ;; optional, already t by default
 (evil-want-keybinding nil)
 (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))))

(use-package
 evil-collection
 :after evil
 :diminish (evil-collection-unimpaired-mode)
 :config
 (declare-function evil-collection-init "evil-collection")
 (evil-collection-init))

(use-package
 evil-org
 :after org
 :hook (org-mode . (lambda () evil-org-mode))
 :config
 (require 'evil-org-agenda)
 (evil-org-agenda-set-keys))

;; TODO: debug diminish for some minor modes (eldoc, etc...)
(use-package diminish)

;; completion system (see additions to the emacs package (below))
;; :custom
;; (vertico-scroll-margin 0) ;; Different scroll margin
;; (vertico-count 20) ;; Show more candidates
;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
(use-package
 vertico
 :init (vertico-mode)
 :custom
 (vertico-count 20) ;; Show more candidates
 (vertico-resize t)) ;; Grow and shrink the Vertico minibuffer

;; Optionally use the `orderless' completion style.
(use-package
 orderless
 :custom
 ;; Configure a custom style dispatcher (see the Consult wiki)
 ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
 ;; (orderless-component-separator #'orderless-escapable-split-on-space)
 (completion-styles '(orderless basic))
 (completion-category-defaults nil)
 (completion-category-overrides '((file (styles partial-completion)))))

(set-face-attribute 'variable-pitch nil :font "Ubuntu")

(provide 'lk-look-and-feel)
;;; lk-look-and-feel.el ends here
