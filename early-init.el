;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;; Turn off package, in favor of elpaca.  Increase gc-cons-threshold
;; to improve performance (unmeasured).

;;; Code:

(setq package-enable-at-startup nil)

;; Performance?
(setq gc-cons-threshold 100000000)

(provide 'init)
;;; early-init.el ends here
