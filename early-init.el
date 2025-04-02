;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; These settings come from the following performance recommendations
;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq gc-cons-threshold 100000000)
(setenv "LSP_USE_PLISTS" "true")
