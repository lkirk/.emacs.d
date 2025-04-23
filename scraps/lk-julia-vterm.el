;; (use-package
;;  julia-vterm
;;  :custom (julia-vterm-repl-program "julia --project=@."))

;; (use-package
;;  ob-julia-vterm
;;  :after org
;;  :config (defalias 'org-babel-execute:julia 'org-babel-execute:julia-vterm)
;;  (defalias
;;    'org-babel-variable-assignments:julia
;;    'org-babel-variable-assignments:julia-vterm))
