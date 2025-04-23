(declare-function eglot--guess-contact "eglot")
(defun lk/eglot-ensure ()
  (require 'eglot)
  (when (nth 3 (eglot--guess-contact))
    (eglot-ensure)))
(defun lk/eglot-ensure ()
  "Run eglot if the current major mode supports it."
  (let ((supported-modes
         (seq-mapcat
          (lambda (l)
            (if (listp l)
                (lk/filter-plist l)
              (ensure-list l)))
          (seq-mapcat
           (lambda (l) (ensure-list (car l))) eglot-server-programs))))
    (when (seq-contains-p supported-modes `,major-mode)
      (eglot-ensure))))

(defun lk/eglot-supports (server-progs)
  "Run eglot if the current major mode supports it, as defined in SERVER-PROGS."
  (let ((supported-modes
         (seq-mapcat
          (lambda (l)
            (if (listp l)
                (lk/filter-plist l)
              (ensure-list l)))
          (seq-mapcat (lambda (l) (ensure-list (car l))) server-progs))))
    ;; for some reason I can't match?
    (seq-contains-p supported-modes `,major-mode)))
(advice-add
 'eglot-ensure
 :around
 (lambda (orig &rest _args)
   (when (lk/eglot-supports eglot-server-programs)
     (funcall orig))))

(defun lk/filter-plist (lst)
  "Filters all plist items from LST.

For example (a b :foo v1 :bar v2 c :baz v3 d) => (a b c d)"
  (let ((last-was-kw nil))
    (seq-mapcat
     (lambda (elm)
       (cond
        ((keywordp elm)
         (setq last-was-kw t)
         nil)
        (last-was-kw
         (setq last-was-kw nil)
         nil)
        (t
         (list elm))))
     lst)))
