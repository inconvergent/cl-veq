
(in-package :veq)

(defun -nsum (dim type n body)
  (awg (res)
    `(veq:f3let ((,res (,(veqsymb dim type "rep") ,(coerce 0 type))))
      (loop repeat ,n
            do (,(veqsymb dim type "vset") (,res)
                 (,(veqsymb dim type "+") ,res ,@body)))
      (values ,res))))

(defmacro map-nsum (dim type)
  (let* ((mname (veqsymb dim type "nsum"))
         (docs (format nil "make ~ad" dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           ((n) &body body) ,,docs
                           (-nsum ,,dim ',',type n body))))))
(map-nsum 1 ff) (map-nsum 2 ff) (map-nsum 3 ff) (map-nsum 4 ff)
(map-nsum 1 df) (map-nsum 2 df) (map-nsum 3 df) (map-nsum 4 df)

