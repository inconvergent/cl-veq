
(in-package :veq)

(defun -nsum (dim type n body)
  (awg (res)
    `(veq:f3let ((,res (,(veqsymb dim type "rep") ,(coerce 0 type))))
      (loop repeat ,n
            do (,(veqsymb dim type "vset") (,res)
                 (,(veqsymb dim type "+") ,res ,@body)))
      (values ,res))))

