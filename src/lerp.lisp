
(in-package :veq)

; TODO: end
(defun fxlspace (n a b expr &key dim type (end t))
  (declare (fixnum dim) (symbol type) (boolean end))
  "
  assumes first form in expr is a function with args (i x y z).
  number of arguments depend on dimension.
  "
  (let ((declare-arr))
    (awg (i n* stp s fx expr*)
    `(let* ((,n* ,n)
            (,stp (coerce (/ ,n*) ',type))
            (arr ,(cond ((not expr) (setf declare-arr t)
                                    `(,(veqsymb dim type "$ZERO") ,n*))
                         ((butlast expr) (setf declare-arr t)
                                         `(progn ,@(butlast expr))))))
     (declare (pos-int ,n*) (,type ,stp)
              ,@(if declare-arr `((,(arrtype type) veq:arr))))
     (loop with ,fx of-type function = ,(last* expr)
           for ,i of-type pos-int from 0 below ,n*
           for ,s of-type ,type from ,(coerce 0 type)
           ; TODO: inefficient use of xdlerp
           do ,(if expr `(mvc ,fx ,i (,(veqsymb dim type "LERP") ,a ,b (* ,s ,stp)))
                        `(-vaset (veq:arr ,dim ,i)
                                 (,(veqsymb dim type "LERP") ,a ,b (* ,s ,stp))))
           finally (return arr))))))

