
(in-package :veq)

; TODO: typed mvb -----------------

(defmacro vset (dim &rest expr)
  (declare (pos-int dim))
  (let ((gs (list))
        (vals (subseq expr dim)))
    (handler-case (caar vals)
      (error (e) (format t "~%--------------~%vset error: ~a ~%
                         the last part of expr must be a form~%" e)
                 (error "quitting")))
    (labels ((mkgs (s) (let ((g (gensym (mkstr s "-"))))
                         (push g gs)
                         g)))
      (let ((res (apply #'concatenate 'list
                        (loop for s in (subseq expr 0 dim) ; symbols
                              collect `(,s ,(mkgs s))))))
        `(mvb ,(reverse gs) ,@vals (setf ,@res))))))


(defmacro -vaset ((arr dim i) &rest expr)
  (declare (symbol arr) (pos-int dim))
  (awg (ii)
    (let ((gs (list)))
      (labels ((mkgs () (let ((g (gensym "VASET")))
                          (push g gs)
                          g)))
        (let ((res (apply #'concatenate 'list
                          (loop for j from 0 below dim
                                collect `((aref ,arr (+ ,ii ,j))
                                          ,(mkgs))))))
          `(let ((,ii (* ,dim ,i)))
             (declare (pos-int ,ii))
             (mvb ,(reverse gs) ,@expr (setf ,@res))))))))
