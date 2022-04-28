
(in-package :veq)

(defparameter *vset-error* "-------------- vset error:")

(defmacro -vset (dim symbs expr &aux (gs (list)))
  (declare (pos-int dim) (cons expr))
  (unless (every #'symbolp symbs)
    (error "~a ~%symbs: ~a~%expr: ~a. arg symbs is not symbols"
           *vset-error* symbs expr))
  (unless (= dim (length symbs))
    (error "~a ~%incorrect number of symbols ~a for dim: ~a"
           *vset-error* symbs dim))
  (labels ((mkgs () (let ((g (gensym (mkstr "TMP"))))
                      (push g gs)
                      g)))
    (let ((res (apply #'concatenate 'list (loop for s in symbs
                                                collect `(,s ,(mkgs))))))
      `(mvb ,(reverse gs) ,expr (setf ,@res)))))


(defmacro -vaset ((arr dim i) &rest expr &aux (gs (list)))
  (declare (symbol arr) (pos-int dim))
  (awg (ii)
    (labels ((mkgs () (let ((g (gensym "TMP")))
                        (push g gs)
                        g)))
      (let ((res (apply #'concatenate 'list
                   (loop for j of-type pos-int from 0 below dim
                         collect `((aref ,arr (+ ,ii ,j))
                                   ,(mkgs))))))
        `(let ((,ii (* ,dim ,i)))
           (declare (pos-int ,ii))
           (mvb ,(reverse gs) ,@expr (setf ,@res)))))))

; makes it possible to do: (setf (3$ a 3) (list 9f0 9f0 9f0))
; TODO: can we write this so new does not need to be a list?
(defsetf $ (a i) (new) `(-vaset (,a 1 ,i) (apply #'values ,new)))
(defsetf 2$ (a i) (new) `(-vaset (,a 2 ,i) (apply #'values ,new)))
(defsetf 3$ (a i) (new) `(-vaset (,a 3 ,i) (apply #'values ,new)))
(defsetf 4$ (a i) (new) `(-vaset (,a 4 ,i) (apply #'values ,new)))


