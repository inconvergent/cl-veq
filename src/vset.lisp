
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

(defmacro map-vset (dim type)
  (let* ((mname (veqsymb dim type "vset"))
         (docs (format nil "set ~ad value.~%ex: (~a (a) (fx ...))
where (fx ...) returns ~a values." dim mname dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           ((&rest symbs) &rest expr) ,,docs
                           `(-vset ,',',dim ,symbs (progn ,@expr)))))))
(map-vset 1 ff) (map-vset 2 ff) (map-vset 3 ff) (map-vset 4 ff)
(map-vset 1 df) (map-vset 2 df) (map-vset 3 df) (map-vset 4 df)


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
(defsetf $ (a &optional (i 0)) (new)
  "use (setf ($ a i) (list x)) to set a[i]."
  `(-vaset (,a 1 ,i) (apply #'values ,new)))
(defsetf 2$ (a &optional (i 0)) (new)
  "use (setf (2$ a i) (list x y)) to set a[i]."
  `(-vaset (,a 2 ,i) (apply #'values ,new)))
(defsetf 3$ (a &optional (i 0)) (new)
  "use (setf (3$ a i) (list x y z)) to set a[i]."
  `(-vaset (,a 3 ,i) (apply #'values ,new)))
(defsetf 4$ (a &optional (i 0)) (new)
  "use (setf (4$ a i) (list x y z w)) to set a[i]."
  `(-vaset (,a 4 ,i) (apply #'values ,new)))

(map-docstring '$vset "use ($vset (a i) (values ...)) to set a[i] of 1d array." :context :nodesc)
(map-docstring '2$vset "use (2$vset (a i) (values ...)) to set a[i] of 2d array." :context :nodesc)
(map-docstring '3$vset "use (3$vset (a i) (values ...)) to set a[i] of 3d array." :context :nodesc)
(map-docstring '4$vset "use (4$vset (a i) (values ...)) to set a[i] of 4d array." :context :nodesc)

(mapcar #'map-symbol
  `(($vset ((a i) &rest expr) `(-vaset (,a 1 ,i) ,@expr))
    (2$vset ((a i) &rest expr) `(-vaset (,a 2 ,i) ,@expr))
    (3$vset ((a i) &rest expr) `(-vaset (,a 3 ,i) ,@expr))
    (4$vset ((a i) &rest expr) `(-vaset (,a 4 ,i) ,@expr))))

