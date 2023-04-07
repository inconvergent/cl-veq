
(in-package :veq)

(defparameter *vset-error* "-------------- vset error:")

(defmacro -vset (dim symbs expr)
  (declare (pos-int dim) (cons expr))
  (unless (every #'symbolp symbs)
    (error "~a ~%symbs: ~a~%expr: ~a. arg symbs is not symbols"
           *vset-error* symbs expr))
  (unless (= dim (length symbs))
    (error "~a ~%incorrect number of symbols ~a for dim: ~a"
           *vset-error* symbs dim))
  ; i didnt know about multiple-value-setq when i wrote this:
  ; is the commented code faster if we introduce type declares?
  ; (labels ((mkgs () (push* (gensym "TMP") gs)))
  ;   (let ((res (apply #'concatenate 'list (loop for s in symbs
  ;                                               collect `(,s ,(mkgs))))))
  ;     `(mvb ,(reverse gs) ,expr (setf ,@res))))
  `(multiple-value-setq ,symbs ,expr))

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
(map-vset 1 in) (map-vset 2 in) (map-vset 3 in) (map-vset 4 in)


(defmacro -vaset ((arr dim i) &rest expr &aux (gs (list)))
  (declare (pos-int dim))
  (awg (ii arr*)
    (labels ((mkgs () (push* (gensym "VASET") gs)))
      (let ((res (apply #'concatenate 'list
                   (loop for j of-type pos-int from 0 below dim
                         collect `((aref ,arr* (+ ,ii ,j))
                                   ,(mkgs))))))
        `(let ((,arr* ,arr)
               (,ii (* ,dim ,i)))
           (declare (pos-int ,ii))
           (mvb ,(reverse gs) ,@expr (setf ,@res)))))))

; makes it possible to do: (setf (3$ a 3) (list 9f0 9f0 9f0))
; TODO: can we write this so new does not need to be a list?
(defsetf $ (a &optional (i 0)) (new)
  "1d vector array setter and getter.
use (setf ($ a i) (list x)) to set a[i].
use ($ a i j ...) to return (values a[i] a[j] ...)"
  `(-vaset (,a 1 ,i) (apply #'values ,new)))
(defsetf 2$ (a &optional (i 0)) (new)
  "2d vector array setter and getter.
use (setf (2$ a i) (list x)) to set a[i].
use (2$ a i j ...) to return (values a[i] a[j] ...)"
  `(-vaset (,a 2 ,i) (apply #'values ,new)))
(defsetf 3$ (a &optional (i 0)) (new)
  "3d vector array setter and getter.
use (setf (3$ a i) (list x)) to set a[i].
use (3$ a i j ...) to return (values a[i] a[j] ...)"
  `(-vaset (,a 3 ,i) (apply #'values ,new)))
(defsetf 4$ (a &optional (i 0)) (new)
  "4d vector array setter and getter.
use (setf (4$ a i) (list x)) to set a[i].
use (4$ a i j ...) to return (values a[i] a[j] ...)"
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

(defmacro $nvset ((a n &optional (i 0))
                       &body body
                       &aux (gs (loop for i from 0 below n collect (gensym))))
  (declare (symbol a) (pn n))
  "set n indices in a, from a[i] with n values from body."
  (awg (i*)
    `(let ((,i* ,i))
       (declare (pn ,i*))
       (mvb (,@(loop for s in gs collect s)) (~ ,@body)
         (progn ,@(loop for s in gs for i from 0
                        collect `(setf (aref ,a (+ ,i* ,i)) ,s))))
       ,a)))

