
(in-package :veq)


(defun numshow (a &key (width 10))
  (declare (number a))
  (format nil "~v@A |" width a))


(defun vprint (a &key n (dim 1) (width 10))
  (declare (simple-array a) (pos-int dim))
  (let* ((l (length a))
         (n (if n (min n (/ l dim)) (/ l dim))))
    (declare (pos-int l n))
    (format t "~%")
    (loop for i from 0 below n
          do (format t "~%~A"
               (apply #'mkstr
                 (loop for j from (* i dim)
                       repeat dim
                       if (>= j l)
                       do (error "incorrect size (~a) for dimension (~a)" n dim)
                       collect (numshow (aref a j)
                                        :width width)))))
    (format t " dim: ~a n: ~a~%~%" dim n))
  a)
