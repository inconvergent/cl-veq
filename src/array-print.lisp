
(in-package :veq)


(defun vprint (a &key n (dim 1) (width 10))
  (declare (simple-array a) (pos-int dim))
  (labels ((numshow (a) (format nil "~v@A |" width a))
           (row (n l i)
             (apply #'mkstr
               (loop for j from (* i dim) repeat dim
                     if (>= j l)
                     do (error "incorrect size (~a) for dimension (~a)" n dim)
                     collect (numshow (aref a j))))))

    (let* ((l (length a))
           (n (if n (min n (/ l dim)) (/ l dim))))
      (format t "~%")
      (loop for i from 0 below n do (format t "~%~a" (row n l i)))
      (format t " dim: ~a n: ~a~%~%" dim n)))
  a)
(defun 2vprint (a &key n (width 10)) (vprint a :n n :dim 2 :width width))
(defun 3vprint (a &key n (width 10)) (vprint a :n n :dim 3 :width width))


(vdef to-list (arr &key (dim 1))
  (declare (simple-array arr) (pos-int dim))
  (let ((n (the pos-int (/ (length arr) dim)))
        (res (list)))
    (declare (pos-int n) (list res))
    (labels ((acc (&rest rest) (push (cdr rest) res))) ; car is row index
      (case dim (3 (3with-rows (n arr) #'acc))
                (2 (2with-rows (n arr) #'acc))
                (t (with-rows (n arr) #'acc)))
      (reverse res))))
(defun 2to-list (arr) (to-list arr :dim 2))
(defun 3to-list (arr) (to-list arr :dim 3))

