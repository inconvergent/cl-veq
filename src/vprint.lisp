
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
(defun 2vprint (a &key n (width 10)) (vprint a :n n :dim 2 :width width))
(defun 3vprint (a &key n (width 10)) (vprint a :n n :dim 3 :width width))


(vdef to-list (arr &key (dim 1))
  (declare (simple-array arr) (pos-int dim))
  (let ((n (the pos-int (/ (length arr) dim))))
    (declare (pos-int n))
    (let ((res (list)))
      (labels ((acc (&rest rest) (push (cdr rest) res)))
        (case dim (3 (3for-all-rows (n arr) #'acc))
                  (2 (2for-all-rows (n arr) #'acc))
                  (t (for-all-rows (n arr) #'acc)))
        (reverse res)))))
(defun 2to-list (arr) (to-list arr :dim 2))
(defun 3to-list (arr) (to-list arr :dim 3))

