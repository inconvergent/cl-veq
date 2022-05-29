
(in-package :veq)

(defun $print (a &key n (dim 1) &aux (n (if n n (/ (length a) dim))))
  (declare (simple-array a) (pos-int dim))
  "pretty print nd array."
  (labels ((arr-max-digits (a n)
            (declare (simple-array a) (pos-int n))
            (loop repeat n for v across a
                  maximizing (length (mkstr v)))))

    (loop with indsize = (1+ (length (mkstr n)))
          with colsize = (arr-max-digits a (* n dim))
          for i from 0 below (* n dim) by dim
          for row-ind from 0
          do (format t "~%~v,' d| " indsize row-ind)
             (loop for i from i repeat dim
                   do (format t "~v@A~[~:; | ~]" colsize (aref a i)
                              (mod (1+ i) dim)))))
  (format t "~%")
  a)
(defun 2$print (a &key n) "pretty print 2d array. returns array." ($print a :n n :dim 2))
(defun 3$print (a &key n) "pretty print 3d array. returns array." ($print a :n n :dim 3))
(defun 4$print (a &key n) "pretty print 4d array. returns array." ($print a :n n :dim 4))


(vdef $to-list (a &key (dim 1))
  (declare (simple-array a) (pos-int dim))
  "return array as a list of lists of length dim."
  (loop for i of-type pos-int from 0 below (length a) by dim
        collect (loop for j of-type pos-int from i repeat dim
                      collect (aref a j))))
(defun 2$to-list (a) "return array as a list of lists of length 2." ($to-list a :dim 2))
(defun 3$to-list (a) "return array as a list of lists of length 3." ($to-list a :dim 3))
(defun 4$to-list (a) "return array as a list of lists of length 4." ($to-list a :dim 4))

