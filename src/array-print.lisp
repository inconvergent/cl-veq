
(in-package :veq)

(defun $print (a &key (dim 1) (start 0) n (s t))
  (declare (simple-array a) (pos-int dim) (fixnum start))
  "pretty print n, or all, rows from vector array of dim.
start at row (start 0).
negative start counts backwards from the last row
use s to overrid output stream."
  (let* ((max-ind (length a))
         (max-row (ceiling max-ind dim))
         (from (if (> start -1) start (max 0 (+ max-row start)))))
    (declare (pos-int max-ind max-row from))
    (labels
      ((-row-info (indsize row-ind i)
         (if (= dim 1)
             (format s "~&~v,' d :  " indsize i)
             (format s "~&~v,' d,~v,' d :  " indsize row-ind indsize i)))
       (-row (c i) (format s "~v@A~[~:; | ~]" c (aref a i) (mod (1+ i) dim)))
       (-arr-max-digits (a n)
         (declare (simple-array a) (pos-int n))
         (loop repeat n for v across a maximizing (length (mkstr v)))))
      (loop with n = (if n n max-row)
            with indsize = (1+ (length (mkstr n)))
            with colsize = (-arr-max-digits a (* n dim))
            repeat n
            for row-ind from from below max-row
            do (-row-info indsize row-ind (* row-ind dim))
               (loop repeat dim
                     for i from (* row-ind dim) below max-ind
                     do (-row colsize i))
            finally (format s "~&"))))
  a)
(defun 2$print (a &key n (s t)) "pretty print 2d array. returns array." ($print a :n n :dim 2 :s s))
(defun 3$print (a &key n (s t)) "pretty print 3d array. returns array." ($print a :n n :dim 3 :s s))
(defun 4$print (a &key n (s t)) "pretty print 4d array. returns array." ($print a :n n :dim 4 :s s))


(defun $to-list (a &key (dim 1))
  (declare (simple-array a) (pos-int dim))
  "return array as a list of lists of length dim."
  (loop for i of-type pos-int from 0 below (length a) by dim
        collect (loop for j of-type pos-int from i repeat dim
                      collect (aref a j))))
(defun 2$to-list (a) "return array as a list of lists of length 2." ($to-list a :dim 2))
(defun 3$to-list (a) "return array as a list of lists of length 3." ($to-list a :dim 3))
(defun 4$to-list (a) "return array as a list of lists of length 4." ($to-list a :dim 4))

