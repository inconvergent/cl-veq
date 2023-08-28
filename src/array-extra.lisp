
(in-package :veq)

(defun $to-list (a &key (dim 1))
  (declare (simple-array a) (pn dim))
  "return array as a list of lists of length dim."
  (loop for i of-type pn from 0 below (length a) by dim
        collect (loop for j of-type pn from i repeat dim
                      collect (aref a j))))
(defun 2$to-list (a) "return array as a list of lists of length 2." ($to-list a :dim 2))
(defun 3$to-list (a) "return array as a list of lists of length 3." ($to-list a :dim 3))
(defun 4$to-list (a) "return array as a list of lists of length 4." ($to-list a :dim 4))

; TODO: make this a general macro instead?
(defmacro -xmima (dim type)
  (let* ((exportname (vvsym type dim :$mima))
         (fxhead `(((:va ,dim x))))
         (vvop (vvsym type dim :x@$mima))
         (docs (format nil "find min and max for all dimensions of ~d array.
ex: (~a &key n) returns (values xmin xmax ...).
use n to limit to first n rows." dim exportname))
         (update-mima (loop for i from 0 repeat dim
                            for v = `(:vr x ,i)
                            for mi = `(:vr mi ,i) for ma = `(:vr ma ,i)
                            collect `(cond ((< ,v ,mi) (setf ,mi ,v))
                                           ((> ,v ,ma) (setf ,ma ,v))))))
    `(progn (export ',exportname)
     (fvdef ,exportname (a &key (n (,(vvsym nil dim :$num) a)) inds)
        (declare (,(arrtype type) a)) ,docs
        (,(vvsym type 1 :vlet)
               ((mm ,dim (,(vvsym type dim "$") a (if inds (car inds) 0)))
                (mi ,dim (values mm)) (ma ,dim (values mm)))
            (if inds (,vvop (l?@ a inds) (,@fxhead ,@update-mima))
                     (,vvop (?@ a 0 n) (,@fxhead ,@update-mima)))
          (values ,@(loop for i from 0 below dim
                          append `((:vr mi ,i) (:vr ma ,i)))))))))
(-xmima 1 ff) (-xmima 2 ff) (-xmima 3 ff)
(-xmima 1 df) (-xmima 2 df) (-xmima 3 df)

(defun $print (a &key (dim 1) (start 0) (n 16) (s t))
  (declare (simple-array a) (pn dim n) (veq:in start))
  "pretty print n, or all, rows from vector array of dim.
start at row (start 0).
negative start counts backwards from the last row
use s to overrid output stream."
  (let* ((max-ind (length a))
         (max-row (ceiling max-ind dim))
         (from (if (> start -1) start (max 0 (+ max-row start))))
         (n (min n max-row)))
    (declare (pn max-ind max-row from n))
    (labels
      ((-row-info (indsize row-ind i)
         (if (= dim 1)
             (format s "~&~v,' d :  " indsize i)
             (format s "~&~v,' d,~v,' d :  " indsize row-ind indsize i)))
       (-row (c i) (format s "~v@A~[~:; | ~]" c (aref a i) (mod (1+ i) dim)))
       (-arr-max-digits (a n)
         (declare (simple-array a) (pn n))
         (loop repeat n for v across a maximizing (length (mkstr v)))))
      (loop
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
(defun 2$print (a &key (n 16) (s t)) "pretty print 2d array. returns array." ($print a :n n :dim 2 :s s))
(defun 3$print (a &key (n 16) (s t)) "pretty print 3d array. returns array." ($print a :n n :dim 3 :s s))
(defun 4$print (a &key (n 16) (s t)) "pretty print 4d array. returns array." ($print a :n n :dim 4 :s s))

