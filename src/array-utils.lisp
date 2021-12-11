
(in-package :veq)

;;;;;;;;;;;;;;;;;; GENERIC INIT ARRAY OF VEC

(defmacro f$make (&key (dim 1) (n 1) (v 0f0))
  " create array with size (n dim), and initial value v"
  `(make-array (the pos-int (* ,dim ,n))
               :initial-element ,v :element-type 'ff :adjustable nil))

(defmacro d$make (&key (dim 1) (n 1) (v 0d0))
  " create array with size (n dim), and initial value v"
  `(make-array (the pos-int (* ,dim ,n))
               :initial-element ,v :element-type 'df :adjustable nil))

;;;;;;;;;;;;;;;;;; COPY ARRAY

(defun d$copy (a)
  (declare #.*opt* (dvec a))
  (make-array (length a) :initial-contents a :element-type 'df :adjustable nil))

(defun f$copy (a)
  (declare #.*opt* (fvec a))
  (make-array (length a) :initial-contents a :element-type 'ff :adjustable nil))

;;;;;;;;;;;;;;;;;; INIT ARRAY OF VEC

(defun $num (a) (declare #.*opt* (simple-array a)) (the pos-int (length a)))
(defun d$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 1 :n n :v 1d0))
(defun d$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 1 :n n :v v))
(defun d$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 1 :n n))
(defun f$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 1 :n n :v 1f0))
(defun f$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 1 :n n :v v))
(defun f$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 1 :n n))

(defun 2$num (a) (declare #.*opt* (simple-array a)) (the pos-int (/ (length a) 2)))
(defun d2$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 2 :n n :v 1d0))
(defun d2$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 2 :n n :v v))
(defun d2$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 2 :n n))
(defun f2$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 2 :n n :v 1f0))
(defun f2$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 2 :n n :v v))
(defun f2$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 2 :n n))

(defun 3$num (a) (declare #.*opt* (simple-array a)) (the pos-int (/ (length a) 3)))
(defun d3$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 3 :n n :v 1d0))
(defun d3$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 3 :n n :v v))
(defun d3$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 3 :n n))
(defun f3$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 3 :n n :v 1f0))
(defun f3$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 3 :n n :v v))
(defun f3$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 3 :n n))


;;;;;;;;;;;;;;;;;;;;; INIT FROM EXPR

(defmacro f$_ (&body body)
  "create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
   or: ($_ '((1d0 2d0) (1d0 2d0)))"
  (awg (body* n dim e)
    `(handler-case
       (let* ((,body* ,@body)
              (,n (length ,body*))
              (,dim (length (the list (car ,body*)))))
         (declare (pos-int ,n ,dim) (list ,body*))
         (make-array (* ,n ,dim) :initial-contents (the list (awf ,body*))
                                 :element-type 'ff
                                 :adjustable nil))
       (error (,e) (error "error in f$_ with: ~a~%. err: ~a~%" ',body ,e)))))

(defmacro d$_ (&body body)
  "create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
   or: ($_ '((1d0 2d0) (1d0 2d0)))"
  (awg (body* n dim e)
    `(handler-case
       (let* ((,body* ,@body)
              (,n (length ,body*))
              (,dim (length (the list (car ,body*)))))
         (declare (pos-int ,n ,dim) (list ,body*))
         (make-array (* ,n ,dim) :initial-contents (the list (awf ,body*))
                                 :element-type 'df
                                 :adjustable nil))
       (error (,e) (error "error in d$_ with: ~a~%. err: ~a~%" ',body ,e)))))

(defmacro f_ (&body body) `(f$_ (list ,@body)))
(defmacro d_ (&body body) `(d$_ (list ,@body)))


;;;;;;;;;;;;;;;;;;;;;; ACCESS

(declaim (inline -d$ -f$ -d2$ -f2$ -d3$ -f3$))
(defun -d$ (v &optional (i 0))
  (declare #.*opt* (dvec v) (pos-int i))
  (the df (aref v i)))

(defun -f$ (v &optional (i 0))
  (declare #.*opt* (fvec v) (pos-int i))
  (the ff (aref v i)))

(defun -d2$ (v &optional (i 0) &aux (ii (* 2 i)))
  (declare #.*opt* (dvec v) (pos-int i ii))
  (values (the df (aref v ii)) (the df (aref v (the pos-int (1+ ii))))))

(defun -f2$ (v &optional (i 0) &aux (ii (* 2 i)))
  (declare #.*opt* (fvec v) (pos-int i ii))
  (values (the ff (aref v ii)) (the ff (aref v (the pos-int (1+ ii))))))

(defun -d3$ (v &optional (i 0) &aux (ii (* 3 i)))
  (declare #.*opt* (dvec v) (pos-int i ii))
  (values (the df (aref v ii))
          (the df (aref v (the pos-int (1+ ii))))
          (the df (aref v (the pos-int (+ 2 ii))))))

(defun -f3$ (v &optional (i 0) &aux (ii (* 3 i)))
  (declare #.*opt* (fvec v) (pos-int i ii))
  (values (the ff (aref v ii))
          (the ff (aref v (the pos-int (1+ ii))))
          (the ff (aref v (the pos-int (+ 2 ii))))))

