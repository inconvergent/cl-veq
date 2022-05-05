
(in-package :veq)

;;;;;;;;;;;;;;;;;; GENERIC INIT ARRAY OF VEC

(export 'f$make)
(defmacro f$make (&key (dim 1) (n 1) (v 0f0))
  " create array with size (n dim), and initial value v"
  `(make-array (the pos-int (* ,dim ,n))
               :initial-element ,v :element-type 'ff :adjustable nil))

(export 'd$make)
(defmacro d$make (&key (dim 1) (n 1) (v 0d0))
  " create array with size (n dim), and initial value v"
  `(make-array (the pos-int (* ,dim ,n))
               :initial-element ,v :element-type 'df :adjustable nil))

;;;;;;;;;;;;;;;;;; COPY ARRAY

(export 'd$copy)
(defun d$copy (a)
  (declare #.*opt* (dvec a))
  "copy array"
  (make-array (length a) :initial-contents a :element-type 'df :adjustable nil))

(export 'f$copy)
(defun f$copy (a)
  (declare #.*opt* (fvec a))
  "copy array"
  (make-array (length a) :initial-contents a :element-type 'ff :adjustable nil))


;;;;;;;;;;;;;;;;;; INIT ARRAY OF VEC

(defmacro make-arr-num (dim)
  (labels ((nm (n) (veqsymb dim nil n)))
    (awg (a)
      `(progn
         (export ',(nm "$num"))
         (defun ,(nm "$num") (,a)
           (declare #.*opt* (simple-array ,a))
           ,(format nil "number of elements in ~ad array.~%untyped." dim)
           (the pos-int (/ (length ,a) ,dim)))))))
(make-arr-num 1) (make-arr-num 2) (make-arr-num 3) (make-arr-num 4)

(defmacro make-arr-util ( dim type )
  (labels ((nm (n) (veqsymb dim type n)))
    (awg (a n)
      `(progn
         (export ',(nm "$num"))
         (defun ,(nm "$num") (,a)
           (declare #.*opt* (,(arrtype type) ,a))
           ,(format nil "number of elements in ~ad array.~%typed." dim)
           (the pos-int (/ (length ,a) ,dim)))
         (export ',(nm "$one"))
         (defun ,(nm "$one") (&optional (,n 1))
           (declare #.*opt* (pos-int ,n))
           ,(format nil "make ~ad array of ones.~%typed." dim)
           (,(veqsymb 1 type "$make") :dim ,dim :n ,n :v ,(coerce 1 type)))
         (export ',(nm "$val"))
         (defun ,(nm "$val") (v &optional (,n 1))
           (declare #.*opt* (pos-int ,n))
           ,(format nil "make ~ad array of val.~%typed." dim)
           (,(veqsymb 1 type "$make") :dim ,dim :n ,n :v v))
         (export ',(nm "$zero"))
         (defun ,(nm "$zero") (&optional (,n 1))
           (declare #.*opt* (pos-int ,n))
           ,(format nil "make ~ad array of zeros.~%typed." dim)
           (,(veqsymb 1 type "$make") :dim ,dim :n ,n))))))
(make-arr-util 1 ff) (make-arr-util 2 ff) (make-arr-util 3 ff) (make-arr-util 4 ff)
(make-arr-util 1 df) (make-arr-util 2 df) (make-arr-util 3 df) (make-arr-util 4 df)

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

(defmacro f_ (&body body)
  "create fvec from body: (f_ '(1f0 2f0 3f0))"
  (awg (l) `(let ((,l (progn ,@body)))
             (declare (list ,l))
             (make-array (length ,l) :initial-contents ,l
                         :element-type 'ff :adjustable nil))))
(defmacro d_ (&body body)
  "create dvec from body: (d_ '(1d0 2d0 3d0))"
  (awg (l) `(let ((,l (progn ,@body)))
             (declare (list ,l))
             (make-array (length ,l) :initial-contents ,l
                         :element-type 'df :adjustable nil))))

;;;;;;;;;;;;;;;;;;;;;; ACCESS

(declaim (inline -d$ -f$ -d2$ -f2$ -d3$ -f3$ -f4$ -d4$))
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

(defun -d4$ (v &optional (i 0) &aux (ii (* 4 i)))
  (declare #.*opt* (dvec v) (pos-int i ii))
  (values (the df (aref v ii))
          (the df (aref v (the pos-int (1+ ii))))
          (the df (aref v (the pos-int (+ 2 ii))))
          (the df (aref v (the pos-int (+ 3 ii))))))

(defun -f4$ (v &optional (i 0) &aux (ii (* 4 i)))
  (declare #.*opt* (fvec v) (pos-int i ii))
  (values (the ff (aref v ii))
          (the ff (aref v (the pos-int (1+ ii))))
          (the ff (aref v (the pos-int (+ 2 ii))))
          (the ff (aref v (the pos-int (+ 3 ii))))))

