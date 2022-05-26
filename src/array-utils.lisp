
(in-package :veq)

;;;;;;;;;;;;;;;;;; GENERIC INIT ARRAY OF VEC

(export 'f$make)
(defmacro f$make (&key (dim 1) (n 1) (v 0f0))
  "create array with size (n dim), and initial value v."
  `(make-array (the pos-int (* ,dim ,n))
               :initial-element ,v :element-type 'ff :adjustable nil))

(export 'd$make)
(defmacro d$make (&key (dim 1) (n 1) (v 0d0))
  "create array with size (n dim), and initial value v."
  `(make-array (the pos-int (* ,dim ,n))
               :initial-element ,v :element-type 'df :adjustable nil))

;;;;;;;;;;;;;;;;;; COPY ARRAY

(export 'd$copy)
(defun d$copy (a)
  (declare #.*opt* (dvec a))
  "copy vector array (dvec)."
  (make-array (length a) :initial-contents a :element-type 'df :adjustable nil))

(export 'f$copy)
(defun f$copy (a)
  (declare #.*opt* (fvec a))
  "copy vector array (fvec)."
  (make-array (length a) :initial-contents a :element-type 'ff :adjustable nil))


;;;;;;;;;;;;;;;;;; INIT ARRAY OF VEC

(defmacro make-arr-num (dim)
  (labels ((nm (n) (veqsymb dim nil n)))
    (awg (a)
      `(progn (export ',(nm "$num"))
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
           ,(format nil "make ~ad vector array of zeros.~%typed." dim)
           (,(veqsymb 1 type "$make") :dim ,dim :n ,n))))))
(make-arr-util 1 ff) (make-arr-util 2 ff) (make-arr-util 3 ff) (make-arr-util 4 ff)
(make-arr-util 1 df) (make-arr-util 2 df) (make-arr-util 3 df) (make-arr-util 4 df)

;;;;;;;;;;;;;;;;;;;;; INIT FROM EXPR

(defmacro f$_ (&body body)
  "create vector array (fvec) from body. where body is a list of lists.
ex: ($_ (loop repeat 2 collect `(1f0 2f0)))
ex: ($_ '((1f0 2f0) (1f0 2f0)))."
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
  "create vector array (dvec) from body. wherebody is a list of lists.
ex: ($_ (loop repeat 2 collect `(1d0 2d0)))
ex: ($_ '((1d0 2d0) (1d0 2d0)))."
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
  "create vector array (fvec) from body: (f_ '(1f0 2f0 3f0))."
  (awg (l) `(let ((,l (progn ,@body)))
             (declare (list ,l))
             (make-array (length ,l) :initial-contents ,l
                         :element-type 'ff :adjustable nil))))
(defmacro d_ (&body body)
  "create vector array (dvec) from body: (d_ '(1d0 2d0 3d0))."
  (awg (l) `(let ((,l (progn ,@body)))
             (declare (list ,l))
             (make-array (length ,l) :initial-contents ,l
                         :element-type 'df :adjustable nil))))

;;;;;;;;;;;;;;;;;;;;;; ACCESS

(defmacro -$ (dim a &key inds type)
  (declare (pos-int dim))
  (unless inds (setf inds '(0)))
  (awg (a*)
    (let ((lets (loop for r in inds
                      collect (let ((gs (gensym)))
                                `((,gs ,(typecase r (fixnum (* dim r))
                                                    (t `(* ,dim ,r))))
                                  ,@(loop for i from 0 below dim
                                          collect `(+ ,gs ,i)))))))
    `(let ((,a* ,a)
           ,@(loop for ss in lets collect (car ss)))
       (declare ,@(when type `((,(arrtype type) ,a*)))
                (pos-int ,@(loop for ss in lets collect (caar ss))))
       (values ,@(loop with res = (list)
                       for ss in lets
                       do (loop for s in (cdr ss)
                                do (push `(aref ,a* ,s) res))
                       finally (return (reverse res))))))))
(defmacro define-$ ()
  `(progn ,@(loop for (dim type)
                  in (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df) 2)
                  collect (let* ((name (veqsymb dim type "$"))
                                 (docs (format nil
"returns indices (default 0) from ~ad vector array as values.
ex: (~a a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension." dim name)))
                            `(defmacro ,name (a &rest rest)
                               ,docs
                               `(-$ ,,dim ,a :inds ,rest :type ,',type))))))
(define-$)

