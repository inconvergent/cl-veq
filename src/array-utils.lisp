
(in-package :veq)

;;;;;;;;;;;;;;;;;; INIT ARRAY OF VEC

(defmacro f$make (&key (dim 1) (n 1) (v 0f0))
  " create array with size (n dim), and initial value v"
  `(values (make-array (the pos-int (* ,dim ,n))
             :initial-element ,v :element-type 'ff :adjustable nil)
           ,dim ,n))

(defmacro d$make (&key (dim 1) (n 1) (v 0d0))
  " create array with size (n dim), and initial value v"
  `(values (make-array (the pos-int (* ,dim ,n))
             :initial-element ,v :element-type 'df :adjustable nil)
           ,dim ,n))


(defmacro f$_ (&body body)
  " create array from body. use either

  ($_ (loop repeat 2 collect `(1d0 2d0)))
  or
  ($_ '((1d0 2d0) (1d0 2d0)))
  "
  (awg (body* n dim e)
    `(handler-case
       (let* ((,body* ,@body)
            (,n (length ,body*))
            (,dim (length (the list (car ,body*)))))
       (declare (pos-int ,n ,dim) (list ,body*))
       (values (make-array (* ,n ,dim) :initial-contents (the list (awf ,body*))
                                       :element-type 'ff
                                       :adjustable nil)
               ,dim ,n))
       (error (,e) (error "error in f$_ with: ~a~%. err: ~a~%" ',body ,e)))))

(defmacro d$_ (&body body)
  " create array from body. use either

  ($_ (loop repeat 2 collect `(1d0 2d0)))
  or
  ($_ '((1d0 2d0) (1d0 2d0)))
  "
  (awg (body* n dim e)
    `(handler-case
       (let* ((,body* ,@body)
              (,n (length ,body*))
              (,dim (length (the list (car ,body*)))))
         (declare (pos-int ,n ,dim) (list ,body*))
         (values (make-array (* ,n ,dim) :initial-contents (the list (awf ,body*))
                                         :element-type 'df
                                         :adjustable nil)
                 ,dim ,n))
       (error (,e) (error "error in d$_ with: ~a~%. err: ~a~%" ',body ,e)))))

(defmacro f_ (&body body)
  " corresponds to ($_ '(body)), that is a single row of (length body).  "
  (awg (body* dim e)
    `(handler-case
       (let* ((,body* ,@body)
              (,dim (length ,body*)))
         (declare (pos-int ,dim) (list ,body*))
         (values (make-array ,dim :initial-contents (the list (awf ,body*))
                                  :element-type 'ff
                                  :adjustable nil)
                 ,dim 1))
       (error (,e) (error "error in f_ with: ~a~%. err: ~a~%" ',body ,e)))))

(defmacro d_ (&body body)
  " corresponds to ($_ '(body)), that is a single row of (length body).  "
  (awg (body* dim e)
    `(handler-case
       (let* ((,body* ,@body)
              (,dim (length ,body*)))
         (declare (pos-int ,dim) (list ,body*))
         (values (make-array ,dim :initial-contents (the list (awf ,body*))
                                  :element-type 'df
                                  :adjustable nil)
                 ,dim 1))
       (error (,e) (error "error in f_ with: ~a~%. err: ~a~%" ',body ,e)))))

