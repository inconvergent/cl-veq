
(in-package :veq)

(declaim (inline d$zero))
(defun d$zero (&optional (n 1))
  (declare #.*opt* (pos-int n))
  ($ :dim 1 :n n :type 'df))

(declaim (inline d$one))
(defun d$one (&optional (n 1))
  (declare #.*opt* (pos-int n))
  ($ :dim 1 :n n :v 1d0 :type 'df))

(declaim (inline d$val))
(defun d$val (v &optional (n 1))
  (declare #.*opt* (pos-int n))
  ($ :dim 1 :n n :v v :type 'df))

(declaim (inline f$zero))
(defun f$zero (&optional (n 1))
  (declare #.*opt* (pos-int n))
  ($ :dim 1 :n n :type 'ff))

(declaim (inline f$one))
(defun f$one (&optional (n 1))
  (declare #.*opt* (pos-int n))
  ($ :dim 1 :n n :v 1f0 :type 'ff))

(declaim (inline f$val))
(defun f$val (v &optional (n 1))
  (declare #.*opt* (pos-int n))
  ($ :dim 1 :n n :v v :type 'ff))


;;;;;;;;;;;;;;;;;;;;;; ACCESS

(defun with (v i type body)
  (declare (symbol v type))
  (awg (ii xx)
    `(let ((,ii ,i))
      (declare (pos-int ,ii))
      (mvb (,xx) (mvc ,@body
                      (funcall #',(veqsymb 1 type ">>" :pref "-")
                               ,v ,ii))
        (declare (,type ,xx))
        (setf (aref ,v ,ii) ,xx)
        ,xx))))

(defmacro dwith ((v i) &body body)
  (declare (symbol v))
  "
  access v[i] as x y, body must return (values x y),
  containing new values for v[i]
  "
  (with v i 'df body))

(defmacro fwith ((v i) &body body)
  (declare (symbol v))
  "
  access v[i] as x y, body must return (values x y),
  containing new values for v[i]
  "
  (with v i 'ff body))

(declaim (inline -d>))
(defun -d> (v)
  (declare #.*opt* (dvec v))
  (the df (aref v 0)))

(declaim (inline -f>))
(defun -f> (v)
  (declare #.*opt* (fvec v))
  (the ff (aref v 0)))

(declaim (inline -d>>))
(defun -d>> (v i)
  (declare #.*opt* (dvec v) (pos-int i))
  (the df (aref v i)))

(declaim (inline -f>>))
(defun -f>> (v i)
  (declare #.*opt* (fvec v) (pos-int i))
  (the ff (aref v i)))


(ops

  (d+ (a b)) (+ a b)
  (d- (a b)) (- a b)
  (d* (a b)) (* a b)
  (d/ (a b)) (/ a b)

  (di- (a b)) (- b a)
  (di/ (a b)) (/ b a)

  (f+ (a b)) (+ a b)
  (f- (a b)) (- a b)
  (f* (a b)) (* a b)
  (f/ (a b)) (/ a b)

  (fi- (a b)) (- b a)
  (fi/ (a b)) (/ b a)


  (dabs (a)) (abs a)
  (dneg (a)) (- a)
  (dsquare (a)) (* a a)
  (dsqrt (a)) (the pos-df (sqrt (the pos-df a)))

  (fabs (a)) (abs a)
  (fneg (a)) (- a)
  (fsquare (a)) (* a a)
  (fsqrt (a)) (the pos-ff (sqrt (the pos-ff a)))


  (d^ (a s)) (expt a s)
  (dmod (a s)) (mod a s)

  (f^ (a s)) (expt a s)
  (fmod (a s)) (mod a s)

  (dcos-sin (a)) (values (cos a) (sin a))
  (fcos-sin (a)) (values (cos a) (sin a))

  (dsin-cos (a)) (values (sin a) (cos a))
  (fsin-cos (a)) (values (sin a) (cos a)))

