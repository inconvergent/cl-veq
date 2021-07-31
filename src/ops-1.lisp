
(in-package :veq)

(declaim (inline d$zero d$one d$val f$one f$val f$zero))
(defun d$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$ :dim 1 :n n :v 1d0))
(defun d$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (d$ :dim 1 :n n :v v))
(defun d$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$ :dim 1 :n n))
(defun f$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$ :dim 1 :n n :v 1f0))
(defun f$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (f$ :dim 1 :n n :v v))
(defun f$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$ :dim 1 :n n))


;;;;;;;;;;;;;;;;;;;;;; ACCESS

(defun with (arr i type body)
  (declare (symbol arr type))
  "
  execute (funcall body x y) for arr[i]. body must be a function that returns
  (values x y), the new value for arr[i]
  "
  (awg (ii xx)
    `(let ((,ii ,i))
      (declare (pos-int ,ii))
      (mvb (,xx) (mvc ,@body (funcall #',(veqsymb 1 type ">>" :pref "-")
                                      ,arr ,ii))
        (declare (,type ,xx))
        (setf (aref ,arr ,ii) ,xx)
        ,xx))))

(defmacro dwith ((arr i) &body body) (declare (symbol arr)) (with arr i 'df body))
(defmacro fwith ((v i) &body body) (declare (symbol v)) (with v i 'ff body))

(declaim (inline -d> -f> -d>> -f>>))
(defun -d> (v) (declare #.*opt* (dvec v)) (the df (aref v 0)))
(defun -f> (v) (declare #.*opt* (fvec v)) (the ff (aref v 0)))
(defun -d>> (v i) (declare #.*opt* (dvec v) (pos-int i)) (the df (aref v i)))
(defun -f>> (v i) (declare #.*opt* (fvec v) (pos-int i)) (the ff (aref v i)))


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

