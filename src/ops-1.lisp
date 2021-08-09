
(in-package :veq)

(defun d$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 1 :n n :v 1d0))
(defun d$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 1 :n n :v v))
(defun d$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (d$make :dim 1 :n n))
(defun f$one (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 1 :n n :v 1f0))
(defun f$val (v &optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 1 :n n :v v))
(defun f$zero (&optional (n 1)) (declare #.*opt* (pos-int n)) (f$make :dim 1 :n n))

(defun $len (a) (declare #.*opt* (simple-array a)) (the pos-int (length a)))


;;;;;;;;;;;;;;;;;;;;;; ACCESS

(declaim (inline -d$))
(defun -d$ (v &optional (i 0))
  (declare #.*opt* (dvec v) (pos-int i))
  (the df (aref v i)))

(declaim (inline -f$))
(defun -f$ (v &optional (i 0))
  (declare #.*opt* (fvec v) (pos-int i))
  (the ff (aref v i)))


(ops

  (d+ (a b)) (+ a b) (d- (a b)) (- a b) (d* (a b)) (* a b) (d/ (a b)) (/ a b)
  (f+ (a b)) (+ a b) (f- (a b)) (- a b) (f* (a b)) (* a b) (f/ (a b)) (/ a b)

  (di- (a b)) (- b a) (di/ (a b)) (/ b a)
  (fi- (a b)) (- b a) (fi/ (a b)) (/ b a)

  (dabs (a)) (abs a)
  (dneg (a)) (- a)
  (dsquare (a)) (* a a)
  (dsqrt (a)) (the pos-df (sqrt (the pos-df a)))

  (fabs (a)) (abs a)
  (fneg (a)) (- a)
  (fsquare (a)) (* a a)
  (fsqrt (a)) (the pos-ff (sqrt (the pos-ff a)))

  (d^ (a s)) (expt a s) (dmod (a s)) (mod a s)
  (f^ (a s)) (expt a s) (fmod (a s)) (mod a s)

  (dcos-sin (a)) (values (cos a) (sin a)) (fcos-sin (a)) (values (cos a) (sin a))
  (dsin-cos (a)) (values (sin a) (cos a)) (fsin-cos (a)) (values (sin a) (cos a)))

