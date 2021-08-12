
(in-package :veq)

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

