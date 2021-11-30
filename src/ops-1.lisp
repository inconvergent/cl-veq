
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

  (dlerp (ax bx s)) (+ ax (* (- bx ax) s))
  (dfrom (ax bx s)) (+ ax (* bx s))
  (dmid (ax bx)) (* 0.5d0 (+ ax bx))

  (flerp (ax bx s)) (+ ax (* (- bx ax) s))
  (ffrom (ax bx s)) (+ ax (* bx s))
  (fmid (ax bx)) (* 0.5f0 (+ ax bx))

  (fclamp (x)) (min 1f0 (max 0f0 x)) (dclamp (x)) (min 1d0 (max 0d0 x))
  (fclamp* (x mi ma)) (min ma (max mi x)) (dclamp* (x mi ma)) (min ma (max mi x))

  (dcos-sin (a)) (values (cos a) (sin a)) (fcos-sin (a)) (values (cos a) (sin a))
  (dsin-cos (a)) (values (sin a) (cos a)) (fsin-cos (a)) (values (sin a) (cos a))

  (fdeg->rad (d)) (* fpi (/ d 180f0)) (ddeg->rad (d)) (* dpi (/ d 180d0)))

