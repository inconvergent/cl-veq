
(in-package :veq)

(ops

  (@+ (a b)) (+ a b) (@- (a b)) (- a b) (@* (a b)) (* a b) (@/ (a b)) (/ a b)
  (@i- (a b)) (- b a) (@i/ (a b)) (/ b a)

  (@scale (a s)) (values (* a s)) (@iscale (a s)) (values (/ a s))

  (@abs (a)) (abs a) (@neg (a)) (- a) (@square (a)) (* a a)
  (@sqrt (a)) (the pos-@f (sqrt (the pos-@f a)))

  (@len2 (a)) (the pos-@f (mvc #'+ (-@square a)))
  (@len (a)) (the pos-@f a)

  (@norm (a)) (mvc #'-@iscale a (mvc #'-@len a))

  (@^ (a s)) (expt a s) (@mod (a s)) (mod a s)
  (@exp (a)) (values (exp a))

  (@lerp (ax bx s)) (+ ax (* (- bx ax) s))
  (@from (ax bx s)) (+ ax (* bx s))
  (@mid (ax bx)) (* 1/2 (+ ax bx))

  (@clamp (x)) (min 1 (max 0 x))
  (@clamp* (x mi ma)) (min ma (max mi x))

  (@cos-sin (a)) (values (cos a) (sin a))
  (@sin-cos (a)) (values (sin a) (cos a))

  ; (@deg (a)) (values (/ a 360))
  (@deg->rad (deg)) (* @pi (/ deg 180)))

