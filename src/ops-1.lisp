
(in-package :veq)

(ops

  (:1 @+ (ax bx)) (+ ax bx)
  (:1 @- (ax bx)) (- ax bx)
  (:1 @* (ax bx)) (* ax bx)
  (:1 @/ (ax bx)) (/ ax bx)
  (:1 @i- (ax bx)) (- bx ax)
  (:1 @i/ (ax bx)) (/ bx ax)

  (:1 @scale (ax s)) (values (* ax s))
  (:1 @iscale (ax s)) (values (/ ax s))

  (:1 @abs (ax)) (abs ax)
  (:1 @neg (ax)) (- ax)
  (:1 @square (ax)) (* ax ax)
  (:1 @sqrt (ax)) (the pos-@f (sqrt (the pos-@f ax)))

  (:1 @len2 (ax)) (the pos-@f (mvc #'+ (-@square ax)))
  (:1 @len (ax)) (the pos-@f ax)

  (:1 @norm (ax)) (mvc #'-@iscale ax (mvc #'-@len ax))

  (:1 @^ (ax s)) (expt ax s)
  (:1 @mod (ax s)) (mod ax s)
  (:1 @exp (ax)) (values (exp ax))

  (:1 @lerp (ax bx s)) (+ ax (* (- bx ax) s))
  (:1 @from (ax bx s)) (+ ax (* bx s))
  (:1 @mid (ax bx)) (* 1/2 (+ ax bx))

  (:1 @clamp (x)) (min 1 (max 0 x))
  (:1 @clamp* (x mi ma)) (min ma (max mi x))

  (:2 @cos-sin (ax)) (values (cos ax) (sin ax))
  (:2 @sin-cos (ax)) (values (sin ax) (cos ax))

  ; (:1 @deg (ax)) (values (/ ax 360))
  (:1 @deg->rad (d)) (* @pi (/ d 180)))

