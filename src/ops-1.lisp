
(in-package :veq)

(ops

  ; these ops have been replaced by vv for all dims:
  ; (:1 @+ (ax bx)) (+ ax bx) (:1 @- (ax bx)) (- ax bx)
  ; (:1 @* (ax bx)) (* ax bx) (:1 @/ (ax bx)) (/ ax bx)
  ; (:1 @abs (ax)) (abs ax) (:1 @neg (ax)) (- ax)
  ; (:1 @inv (ax)) (/ ax)

  (:1 @id (ax)) (values ax)
  (:1 @i- (ax bx)) (- bx ax) (:1 @i/ (ax bx)) (/ bx ax)

  (:1 @scale (ax s)) (* ax s) (:1 @iscale (ax s)) (/ ax s)

  (:1 @square (ax)) (* ax ax)
  (:1 @sqrt (ax)) (the pos-@f (sqrt (the pos-@f ax)))

  (:1 @len2 (ax)) (the pos-@f (mvc #'+ (-@square ax)))
  (:1 @len (ax)) (the pos-@f ax)

  (:1 @lerp (ax bx s)) (+ ax (* (- bx ax) s))
  (:1 @from (ax bx s)) (+ ax (* bx s))
  (:1 @mid (ax bx)) (* (+ ax bx) 1/2)

  (:1 @clamp (x)) (min 1 (max 0 x))
  (:1 @clamp* (x mi ma)) (min ma (max mi x))

  (:2 @cos-sin (ax)) (values (cos ax) (sin ax))
  (:2 @sin-cos (ax)) (values (sin ax) (cos ax))

  (:1 @deg->rad (d)) (* @pi (/ d 180)))

