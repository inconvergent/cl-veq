
(in-package :veq)

(ops

  (:2 @2+ (2!a 2!b)) (values (+ ax bx) (+ ay by))
  (:2 @2- (2!a 2!b)) (values (- ax bx) (- ay by))
  (:2 @2* (2!a 2!b)) (values (* ax bx) (* ay by))
  (:2 @2/ (2!a 2!b)) (values (/ ax bx) (/ ay by))

  (:2 @2i- (2!a 2!b)) (values (- bx ax) (- by ay))
  (:2 @2i/ (2!a 2!b)) (values (/ bx ax) (/ by ay))

  (:2 @2square (2!a)) (values (* ax ax) (* ay ay))

  (:1 @2len2 (2!a)) (the pos-@f (mvc #'+ (-@2square ax ay)))
  (:1 @2len (2!a)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@2square ax ay)))))

  (:2 @2^ (2!a s)) (values (expt ax s) (expt ay s))

  (:2 @2exp (2!a)) (values (exp ax) (exp ay))
  (:2 @2mod (2!a s)) (values (mod ax s) (mod ay s))
  (:2 @2scale (2!a s)) (values (* ax s) (* ay s))
  (:2 @2iscale (2!a s)) (values (/ ax s) (/ ay s))

  (:2 @2abs (2!a)) (values (abs ax) (abs ay))
  (:2 @2neg (2!a)) (values (- ax) (- ay))
  (:2 @2inv (2!a)) (values (/ ax) (/ ay))
  (:2 @2perp (2!a)) (values ay (- ax))
  (:2 @2perp* (2!a)) (values (- ay) ax)
  (:2 @2flip (2!a)) (values ay ax)
  (:2 @2sqrt (2!a)) (values (the pos-@f (sqrt (the pos-@f ax)))
                            (the pos-@f (sqrt (the pos-@f ay))))

  (:1 @2max (2!a)) (max ax ay)
  (:1 @2min (2!a)) (min ax ay)

  (:2 @2norm (2!a)) (mvc #'-@2iscale ax ay (mvc #'-@2len ax ay))

  (:1 @2angle (2!a)) (mvc #'atan (-@2norm ay ax))

  (:1 @2. (2!a 2!b)) (+ (* ax bx) (* ay by))
  (:2 @2cross (2!a 2!b)) (- (* ax by) (* ay bx))

  (:1 @2dst2 (2!a 2!b)) (mvc #'+ (-@2square (- bx ax) (- by ay)))
  (:1 @2dst  (2!a 2!b)) (sqrt (the pos-@f (mvc #'+ (-@2square (- bx ax)
                                                              (- by ay)))))

  (:2 @2lerp (2!a 2!b s)) (-@2+ ax ay (* (- bx ax) s) (* (- by ay) s))
  (:2 @2from (2!a 2!b s)) (-@2+ ax ay (* bx s) (* by s))
  (:2 @2mid (2!a 2!b)) (values (* 1/2 (+ ax bx)) (* 1/2 (+ ay by)))

  ; ;: OTHER

  (:2 @2on-circ (ax rad)) (mvc #'-@2scale (-@cos-sin (* ax @pii)) rad)
  (:2 @2on-circ* (ax rad)) (mvc #'-@2scale (-@cos-sin ax) rad)

  (:2 @2rot (2!a angle))
    (let ((cosa (cos angle)) (sina (sin angle)))
      (declare (@f cosa sina))
      (values (- (* ax cosa) (* ay sina))
              (+ (* ax sina) (* ay cosa))))
  (:2 @2rots (2!a angle 2!s))
     (mvc #'-@2+ (mvc #'-@2rot (-@2- ax ay sx sy) angle) sx sy))

