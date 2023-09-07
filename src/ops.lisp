
(in-package :veq)

(ops
  (:1 @id (ax)) (values ax)
  (:1 @i- (ax bx)) (- bx ax) (:1 @i/ (ax bx)) (/ bx ax)

  (:1 @scale (ax s)) (* ax s) (:1 @iscale (ax s)) (/ ax s)

  (:1 @square (ax)) (* ax ax)

  (:1 @len2 (ax)) (the pos-@f (mvc #'+ (-@square ax)))
  (:1 @len (ax)) (the pos-@f ax)

  (:1 @lerp (ax bx s)) (+ ax (* (- bx ax) s))
  (:1 @from (ax bx s)) (+ ax (* bx s))
  (:1 @mid (ax bx)) (* (+ ax bx) 1/2)

  (:1 @clamp (x)) (min 1 (max 0 x)) (:1 @clamp* (x mi ma)) (min ma (max mi x))

  (:2 @cos-sin (ax)) (values (cos ax) (sin ax))
  (:2 @sin-cos (ax)) (values (sin ax) (cos ax))

  (:1 @deg->rad (d)) (* @pi (/ d 180)))

(ops
  (:2 @2id (2!a)) (values ax ay)

  (:2 @2i- (2!a 2!b)) (values (- bx ax) (- by ay))
  (:2 @2i/ (2!a 2!b)) (values (/ bx ax) (/ by ay))

  (:2 @2square (2!a)) (values (* ax ax) (* ay ay))

  (:1 @2len2 (2!a)) (the pos-@f (mvc #'+ (-@2square ax ay)))
  (:1 @2len (2!a)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@2square ax ay)))))

  (:2 @2scale (2!a s)) (values (* ax s) (* ay s))
  (:2 @2iscale (2!a s)) (values (/ ax s) (/ ay s))

  (:2 @2perp (2!a)) (values ay (- ax))
  (:2 @2perp* (2!a)) (values (- ay) ax)
  (:2 @2flip (2!a)) (values ay ax)

  (:1 @2max (2!a)) (max ax ay) (:1 @2min (2!a)) (min ax ay)

  (:2 @2norm (2!a)) (mvc #'-@2iscale ax ay (mvc #'-@2len ax ay))

  (:1 @2angle (2!a)) (mvc #'atan (-@2norm ay ax))

  (:1 @2dot (2!a 2!b)) (+ (* ax bx) (* ay by))
  (:2 @2cross (2!a 2!b)) (- (* ax by) (* ay bx))

  (:1 @2dst2 (2!a 2!b)) (mvc #'+ (-@2square (- bx ax) (- by ay)))
  (:1 @2dst  (2!a 2!b)) (sqrt (the pos-@f (mvc #'+ (-@2square (- bx ax)
                                                              (- by ay)))))

  (:2 @2lerp (2!a 2!b s)) (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s)))
  (:2 @2from (2!a 2!b s)) (values (+ ax (* bx s)) (+ ay (* by s)))
  (:2 @2mid (2!a 2!b)) (values (* (+ ax bx) 1/2) (* (+ ay by) 1/2))

  ; ;: OTHER

  (:2 @2on-circ (ax rad)) (mvc #'-@2scale (-@cos-sin (* ax @pii)) rad)
  (:2 @2on-circ* (ax rad)) (mvc #'-@2scale (-@cos-sin ax) rad)

  (:2 @2rot (2!a angle))
    (let ((cosa (cos angle)) (sina (sin angle)))
      (declare (@f cosa sina))
      (values (- (* ax cosa) (* ay sina))
              (+ (* ax sina) (* ay cosa))))
  (:2 @2rots (2!a angle 2!s)) (mvb (rx ry) (mvc #'-@2rot (- ax sx) (- ay sy) angle)
                                   (+ sx rx) (+ sy ry)))

(ops
  (:3 @3id (3!a)) (values ax ay az)
  (:3 @3square (3!a)) (values (the pos-@f (* ax ax))
                              (the pos-@f (* ay ay))
                              (the pos-@f (* az az)))

  (:1 @3len2 (3!a)) (the pos-@f (mvc #'+ (-@3square ax ay az)))
  (:1 @3len (3!a)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@3square ax ay az)))))

  (:1 @3max (3!a)) (max ax ay az) (:1 @3min (3!a)) (min ax ay az)

  (:3 @3i- (3!a 3!b)) (values (- bx ax) (- by ay) (- bz az))
  (:3 @3i/ (3!a 3!b)) (values (/ bx ax) (/ by ay) (/ bz az))

  (:3 @3cross (3!a 3!b)) (values (- (* ay bz) (* az by))
                                 (- (* az bx) (* ax bz))
                                 (- (* ax by) (* ay bx)))

  (:1 @3dot (3!a 3!b)) (+ (* ax bx) (* ay by) (* az bz))

  (:1 @3dst2 (3!a 3!b)) (mvc #'+ (-@3square (- bx ax) (- by ay) (- bz az)))
  (:1 @3dst (3!a 3!b)) (sqrt (the pos-@f (mvc #'+ (-@3square
                                                    (- bx ax) (- by ay) (- bz az)))))

  (:3 @3lerp (3!a 3!b s)) (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                                  (+ az (* (- bz az) s)))

  (:3 @3from (3!a 3!b s)) (values (+ ax (* bx s)) (+ ay (* by s))
                                  (+ az (* bz s)))
  (:3 @3mid (3!a 3!b)) (values (* (+ ax bx) 1/2) (* (+ ay by) 1/2)
                               (* (+ az bz) 1/2))

  (:3 @3scale (3!a s)) (values (* ax s) (* ay s) (* az s))
  (:3 @3iscale (3!a s)) (values (/ ax s) (/ ay s) (/ az s))

  (:3 @3norm (3!a)) (mvc #'-@3iscale ax ay az (the pos-@f (mvc #'-@3len ax ay az)))

  (:3 @3rot (3!a 3!n a))
       (let ((cosa (cos a)))
         (declare (@f cosa))
         (mvc #'-@3from
              (mvc #'-@3from (-@3scale ax ay az cosa)
                             (-@3cross nx ny nz ax ay az) (sin a))
              nx ny nz (* (-@3dot nx ny nz ax ay az) (- 1 cosa))))
  (:3 @3rots (3!a 3!n a 3!s))
       (mvb (rx ry rz) (mvc #'-@3rot (- ax sx) (- ay sy) (- az sz) nx ny nz a)
            (values (+ (the @f rx) sx) (+ (the @f ry) sy) (+ (the @f rz) sz))))

(ops
  (:4 @4id (4!a)) (values ax ay az aw)
  (:4 @4square (4!a)) (values (the pos-@f (* ax ax)) (the pos-@f (* ay ay))
                              (the pos-@f (* az az)) (the pos-@f (* aw aw)))

  (:1 @4len2 (4!a)) (the pos-@f (mvc #'+ (-@4square ax ay az aw)))
  (:1 @4len (4!a)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@4square ax ay az aw)))))

  (:1 @4max (4!a)) (max ax ay az aw)
  (:1 @4min (4!a)) (min ax ay az aw)

  (:4 @4i- (4!a 4!b)) (values (- bx ax) (- by ay) (- bz az) (- bw aw))
  (:4 @4i/ (4!a 4!b)) (values (/ bx ax) (/ by ay) (/ bz az) (/ bw aw))

  (:1 @4dot (4!a 4!b)) (+ (* ax bx) (* ay by) (* az bz) (* aw bw))

  (:1 @4dst2 (4!a 4!b)) (mvc #'+ (-@4square (- bx ax) (- by ay) (- bz az) (- bw aw)))
  (:1 @4dst (4!a 4!b)) (sqrt (the pos-@f (mvc #'+ (-@4square (- bx ax) (- by ay)
                                                             (- bz az) (- bw aw)))))

  (:4 @4lerp (4!a 4!b s)) (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                                  (+ az (* (- bz az) s)) (+ aw (* (- bw aw) s)))
  (:4 @4from (4!a 4!b s)) (values (+ ax (* bx s)) (+ ay (* by s))
                                  (+ az (* bz s)) (+ aw (* bw s)))
  (:4 @4mid (4!a 4!b)) (values (* (+ ax bx) 1/2) (* (+ ay by) 1/2)
                               (* (+ az bz) 1/2) (* (+ aw bw) 1/2))

  (:4 @4scale (4!a s)) (values (* ax s) (* ay s) (* az s) (* aw s))
  (:4 @4iscale (4!a s)) (values (/ ax s) (/ ay s) (/ az s) (/ aw s))

  (:4 @4norm (4!a)) (mvc #'-@4iscale ax ay az aw (the pos-@f (mvc #'-@4len ax ay az aw))))

