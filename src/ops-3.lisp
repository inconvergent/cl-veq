
(in-package :veq)

(ops

  (:3 @3id (3!a)) (values ax ay az)
  (:3 @3square (3!a)) (values (the pos-@f (* ax ax))
                              (the pos-@f (* ay ay))
                              (the pos-@f (* az az)))
  (:3 @3sqrt (3!a)) (values (the pos-@f (sqrt (the pos-@f ax)))
                            (the pos-@f (sqrt (the pos-@f ay)))
                            (the pos-@f (sqrt (the pos-@f az))))


  (:1 @3len2 (3!a)) (the pos-@f (mvc #'+ (-@3square ax ay az)))
  (:1 @3len (3!a)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@3square ax ay az)))))

  (:1 @3max (3!a)) (max ax ay az)
  (:1 @3min (3!a)) (min ax ay az)

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

; ;:;;;;;;;;;;;;;;;;;; other

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

