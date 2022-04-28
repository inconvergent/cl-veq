
(in-package :veq)

(ops

  (@3abs (a b c)) (values (abs a) (abs b) (abs c))
  (@3neg (a b c)) (values (- a) (- b) (- c))
  (@3square (a b c)) (values (the pos-@f (* a a)) (the pos-@f (* b b)) (the pos-@f (* c c)))
  (@3sqrt (a b c)) (values (the pos-@f (sqrt (the pos-@f a))) (the pos-@f (sqrt (the pos-@f b))) (the pos-@f (sqrt (the pos-@f c))))


  (@3len2 (a b c)) (the pos-@f (mvc #'+ (-@3square a b c)))
  (@3len (a b c)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@3square a b c)))))

  (@3max (a b c)) (max a b c) (@3min (a b c)) (min a b c)

  (@3+ (ax ay az bx by bz)) (values (+ ax bx) (+ ay by) (+ az bz))
  (@3- (ax ay az bx by bz)) (values (- ax bx) (- ay by) (- az bz))
  (@3* (ax ay az bx by bz)) (values (* ax bx) (* ay by) (* az bz))
  (@3/ (ax ay az bx by bz)) (values (/ ax bx) (/ ay by) (/ az bz))

  (@3i- (ax ay az bx by bz)) (values (- bx ax) (- by ay) (- bz az))
  (@3i/ (ax ay az bx by bz)) (values (/ bx ax) (/ by ay) (/ bz az))

  (@3cross (ax ay az bx by bz)) (values (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))

  (@3. (ax ay az bx by bz)) (+ (* ax bx) (* ay by) (* az bz))

  (@3dst2 (ax ay az bx by bz)) (mvc #'+ (-@3square (- bx ax) (- by ay) (- bz az)))
  (@3dst (ax ay az bx by bz)) (sqrt (the pos-@f (mvc #'+ (-@3square (- bx ax) (- by ay) (- bz az)))))

  (@3lerp (ax ay az bx by bz s)) (-@3+ ax ay az (* (- bx ax) s) (* (- by ay) s) (* (- bz az) s))
  (@3from (ax ay az bx by bz s)) (-@3+ ax ay az (* bx s) (* by s) (* bz s))
  (@3mid (ax ay az bx by bz)) (values (* (+ bx ax) 1/2) (* (+ by ay) 1/2) (* (+ bz az) 1/2))

  (@3^ (a b c s)) (values (expt a s) (expt b s) (expt c s))
  (@3exp (a b c)) (values (exp a) (exp b) (exp c))

  (@3mod (a b c s)) (values (mod a s) (mod b s) (mod c s))

  (@3scale (a b c s)) (values (* a s) (* b s) (* c s))
  (@3iscale (a b c s)) (values (/ a s) (/ b s) (/ c s))

  (@3norm (a b c)) (mvc #'-@3iscale a b c (the pos-@f (mvc #'-@3len a b c)))

; ;;;;;;;;;;;;;;;;;;; other

  (@3rot (x y z nx ny nz a))
    (let ((cosa (cos a)))
      (declare (@f cosa))
      (mvc #'-@3from
           (mvc #'-@3from (-@3scale x y z cosa) (-@3cross nx ny nz x y z) (sin a))
           nx ny nz (* (-@3. nx ny nz x y z) (- 1 cosa))))

  (@3rots (x y z nx ny nz a sx sy sz))
    (mvc #'-@3+ (mvc #'-@3rot (-@3- x y z sx sy sz) nx ny nz a) sx sy sz))

