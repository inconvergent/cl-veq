
(in-package :veq)

(ops

  (d3abs (a b c)) (values (abs a) (abs b) (abs c))
  (d3neg (a b c)) (values (- a) (- b) (- c))
  (d3square (a b c)) (values (* a a) (* b b) (* c c))
  (d3sqrt (a b c)) (values (the pos-df (sqrt (the pos-df a)))
                           (the pos-df (sqrt (the pos-df b)))
                           (the pos-df (sqrt (the pos-df c))))

  (f3abs (a b c)) (values (abs a) (abs b) (abs c))
  (f3neg (a b c)) (values (- a) (- b) (- c))
  (f3square (a b c)) (values (* a a) (* b b) (* c c))
  (f3sqrt (a b c)) (values (the pos-ff (sqrt (the pos-ff a)))
                           (the pos-ff (sqrt (the pos-ff b)))
                           (the pos-ff (sqrt (the pos-ff c))))


  (d3len2 (a b c)) (the pos-df (mvc #'+ (-d3square a b c)))
  (d3len (a b c)) (the pos-df (sqrt (the pos-df (mvc #'+ (-d3square a b c)))))

  (f3len2 (a b c)) (the pos-ff (mvc #'+ (-f3square a b c)))
  (f3len (a b c)) (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f3square a b c)))))

  (d3max (a b c)) (max a b c) (d3min (a b c)) (min a b c)
  (f3max (a b c)) (max a b c) (f3min (a b c)) (min a b c)

  (d3+ (ax ay az bx by bz)) (values (+ ax bx) (+ ay by) (+ az bz))
  (d3- (ax ay az bx by bz)) (values (- ax bx) (- ay by) (- az bz))
  (d3* (ax ay az bx by bz)) (values (* ax bx) (* ay by) (* az bz))
  (d3/ (ax ay az bx by bz)) (values (/ ax bx) (/ ay by) (/ az bz))

  (d3i- (ax ay az bx by bz)) (values (- bx ax) (- by ay) (- bz az))
  (d3i/ (ax ay az bx by bz)) (values (/ bx ax) (/ by ay) (/ bz az))

  (f3+ (ax ay az bx by bz)) (values (+ ax bx) (+ ay by) (+ az bz))
  (f3- (ax ay az bx by bz)) (values (- ax bx) (- ay by) (- az bz))
  (f3* (ax ay az bx by bz)) (values (* ax bx) (* ay by) (* az bz))
  (f3/ (ax ay az bx by bz)) (values (/ ax bx) (/ ay by) (/ az bz))

  (f3i- (ax ay az bx by bz)) (values (- bx ax) (- by ay) (- bz az))
  (f3i/ (ax ay az bx by bz)) (values (/ bx ax) (/ by ay) (/ bz az))

  (d3cross (ax ay az bx by bz)) (values (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))
  (f3cross (ax ay az bx by bz)) (values (- (* ay bz) (* az by)) (- (* az bx) (* ax bz)) (- (* ax by) (* ay bx)))

  (d3. (ax ay az bx by bz)) (+ (* ax bx) (* ay by) (* az bz))

  (d3dst2 (ax ay az bx by bz)) (mvc #'+ (-d3square (- bx ax) (- by ay) (- bz az)))
  (d3dst (ax ay az bx by bz)) (sqrt (the pos-df (mvc #'+ (-d3square (- bx ax) (- by ay) (- bz az)))))

  (f3. (ax ay az bx by bz)) (+ (* ax bx) (* ay by) (* az bz))

  (f3dst2 (ax ay az bx by bz)) (mvc #'+ (-f3square (- bx ax) (- by ay) (- bz az)))
  (f3dst (ax ay az bx by bz)) (sqrt (the pos-ff (mvc #'+ (-f3square (- bx ax) (- by ay) (- bz az)))))

  (d3lerp (ax ay az bx by bz s)) (-d3+ ax ay az (* (- bx ax) s) (* (- by ay) s) (* (- bz az) s))
  (d3from (ax ay az bx by bz s)) (-d3+ ax ay az (* bx s) (* by s) (* bz s))
  (d3mid (ax ay az bx by bz)) (values (* (+ bx ax) 0.5d0) (* (+ by ay) 0.5d0) (* (+ bz az) 0.5d0))

  (f3lerp (ax ay az bx by bz s)) (-f3+ ax ay az (* (- bx ax) s) (* (- by ay) s) (* (- bz az) s))
  (f3from (ax ay az bx by bz s)) (-f3+ ax ay az (* bx s) (* by s) (* bz s))
  (f3mid (ax ay az bx by bz)) (values (* (+ bx ax) 0.5f0) (* (+ by ay) 0.5f0) (* (+ bz az) 0.5f0))

  (d3^ (a b c s)) (values (expt a s) (expt b s) (expt c s))
  (d3mod (a b c s)) (values (mod a s) (mod b s) (mod c s))

  (f3^ (a b c s)) (values (expt a s) (expt b s) (expt c s))
  (f3mod (a b c s)) (values (mod a s) (mod b s) (mod c s))

  (d3scale (a b c s)) (values (* a s) (* b s) (* c s))
  (d3iscale (a b c s)) (values (/ a s) (/ b s) (/ c s))

  (f3scale (a b c s)) (values (* a s) (* b s) (* c s))
  (f3iscale (a b c s)) (values (/ a s) (/ b s) (/ c s))

  (d3norm (a b c)) (mvc #'-d3iscale a b c (the pos-df (mvc #'-d3len a b c)))
  (f3norm (a b c)) (mvc #'-f3iscale a b c (the pos-ff (mvc #'-f3len a b c)))

; ;;;;;;;;;;;;;;;;;;; other

  (d3rot (x y z nx ny nz a))
  (let ((cosa (cos a)))
    (declare (df cosa))
    (mvc #'-d3from
         (mvc #'-d3from (-d3scale x y z cosa) (-d3cross nx ny nz x y z) (sin a))
         nx ny nz (* (-d3. nx ny nz x y z) (- 1d0 cosa))))

  (d3rots (x y z nx ny nz a sx sy sz))
  (mvc #'-d3+ (mvc #'-d3rot (-d3- x y z sx sy sz) nx ny nz a) sx sy sz)

  (f3rot (x y z nx ny nz a))
  (let ((cosa (cos a)))
    (declare (ff cosa))
    (mvc #'-f3from (mvc #'-f3from
                        (-f3scale x y z cosa) (-f3cross nx ny nz x y z) (sin a))
         nx ny nz (* (-f3. nx ny nz x y z) (- 1f0 cosa))))

  (f3rots (x y z nx ny nz a sx sy sz))
  (mvc #'-f3+ (mvc #'-f3rot (-f3- x y z sx sy sz) nx ny nz a) sx sy sz))

