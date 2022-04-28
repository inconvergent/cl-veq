
(in-package :veq)

(ops

  (@4abs (a b c d)) (values (abs a) (abs b) (abs c) (abs d))
  (@4neg (a b c d)) (values (- a) (- b) (- c) (- d))
  (@4square (a b c d)) (values (the pos-@f (* a a)) (the pos-@f (* b b)) (the pos-@f (* c c)) (the pos-@f (* d d)))
  (@4sqrt (a b c d)) (values (the pos-@f (sqrt (the pos-@f a))) (the pos-@f (sqrt (the pos-@f b))) (the pos-@f (sqrt (the pos-@f c))) (the pos-@f (sqrt (the pos-@f d))))

  (@4len2 (a b c d)) (the pos-@f (mvc #'+ (-@4square a b c d)))
  (@4len (a b c d)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@4square a b c d)))))


  (@4max (a b c d)) (max a b c d) (@4min (a b c d)) (min a b c d)

  (@4+ (ax ay az aw bx by bz bw)) (values (+ ax bx) (+ ay by) (+ az bz) (+ aw bw))
  (@4- (ax ay az aw bx by bz bw)) (values (- ax bx) (- ay by) (- az bz) (- aw bw))
  (@4* (ax ay az aw bx by bz bw)) (values (* ax bx) (* ay by) (* az bz) (* aw bw))
  (@4/ (ax ay az aw bx by bz bw)) (values (/ ax bx) (/ ay by) (/ az bz) (/ aw bw))

  (@4i- (ax ay az aw bx by bz bw)) (values (- bx ax) (- by ay) (- bz az) (- bw aw))
  (@4i/ (ax ay az aw bx by bz bw)) (values (/ bx ax) (/ by ay) (/ bz az) (/ bw aw))

  (@4. (ax ay az aw bx by bz bw)) (+ (* ax bx) (* ay by) (* az bz) (* aw bw))

  (@4dst2 (ax ay az aw bx by bz bw)) (mvc #'+ (-@4square (- bx ax) (- by ay) (- bz az) (- bw aw)))
  (@4dst (ax ay az aw bx by bz bw)) (sqrt (the pos-@f (mvc #'+ (-@4square (- bx ax) (- by ay) (- bz az) (- bw aw)))))

  (@4lerp (ax ay az aw bx by bz bw s)) (-@4+ ax ay az aw (* (- bx ax) s) (* (- by ay) s) (* (- bz az) s) (* (- bw aw) s))
  (@4from (ax ay az aw bx by bz bw s)) (-@4+ ax ay az aw (* bx s) (* by s) (* bz s) (* bw s))
  (@4mid (ax ay az aw bx by bz bw)) (values (* (+ bx ax) 1/2) (* (+ by ay) 1/2) (* (+ bz az) 1/2) (* (+ bw aw) 1/2))

  (@4^ (a b c d s)) (values (expt a s) (expt b s) (expt c s) (expt d s))
  (@4exp (a b c d)) (values (exp a) (exp b) (exp c) (exp d))
  (@4mod (a b c d s)) (values (mod a s) (mod b s) (mod c s) (mod d s))

  (@4scale (a b c d s)) (values (* a s) (* b s) (* c s) (* d s))
  (@4iscale (a b c d s)) (values (/ a s) (/ b s) (/ c s) (/ d s))

  (@4norm (a b c d)) (mvc #'-@4iscale a b c d (the pos-@f (mvc #'-@4len a b c d))))

