
(in-package :veq)

(ops

  (:4 @4abs (4!a)) (values (abs ax) (abs ay) (abs az) (abs aw))
  (:4 @4neg (4!a)) (values (- ax) (- ay) (- az) (- aw))
  (:4 @4inv (4!a)) (values (/ ax) (/ ay) (/ az) (/ aw))
  (:4 @4square (4!a)) (values (the pos-@f (* ax ax))
                              (the pos-@f (* ay ay))
                              (the pos-@f (* az az))
                              (the pos-@f (* aw aw)))
  (:4 @4sqrt (4!a)) (values (the pos-@f (sqrt (the pos-@f ax)))
                            (the pos-@f (sqrt (the pos-@f ay)))
                            (the pos-@f (sqrt (the pos-@f az)))
                            (the pos-@f (sqrt (the pos-@f aw))))

  (:1 @4len2 (4!a)) (the pos-@f (mvc #'+ (-@4square ax ay az aw)))
  (:1 @4len (4!a)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@4square ax ay az aw)))))

  (:1 @4max (4!a)) (max ax ay az aw)
  (:1 @4min (4!a)) (min ax ay az aw)

  (:4 @4+ (4!a 4!b)) (values (+ ax bx) (+ ay by) (+ az bz) (+ aw bw))
  (:4 @4- (4!a 4!b)) (values (- ax bx) (- ay by) (- az bz) (- aw bw))
  (:4 @4* (4!a 4!b)) (values (* ax bx) (* ay by) (* az bz) (* aw bw))
  (:4 @4/ (4!a 4!b)) (values (/ ax bx) (/ ay by) (/ az bz) (/ aw bw))

  (:4 @4i- (4!a 4!b)) (values (- bx ax) (- by ay) (- bz az) (- bw aw))
  (:4 @4i/ (4!a 4!b)) (values (/ bx ax) (/ by ay) (/ bz az) (/ bw aw))

  (:1 @4. (4!a 4!b)) (+ (* ax bx) (* ay by) (* az bz) (* aw bw))

  (:1 @4dst2 (4!a 4!b))
       (mvc #'+ (-@4square (- bx ax) (- by ay) (- bz az) (- bw aw)))
  (:1 @4dst (4!a 4!b))
       (sqrt (the pos-@f (mvc #'+ (-@4square (- bx ax) (- by ay)
                                             (- bz az) (- bw aw)))))

  (:4 @4lerp (4!a 4!b s))
      (-@4+ ax ay az aw (* (- bx ax) s) (* (- by ay) s) (* (- bz az) s) (* (- bw aw) s))
  (:4 @4from (4!a 4!b s))
       (-@4+ ax ay az aw (* bx s) (* by s) (* bz s) (* bw s))
  (:4 @4mid (4!a 4!b))
       (values (* (+ bx ax) 1/2) (* (+ by ay) 1/2)
               (* (+ bz az) 1/2) (* (+ bw aw) 1/2))

  (:4 @4^ (4!a s)) (values (expt ax s) (expt ay s) (expt az s) (expt aw s))
  (:4 @4exp (4!a)) (values (exp ax) (exp ay) (exp az) (exp aw))
  (:4 @4mod (4!a s)) (values (mod ax s) (mod ay s) (mod az s) (mod aw s))

  (:4 @4scale (4!a s)) (values (* ax s) (* ay s) (* az s) (* aw s))
  (:4 @4iscale (4!a s)) (values (/ ax s) (/ ay s) (/ az s) (/ aw s))

  (:4 @4norm (4!a)) (mvc #'-@4iscale ax ay az aw (the pos-@f (mvc #'-@4len ax ay az aw))))

