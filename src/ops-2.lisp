
(in-package :veq)

(ops

  (@2^ (a b s)) (values (expt a s) (expt b s))
  (@2exp (a b)) (values (exp a) (exp b))
  (@2mod (a b s)) (values (mod a s) (mod b s))
  (@2scale (a b s)) (values (* a s) (* b s))
  (@2iscale (a b s)) (values (/ a s) (/ b s))

  (@2abs (a b)) (values (abs a) (abs b))
  (@2neg (a b)) (values (- a) (- b))
  (@2perp (a b)) (values b (- a))
  (@2perp* (a b)) (values (- b) a)
  (@2flip (a b)) (values b a)
  (@2square (a b)) (values (* a a) (* b b))
  (@2sqrt (a b)) (values (the pos-@f (sqrt (the pos-@f a))) (the pos-@f (sqrt (the pos-@f b))))

  (@2len2 (a b)) (the pos-@f (mvc #'+ (-@2square a b)))
  (@2len (a b)) (the pos-@f (sqrt (the pos-@f (mvc #'+ (-@2square a b)))))

  (@2max (a b)) (max a b) (@2min (a b)) (min a b)

  (@2norm (a b)) (mvc #'-@2iscale a b (mvc #'-@2len a b))

  (@2angle (a b)) (mvc #'atan (-@2norm b a))

  (@2+ (ax ay bx by)) (values (+ ax bx) (+ ay by))
  (@2- (ax ay bx by)) (values (- ax bx) (- ay by))
  (@2* (ax ay bx by)) (values (* ax bx) (* ay by))
  (@2/ (ax ay bx by)) (values (/ ax bx) (/ ay by))

  (@2i- (ax ay bx by)) (values (- bx ax) (- by ay))
  (@2i/ (ax ay bx by)) (values (/ bx ax) (/ by ay))

  (@2cross (ax ay bx by)) (- (* ax by) (* ay bx))

  (@2. (ax ay bx by)) (+ (* ax bx) (* ay by))

  (@2dst2 (ax ay bx by)) (mvc #'+ (-@2square (- bx ax) (- by ay)))
  (@2dst (ax ay bx by)) (sqrt (the pos-@f (mvc #'+ (-@2square (- bx ax) (- by ay)))))

  (@2lerp (ax ay bx by s)) (-@2+ ax ay (* (- bx ax) s) (* (- by ay) s))
  (@2from (ax ay bx by s)) (-@2+ ax ay (* bx s) (* by s))
  (@2mid (ax ay bx by)) (values (* 1/2 (+ ax bx)) (* 1/2 (+ ay by)))

  ; OTHER

  (@2on-circ (a rad)) (mvc #'-@2scale (-@cos-sin (* a @pii)) rad)
  (@2on-circ* (a rad)) (mvc #'-@2scale (-@cos-sin a) rad)

  (@2rot (x y a)) (let ((cosa (cos a)) (sina (sin a)))
                    (declare (@f cosa sina))
                    (values (- (* x cosa) (* y sina))
                            (+ (* x sina) (* y cosa))))
  (@2rots (x y a sx sy)) (mvc #'-@2+ (mvc #'-@2rot (-@2- x y sx sy) a) sx sy))
