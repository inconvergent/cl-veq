
(in-package :veq)

(declaim (inline d2$zero))
(defun d2$zero (&optional (n 1))
  (declare #.*opt* (pos-int n))
  (d$ :dim 2 :n n))

(declaim (inline d2$one))
(defun d2$one (&optional (n 1))
  (declare #.*opt* (pos-int n))
  (d$ :dim 2 :n n :v 1d0))

(declaim (inline d2$val))
(defun d2$val (v &optional (n 1))
  (declare #.*opt* (pos-int n))
  (d$ :dim 2 :n n :v v))

(declaim (inline f2$zero))
(defun f2$zero (&optional (n 1))
  (declare #.*opt* (pos-int n))
  (f$ :dim 2 :n n))

(declaim (inline f2$one))
(defun f2$one (&optional (n 1))
  (declare #.*opt* (pos-int n))
  (f$ :dim 2 :n n :v 1f0))

(declaim (inline f2$val))
(defun f2$val (v &optional (n 1))
  (declare #.*opt* (pos-int n))
  (f$ :dim 2 :n n :v v))


;;;;;;;;;;;;;;;;;;;;;; ACCESS


(defun 2with (v i type body)
  (declare (symbol v type))
  (awg (i* ii xx yy)
    `(let* ((,i* ,i)
            (,ii (* 2 ,i*)))
      (declare (pos-int ,i* ,ii))
      (mvb (,xx ,yy) (mvc ,@body
                          (funcall #',(veqsymb 2 type ">>" :pref "-")
                                   ,v ,i*))
        (declare (,type ,xx ,yy))
        (setf (aref ,v ,ii) ,xx
              (aref ,v (the pos-int (1+ ,ii))) ,yy)
        (values ,xx ,yy)))))

(defmacro d2with ((v i) &body body)
  (declare (symbol v))
  "
  execute (funcall body x y) for v[i]. body must be a function that returns
  (values x y), the new value for v[i]
  "
  (2with v i 'df body))

(defmacro f2with ((v i) &body body)
  (declare (symbol v))
  "
  execute (funcall body x y) for v[i]. body must be a function that returns
  (values x y), the new value for v[i]
  "
  (2with v i 'ff body))


(declaim (inline -d2>))
(defun -d2> (v)
  (declare #.*opt* (dvec v))
  (values (the df (aref v 0)) (the df (aref v 1))))

(declaim (inline -f2>))
(defun -f2> (v)
  (declare #.*opt* (fvec v))
  (values (the ff (aref v 0)) (the ff (aref v 1))))

(declaim (inline -d2>>))
(defun -d2>> (v i)
  (declare #.*opt* (dvec v) (pos-int i))
  (let ((ii (* 2 i)))
    (declare (pos-int ii))
    (values (the df (aref v ii))
            (the df (aref v (the pos-int (1+ ii)))))))

(declaim (inline -f2>>))
(defun -f2>> (v i)
  (declare #.*opt* (fvec v) (pos-int i))
  (let ((ii (* 2 i)))
    (declare (pos-int ii))
    (values (the ff (aref v ii))
            (the ff (aref v (the pos-int (1+ ii)))))))


(ops

  (d2^ (a b s)) (values (expt a s) (expt b s))
  (d2mod (a b s)) (values (mod a s) (mod b s))

  (d2scale (a b s)) (values (* a s) (* b s))
  (d2iscale (a b s)) (values (/ a s) (/ b s))

  (f2scale (a b s)) (values (* a s) (* b s))
  (f2iscale (a b s)) (values (/ a s) (/ b s))

  (f2^ (a b s)) (values (expt a s) (expt b s))
  (f2mod (a b s)) (values (mod a s) (mod b s))

  (d2abs (a b)) (values (abs a) (abs b))
  (d2neg (a b)) (values (- a) (- b))
  (d2perp (a b)) (values (- b) a)
  (d2perp* (a b)) (values b (- a))
  (d2flip (a b)) (values b a)
  (d2square (a b)) (values (* a a) (* b b))
  (d2sqrt (a b)) (values (the pos-df (sqrt (the pos-df a)))
                         (the pos-df (sqrt (the pos-df b))))

  (f2abs (a b)) (values (abs a) (abs b))
  (f2neg (a b)) (values (- a) (- b))
  (f2perp (a b)) (values (- b) a)
  (f2perp* (a b)) (values b (- a))
  (f2flip (a b)) (values b a)
  (f2square (a b)) (values (* a a) (* b b))
  (f2sqrt (a b)) (values (the pos-ff (sqrt (values (the pos-ff a))))
                         (the pos-ff (sqrt (values (the pos-ff b)))))


  (d2len2 (a b)) (the pos-df (mvc #'+ (-d2square a b)))
  (d2len (a b)) (the pos-df (sqrt (the pos-df (mvc #'+ (-d2square a b)))))

  (d2max (a b)) (max a b)
  (d2min (a b)) (min a b)

  (f2len2 (a b)) (the pos-ff (mvc #'+ (-f2square a b)))
  (f2len (a b)) (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f2square a b)))))

  (f2max (a b)) (max a b)
  (f2min (a b)) (min a b)

  (d2norm (a b)) (mvc #'-d2iscale a b (mvc #'-d2len a b))
  (f2norm (a b)) (mvc #'-f2iscale a b (mvc #'-f2len a b))

  (d2angle (a b)) (mvc #'atan (-d2norm b a))
  (f2angle (a b)) (mvc #'atan (-f2norm b a))


  (d2+ (ax ay bx by)) (values (+ ax bx) (+ ay by))
  (d2- (ax ay bx by)) (values (- ax bx) (- ay by))
  (d2* (ax ay bx by)) (values (* ax bx) (* ay by))
  (d2/ (ax ay bx by)) (values (/ ax bx) (/ ay by))

  (d2i- (ax ay bx by)) (values (- bx ax) (- by ay))
  (d2i/ (ax ay bx by)) (values (/ bx ax) (/ by ay))

  (f2+ (ax ay bx by)) (values (+ ax bx) (+ ay by))
  (f2- (ax ay bx by)) (values (- ax bx) (- ay by))
  (f2* (ax ay bx by)) (values (* ax bx) (* ay by))
  (f2/ (ax ay bx by)) (values (/ ax bx) (/ ay by))

  (f2i- (ax ay bx by)) (values (- bx ax) (- by ay))
  (f2i/ (ax ay bx by)) (values (/ bx ax) (/ by ay))

  (d2cross (ax ay bx by)) (- (* ax by) (* ay bx))
  (f2cross (ax ay bx by)) (- (* ax by) (* ay bx))


  (d2. (ax ay bx by)) (+ (* ax bx) (* ay by))

  (d2dst2 (ax ay bx by)) (mvc #'+ (-d2square (- bx ax) (- by ay)))
  (d2dst (ax ay bx by)) (sqrt (the pos-df (mvc #'+ (-d2square (- bx ax) (- by ay)))))

  (f2. (ax ay bx by)) (+ (* ax bx) (* ay by))

  (f2dst2 (ax ay bx by)) (mvc #'+ (-f2square (- bx ax) (- by ay)))
  (f2dst (ax ay bx by)) (sqrt (the pos-ff (mvc #'+ (-f2square (- bx ax) (- by ay)))))


  (d2lerp (ax ay bx by s)) (-d2+ ax ay (* (- bx ax) s) (* (- by ay) s))
  (d2from (ax ay bx by s)) (-d2+ ax ay (* bx s) (* by s))
  (d2mid (ax ay bx by)) (values (* 0.5d0 (+ ax bx)) (* 0.5d0 (+ ay by)))

  (f2lerp (ax ay bx by s)) (-f2+ ax ay (* (- bx ax) s) (* (- by ay) s))
  (f2from (ax ay bx by s)) (-f2+ ax ay (* bx s) (* by s))
  (f2mid (ax ay bx by)) (values (* 0.5f0 (+ ax bx)) (* 0.5f0 (+ ay by)))

  ; OTHER

  (d2on-circ (a rad)) (mvc #'-d2scale (-dcos-sin (* a dpii)) rad)
  (f2on-circ (a rad)) (mvc #'-f2scale (-fcos-sin (* a fpii)) rad)

  (d2rot (x y a)) (let ((cosa (cos a)) (sina (sin a)))
                    (declare (veq:df cosa sina))
                    (values (- (* x cosa) (* y sina))
                            (+ (* x sina) (* y cosa))))

  (d2rots (x y a sx sy)) (mvc #'-d2+ (mvc #'-d2rot (-d2- x y sx sy ) a) sx sy)

  (f2rot (x y a)) (let ((cosa (cos a)) (sina (sin a)))
                    (declare (veq:ff cosa sina))
                    (values (- (* x cosa) (* y sina))
                            (+ (* x sina) (* y cosa))))

  (f2rots (x y a sx sy)) (mvc #'-f2+ (mvc #'-f2rot (-f2- x y sx sy ) a) sx sy))

