
(in-package :rnd)

(declaim (inline %3non-line %3in-box %3nin-box %3nin-cube %3nin-hemi %3non-hemi
                 3on-sphere 3in-sphere %3on-hemi %3in-hemi 3non-sphere %3on-line 3nin-sphere))

(veq:fvdef* 3on-line ((:va 3 a b))
  (declare #.*opt* (veq:ff a b)) "random point between a,b."
  (veq:f3from a (f3!@- b a) (rnd)))
(veq:fvdef* 3non-line (n (:va 3 a b))
  (declare #.*opt* (veq:ff a b)) "n random points between a,b."
  (f3!@$+! (veq:f3$zero n) (?@ (3on-line a b))))


(veq:fvdef* 3in-box ((:va 3 s))
  (declare #.*opt* (veq:ff s)) "random point in box of size sx,sy,sz."
  (f3.@rnd* s))
(veq:fvdef* 3in-cube (&optional (s 1.0))
  (declare #.*opt* (veq:ff s)) "random point in cube of size s."
  (3in-box s s s))

(veq:fvdef* 3nin-box (n (:va 3 s))
  (declare #.*opt* (veq:ff s)) "n random points in box of size sx,sy,sz."
  (f3!@$+! (veq:f3$zero n) (?@ (3in-box s))))
(veq:fvdef 3nin-cube (n &optional (s 1.0))
  (declare #.*opt* (veq:ff s)) "n random points in cube of size sx."
  (f3!@$+! (veq:f3$zero n) (?@ (3in-cube s))))

(veq:fvdef 3on-sphere (&optional (r 1.0))
  (declare #.*opt* (veq:ff r))
  "random point on sphere with rad r."
  (veq:xlet ((f!th (* veq:fpii (rnd:rnd)))
             (f!la (- (the veq:ff (acos (- (* 2.0 (rnd:rnd)) 1.0))) veq:fpi5))
             (f!co (* r (cos la))))
    (values (* co (cos th)) (* co (sin th)) (* r (sin la)))))

(veq:fvdef 3in-sphere (&optional (r 1.0))
  (declare #.*opt* (veq:ff r))
  "random point in sphere with rad r."
  (veq:xlet ((f3!cand (veq:f3val 0f0)))
    (loop while t do (setf (veq:f3 cand) (veq:f3rep (rnd*)))
                     (when (< (veq:f3len2 cand) 1.0)
                       (return-from 3in-sphere (f3!@*. cand r))))))

(veq:fvdef 3non-sphere (n &optional (r 1.0))
  (declare #.*opt* (veq:ff r))
  "n random points on sphere with rad r."
  (f3!@$+! (veq:f3$zero n) (?@ (3on-sphere r))))
(veq:fvdef 3nin-sphere (n &optional (r 1.0))
  (declare #.*opt* (veq:ff r))
  "n random points in sphere with rad r."
  (f3!@$+! (veq:f3$zero n) (?@ (3in-sphere r))))


(veq:fvdef* 3on-hemi ((:va 3 dir) &optional (r 1.0))
  (declare #.*opt* (veq:ff dir r))
  "random point on hemisphere where (dot dir res) is positive."
  (veq:xlet ((f3!res (3on-sphere 1.0)))
    (f3!@*. res (* r (float-sign (veq:f3dot dir res))))))
(veq:fvdef* 3non-hemi (n (:va 3 dir) &optional (r 1.0))
  (declare #.*opt* (veq:ff r))
  "n random points on hemisphere, rad r."
  (f3!@$+! (veq:f3$zero n) (?@ (3on-hemi dir r))))

(veq:fvdef* 3in-hemi ((:va 3 dir) &optional (r 1.0))
  (declare #.*opt* (veq:ff dir r))
  "random point in hemisphere where (dot dir res) is positive."
  (veq:xlet ((f3!res (3in-sphere 1.0)))
    (f3!@*. res (* r (float-sign (veq:f3dot dir res))))))
(veq:fvdef* 3nin-hemi (n (:va 3 dir) &optional (r 1.0))
  (declare #.*opt* (veq:ff r))
  "n random points in hemisphere, rad r."
  (f3!@$+! (veq:f3$zero n) (?@ (3in-hemi dir r))))
