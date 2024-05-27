(in-package :veq)

(declaim (inline %falpha %dalpha %feps=))

(veq:fvdef* falpha (x y &aux (a (atan (- y) x))) (declare #.*opt* (ff x y a))
  ; x+ → | y+ ↓
  ; (alpha  1f0  0f0) ; →
  ; (alpha  0f0 -1f0) ; ↑
  ; (alpha -1f0  0f0) ; ←
  ; (alpha  0f0  1f0) ; ↓
  (mod (abs (if (<= a 0f0) (- veq:fpii (abs a)) a)) veq:fpii))
(veq:fvdef* dalpha (x y &aux (a (atan (- y) x))) (declare #.*opt* (df x y a))
  (mod (abs (if (<= a 0d0) (- veq:dpii (abs a)) a)) veq:dpii))

(veq:fvdef* feps= (a b &optional (e (* 10f0 *eps*))) (declare #.*opt* (ff a b e)) (< (abs (- b a)) e))
