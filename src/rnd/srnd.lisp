
(in-package :srnd)

(declaim (inline -srnd make srnd do-rnd do-rndi 1rnd rrnd 1rad rrad rndi
                 3in-sphere 3on-sphere 2in-circ 2on-circ))


(defmacro prob (rs p a &optional b)
  "evaluate first form in body with probability p. second form (optional) is
executed with probability 1-p. ex: (prob 0.1 (print :a) (print :b))"
  `(if (< (rnd ,rs) (the veq:ff ,p)) ,a ,b))

(defmacro rcond (rs &rest clauses) ; TODO: sum to 1?
  "executes the forms in clauses according to the probability of the weighted sum
ex: (rcond (0.1 (print :a)) (0.3 (print :b)) ...)"
  (veq::awg (val)
    (let* ((tot 0f0)
           (clauses (loop for (p . body) in clauses
                          do (incf tot (veq:ff p))
                          collect `((< ,val ,tot) ,@body))))
      (declare (veq:ff tot) (list clauses))
      `(let ((,val (rnd ,rs ,tot)))
         (declare (veq:ff ,val))
         (cond ,@clauses)))))
; (defmacro rep (a &optional b &body body)
;   "repeat body at most a times, or between a and b times."
;   `(loop repeat ,(if (and a b) `(rndrngi ,a ,b) `(rndi ,a))
;          do (progn ,@body)))

(defstruct (srnd (:constructor -srnd (s)))
  (s 0 :type veq:pn :read-only nil))
#+SBCL(declaim (sb-ext:freeze-type srnd))

(defun make (rs)
  (declare #.*srndopt* (fixnum rs)) "make stateful rnd generator"
  (-srnd (mod (abs rs) 2147483647)))
(defun srnd (rs) "see make" (make rs))

(defun do-rnd (rs)
  (declare #.*srndopt* (srnd rs))
  (setf (srnd-s rs) (the veq:pn (rem (the fixnum (* 16807 (srnd-s rs))) 2147483647)))
  (* (veq:ff (srnd-s rs)) #.(/ 1f0 (veq:ff 2147483647))))

(defun do-rndi (rs)
  (declare #.*srndopt* (srnd rs))
  (setf (srnd-s rs) (the veq:pn (rem (the fixnum (* 16807 (srnd-s rs))) 2147483647)))
  (srnd-s rs))

(defun 1rnd (rs)
  (declare #.*srndopt* (srnd rs))
  (the veq:ff (abs (the veq:ff (do-rnd rs)))))
(defun rrnd (rs r)
  (declare #.*srndopt* (srnd rs) (veq:ff r))
  (the veq:ff (the veq:ff (* (the veq:ff r) (1rnd rs)))))
(defmacro rnd (rs &optional r)
   (declare (symbol rs)) "get a random float [0.0 1.0] from state rs (scaled by r)"
   (if r `(rrnd ,rs ,r) `(1rnd ,rs)))

(defun rndi (rs n)
  (declare #.*srndopt* (srnd rs) (veq:pn n)) "get random integer in range [0 n)."
  (the veq:pn (mod (do-rndi rs) n)))

(defun 1rad (rs)
   (declare #.*srndopt* (srnd rs)) "random float in range [-1 1]."
   (- 1f0 (* 2.0 (rnd rs))))
(defun rrad (rs r)
   (declare #.*srndopt* (srnd rs) (veq:ff r)) "random float in range [-r r]"
   (* r (1rad rs)))
(defmacro rnd* (rs &optional r)
   (declare (symbol rs)) "get a random float in range [-r r] from state rs. r defaults to 1.0"
   (if r `(rrad ,rs ,r) `(1rad ,rs)))

(defmacro rndrng (rs a b &aux (a* (gensym)))
  (declare (symbol rs)) "get a random number in range [a b]"
  (cond ((and (numberp a) (numberp b)) `(+ ,a (rnd ,rs ,(- b a))))
        ((numberp a) `(+ ,a (rnd ,rs (- ,b ,a))))
        (t `(let ((,a* ,a))
              (declare (veq:ff ,a*))
              (+ ,a* (rnd ,rs (- ,b ,a*)))))))

(veq:fvdef 3in-sphere (rs r)
  (declare #.*srndopt* (srnd rs) (veq:ff r))
  "get random point in sphere with rad r."
  (veq:xlet ((f3!cand (veq:f3val 0f0)))
    (loop while t do (setf (veq:f3 cand) (veq:f3rep (1rad rs)))
                     (when (< (veq:f3len2 cand) 1f0)
                           (return-from 3in-sphere (f3!@*. cand r))))))

(veq:fvdef 3diffuse (rs noise (:va 3 hn))
  (declare #.*srndopt* (srnd rs) (veq:ff noise hn))
  "offset hn with bias."
  (veq:f3norm (f3!@+ hn (3in-sphere rs noise))))

(veq:fvdef 3in-hemi (rs (:va 3 dir))
  (declare #.*srndopt* (srnd rs) (veq:ff dir))
  "get random point in hemisphere where (dot dir res) is positive."
  (veq:xlet ((f3!res (3in-sphere rs 1.0)))
    (f3!@*. res (float-sign (veq:f3dot dir res)))))

(veq:fvdef 3on-sphere (rs r)
  (declare #.*srndopt* (srnd rs) (veq:ff r))
  "get random point on sphere."
  (veq:xlet ((f!th (rnd rs #.veq:fpii))
             (f!la (- (the veq:ff (acos (- (rnd rs 2.0) 1f0))) #.veq:fpi5))
             (f!co (* r (cos la))))
    (values (* co (cos th)) (* co (sin th)) (* r (sin la)))))

(veq:fvdef 3on-hemi (rs (:va 3 dir))
  (declare #.*srndopt* (srnd rs) (veq:ff dir))
  "get random point on hemisphere where (dot dir res) is positive."
  (veq:xlet ((f3!res (3on-sphere rs 1.0)))
    (f3!@*. res (float-sign (veq:f3dot dir res)))))

(veq:fvdef 2in-circ (rs r)
  (declare #.*srndopt* (srnd rs) (veq:ff r))
  "random point in circle."
  (veq:xlet ((f!a (1rnd rs)) (f!b (1rnd rs)))
    (if (< a b) (setf a (* veq:fpii (/ a b)) b (* b r))
                (veq:xlet ((f!d a)) (setf a (* veq:fpii (/ b a)) b (* d r))))
    (values (* (cos a) b) (* (sin a) b))))
(veq:fvdef 2on-circ (rs r)
  (declare #.*srndopt* (srnd rs) (veq:ff r))
  "get random point on circle."
  (f2!@*. (veq:fcos-sin (* #.veq:fpii (rnd rs))) r))

(defmacro 2on-circ+ (rs s &rest rest) (veq:proc-vv `(f2!@+ (2on-circ ,rs ,s) ,@rest)))
(defmacro 2in-circ+ (rs s &rest rest) (veq:proc-vv `(f2!@+ (2in-circ ,rs ,s) ,@rest)))
(defmacro 3on-sphere+ (rs s &rest rest) (veq:proc-vv `(f3!@+ (3on-sphere ,rs ,s) ,@rest)))
(defmacro 3in-sphere+ (rs s &rest rest) (veq:proc-vv `(f3!@+ (3in-sphere ,rs ,s) ,@rest)))

