
(in-package :veq)


(defmacro make-broadcast-ops (dimtype &rest all-fxpairs &aux (res (list)))
  (labels ((vargs (d) (replace-varg (subst d 'dim all-fxpairs))))
    (loop for (dim type) in dimtype
          do (loop for fxs in (vargs dim)
                   do (loop for (fx arr-arg br-arg out) in fxs
                            do (push (broadcast-op dim type
                                       fx arr-arg br-arg :out out)
                                     res)))))
  `(fvprogn ,@res))


(make-broadcast-ops #.(group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df) 2)
  #.(group '(+ ((:va dim a)) ((:va dim s)) dim
             - ((:va dim a)) ((:va dim s)) dim
             / ((:va dim a)) ((:va dim s)) dim
             * ((:va dim a)) ((:va dim s)) dim
             i- ((:va dim a)) ((:va dim s)) dim
             i/ ((:va dim a)) ((:va dim s)) dim
             norm ((:va dim a)) () dim
             ; ^ ((:va dim a)) (s) dim
             from ((:va dim a)) ((:va dim s) ss) dim
             scale ((:va dim a)) (s) dim
             iscale ((:va dim a)) (s) dim
             abs ((:va dim a)) () dim
             neg ((:va dim a)) () dim
             len ((:va dim a)) () 1       ; <-- NOTE 1
             len2 ((:va dim a)) () 1) 4)  ; <-- NOTE 1
  #.(group '(+ ((:va dim a)) ((:va dim s))
             - ((:va dim a)) ((:va dim s))
             / ((:va dim a)) ((:va dim s))
             * ((:va dim a)) ((:va dim s))
             i- ((:va dim a)) ((:va dim s))
             i/ ((:va dim a)) ((:va dim s))
             norm ((:va dim a)) ()
             ; ^ ((:va dim a)) (s)
             from ((:va dim a)) ((:va dim s) ss)
             scale ((:va dim a)) (s)
             iscale ((:va dim a)) (s)
             abs ((:va dim a)) ()
             neg ((:va dim a)) ()) 3))


(make-broadcast-ops #.(group '(1 ff 1 df) 2)
  #.(group '(cos-sin (a) () 2) 4))

(make-broadcast-ops #.(group '(2 ff 2 df) 2)
  #.(group '(rot ((:va dim a)) (v) dim
             rots ((:va dim a)) (v (:va dim s)) dim) 4)
  #.(group '(rot ((:va dim a)) (v)
             rots ((:va dim a)) (v (:va dim s))) 3))

(make-broadcast-ops #.(group '(3 ff 3 df) 2)
  #.(group '(rot ((:va dim a)) ((:va dim n) v) dim
             rots ((:va dim a)) ((:va dim n) v (:va dim s)) dim) 4)
  #.(group '(rot ((:va dim a)) ((:va dim n) v)
             rots ((:va dim a)) ((:va dim n) v (:va dim s))) 3))

