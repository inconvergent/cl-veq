
(in-package :veq)


(defmacro broadcast-op (dim type exportname fxname arr-arg br-arg
                        &optional out)
  "
  exportname: name of macro definition for broadcast op
  fxname: name of the internal function
  arr-arg: name of arguments from the array
  br-arg: name of arguments for the broadcast
  out: is the dimensions of the output array. if nil, the input array is used
  "
  (awg (arr arr-out)
    (labels ((fxarg (s) (and (symbolp s) (not (eq s :va))))
             (-varg (l) (remove-if-not #'fxarg (awf l))))
    (let ((docs (format nil "broadcast for fx: ~a~%macroname: ~a
ex: (~a a ...) performs (mvc #'~a a[i] ...) for every row in a.
~:[~&destructive.~;~]" fxname  exportname  exportname  fxname out)))
      `(fvprogn (export ',exportname)
          (map-docstring ',exportname ,docs)
          (def* ,exportname (,arr ,@br-arg)
                (declare #.*opt* (,(arrtype type) ,arr))
                ,docs
                (,(veqsymb 1 type "WITH-ARRAYS")
                  (:itr k :n (/ (length ,arr) ,dim)
                   :arr ((,arr ,dim ,arr) ,@(if out `((,arr-out ,out))))
                   :fxs ((fx (,@arr-arg) (,fxname ,@(-varg arr-arg)
                                                  ,@(-varg br-arg))))
                   :exs ((,(if out arr-out arr) k (fx ,arr))))
                  ,(if out arr-out arr))))))))

(defmacro make-broadcast-ops (typedim fxs &optional destructive)
  (labels  ((make (dim type)
              (let ((fxs (subst dim 'dim (eval fxs))))
                (loop for (fx . rest) in fxs
                      collect `(broadcast-op ,dim ,type
                                ,(veqsymb dim type
                                  (format nil "$~a~:[~;!~]" fx destructive))
                                ,(veqsymb dim type fx :pref "-")
                                ,@rest)))))
    `(progn ,@(loop for pair in (eval typedim)
                    collect `(progn ,@(apply #'make pair))))))

(broadcast-op 1 ff f$cos-sin -fcos-sin (a) () 2)
(broadcast-op 1 df d$cos-sin -dcos-sin (a) () 2)

; TODO: add most of the other ops
(make-broadcast-ops ; non-destructive
  (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df) 2)
  (group '(+ ((:va dim a)) ((:va dim s)) dim
           - ((:va dim a)) ((:va dim s)) dim
           / ((:va dim a)) ((:va dim s)) dim
           * ((:va dim a)) ((:va dim s)) dim
           i- ((:va dim a)) ((:va dim s)) dim
           i/ ((:va dim a)) ((:va dim s)) dim
           norm ((:va dim a)) () dim
           ; ^ ((:va dim a)) (s) dim
           from ((:va dim a)) ((:va dim s) ss) dim
           scale ((:va dim a)) (s) dim iscale ((:va dim a)) (s) dim
           len2 ((:va dim a)) () 1 len ((:va dim a)) () 1
           abs ((:va dim a)) () dim neg ((:va dim a)) () dim) 4))
(make-broadcast-ops ; destructive
  (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df) 2)
  (group '(+ ((:va dim a)) ((:va dim s))
           - ((:va dim a)) ((:va dim s))
           / ((:va dim a)) ((:va dim s))
           * ((:va dim a)) ((:va dim s))
           i- ((:va dim a)) ((:va dim s))
           i/ ((:va dim a)) ((:va dim s))
           norm ((:va dim a)) ()
           ; ^ ((:va dim a)) (s)
           from ((:va dim a)) ((:va dim s) ss)
           scale ((:va dim a)) (s) iscale ((:va dim a)) (s)
           abs ((:va dim a)) () neg ((:va dim a)) ()) 3)
  t)
(make-broadcast-ops (group '(2 ff 2 df) 2) ; non-destructive
                    (group '(rot ((:va dim a)) (aa) dim
                             rots ((:va dim a)) (aa (:va dim s)) dim) 4))
(make-broadcast-ops (group '(2 ff 2 df) 2) ; destructive
                    (group '(rot ((:va dim a)) (aa)
                             rots ((:va dim a)) (aa (:va dim s))) 3)
                    t)

