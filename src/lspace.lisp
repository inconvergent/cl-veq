
(in-package :veq)

; TODO: inefficient use of xdlerp

(defmacro -lspace (dim type)
  (let ((exportname (veqsymb dim type "$LSPACE"))
        (with-arrays (veqsymb 1 type "WITH-ARRAYS")))
    `(progn
      (export ',exportname)
      (fvdef* ,exportname (n (varg ,dim a b) &key (end t))
        (declare (fixnum n) (,type a b) (boolean end))
        (let ((stp (,type (if end (/ (1- n)) (/ n)))))
          (declare (,type stp))
          (,with-arrays (:n n :itr k
           :arr ((arr ,dim))
           :fxs ((lspacefx (i) (,(veqsymb dim type "LERP") a b
                                  (,type (* i stp)))))
           :exs ((arr k (lspacefx k))))
             arr))))))

(-lspace 1 ff) (-lspace 2 ff) (-lspace 3 ff) (-lspace 4 ff)
(-lspace 1 df) (-lspace 2 df) (-lspace 3 df) (-lspace 4 df)

