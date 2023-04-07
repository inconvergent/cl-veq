
(in-package :veq)

; TODO: make general macro similar to array-broadcast

(defmacro -sum-agg (dim type name agg)
  (declare (symbol type) (fixnum dim))
  (let ((fxname (veqsymb dim type name))
        (arrmacro (veqsymb 1 type :with-arrays))
        (vlet (veqsymb 1 type :vlet)))
    `(progn
       (export ',fxname)
       (fvdef ,fxname (a &key n)
       (declare (,(arrtype type) a))
       ,(format nil "sum all rows of ~ad array." dim)
       (let ((n (if n n (,(veqsymb dim nil :$num) a))))
          (declare (pos-int n))
          (,vlet ((res ,dim (,(veqsymb dim type :val) (coerce 0 ',type))))
            (,arrmacro (:n n
            :arr ((a ,dim a))
            :fxs ((acc ((varg ,dim new)) (,(veqsymb dim type :vset)
                                            (res)
                                            (,(veqsymb dim type agg) res new))))
            :nxs ((acc a))))
            (values res)))))))
(-sum-agg 1 ff "$SUM" +) (-sum-agg 2 ff "$SUM" +)
(-sum-agg 3 ff "$SUM" +) (-sum-agg 4 ff "$SUM" +)
(-sum-agg 1 df "$SUM" +) (-sum-agg 2 df "$SUM" +)
(-sum-agg 3 df "$SUM" +) (-sum-agg 4 df "$SUM" +)

