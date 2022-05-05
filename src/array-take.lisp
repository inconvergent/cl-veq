
(in-package :veq)


(defmacro make-take-fx (dim type)
  (declare (symbol type) (fixnum dim))
  (let ((fxname (veqsymb dim type "$TAKE")) ; f2$take
        (arrmacro (veqsymb 1 type "WITH-ARRAYS")) ; fwith-arrays
        (resarr (veqsymb 1 type "$MAKE"))
        (docs (format nil "returns ~ad array with rows for inds.
use :res put result in existing array" dim))) ; f2$make
    `(progn
       (export ',fxname)
       (fvdef ,fxname (a inds &key res)
         ,docs
         (declare (,(arrtype type) a) (sequence inds))
         (let ((n (length inds)))
            (declare (pos-int n))
            (,arrmacro (:cnt cnt :inds inds
              :arr ((a ,dim a)
                    (res ,dim (if res res (,resarr :dim ,dim :n n))))
              :fxs ((acc ((varg ,dim x)) (values x)))
              :exs ((res cnt (acc a))))
              res))))))

(make-take-fx 1 ff) (make-take-fx 2 ff) (make-take-fx 3 ff) (make-take-fx 4 ff)
(make-take-fx 1 df) (make-take-fx 2 df) (make-take-fx 3 df) (make-take-fx 4 df)

