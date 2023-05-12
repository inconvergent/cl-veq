
(in-package :veq)

; TODO: make this a general macro instead?
(defmacro -xmima (dim type)
  (let* ((exportname (vvsym type dim :$mima))
         (fxhead `(((:va ,dim x))))
         (vvop (vvsym type dim :x@$mima))
         (docs (format nil "find min and max for all dimensions of ~d array.
ex: (~a &key n) returns (values xmin xmax ...).
use n to limit to first n rows." dim exportname))
         (update-mima (loop for i from 0 repeat dim
                            for v = `(:vr x ,i)
                            for mi = `(:vr mi ,i) for ma = `(:vr ma ,i)
                            collect `(cond ((< ,v ,mi) (setf ,mi ,v))
                                           ((> ,v ,ma) (setf ,ma ,v))))))
    `(progn (export ',exportname)
     (fvdef ,exportname (a &key (n (,(vvsym nil dim :$num) a)) inds)
        (declare (,(arrtype type) a)) ,docs
        (,(vvsym type 1 :vlet)
               ((mm ,dim (,(vvsym type dim "$") a (if inds (car inds) 0)))
                (mi ,dim (values mm)) (ma ,dim (values mm)))
            (if inds (,vvop (l?@ a inds) (,@fxhead ,@update-mima))
                     (,vvop (?@ a 0 n) (,@fxhead ,@update-mima)))
          (values ,@(loop for i from 0 below dim
                          append `((:vr mi ,i) (:vr ma ,i)))))))))
(-xmima 1 ff) (-xmima 2 ff) (-xmima 3 ff)
(-xmima 1 df) (-xmima 2 df) (-xmima 3 df)

