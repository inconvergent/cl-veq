
(in-package :veq)


(defmacro update-mima (v mi ma)
  (declare (symbol v mi ma))
  `(cond ((< ,v ,mi) (setf ,mi ,v))
         ((> ,v ,ma) (setf ,ma ,v))))

; TODO: from to as supported in with-arrays
(defmacro -xmima (dim type)
  (awg (a mimafx)
    (let* ((exportname (veqsymb dim type "$MIMA"))
           (vlet (veqsymb 1 type "VLET"))
           (with-arrays (veqsymb 1 type "WITH-ARRAYS"))
           (indref (veqsymb dim type "$"))
           (with `(:itr k
                   :arr ((,a ,dim ,a))
                   :fxs ((,mimafx ((varg ,dim x))
                           (progn ,@(loop for i from 0 below dim
                                          collect `(update-mima
                                                     (:vr x ,i)
                                                     (:vr mi ,i)
                                                     (:vr ma ,i))))))
                   :nxs ((,mimafx ,a))))
           (docs (format nil "find min and max for all dimensions of ~d array.
ex: (~a &key n) returns (values xmin xmax ...).
use n to limit to first n rows." dim exportname)))
      `(progn (export ',exportname)
       (fvdef ,exportname (,a &key (n (,(veqsymb dim nil "$NUM") ,a)) inds)
          (declare (,(arrtype type) ,a))
          ,docs
          (let ((,a ,a))
            ; TODO: what should happen when a is empty?
            ; early exit when a is empty. returns (values 0 ...) for dim
            (when (< (length ,a) 1)
                  (return-from ,exportname
                               (values ,@(loop repeat (* dim 2)
                                               collect (coerce 0 type)))))
            (,vlet ((mm ,dim (,indref ,a (if inds (car inds) 0)))
                    (mi ,dim (values mm))
                    (ma ,dim (values mm)))
                ; TODO: there was s bug here where inds was not used correctly.
                ; should rewrite with-arrays to use n if n, else inds?
                (if inds (,with-arrays (:inds inds ,@with))
                         (,with-arrays (:n n ,@with)))
              (values ,@(loop with res = (list)
                              for i from 0 below dim
                              do (push `(:vr mi ,i) res)
                                 (push `(:vr ma ,i) res)
                              finally (return (reverse res)))))))))))

(-xmima 1 ff) (-xmima 2 ff) (-xmima 3 ff)
(-xmima 1 df) (-xmima 2 df) (-xmima 3 df)

