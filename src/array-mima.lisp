
(in-package :veq)


(defmacro update-mima (v mi ma)
  (declare (symbol v mi ma))
  `(cond ((< ,v ,mi) (setf ,mi ,v))
         ((> ,v ,ma) (setf ,ma ,v))))

; TODO: from to as supported in with-arrays
(defmacro -xmima (dim type)
  (awg (a mimafx)
    (let ((exportname (veqsymb dim type "$MIMA"))
          (vlet (veqsymb 1 type "VLET"))
          (with-arrays (veqsymb 1 type "WITH-ARRAYS"))
          (indref (veqsymb dim type "$")))
      `(progn (export ',exportname)
       (fvdef ,exportname (,a &key (n (,(veqsymb dim nil "$NUM") ,a)) inds)
          (declare (,(arrtype type) ,a))
          (let ((,a ,a))
            (,vlet ((mm ,dim (,indref ,a (if inds (car inds) 0)))
                    (mi ,dim (values mm))
                    (ma ,dim (values mm)))
              (,with-arrays (:n n :itr k
                  :arr ((,a ,dim ,a))
                  :fxs ((,mimafx ((varg ,dim x))
                          (progn ,@(loop for i from 0 below dim
                                         collect `(update-mima
                                                    (vref x ,i) (vref mi ,i)
                                                    (vref ma ,i))))))
                  :nxs ((,mimafx ,a))))
              (values ,@(loop with res = (list)
                              for i from 0 below dim
                              do (push `(vref mi ,i) res)
                                 (push `(vref ma ,i) res)
                              finally (return (reverse res)))))))))))

(-xmima 1 ff) (-xmima 2 ff) (-xmima 3 ff)
(-xmima 1 df) (-xmima 2 df) (-xmima 3 df)

