
(in-package :veq)

(defun broadcast-op (arr-dim br-dim type fxname arr* br-arg* &key out)
  (awg (arr)
    (let ((arr-arg (loop repeat arr-dim collect (gensym "ARR")))
          (br-arg (loop repeat br-dim collect (gensym "BR"))))
      `(let ((,arr ,arr*))
         (declare (,(arrtype type) ,arr))
         (mvb (,@br-arg) (~ ,@br-arg*)
           (declare (,type ,@br-arg))
           (select-macrolets (1 ,type "WITH-ARRAYS")
             (,(veqsymb 1 type "WITH-ARRAYS")
               (:itr k :n (/ (length ,arr) ,arr-dim)
                :arr ((,arr ,arr-dim ,arr) ,@(if out `((arr-out ,out))))
                :fxs ((fx (,@arr-arg) (mvc #',fxname ,@arr-arg ,@br-arg)))
                :exs ((,(if out 'arr-out arr) k (fx ,arr))))
               ,(if out 'arr-out arr))))))))

