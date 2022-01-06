
(in-package :veq)


(defmacro broadcast-op (dim type exportname fxname arr-arg br-arg &key dim-out)
  "
  exportname: name of macro definition for broadcast op
  fxname: name of the internal function
  arr-arg: name of arguments from the array
  br-arg: name of arguments for the broadcast
  "
  (labels ((remove-varg (ll)
             (awf
               (loop for l in ll
                     collect (if (and (consp l) (eq (car l) 'varg))
                                 (cddr l) l) ))))

    (let ((arr (gensym "arr"))
          (arr-out (gensym "arr-out")))
    `(progn (export ',exportname)
            ,(if dim-out

              ; create new output array
              `(vdef* ,exportname (,arr ,@br-arg)
                (declare #.*opt* (,(arrtype type) ,arr))
                (,(veqsymb 1 type "WITH-ARRAYS") ; fwith-arrays
                  (:itr k :n (/ (length ,arr) ,dim)
                   :arr ((,arr ,dim ,arr)
                         (,arr-out ,dim-out))
                   :fxs ((bcastfx (,@arr-arg) (,fxname ,@(remove-varg arr-arg)
                                                  ,@(remove-varg br-arg))))
                   :exs ((,arr-out k (bcastfx ,arr))))
                  ,arr-out))

              ; alter input array
              `(vdef* ,exportname (,arr ,@br-arg)
                (declare #.*opt* (,(arrtype type) ,arr))
                (,(veqsymb 1 type "WITH-ARRAYS") ; fwith-arrays
                  (:itr k :n (/ (length ,arr) ,dim)
                   :arr ((,arr ,dim ,arr))
                   :fxs ((fx (,@arr-arg) (,fxname ,@(remove-varg arr-arg)
                                                  ,@(remove-varg br-arg))))
                   :exs ((,arr k (fx ,arr))))
                  ,arr)))))))


(broadcast-op 1 ff f$+ -f+ (a) (s))
(broadcast-op 1 ff f$* -f* (a) (s))
(broadcast-op 1 ff f$- -f- (a) (s))
(broadcast-op 1 ff f$/ -f/ (a) (s))
(broadcast-op 1 ff f$i- -fi- (a) (s))
(broadcast-op 1 ff f$i/ -fi/ (a) (s))

(broadcast-op 1 ff f$abs -fabs (a) ())
(broadcast-op 1 ff f$neg -fneg (a) ())

(broadcast-op 1 ff f$cos-sin -fcos-sin (a) () :dim-out 2)

(broadcast-op 2 ff f2$+ -f2+ ((varg 2 a)) ((varg 2 s)))
(broadcast-op 2 ff f2$* -f2* ((varg 2 a)) ((varg 2 s)))
(broadcast-op 2 ff f2$- -f2- ((varg 2 a)) ((varg 2 s)))
(broadcast-op 2 ff f2$/ -f2/ ((varg 2 a)) ((varg 2 s)))
(broadcast-op 2 ff f2$i- -f2i- ((varg 2 a)) ((varg 2 s)))
(broadcast-op 2 ff f2$i/ -f2i/ ((varg 2 a)) ((varg 2 s)))
(broadcast-op 2 ff f2$scale -f2scale ((varg 2 a)) (s))
(broadcast-op 2 ff f2$iscale -f2iscale ((varg 2 a)) (s))

(broadcast-op 2 ff f2$rot -f2rot ((varg 2 arr)) (a))
(broadcast-op 2 ff f2$rots -f2rots ((varg 2 arr)) (a x y))

(broadcast-op 2 ff f$2abs -f2abs ((varg 2 a)) ())
(broadcast-op 2 ff f$2neg -f2neg ((varg 2 a)) ())

; TODO: more reduce operations
(broadcast-op 2 ff f2$len2 -f2len2 ((varg 2 a)) () :dim-out 1)
(broadcast-op 2 ff f2$len -f2len ((varg 2 a)) () :dim-out 1)


(broadcast-op 3 ff f3$+ -f3+ ((varg 3 a)) ((varg 3 s)))
(broadcast-op 3 ff f3$* -f3* ((varg 3 a)) ((varg 3 s)))
(broadcast-op 3 ff f3$- -f3- ((varg 3 a)) ((varg 3 s)))
(broadcast-op 3 ff f3$/ -f3/ ((varg 3 a)) ((varg 3 s)))
(broadcast-op 3 ff f3$i- -f3i- ((varg 3 a)) ((varg 3 s)))
(broadcast-op 3 ff f3$i/ -f3i/ ((varg 3 a)) ((varg 3 s)))
(broadcast-op 3 ff f3$scale -f3scale ((varg 3 a)) (s))
(broadcast-op 3 ff f3$iscale -f3iscale ((varg 3 a)) (s))

(broadcast-op 2 df d2$rot -d2rot ((varg 2 arr)) (a))
(broadcast-op 2 df d2$rots -d2rots ((varg 2 arr)) (a x y))

(broadcast-op 3 ff f$3abs -f3abs ((varg 3 a)) ())
(broadcast-op 3 ff f$3neg -f3neg ((varg 3 a)) ())

(broadcast-op 3 ff f3$len2 -f3len2 ((varg 3 a)) () :dim-out 1)
(broadcast-op 3 ff f3$len -f3len ((varg 3 a)) () :dim-out 1)

