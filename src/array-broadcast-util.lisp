
(in-package :veq)


(defun broadcast-op (dim type fx arr-arg br-arg &key out)
  "
  exportname: name of macro definition for broadcast op
  fxname: name of the internal function
  arr-arg: name of arguments from the array
  br-arg: name of arguments for the broadcast
  out: is the dim of the output array. if nil, the op is destructive
  "
  (labels ((name () (mkstr (format nil "$~a~:[!~;~]" fx out))))
    (let* ((internal (veqsymb dim type fx :pref "-"))
           (exportname (veqsymb dim type (name)))
           (docs (format nil "broadcast for fx: ~a~%macroname: ~a
ex: (~a a ...) performs (mvc #'~a a[i] ...) for every row in a.
~:[destructive.~&~;~]" internal  exportname  exportname  internal out)))
      `(progn (export ',exportname)
        (map-docstring ',exportname ,docs)
        (def* ,exportname (arr ,@br-arg)
          (declare #.*opt* (,(arrtype type) arr))
          ,docs
          (,(veqsymb 1 type "WITH-ARRAYS")
            (:itr k :n (/ (length arr) ,dim)
             :arr ((arr ,dim arr) ,@(if out `((arr-out ,out))))
             :fxs ((fx (,@arr-arg) (,internal ,@arr-arg ,@br-arg)))
             :exs ((,(if out 'arr-out 'arr) k (fx arr))))
            ,(if out 'arr-out 'arr)))))))

