
(in-package :veq)

; TODO: handle various numberof arguments. so that it is possible to eg. do
; (veq:f2$^ arr 4.0) to raise all elements to the power of 4

(defmacro broadcast-op (dim type op)
  "
  fxname: name of the internal array function.
  opname: name of the internal function for the operation
          (defined in ops-dim.lisp)
  arrmacro: with-arrays macro (from rows.lisp)
  exportname: name of macrolet definition for broadcast op
  "
  (let ((fxname (veqsymb dim type (mkstr "$" op) :pref "-")) ; -f2$+
        (opname (veqsymb dim type (mkstr op) :pref "-")) ; -f2+
        (arrmacro (veqsymb 1 type "WITH-ARRAYS")) ; fwith-arrays
        (exportname (veqsymb dim type (mkstr "$" op)))) ; f2$+
    `(progn (map-symbol `(,',exportname (a &rest rest)
                          `(mvc #',',',fxname (values ,a) ,@rest)))
            (export ',exportname)
            (declaim (inline ,fxname))
            (vdef ,fxname (a (varg ,dim x))
              (declare #.*opt* (,(arrtype type) a) (,type x))
              (,arrmacro (:itr k :n (/ (length a) ,dim)
                          :arr ((a ,dim a))
                          :fxs ((fx ((varg ,dim vx)) (,opname x vx)))
                          :exs ((a k (fx a)))))
              a))))


(defmacro make-broadcast-fx (op)
  "makes function (f2$+ arr x y) from operation +.
   so that x y is added to every row of arr."
  `(progn
    ,@(loop for (dim type) in (group `(1 ff 2 ff 3 ff 1 df 2 df 3 df) 2)
            collect `(broadcast-op ,dim ,type ,op))))

(make-broadcast-fx +)
(make-broadcast-fx -)
(make-broadcast-fx /)
(make-broadcast-fx *)

; redefine vdef/vprogn macros with new elements in symbols map
; (added via broadcast-op)
(define-env-macros *symbols-map*)

