
(in-package :veq)

; TODO: implement for double
; this will work for now, but should make a more general version
; maybe just include it in the macrolet/vprogn/vdef?

(defmacro make-broadcast-fx (macro-name fxname)

  (let ((vdef-name (intern (mkstr "-" macro-name))))
    `(progn
       (declaim (inline ,vdef-name))
       (veq:vdef ,vdef-name (a (veq:varg 2 x))
         (declare #.*opt* (fvec a) (ff x))
         (veq:fwith-arrays (:itr k :n (/ (length a) 2)
           :arr ((a 2 a))
           :fxs ((fx ((veq:varg 2 vx)) (,fxname x vx)))
           :exs ((a k (fx a)))))
         a)
       (export ',macro-name)
       (defmacro ,macro-name (arr b)
         `(mvc #',',vdef-name (values ,arr) ,b)))))

(make-broadcast-fx f2$+ -f2+)
(make-broadcast-fx f2$- -f2-)
(make-broadcast-fx f2$* -f2*)
(make-broadcast-fx f2$/ -f2/)

