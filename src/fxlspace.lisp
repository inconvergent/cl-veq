(in-package :veq)

; TODO: inefficient use of xdlerp

(defun -wrapnum (n) (typecase n (cons n) (t `(values ,n))))

(defun -fxlspace (n a b fx &key dim type (end t))
  (declare (pn dim) (symbol type) (boolean end))
  (awg (i n* stp s fx* a* b*)
    `(let* ((,n* ,n)
            (,fx* (progn ,@fx))
            (,stp (coerce ,(if end `(/ (1- ,n*)) `(/ ,n*)) ',type)))
     (declare (pn ,n*) (,type ,stp) (function ,fx*))
     (fvprogn ; fvprogn to enable macrolet for fvlet and LERP.
              ; otherwise fxlspace can't be compiled inside a fvprogn.
              ; this is still faster
       (fvlet ((,a* ,dim ,(-wrapnum a))
               (,b* ,dim ,(-wrapnum b)))
         (loop for ,i of-type pn from 0 below ,n*
               for ,s of-type ,type from ,(coerce 0 type)
               do (mvc ,fx* ,i (,(veqsymb dim type "LERP")
                                 ,a* ,b* (* ,s ,stp)))))))))

(defmacro map-fxlspace (dim type)
  (let* ((mname (veqsymb dim type "$fxlspace"))
         (docs (format nil "args: ((n a b &key (end t)) &body fx)
for ~ad points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (~a (n a b) (lambda (i (:va ~a a b)) (vpr i a b)))" dim mname dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           ((n a b &key (end t)) &body fx) ,,docs
                           (-fxlspace n a b fx :end end :dim ,,dim :type ',',type))))))
(map-fxlspace 1 ff) (map-fxlspace 2 ff) (map-fxlspace 3 ff) (map-fxlspace 4 ff)
(map-fxlspace 1 df) (map-fxlspace 2 df) (map-fxlspace 3 df) (map-fxlspace 4 df)


; TODO: inefficient use of xdlerp

(defmacro -lspace (dim type)
  (let ((exportname (veqsymb dim type "$LSPACE"))
        (with-arrays (veqsymb 1 type "WITH-ARRAYS")))
    `(progn
      (export ',exportname)
      (fvdef* ,exportname (n (varg ,dim a b) &key (end t))
        (declare (pn n) (,type a b) (boolean end))
        (let ((stp (,type (if end (/ (1- n)) (/ n)))))
          (declare (,type stp))
          (,with-arrays (:n n :itr k
           :arr ((arr ,dim))
           :fxs ((lspacefx (i) (,(veqsymb dim type "LERP") a b
                                  (,type (* i stp)))))
           :exs ((arr k (lspacefx k))))
             arr))))))
(-lspace 1 ff) (-lspace 2 ff) (-lspace 3 ff) (-lspace 4 ff)
(-lspace 1 df) (-lspace 2 df) (-lspace 3 df) (-lspace 4 df)

