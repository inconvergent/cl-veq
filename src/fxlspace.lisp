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

