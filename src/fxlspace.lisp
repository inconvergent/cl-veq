(in-package :veq)

; TODO: inefficient use of xdlerp

(defun -wrapnum (n) (typecase n (cons n) (t `(values ,n))))

(defun -fxlspace (n a b fx &key dim type (end t))
  (declare (fixnum dim) (symbol type) (boolean end))
  "do (mvc fx i x y ...) for n points between a, b"
  (awg (i n* stp s fx* a* b*)
    `(let* ((,n* ,n)
            (,fx* (progn ,@fx))
            (,stp (coerce ,(if end `(/ (1- ,n*)) `(/ ,n*)) ',type)))
     (declare (pos-int ,n*) (,type ,stp) (function ,fx*))
     (fvprogn ; fvprogn to enable macrolet for fvlet and LERP.
              ; otherwise fxlspace can't be compiled inside a fvprogn.
              ; this is still faster
       (fvlet ((,a* ,dim ,(-wrapnum a))
               (,b* ,dim ,(-wrapnum b)))
         (loop for ,i of-type pos-int from 0 below ,n*
               for ,s of-type ,type from ,(coerce 0 type)
               do (mvc ,fx* ,i (,(veqsymb dim type "LERP")
                                 ,a* ,b* (* ,s ,stp)))))))))

