
(in-package :veq)

; TODO: inefficient use of xdlerp

; wrap non cons n in (values n)
(defun -wrapnum (n) (typecase n (cons n) (t `(values ,n))))

(defun lspace (n a b &key dim type (end t))
  (declare (fixnum dim) (symbol type) (boolean end))
  "n points between a, b"
  (awg (i n* stp s arr a* b*)
    `(let* ((,n* ,n)
            (,stp (coerce ,(if end `(/ (1- ,n*)) `(/ ,n*)) ',type))
            (,arr (,(veqsymb dim type "$ZERO") ,n*)))

     (declare (pos-int ,n*) (,type ,stp) (,(arrtype type) ,arr))
     (fvlet ((,a* ,dim ,(-wrapnum a))
             (,b* ,dim ,(-wrapnum b)))
       (loop for ,i of-type pos-int from 0 below ,n*
             for ,s of-type ,type from ,(coerce 0 type)
             do (-vaset (,arr ,dim ,i)
                        (,(veqsymb dim type "LERP") ,a* ,b* (* ,s ,stp)))
             finally (return ,arr))))))


(defun fxlspace (n a b fx &key dim type (end t))
  (declare (fixnum dim) (symbol type) (boolean end))
  "do (mvc fx i x y ...) for n points between a, b"
  (awg (i n* stp s fx* a* b*)
    `(let* ((,n* ,n)
            (,fx* (progn ,@fx))
            (,stp (coerce ,(if end `(/ (1- ,n*)) `(/ ,n*)) ',type)))
     (declare (pos-int ,n*) (,type ,stp) (function ,fx*))
     (fvlet ((,a* ,dim ,(-wrapnum a))
             (,b* ,dim ,(-wrapnum b)))
       (loop for ,i of-type pos-int from 0 below ,n*
             for ,s of-type ,type from ,(coerce 0 type)
             do (mvc ,fx* ,i (,(veqsymb dim type "LERP")
                               ,a* ,b* (* ,s ,stp))))))))

