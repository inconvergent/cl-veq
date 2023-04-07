
(in-package :veq)


; TODO: :res will cause unexpected results if indices are not monotonic
; increasing?
(defmacro define-take-fx (dim type)
  (declare (symbol type) (fixnum dim))
  (let ((fxname (veqsymb dim type :$take)) ; f2$take
        (arrmacro (veqsymb 1 type :with-arrays)) ; fwith-arrays
        (resarr (veqsymb 1 type :$make)) ; f2$make
        (docs (format nil "returns ~ad array with rows for inds.
use :res to put result in existing array." dim)))
    `(progn
       (export ',fxname)
       (fvdef ,fxname (a inds &key res)
         ,docs
         (declare (,(arrtype type) a) (sequence inds))
         (let ((n (length inds)))
            (declare (pos-int n))
            (,arrmacro (:cnt cnt :inds inds
              :arr ((a ,dim a)
                    (res ,dim (if res res (,resarr :dim ,dim :n n))))
              :fxs ((acc ((varg ,dim x)) (values x)))
              :exs ((res cnt (acc a))))
              res))))))

(defmacro define-takes ()
 `(progn
    ,@(loop for (d ty) in (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df
                                   1 in 2 in 3 in 4 in 1 pn 2 pn 3 pn 4 pn) 2)
            append `((define-take-fx ,d ,ty)))))
(define-takes)

; (defmacro define-slice-fx (dim type)
;   (declare (symbol type) (fixnum dim))
;   (awg (a* start* n* res)
;     (let ((fxname (veqsymb dim type "$SLICE")) ; f2$take
;         (arrmacro (veqsymb 1 type "WITH-ARRAYS")) ; fwith-arrays
;         (resarr (veqsymb 1 type "$MAKE"))
;         (docs (format nil "~a" dim))) ; f2$make
;     `(progn
;        (export ',fxname)
;        (fvdef ,fxname (a start &key n)
;          ,docs
;          (declare (,(arrtype type) a))
;          (let* ((,a* a)
;                 (,start* start)
;                 (,n* (- (,(veqsymb dim type "$NUM") ,a*) ,start*)))
;             (declare (pos-int ,n* ,start*) (,(arrtype type) ,a*))
;             (,arrmacro (:cnt cnt
;               :arr ((,a* ,dim ,a*)
;                     (,res ,dim (,resarr :dim ,dim :n n)))
;               :fxs ((acc ((varg ,dim x)) (values x)))
;               :exs ((,res cnt (acc ,a*))))
;               ,res)))))))

