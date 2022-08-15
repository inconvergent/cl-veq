
(in-package :veq)


; TODO: :res will cause unexpected results if indices are not monotonic
; increasing?
(defmacro define-take-fx (dim type)
  (declare (symbol type) (fixnum dim))
  (let ((fxname (veqsymb dim type "$TAKE")) ; f2$take
        (arrmacro (veqsymb 1 type "WITH-ARRAYS")) ; fwith-arrays
        (resarr (veqsymb 1 type "$MAKE"))
        (docs (format nil "returns ~ad array with rows for inds.
use :res to put result in existing array." dim))) ; f2$make
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
(define-take-fx 1 ff) (define-take-fx 2 ff) (define-take-fx 3 ff) (define-take-fx 4 ff)
(define-take-fx 1 df) (define-take-fx 2 df) (define-take-fx 3 df) (define-take-fx 4 df)

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
; (define-slice-fx 1 ff) (define-slice-fx 2 ff) (define-slice-fx 3 ff) (define-slice-fx 4 ff)
; (define-slice-fx 1 df) (define-slice-fx 2 df) (define-slice-fx 3 df) (define-slice-fx 4 df)

