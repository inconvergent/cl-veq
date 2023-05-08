(in-package :veq)


(defmacro make-last (dim type)
  (awg (a)
    `(progn (export ',(vvsym type dim "$last"))
            (defun ,(vvsym type dim "$last") (,a)
              (declare #.*opt* (,(arrtype type) ,a))
              ,(format nil "return values from last row of ~ad vector array." dim)
            (,(vvsym type dim "$") ,a
               (1- (the pn (,(vvsym nil dim "$num") ,a))))))))
(make-last 1 ff) (make-last 2 ff) (make-last 3 ff) (make-last 4 ff)
(make-last 1 df) (make-last 2 df) (make-last 3 df) (make-last 4 df)

; TODO: protect c
(defun -struct-fields (dim type s c slots)
  `(with-struct
     (,s ,@(mapcar #'symb (remove-if-not #'keywordp slots))) ,c
     (~ ,@(mapcar (lambda (o) (if (not (keywordp o)) o
                                `(,(vvsym type dim "$") ,(symb o))))
                  slots))))
(defmacro struct-fields (dim type)
  (let* ((mname (vvsym type dim "$s"))
         (docs (format nil "get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (~a c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are ~a of dim ~a" mname (arrtype type) dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (c s &rest rest) ,,docs
                                   (-struct-fields ,,dim ',',type s c rest))))))
(struct-fields 1 ff) (struct-fields 2 ff) (struct-fields 3 ff) (struct-fields 4 ff)
(struct-fields 1 df) (struct-fields 2 df) (struct-fields 3 df) (struct-fields 4 df)

