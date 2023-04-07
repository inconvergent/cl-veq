(in-package :veq)

;;;;;;;;;;;;;;;;;; VMVB/VLET

(defun -vmvb (type dim arg expr body &key (repvarg t))
  (declare (symbol arg) (list expr) (fixnum dim))
  "returns (mvb ((:va dim arg)) expr body)"
  (let ((body `(mvb ((:va ,dim ,arg)) ,expr
                    ,@(unless (or (null type) (equal :nil type))
                              `((declare (,type ,arg))))
                    ,@body)))
    (if repvarg (replace-varg body) body)))
(defun -vmvb* (type dim arg expr body)
  (-vmvb type dim arg expr body :repvarg nil))

(defun -vlet (type dim all-args body)
  (let ((sme (some (lambda (a) (not (= (length a) 2))) all-args)))
    (when sme (error "vlet error. expected (var expr)~% got: ~a" sme)))
  (loop for (arg expr) in (reverse all-args)
        do (setf body (list (-vmvb* type dim arg expr body)))
        finally (return (replace-varg `(progn ,@body)))))

(defun -vlet* (type all-args body)
  (when (some (lambda (a) (not (> 4 (length a) 1))) all-args)
        (error "vlet* error. expected (var dim expr)~% got: ~a" all-args))
  (labels ((dim-fill (a) (if (= (length a) 2) ; (arg expr) -> (arg 1 expr)
                             (dsb (arg expr) a (list arg 1 expr))
                             a)))
    (loop with all-args = (mapcar #'dim-fill all-args)
          for (arg dim expr) of-type (symbol fixnum cons) in (reverse all-args)
          do (setf body (list (-vmvb* type dim arg expr body)))
          finally (return (replace-varg `(progn ,@body))))))

; TODO: deprecate in favour of xlet or programmable interface for xlet?
; TODO: map-docstring for these
(mapcar #'map-symbol
  `((dvlet (all-args &body body) (-vlet* 'df all-args body))
    (fvlet (all-args &body body) (-vlet* 'ff all-args body))
    (ivlet (all-args &body body) (-vlet* 'in all-args body))
    (pvlet (all-args &body body) (-vlet* 'pn all-args body))))

(defmacro define-vlet (dim type)
  (let* ((mname (veqsymb dim type "let"))
         (docs (format nil "make ~ad let.~%ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*." dim)))
    `(progn (export ',mname)
            (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (all-args &body body) ,,docs
                                   (-vlet ',',type ,,dim all-args body))))))

(defun dim? (a dim &key type &aux (l (length a)))
  (unless (= l dim)
          (error "incorrect dim. got: ~a expected: ~a. ~% expr: ~% ~a" l dim a))
  (if type (mapcar (lambda (v) (if (numberp v) (coerce v type) v)) a) a))

(defmacro define-td (dim type)
  (let* ((mname (veqsymb dim type ""))
         (docs (format nil "strict make ~ad vector in veq context." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (&body body) ,,docs
                                   `(values ,@(dim? body ',',dim :type ',',type)))))))

(defmacro define-td~ (dim type)
  (let* ((mname (veqsymb dim type "~"))
         (dimtype `(,dim ,type))
         (docs (format nil "make ~ad vector in veq context.
wraps body in mvc so that (f3~~ 1 (f2~~ 2f0 3))
returns (values 1f0 2f0 3f0)" dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (&body body) ,,docs
                                   `(mvcmap ,',',dimtype ,@body))))))
; TODO: nrep/nrep* typed/untyped
(defmacro define-rep (dim type)
  (let* ((mname (veqsymb dim type "rep"))
         (docs (format nil "repeat argument ~ad times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx))." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (expr) ,,docs
                                   `(values ,@(loop repeat ',',dim
                                                    collect expr of-type ',',type)))))))
(defmacro define-val (dim type)
  (awg (e)
    (let ((mname (veqsymb dim type "val"))
          (vals `(values ,@(loop repeat dim collect e)))
          (docs (format nil "repeat the evaluated argument ~a times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v))." dim)))
      `(progn (map-docstring ',mname ,docs :nodesc :context)
              (map-symbol `(,',mname (expr) ,,docs
                                     `(let ((,',',e ,expr))
                                        (declare (,',',type ,',',e))
                                        ,',',vals)))))))

(defmacro define-creators ()
 `(progn
    ,@(loop for (d ty) in (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df
                                   1 in 2 in 3 in 4 in 1 pn 2 pn 3 pn 4 pn) 2)
            append `((define-td ,d ,ty) (define-td~ ,d ,ty)
                     (define-rep ,d ,ty) (define-val ,d ,ty)
                     ; flet conflicts with cl:flet
                     ,(when (> d 1) `(define-vlet ,d ,ty))))))
(define-creators)

;;;;;;;;;;;;;;;;;; XLET

(defmacro xlet (all-vars &body body) ;,docs
  "bind typed veqs, and other variables:
(veq:xlet ((f2!a (f2 1f0 2f0)) ; 2d veq:ff/float
           (d!b 1d0) ; 1d veq:df/double
           (h :a)
           (i4!c (values 1 2 3 4))) ; 4d veq:in/integer
  (declare (keyword h))
  (do-something a b c h))

names without ! will be treated (mostly) as in a regular let.
declare can be used to declare types.
NOTE: xlet behaves more like CL let* in that bindings are available
immediately."
  (labels ((ensure-values (v) (typecase v (list v) (otherwise `(values ,v))))
           (select-type (decl var ty &aux (d (cdr (assoc var decl))))
             (if d d (type-from-short ty t)))
           (inverse-decl (&aux (decl (list)))
             (loop for (ty . vars) in (cdar body)
                   do (loop for v in vars do (push `(,v . ,ty) decl)))
             decl)
           (parse-declare ()
             (handler-case
               (if (equal (symbol-name (caar body)) "DECLARE")
                   (values (inverse-decl) (cdr body))
                   (values nil body))
               (error () (values nil body)))))
     (mvb (decl body) (parse-declare)
       (loop for (arg expr) in (reverse all-vars)
             for (ty dim var) = (lst (unpack-veqsymb arg))
             do ; dim = 1 behaves like normal let; no gensym. we dont move
                ; this into vmvb because we want to vmvb to be generic eg. so
                ; that :va :vr works for all dimensions
                (setf body `(,(if (= dim 1)
                                  `(let ((,var ,(ensure-values expr)))
                                     (declare (,(select-type decl var ty) ,var))
                                     ,@body)
                                  (-vmvb* (select-type decl var ty) dim var
                                          (ensure-values expr) body)))))
       (replace-varg (car body)))))

;;;;;;;;;;;;;;;;;; VLABELS

(defmacro vlabels ((&rest labs) &body body)
  "wraps labels so that it can be used with implicit mvc. that is, all labels
are defined as if with def*.
use %labelname to call the function directly."
  (labels ((mfx (l)
             (declare (list l))
             `(,(car l) (&rest rest)
                        `(veq:mvc #',',(cadr l) ,@rest)))
           (ffx (l &aux (l* (cdr l)))
             `(,(car l*) ,@(subst (car l*) (car l) (cdr l*)))))
    (let ((labnames (loop for l in labs
                          collect `(,(car l) ,(symb "%" (car l)) ,@(cdr l)))))
      (declare (list labnames))
      `(macrolet (,@(mapcar #'mfx labnames))
         (labels (,@(mapcar #'ffx labnames))
           ,@body)))))


