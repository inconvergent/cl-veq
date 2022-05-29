
(in-package :veq)

;;;;;;;;;;;;;;;;;; VMVB/VLET

(defun -vmvb (type dim arg expr body &key (repvarg t))
  (declare (symbol arg) (list expr) (fixnum dim))
  (let ((body `(mvb ((varg ,dim ,arg)) ,expr
                    (declare (,type ,arg))
                    ,@body)))
    (if repvarg (replace-varg body) body)))

(defun -vlet (type dim all-args body)
  (let ((sme (some (lambda (a) (not (= (length a) 2))) all-args)))
    (when sme (error "vlet error. expected (var expr)~% got: ~a" sme)))
  (loop for (arg expr) in (reverse all-args)
        do (setf body (list (-vmvb type dim arg expr body :repvarg nil)))
        finally (return (replace-varg `(progn ,@body)))))

(defun -vlet* (type all-args body)
  (when (some (lambda (a) (not (> 4 (length a) 1))) all-args)
        (error "vlet* error. expected (var dim expr)~% got: ~a" all-args))
  (labels ((dim-fill (a) (if (= (length a) 2) ; (arg expr) -> (arg 1 expr)
                             (dsb (arg expr) a (list arg 1 expr))
                             a)))
    (loop with all-args = (mapcar #'dim-fill all-args)
          for (arg dim expr) of-type (symbol fixnum cons) in (reverse all-args)
          do (setf body (list (-vmvb type dim arg expr body :repvarg nil)))
          finally (return (replace-varg `(progn ,@body))))))


; TODO: escape all variables that can cause capture in all these macros

(mapcar #'map-symbol
  `((dvlet (all-args &body body) (-vlet* 'df all-args body))
    (fvlet (all-args &body body) (-vlet* 'ff all-args body))))

(defmacro define-vlet (dim type)
  (let* ((mname (veqsymb dim type "let"))
         (docs (format nil "make ~ad let.~%ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (all-args &body body) ,,docs
                                   (-vlet ',',type ,,dim all-args body))))))
(define-vlet 2 ff) (define-vlet 3 ff) (define-vlet 4 ff)
(define-vlet 2 df) (define-vlet 3 df) (define-vlet 4 df)

;;;;;;;;;;;;;;;;;; VLABELS

(defmacro vlabels ((&rest labs) &body body)
  "wraps labels so that it can be used with implicit mvc. that is,
all labels are defined as if with def*, vdef* or fvdef*
use %labelname to call the function directly, not via mvc."
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

;;;;;;;;;;;;;;;;;; MAKE VECTOR


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
(define-td 1 ff) (define-td 2 ff) (define-td 3 ff) (define-td 4 ff)
(define-td 1 df) (define-td 2 df) (define-td 3 df) (define-td 4 df)

(defmacro define-td~ (dim type)
  (let* ((mname (veqsymb dim type "~"))
         (dimtype `(,dim ,type))
         (docs (format nil "make ~ad vector in veq context.
wraps body in mvc so that (f3~~ 1 (f2~~ 2f0 3))
returns (values 1f0 2f0 3f0)" dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (&body body) ,,docs
                                   `(mvcmap ,',',dimtype ,@body))))))
(define-td~ 1 ff) (define-td~ 2 ff) (define-td~ 3 ff) (define-td~ 4 ff)
(define-td~ 1 df) (define-td~ 2 df) (define-td~ 3 df) (define-td~ 4 df)

;;;;;;;;;;;;;;;;;; REP/REP*

(defmacro define-rep (dim type)
  (let* ((mname (veqsymb dim type "rep"))
         (docs (format nil "repeat argument ~ad times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx))." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (expr) ,,docs
                                   `(values ,@(loop repeat ',',dim
                                                    collect expr of-type ',',type)))))))
(define-rep 1 ff) (define-rep 2 ff) (define-rep 3 ff) (define-rep 4 ff)
(define-rep 1 df) (define-rep 2 df) (define-rep 3 df) (define-rep 4 df)

(defmacro define-rep* (dim type)
  (awg (e)
    (let ((mname (veqsymb dim type "rep*"))
          (vals `(values ,@(loop repeat dim collect e)))
          (docs (format nil "repeat the evaluated argument ~a times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v))." dim)))
      `(progn (map-docstring ',mname ,docs :nodesc :context)
              (map-symbol `(,',mname (expr) ,,docs
                                     `(let ((,',',e ,expr))
                                        (declare (,',',type ,',',e))
                                        ,',',vals)))))))
(define-rep* 1 ff) (define-rep* 2 ff) (define-rep* 3 ff) (define-rep* 4 ff)
(define-rep* 1 df) (define-rep* 2 df) (define-rep* 3 df) (define-rep* 4 df)

