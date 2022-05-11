
(in-package :veq)

;;;;;;;;;;;;;;;;;; VMVB

(defun -vmvb (type dim arg expr body &key (repvarg t))
  (declare (symbol arg) (list expr) (fixnum dim))
  (let ((body `(mvb ((varg ,dim ,arg)) ,expr
                    (declare (,type ,arg))
                    ,@body)))
    (if repvarg (replace-varg body) body)))

; (defmacro map-mvb (dim type)
;   (let* ((mname (veqsymb dim type "mvb"))
;          (docs (format nil "make ~ad mvb.~%ex: (f3mvb ((:va 3 x)) (fx ...) ...)
; assumes (fx ...) returns three values and assigns them to x." dim)))
;     `(progn (map-docstring ',mname ,docs :nodesc :context)
;             (map-symbol `(,',mname
;                            (arg expr &body body) ,,docs
;                            (-vmvb ',',type ,,dim arg expr body))))))
; (map-mvb 1 ff) (map-mvb 2 ff) (map-mvb 3 ff) (map-mvb 4 ff)
; (map-mvb 1 df) (map-mvb 2 df) (map-mvb 3 df) (map-mvb 4 df)

;;;;;;;;;;;;;;;;;; VLET

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

(defmacro map-vlet (dim type)
  (let* ((mname (veqsymb dim type "let"))
         (docs (format nil "make ~ad let.~%ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           (all-args &body body) ,,docs
                           (-vlet ',',type ,,dim all-args body))))))
(map-vlet 2 ff) (map-vlet 3 ff) (map-vlet 4 ff)
(map-vlet 2 df) (map-vlet 3 df) (map-vlet 4 df)


(defun dim? (a dim &key type &aux (l (length a)))
  (unless (= l dim)
          (error "incorrect dim. got: ~a expected: ~a. ~% expr: ~% ~a" l dim a))
  (if type (mapcar (lambda (v) (if (numberp v) (coerce v type) v)) a) a))
(defun ddim? (a dim) (dim? a dim :type 'df))
(defun fdim? (a dim) (dim? a dim :type 'ff))

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


;;;;;;;;;;;;;;;;;; (d2 ...)

(defmacro map-td (dim type)
  (let* ((mname (veqsymb dim type ""))
         (docs (format nil "make ~ad vector in veq context.~%strict." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           (&body body) ,,docs
                           `(~ ,@(dim? body ',',dim :type ',',type)))))))
(map-td 1 ff) (map-td 2 ff) (map-td 3 ff) (map-td 4 ff)
(map-td 1 df) (map-td 2 df) (map-td 3 df) (map-td 4 df)


;;;;;;;;;;;;;;;;;; (d2~ ...)

(defmacro map-td~ (dim type)
  (let* ((mname (veqsymb dim type "~"))
         (docs (format nil "make ~ad vector in veq context.~%coerce to type." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           (&body body) ,,docs
                           `(~ (,',',(symb type "*")
                                 ,@(dim? body ',',dim :type ',',type))))))))
(map-td~ 1 ff) (map-td~ 2 ff) (map-td~ 3 ff) (map-td~ 4 ff)
(map-td~ 1 df) (map-td~ 2 df) (map-td~ 3 df) (map-td~ 4 df)

;;;;;;;;;;;;;;;;;; (d2rep ...)

(defmacro map-rep (dim type)
  (let* ((mname (veqsymb dim type "rep"))
         (docs (format nil "repeat argument ~ad times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx))." dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           (expr) ,,docs
                           `(values
                              ,@(loop repeat ',',dim
                                      collect expr of-type ',',type)))))))
(map-rep 1 ff) (map-rep 2 ff) (map-rep 3 ff) (map-rep 4 ff)
(map-rep 1 df) (map-rep 2 df) (map-rep 3 df) (map-rep 4 df)

;;;;;;;;;;;;;;;;;; (d2rep* ...)

(defmacro map-rep* (dim type)
  (awg (e)
  (let ((mname (veqsymb dim type "rep*"))
        (docs (format nil "repeat the evaluated argument ~ad times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once." dim))
        (vals `(values ,@(loop repeat dim collect e))))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           (expr) ,,docs
                           `(let ((,',',e ,expr))
                                   (declare (,',',type ,',',e))
                                   ,',',vals)))))))
(map-rep* 1 ff) (map-rep* 2 ff) (map-rep* 3 ff) (map-rep* 4 ff)
(map-rep* 1 df) (map-rep* 2 df) (map-rep* 3 df) (map-rep* 4 df)

