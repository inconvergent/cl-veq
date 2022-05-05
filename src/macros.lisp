
(in-package :veq)

(defparameter *duperr* "~%duplicate definitions in *symbols-map*: ~a~%
dupes: ~a~%
did you load :veq multiple times?~%")


(defun filter-macrolets (symbols-map body)
  "remove macrolet tuples that are not present in body. this speeds up
  compilation time considerably, and makes it easier to read output code.

  it may cause errors in in cases with nested macros?
  can it cause problems when imported in a different env?"
  (declare #.*opt* (list symbols-map body))
  (let ((macrolet-symbols-in-body
          (remove-duplicates (remove-if-not #'symbolp (awf body)))))
    (declare (list macrolet-symbols-in-body))
    (remove-if-not (lambda (s) (declare (symbol s))
                               (member s macrolet-symbols-in-body))
                   symbols-map :key #'car)))

(defun define-env-macros (symbols-map)
  "
  make macros vdef/vprogn with all definitions in symbols-map.  this can be
  called multiple times if new items are added to symbols map.

  NOTE: for internal use only. this will not work as expected when called after
  veq has been loaded
  "
  (let* ((names (sort (mapcar (lambda (v) (mkstr (first v))) symbols-map)
                   #'string-lessp))
         (dupes (dupes names)))
  ; this should not happen anymore as duplicates are removed when
  ; adding pairs via (map-symbol pair). keep this test for now.
  (when dupes (error *duperr* names dupes))

  (defmacro vprogn (&body body)
    "enable veq context inside this progn.
    handles propagation and resolution of uses of (varg d var) and (vref var i).

    fvprogn is faster, but has some limitations."
    `(macrolet ,symbols-map (progn ,@(replace-varg body))))

  (defmacro fvprogn (&body body)
    "enable veq context inside this progn.
    handles propagation and resolution of uses of (varg d var) and (vref var i).

    works the same way as vprogn. but removes all macrolets that are not
    directly used in body. this is faster, but may fail in some cases where
    body is complex. in the event of errors try vprogn instead."
    `(macrolet ,(filter-macrolets symbols-map body)
       (progn ,@(replace-varg body))))

  (defmacro vdef (fname &body body)
    "define function with veq context enabled. uses vprogn."
    `(vprogn (defun ,fname ,@body)))

  (defmacro fvdef (fname &body body)
    "define function with veq context enabled. uses fvprogn."
    `(fvprogn (defun ,fname ,@body)))

  (defmacro vdef* (mname &body body)
    "defines a function named: %fx
and a wrapper macro named: fx
veq context is enabled. uses vprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...)."
    (let ((fname (symb "%" mname)))
      `(vprogn ; replace internal references to mname
               (defun ,fname ,@(subst fname mname body))
               (defmacro ,mname (&rest rest)
                  ,(format nil "fx: ~a~%macro wrapper: ~a~%
defined veq:vdef*" fname mname)
                 `(mvc #',',fname ,@rest)))))

  (defmacro fvdef* (mname &body body)
    "defines a function named: %fx
and a wrapper macro named: fx
veq context is enabled. uses fvprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...)."
    (let ((fname (symb "%" mname)))
      `(fvprogn ; replace internal references to mname
                (defun ,fname ,@(subst fname mname body))
                (defmacro ,mname (&rest rest)
                  ,(format nil "fx: ~a~%macro wrapper: ~a~%
defined via veq:fvdef*" fname mname)
                  `(mvc #',',fname ,@rest)))))

  (defmacro def* (mname &body body)
    "defines a function named: %fx
and a wrapper macro named: fx

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...)."
    (let ((fname (symb "%" mname)))
      `(progn (defun ,fname ,@body)
              (defmacro ,mname (&rest rest)
                  ,(format nil "fx: ~a~%macro wrapper: ~a~%
defined via veq:def*" fname mname)
                `(mvc #',',fname ,@rest)))))))


;;;;;;;;;;;;;;;;; VARIOUS

; NOTE: using (lst ...) makes things slower in some cases.  because of the
; consing or because the number of values is unknown?. avoid when possible.
(defmacro lst (&body body)
  "wrap (values ..) in (list ..)"
  `(mvc #'list ,@body))
(defmacro from-lst (l)
  "get values from list"
  `(apply #'values ,l))
(defmacro ~ (&rest rest)
  "wraps arguments in (mvc #'values ...)"
  `(mvc #'values ,@rest))


;;;;;;;;;;;;;;;;;; MACRO UTILS

(map-docstring 'vref
  "use (veq:vref s x) or (:vr s x) to get dim x of symbol s
in fvdef*, vdef*, def*. see replace-varg for implementation details
" :nodesc :context)

(map-docstring 'varg
  "use (veq:varg n a b ...) or (:vr n a b ...) to represent n dim
vectors a,b of dim n in fvdef*, vdef*, def*. see replace-varg
for details
" :nodesc :context)

(defun replace-varg (body &optional (root-rmap (list)))
  "this is used to replace instances of varg/:varg/:va and vref/:vref/vr with
  approprite symbols for the dimension.

  local maps vref/varg maps are propagated forwards in the list so a given
  arg/ref should be available under it's scope.
  it seems to work for all cases i have tested. but i'm mot sure if this
  propagation will eventually break somewhere."
  (labels
    ((is-varg (v) (and (listp v) (member (car v) '(varg :varg :va) :test #'eq)))
     (is-vref (root) (and (listp root) (listp (car root))
                          (member (caar root) '(vref :vref :vr) :test #'eq)))
     (-safe-do-list (root rmap)
       (handler-case
         (loop with res = (list)
               for v in (reverse root)
               if (is-varg v)
               do (dsb (n &rest rest) (cdr v)
                    (loop for name in rest
                          collect (if (assoc name rmap)
                                    ; already in rmap, use existing gensyms
                                    `(,(cdr (assoc name rmap)))
                                    ; make new gensyms and add to rmap
                                    (let ((syms (-gensyms name n)))
                                      (push `(,name . ,syms) rmap)
                                      syms)) into new
                          finally (setf res `(,@(awf new) ,@res))))

               else do (push v res)
               finally (return (values res rmap)))
         ; this breaks eg when root contains a dotted pair: '(:a :b . :c)
         (error (e) (declare (ignore e)) (values root rmap))))
     (get-car-rmap (root rmap)
       (if (listp root) (-safe-do-list root rmap)
                        (values root rmap)))
     (do-vref (root rmap)
       ; replace-varg can encounter vrefs that have no match in rmap (eg
       ; in -vmvb). so we need to ignore missing matches for vref.
       ; these vrefs will be replaced at a later time (eg. in the call
       ; to replace-varg in -vlet)
       (aif (assoc (cadr root) rmap) ; if ref in rmap
            (mapcar (lambda (i)
                      (nth (the pos-int i) (cdr it))) ; (cdr it) == symbs
                    (cddr root)) ; inds
            (list root))) ; do nothing
     (walk (root rmap)
       (cond ((atom root) root)
             ((is-vref root)
               `(,@(do-vref (car root) rmap)
                 ,@(walk (cdr root) rmap)))

             ((and (listp root) (assoc (car root) rmap))
               `(,@(cdr (assoc (car root) rmap))
                 ,@(walk (cdr root) rmap)))

             (t (mvb (car* rmap) (get-car-rmap (car root) rmap)
                  (cons (walk car* rmap)
                        (walk (cdr root) rmap)))))))
    (walk body root-rmap)))


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
note that this behaves as native lisp let*." dim)))
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
  "wraps labels so that it can be used with implicit multiple value call (mvc).
   that is, all labels are defined as if with vdef* or fvdef*
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
         (docs (format nil "make ~ad rep.~%ex: (f3rep (fx))~%corresponds to
(values (fx) (fx) (fx))" dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           ( expr ) ,,docs
                           `(values
                              ,@(loop repeat ',',dim
                                      collect expr of-type ',',type)))))))
(map-rep 1 ff) (map-rep 2 ff) (map-rep 3 ff) (map-rep 4 ff)
(map-rep 1 df) (map-rep 2 df) (map-rep 3 df) (map-rep 4 df)

;;;;;;;;;;;;;;;;;; (d2rep* ...)

(defmacro map-rep* (dim type)
  (awg (e)
  (let ((mname (veqsymb dim type "rep*"))
        (docs (format nil "make ~ad rep*.~%ex: (f3rep (fx))
returns (values v v v) where v = (fx)" dim))
        (vals `(values ,@(loop repeat dim collect e))))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname
                           ( expr ) ,,docs
                           `(let ((,',',e ,expr))
                                   (declare (,',',type ,',',e))
                                   ,',',vals)))))))
(map-rep* 1 ff) (map-rep* 2 ff) (map-rep* 3 ff) (map-rep* 4 ff)
(map-rep* 1 df) (map-rep* 2 df) (map-rep* 3 df) (map-rep* 4 df)

; define vlet/vprogn with current symbols-map.
; depends on several preceeding ; files
(define-env-macros *symbols-map*)

