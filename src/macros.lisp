
(in-package :veq)


(defun filter-macrolets (symbols-map body)
  "remove macrolet tuples that are not present in body. this speeds up
  compilation time considerably, and makes it easier to read output code.

  it may cause errors in in cases with nested macros?
  can it cause problems when imported in a different env?"
  (declare #.*opt* (list symbols-map body))
  (let ((macrolet-symbols-in-body
          (remove-duplicates (remove-if-not #'symbolp (awf body)))))
    (declare (list macrolet-symbols-in-body))
    (remove-if-not (lambda (s) (declare (symbol s)) (member s macrolet-symbols-in-body))
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
  (when dupes (error "duplicate definitions in *symbols-map*: ~a~%
dupes: ~a~%
did you load :veq multiple times?~%" names dupes))

  (defmacro vprogn (&body body)
    "enable veq inside this progn"
    `(macrolet ,symbols-map (progn ,@(replace-varg body))))

  (defmacro fvprogn (&body body)
    "enable veq inside this progn.  removes all macrolets that are not directly
    used in body. this is faster, but will fail if macros in body introduce
    macrolets from veq"
    `(macrolet ,(filter-macrolets symbols-map body)
       (progn ,@(replace-varg body))))

  (defmacro vdef (fname &body body)
    "define function with veq enabled. see vprogn."
    `(vprogn (defun ,fname ,@body)))

  (defmacro fvdef (fname &body body)
    "define function with veq enabled. see fvprogn."
    `(fvprogn (defun ,fname ,@body)))

  (defmacro vdef* (mname &body body)
    "define function, and corresponding macro, with veq enabled."
    (let ((fname (symb "%" mname)))
      `(vprogn ; replace internal references to mname
               (defun ,fname ,@(subst fname mname body))
               (defmacro ,mname (&rest rest)
                 `(mvc #',',fname ,@rest)))))

  (defmacro fvdef* (mname &body body)
    "define function, and corresponding macro, with veq enabled. see fvprogn."
    (let ((fname (symb "%" mname)))
      `(fvprogn ; replace internal references to mname
                (defun ,fname ,@(subst fname mname body))
                (defmacro ,mname (&rest rest)
                  `(mvc #',',fname ,@rest)))))

  (defmacro def* (mname &body body)
    "define function, and corresponding macro, without veq enabled."
    (let ((fname (symb "%" mname)))
      `(progn (defun ,fname ,@body)
              (defmacro ,mname (&rest rest)
                `(mvc #',',fname ,@rest)))))))


;;;;;;;;;;;;;;;;; VARIOUS

; using this makes things much slower because of the consing or because the
; number of values is unknown?. avoid when possible.
; TODO: find a different solution for some cases?
(defmacro lst (&body body)
  "wrap (values ..) in (list ..)"
  `(mvc #'list ,@body))
(defmacro from-lst (v) `(apply #'values ,v))


;;;;;;;;;;;;;;;;;; MACRO UTILS

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

(defun dim? (a dim &key type &aux (l (length a)))
  (unless (= l dim)
          (error "incorrect dim. got: ~a expected: ~a. ~% expr: ~% ~a" l dim a))
  (if type (mapcar (lambda (v) (if (numberp v) (coerce v type) v)) a) a))
(defun ddim? (a dim) (dim? a dim :type 'df))
(defun fdim? (a dim) (dim? a dim :type 'ff))


(mapcar (lambda (pair) (push pair *symbols-map*)
                       (export (car pair)))
  `((d (&body body) `(mvc #'values ,@(ddim? body 1)))
    (f (&body body) `(mvc #'values ,@(fdim? body 1)))
    (d2 (&body body) `(mvc #'values ,@(ddim? body 2)))
    (f2 (&body body) `(mvc #'values ,@(fdim? body 2)))
    (d3 (&body body) `(mvc #'values ,@(ddim? body 3)))
    (f3 (&body body) `(mvc #'values ,@(fdim? body 3)))
    (d4 (&body body) `(mvc #'values ,@(ddim? body 4)))
    (f4 (&body body) `(mvc #'values ,@(fdim? body 4)))

    (d~ (&body body) `(mvc #'values (df* ,@(dim? body 1))))
    (f~ (&body body) `(mvc #'values (ff* ,@(dim? body 1))))
    (d2~ (&body body) `(mvc #'values (df* ,@(dim? body 2))))
    (f2~ (&body body) `(mvc #'values (ff* ,@(dim? body 2))))
    (d3~ (&body body) `(mvc #'values (df* ,@(dim? body 3))))
    (f3~ (&body body) `(mvc #'values (ff* ,@(dim? body 3))))
    (d4~ (&body body) `(mvc #'values (df* ,@(dim? body 4))))
    (f4~ (&body body) `(mvc #'values (ff* ,@(dim? body 4))))

    (d2mvb (arg expr &body body) (-vmvb 'df 2 arg expr body))
    (d3mvb (arg expr &body body) (-vmvb 'df 3 arg expr body))
    (d4mvb (arg expr &body body) (-vmvb 'df 4 arg expr body))
    (f2mvb (arg expr &body body) (-vmvb 'ff 2 arg expr body))
    (f3mvb (arg expr &body body) (-vmvb 'ff 3 arg expr body))
    (f4mvb (arg expr &body body) (-vmvb 'ff 4 arg expr body))

    (dvlet (all-args &body body) (-vlet* 'df all-args body))
    (fvlet (all-args &body body) (-vlet* 'ff all-args body))

    (d2let (all-args &body body) (-vlet 'df 2 all-args body))
    (d3let (all-args &body body) (-vlet 'df 3 all-args body))
    (d4let (all-args &body body) (-vlet 'df 4 all-args body))
    (f2let (all-args &body body) (-vlet 'ff 2 all-args body))
    (f3let (all-args &body body) (-vlet 'ff 3 all-args body))
    (f4let (all-args &body body) (-vlet 'ff 4 all-args body))

    ; this is a bit hacky. but we need it for easy generaliziation in
    ; array-reduce
    (drep (expr) `( ,@expr)) (drep* (expr) `( ,@expr))
    (frep (expr) `( ,@expr)) (frep* (expr) `( ,@expr))

    (d2rep (expr) `(values ,@(loop repeat 2 collect expr of-type df)))
    (d2rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (df ,e)) (values ,e ,e))))
    (f2rep (expr) `(values ,@(loop repeat 2 collect expr of-type ff)))
    (f2rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (ff ,e)) (values ,e ,e))))

    (d3rep (expr) `(values ,@(loop repeat 3 collect expr of-type df)))
    (d3rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (df ,e)) (values ,e ,e ,e))))
    (f3rep (expr) `(values ,@(loop repeat 3 collect expr of-type ff)))
    (f3rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (ff ,e)) (values ,e ,e ,e))))

    (d4rep (expr) `(values ,@(loop repeat 4 collect expr of-type df)))
    (d4rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (df ,e)) (values ,e ,e ,e ,e))))
    (f4rep (expr) `(values ,@(loop repeat 4 collect expr of-type ff)))
    (f4rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (ff ,e)) (values ,e ,e ,e ,e))))

    (fvset ((&rest symbs) &rest expr) `(-vset 1 ,symbs (progn ,@expr)))
    (dvset ((&rest symbs) &rest expr) `(-vset 1 ,symbs (progn ,@expr)))

    (f2vset ((&rest symbs) &rest expr) `(-vset 2 ,symbs (progn ,@expr)))
    (d2vset ((&rest symbs) &rest expr) `(-vset 2 ,symbs (progn ,@expr)))

    (f3vset ((&rest symbs) &rest expr) `(-vset 3 ,symbs (progn ,@expr)))
    (d3vset ((&rest symbs) &rest expr) `(-vset 3 ,symbs (progn ,@expr)))

    (f4vset ((&rest symbs) &rest expr) `(-vset 4 ,symbs (progn ,@expr)))
    (d4vset ((&rest symbs) &rest expr) `(-vset 4 ,symbs (progn ,@expr)))

    (d$ (a &rest inds) (-ind-to-val 'df 1 a inds))
    (d2$ (a &rest inds) (-ind-to-val 'df 2 a inds))
    (d3$ (a &rest inds) (-ind-to-val 'df 3 a inds))
    (d4$ (a &rest inds) (-ind-to-val 'df 4 a inds))
    (f$ (a &rest inds) (-ind-to-val 'ff 1 a inds))
    (f2$ (a &rest inds) (-ind-to-val 'ff 2 a inds))
    (f3$ (a &rest inds) (-ind-to-val 'ff 3 a inds))
    (f4$ (a &rest inds) (-ind-to-val 'ff 4 a inds))

    ($vset ((a i) &rest expr) `(-vaset (,a 1 ,i) ,@expr))
    (2$vset ((a i) &rest expr) `(-vaset (,a 2 ,i) ,@expr))
    (3$vset ((a i) &rest expr) `(-vaset (,a 3 ,i) ,@expr))
    (4$vset ((a i) &rest expr) `(-vaset (,a 4 ,i) ,@expr))

    (f$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 1 :type 'ff))
    (f2$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 2 :type 'ff))
    (f3$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 3 :type 'ff))
    (f4$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 4 :type 'ff))

    (d$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 1 :type 'df))
    (d2$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 2 :type 'df))
    (d3$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 3 :type 'df))
    (d4$fxlspace ((n a b &key (end t)) &body body)
      (-fxlspace n a b body :end end :dim 4 :type 'df))

    (f$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 1 :type 'ff))
    (f2$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 2 :type 'ff))
    (f3$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 3 :type 'ff))
    (f4$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 4 :type 'ff))

    (d$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 1 :type 'df))
    (d2$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 2 :type 'df))
    (d3$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 3 :type 'df))
    (d4$with-rows ((n &rest arrs) &body expr) (-with-rows n arrs expr :dim 4 :type 'df))

    (dwith-arrays ((&key (n 0) (inds nil inds?) (start 0) itr cnt arr
                         fxs exs nxs)
                    &body body)
      `(-with-arrays (:type 'df :n ,n ,@(when inds? `(:inds ,inds))
                      :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs :nxs ,nxs
                      :exs ,exs :start ,start)
                     ,@body))
    (fwith-arrays ((&key (n 0) (inds nil inds?) (start 0) itr cnt arr
                         fxs exs nxs)
                   &body body)
      `(-with-arrays (:type 'ff :n ,n ,@(when inds? `(:inds ,inds))
                      :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs :nxs ,nxs
                      :exs ,exs :start ,start)
                     ,@body))))

; define vlet/vprogn with current symbols-map
(define-env-macros *symbols-map*)

