
(in-package :veq)


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
    `(macrolet ,symbols-map (progn ,@(replace-varg body))))

  (defmacro vdef (fname &body body)
    `(macrolet ,symbols-map (defun ,fname ,@(replace-varg body))))

  (defmacro ivdef (mname &body body)
    `(progn (declaim (inline ,mname))
            (vdef ,mname ,@body)))

  (defmacro vdef* (mname &body body)
    (let ((fname (symb "%" mname)))
      `(progn (macrolet ,symbols-map
                (defun ,fname ,@(replace-varg ; replace internal references to mname
                                  (subst fname mname body))))
              (defmacro ,mname (&rest rest)
                `(mvc #',',fname ,@rest)))))

  (defmacro ivdef* (mname &body body)
    `(progn (declaim (inline ,(symb "%" mname)))
            (vdef* ,mname ,@body)))))


;;;;;;;;;;;;;;;;; VARIOUS

; using this makes things much slower because of the consing or because the
; number of values is unknown?. avoid when possible.
; TODO: find a different solution.
(defmacro lst (&body body) `(mvc #'list ,@body))


;;;;;;;;;;;;;;;;;; MACRO UTILS

(defun replace-varg (body &optional (root-rmap (list)))
  ; TODO: handle dotted pairs
  (labels
    ((is-varg (v) (and (listp v) (eq (car v) 'varg)))
     (is-vref (root) (and (listp root)
                          (listp (car root))
                          (eq (caar root) 'vref)))
     (get-car-rmap (root rmap)
       (if (listp root)
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
  (let ((sme (find-if (lambda (a) (not (= (length a) 2))) all-args)))
    (when sme (error "vlet error. expected (var expr)~% got: ~a" sme)))
  (loop for (arg expr) in (reverse all-args)
        do (setf body (list (-vmvb type dim arg expr body :repvarg nil)))
        finally (return (replace-varg `(progn ,@body)))))

(defun -vlet* (type all-args body)
  (when (find-if (lambda (a) (not (> 4 (length a) 1))) all-args)
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
  `((v< (&body body) `(mvc #'values ,@body))
    (d< (&body body) `(mvc #'values ,@(ddim? body 1)))
    (f< (&body body) `(mvc #'values ,@(fdim? body 1)))
    (d2< (&body body) `(mvc #'values ,@(ddim? body 2)))
    (f2< (&body body) `(mvc #'values ,@(fdim? body 2)))
    (d3< (&body body) `(mvc #'values ,@(ddim? body 3)))
    (f3< (&body body) `(mvc #'values ,@(fdim? body 3)))

    (d<* (&body body) `(mvc #'values (df* ,@(dim? body 1))))
    (f<* (&body body) `(mvc #'values (ff* ,@(dim? body 1))))
    (d2<* (&body body) `(mvc #'values (df* ,@(dim? body 2))))
    (f2<* (&body body) `(mvc #'values (ff* ,@(dim? body 2))))
    (d3<* (&body body) `(mvc #'values (df* ,@(dim? body 3))))
    (f3<* (&body body) `(mvc #'values (ff* ,@(dim? body 3))))

    (f$ (a &rest inds) (-ind-to-val 'ff 1 a inds))
    (d$ (a &rest inds) (-ind-to-val 'df 1 a inds))
    (f2$ (a &rest inds) (-ind-to-val 'ff 2 a inds))
    (d2$ (a &rest inds) (-ind-to-val 'df 2 a inds))
    (f3$ (a &rest inds) (-ind-to-val 'ff 3 a inds))
    (d3$ (a &rest inds) (-ind-to-val 'df 3 a inds))

    (d2mvb (arg expr &body body) (-vmvb 'df 2 arg expr body))
    (d3mvb (arg expr &body body) (-vmvb 'df 3 arg expr body))
    (f2mvb (arg expr &body body) (-vmvb 'ff 2 arg expr body))
    (f3mvb (arg expr &body body) (-vmvb 'ff 3 arg expr body))

    (dvlet (all-args &body body) (-vlet* 'df all-args body))
    (fvlet (all-args &body body) (-vlet* 'ff all-args body))

    (d2let (all-args &body body) (-vlet 'df 2 all-args body))
    (f2let (all-args &body body) (-vlet 'ff 2 all-args body))
    (f3let (all-args &body body) (-vlet 'ff 3 all-args body))
    (d3let (all-args &body body) (-vlet 'df 3 all-args body))

    (d2rep (expr) `(values ,@(loop repeat 2 collect expr of-type df)))
    (d2rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (df ,e)) (values ,e ,e))))
    (f2rep (expr) `(values ,@(loop repeat 2 collect expr of-type ff)))
    (f2rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (ff ,e)) (values ,e ,e))))

    (d3rep (expr) `(values ,@(loop repeat 3 collect expr of-type df)))
    (d3rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (df ,e)) (values ,e ,e ,e))))
    (f3rep (expr) `(values ,@(loop repeat 3 collect expr of-type ff)))
    (f3rep* (expr) (awg (e) `(let ((,e ,expr)) (declare (ff ,e)) (values ,e ,e ,e))))

    ; linear interpolation
    (flspace ((n a b &key (end t)) &body body) (fxlspace n a b body :end end :dim 1 :type 'ff))
    (dlspace ((n a b &key (end t)) &body body) (fxlspace n a b body :end end :dim 1 :type 'df))
    (f2lspace ((n a b &key (end t)) &body body) (fxlspace n a b body :end end :dim 2 :type 'ff))
    (d2lspace ((n a b &key (end t)) &body body) (fxlspace n a b body :end end :dim 2 :type 'df))
    (f3lspace ((n a b &key (end t)) &body body) (fxlspace n a b body :end end :dim 3 :type 'ff))
    (d3lspace ((n a b &key (end t)) &body body) (fxlspace n a b body :end end :dim 3 :type 'df))

    (d$mima (n a) (minmax a n :type 'df))
    (f$mima (n a) (minmax a n :type 'ff))
    (d$mima* (inds a) (minmax* a inds :type 'df))
    (f$mima* (inds a) (minmax* a inds :type 'ff))

    (d2$mima (n a) (2minmax a n :type 'df))
    (f2$mima (n a) (2minmax a n :type 'ff))
    (d2$mima* (inds a) (2minmax* a inds :type 'df))
    (f2$mima* (inds a) (2minmax* a inds :type 'ff))

    (d3$mima (n a) (3minmax a n :type 'df))
    (f3$mima (n a) (3minmax a n :type 'ff))
    (d3$mima* (inds a) (3minmax* a inds :type 'df))
    (f3$mima* (inds a) (3minmax* a inds :type 'ff))

    ; TODO: typed
    ; vset for setting multi symbols
    (fvset (s &rest expr) `(vset 1 ,s ,@expr))
    (dvset (s &rest expr) `(vset 1 ,s ,@expr))

    (f2vset (s &rest expr) `(vset 2 ,s ,@expr))
    (d2vset (s &rest expr) `(vset 2 ,s ,@expr))

    (f3vset (s &rest expr) `(vset 3 ,s ,@expr))
    (d3vset (s &rest expr) `(vset 3 ,s ,@expr))

    (vaset ((a i) &rest expr) `(-vaset (,a 1 ,i) ,@expr))
    (2vaset ((a i) &rest expr) `(-vaset (,a 2 ,i) ,@expr))
    (3vaset ((a i) &rest expr) `(-vaset (,a 3 ,i) ,@expr))

    (with-rows ((n &rest arrs) &body expr) (with-rows n arrs expr :dim 1))
    (2with-rows ((n &rest arrs) &body expr) (with-rows n arrs expr :dim 2))
    (3with-rows ((n &rest arrs) &body expr) (with-rows n arrs expr :dim 3))

    (dwith-arrays ((&key (n 0) (inds nil inds?) (start 0)
                         itr cnt arr fxs exs)
                    &body body)
      `(-with-arrays
         (:type 'df :n ,n ,@(when inds? `(:inds ,inds))
                :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs
                :exs ,exs :start ,start)
         ,@body))
    (fwith-arrays ((&key (n 0) (inds nil inds?) (start 0)
                         itr cnt arr fxs exs)
                   &body body)
      `(-with-arrays
         (:type 'ff :n ,n ,@(when inds? `(:inds ,inds))
                :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs
                :exs ,exs :start ,start)
         ,@body))))


; define vlet/vprogn with current symbols-map
(define-env-macros *symbols-map*)

