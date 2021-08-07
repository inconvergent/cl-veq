
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

  (defmacro vdef (fname &body body)
    `(macrolet ,symbols-map (defun ,fname ,@(replace-varg body))))

  (defmacro vprogn (&body body)
    `(macrolet ,symbols-map (progn ,@(replace-varg body))))))


;;;;;;;;;;;;;;;;; VARIOUS

; using this makes thing much slower because of the consing.
; avoid when possible
; TODO: find a different solution.
(defmacro lst (&body body)
  " (values ...) to (list ...) "
  `(mvc #'list ,@body))


;;;;;;;;;;;;;;;;;; MACRO UTILS


(defun replace-varg (body &optional (root-rmap (list)))
  (labels
    ((car-vargp (root rmap)
       (if (listp root)
         (loop with res = (list)
               for v in (reverse root)
               if (and (listp v) (eq (car v) 'varg))
               do (dsb (n &rest rest) (cdr v)
                    (loop for name in rest
                          collect (let ((syms (-gensyms name n)))
                                    (push `(,name . ,syms) rmap)
                                    syms) into new
                          finally (setf res `(,@(awf new) ,@res))))
               else do (push v res)
               finally (return (values res rmap)))
         (values root rmap)))
     (-vref (root rmap)
            (dsb (ref i) (cdr root)
              ; replace-varg can encounter vrefs that have no match in rmap (eg
              ; in -vmvb). so we need to ignore missing matches for vref.
              ; these vrefs will be replaced at a later time (eg. in the call
              ; to replace-varg in -vlet)
              (aif (assoc ref rmap) (nth i (cdr it)) root)))
     (-walk (root rmap)
       (cond ((atom root) root)
             ; TODO: handle dotted pairs

             ((and (listp root) (eq (car root) 'vref))
               (-vref root rmap))

             ((and (listp root) (assoc (car root) rmap))
               `(,@(cdr (assoc (car root) rmap))
                  ,@(-walk (cdr root) rmap)))

             (t (mvb (car* rmap) (car-vargp (car root) rmap)
                  (cons (-walk car* rmap)
                        (-walk (cdr root) rmap)))))))
    (-walk body root-rmap)))

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
  (let ((sme (find-if (lambda (a) (not (= (length a) 3))) all-args)))
    (when sme (error "vlet* error. expected (var dim expr)~% got: ~a" sme)))
  (loop for (arg dim expr) in (reverse all-args)
        do (setf body (list (-vmvb type dim arg expr body :repvarg nil)))
        finally (return (replace-varg `(progn ,@body)))))

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

    (f$ (a &rest rest) (-ind-to-val 'ff 1 a rest))
    (d$ (a &rest rest) (-ind-to-val 'df 1 a rest))
    (f2$ (a &rest rest) (-ind-to-val 'ff 2 a rest))
    (d2$ (a &rest rest) (-ind-to-val 'df 2 a rest))
    (f3$ (a &rest rest) (-ind-to-val 'ff 3 a rest))
    (d3$ (a &rest rest) (-ind-to-val 'df 3 a rest))

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

    (dmima (n a) (minmax a n :type 'df))
    (fmima (n a) (minmax a n :type 'ff))
    (dmima* (inds a) (minmax* a inds :type 'df))
    (fmima* (inds a) (minmax* a inds :type 'ff))

    (d2mima (n a) (2minmax a n :type 'df))
    (f2mima (n a) (2minmax a n :type 'ff))
    (d2mima* (inds a) (2minmax* a inds :type 'df))
    (f2mima* (inds a) (2minmax* a inds :type 'ff))

    (d3mima (n a) (3minmax a n :type 'df))
    (f3mima (n a) (3minmax a n :type 'ff))
    (d3mima* (inds a) (3minmax* a inds :type 'df))
    (f3mima* (inds a) (3minmax* a inds :type 'ff))

    (for-all-rows ((n &rest arr) &body body) (for-all-rows n arr body :dim 1))
    (2for-all-rows ((n &rest arr) &body body) (for-all-rows n arr body :dim 2))
    (3for-all-rows ((n &rest arr) &body body) (for-all-rows n arr body :dim 3))

    ; TODO: typed
    ; vset for setting multi symbols
    (fvset (a &rest expr) `(vset 1 ,a ,@expr))
    (dvset (a &rest expr) `(vset 1 ,a ,@expr))

    (f2vset (a &rest expr) `(vset 2 ,a ,@expr))
    (d2vset (a &rest expr) `(vset 2 ,a ,@expr))

    (f3vset (a &rest expr) `(vset 3 ,a ,@expr))
    (d3vset (a &rest expr) `(vset 3 ,a ,@expr))

    (vaset ((arr i) &rest expr) `(-vaset (,arr 1 ,i) ,@expr))
    (2vaset ((arr i) &rest expr) `(-vaset (,arr 2 ,i) ,@expr))
    (3vaset ((arr i) &rest expr) `(-vaset (,arr 3 ,i) ,@expr))

    (dwith-arrays ((&key (n 0) (inds nil inds?) itr cnt arr fxs exs) &body body)
      `(-with-arrays (:type 'df :n ,n ,@(when inds? `(:inds ,inds))
                            :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs
                            :exs ,exs)
                     ,@body))
    (fwith-arrays ((&key (n 0) (inds nil inds?) itr cnt arr fxs exs) &body body)
      `(-with-arrays (:type 'ff :n ,n ,@(when inds? `(:inds ,inds))
                            :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs
                            :exs ,exs)
                     ,@body))))


; define vlet/vprogn with current symbols-map
(define-env-macros *symbols-map*)

