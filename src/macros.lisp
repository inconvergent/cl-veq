
(in-package :veq)


;;;;;;;;;;;;;;;;;; INIT ARRAY OF VEC

(defmacro f$ (&key (dim 1) (n 1) (v 0f0))
  " create array with size (n dim), and initial value v"
  `(values (make-array (the pos-int (* ,dim ,n))
             :initial-element ,v
             :element-type 'ff
             :adjustable nil)
           ,dim ,n))

(defmacro d$ (&key (dim 1) (n 1) (v 0d0))
  " create array with size (n dim), and initial value v"
  `(values (make-array (the pos-int (* ,dim ,n))
             :initial-element ,v
             :element-type 'df
             :adjustable nil)
           ,dim ,n))


(defmacro f$_ (&body body)
  " create array from body. use either

  ($_ (loop repeat 2 collect `(1d0 2d0)))
  or
  ($_ '((1d0 2d0) (1d0 2d0)))
  "
  (awg (body* n dim)
    `(let* ((,body* ,@body)
            (,n (length ,body*))
            (,dim (length (car ,body*))))
       (declare (pos-int ,n ,dim) (list ,body*))
       (values (make-array (* ,n ,dim) :initial-contents (awf ,body*)
                                       :element-type 'ff
                                       :adjustable nil)
               ,dim ,n))))

(defmacro d$_ (&body body)
  " create array from body. use either

  ($_ (loop repeat 2 collect `(1d0 2d0)))
  or
  ($_ '((1d0 2d0) (1d0 2d0)))
  "
  (awg (body* n dim)
    `(let* ((,body* ,@body)
            (,n (length ,body*))
            (,dim (length (car ,body*))))
       (declare (pos-int ,n ,dim) (list ,body*))
       (values (make-array (* ,n ,dim) :initial-contents (awf ,body*)
                                       :element-type 'df
                                       :adjustable nil)
               ,dim ,n))))

(defmacro f_ (&body body)
  "
  corresponds to ($_ '(body)), that is a single row of (length body).
  "

  (awg (body* dim)
    `(let* ((,body* ,@body)   ; TODO: mvc list?
            (,dim (length ,body*)))
       (declare (pos-int ,dim) (list ,body*))
       (values (make-array ,dim :initial-contents (awf ,body*)
                                       :element-type 'ff
                                       :adjustable nil)
               ,dim 1))))

(defmacro d_ (&body body)
  "
  corresponds to ($_ '(body)), that is a single row of (length body).
  "

  (awg (body* dim)
    `(let* ((,body* ,@body)   ; TODO: mvc list?
            (,dim (length ,body*)))
       (declare (pos-int ,dim) (list ,body*))
       (values (make-array ,dim :initial-contents (awf ,body*)
                                       :element-type 'df
                                       :adjustable nil)
               ,dim 1))))


; using this makes thing much slower because of the consing.
; avoid when possible
; TODO: find a different solution.
(defmacro lst (&body body)
  " (values ...) to (list ...) "
  `(mvc #'list ,@body))

;;;;;;;;;;;;;;;;;; MACRO UTILS


(defun replace-varg (body)
  (labels
    ((-syms (n name) (-gensyms name n))
     (car-vargp (root rmap)
       (if (listp root)
         (loop with res = (list)
               for v in (reverse root)
               if (and (listp v) (eq (car v) 'varg))
               do (dsb (n &rest rest) (cdr v)
                    (loop for name in rest
                          collect (let ((syms (-syms n name)))
                                    (push `(,name . ,syms) rmap)
                                    syms) into new
                          finally (setf res `(,@(awf new)
                                               ,@res))))
               else do (push v res)
               finally (return (values res rmap)))
         (values root rmap)))
     (-vref (root rmap) (dsb (ref i) (cdr root)
                             (let ((as (assoc ref rmap)))
                               (if as (nth i (cdr as))
                                      (error "vref error: no match for: ~a~%" root)))))
     (-walk (root &optional (rmap (list)))
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
    (-walk body)))

;;;;;;;;;;;;;;;;;; VMVB

(defun -vmvb (type dim arg expr body)
  (declare (symbol arg) (list expr) (fixnum dim))
  (replace-varg `(mvb ((varg ,dim ,arg)) ,expr
                  (declare (,type ,arg))
                  ,@body)))


;;;;;;;;;;;;;;;;;; VLET

(defun -vlet (type dim all-args body)
  (let ((sme (find-if (lambda (a) (not (= (length a) 2))) all-args)))
    (when sme (error "vlet error. expected (var expr)~% got: ~a"
                     sme)))
  (loop for (arg expr) in (reverse all-args)
        do (setf body (list (-vmvb type dim arg expr body)))
        finally (return `(progn ,@body))))

(defun -vlet* (type all-args body)
  (let ((sme (find-if (lambda (a) (not (= (length a) 3))) all-args)))
    (when sme (error "vlet* error. expected (var dim expr)~% got: ~a"
                     sme)))
  (loop for (arg dim expr) in (reverse all-args)
        do (setf body (list (-vmvb type dim arg expr body)))
        finally (return `(progn ,@body))))

(defun dim? (a dim &key type)
  (unless (= (length a) dim)
    (error "incorrect dim. got: ~a expected: ~a. ~% expr: ~% ~a"
           (length a) dim a))
  (if type (mapcar (lambda (v) (if (numberp v) (coerce v type) v)) a)
           a))
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

    (d3> (&body body) `(,(if (lst>n body 1) '-d3>> '-d3>) ,@body))
    (f3> (&body body) `(,(if (lst>n body 1) '-f3>> '-f3>) ,@body))
    (d2> (&body body) `(,(if (lst>n body 1) '-d2>> '-d2>) ,@body))
    (f2> (&body body) `(,(if (lst>n body 1) '-f2>> '-f2>) ,@body))
    (d> (&body body) `(,(if (lst>n body 1) '-d1>> '-d1>) ,@body))
    (f> (&body body) `(,(if (lst>n body 1) '-f1>> '-f1>) ,@body))

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
    (d2rep* (expr) (awg (e) `(let ((,e ,expr))
                               (declare (df ,e)) (values ,e ,e))))
    (f2rep (expr) `(values ,@(loop repeat 2 collect expr of-type ff)))
    (f2rep* (expr) (awg (e) `(let ((,e ,expr))
                               (declare (ff ,e)) (values ,e ,e))))

    (d3rep (expr) `(values ,@(loop repeat 3 collect expr of-type df)))
    (d3rep* (expr) (awg (e) `(let ((,e ,expr))
                               (declare (df ,e)) (values ,e ,e ,e))))
    (f3rep (expr) `(values ,@(loop repeat 3 collect expr of-type ff)))
    (f3rep* (expr) (awg (e) `(let ((,e ,expr))
                               (declare (ff ,e)) (values ,e ,e ,e))))

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

(defmacro vdef (fname &body body)
  `(macrolet ,*symbols-map* (defun ,fname ,@(replace-varg body))))

(defmacro vprogn (&body body)
  `(macrolet ,*symbols-map* (progn ,@(replace-varg body))))

