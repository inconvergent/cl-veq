(in-package :veq)

(deftype df () 'double-float) (deftype ff () 'single-float)
(deftype in () 'fixnum)
(deftype pn (&optional (bits 31)) `(unsigned-byte ,bits))
(deftype kv () 'keyword) (deftype sy () 'symbol)
(deftype ll () 'list)
(deftype pos-df () `(double-float 0d0 *))
(deftype pos-ff () `(single-float 0f0 *))

(deftype dvec () `(simple-array df)) (deftype fvec () `(simple-array ff))
(deftype ivec () `(simple-array in)) (deftype pvec () `(simple-array pn))
(deftype kvec () `(simple-array kv)) (deftype svec () `(simple-array sy))
(deftype lvec () `(simple-array ll))

(declaim (inline df ff in pn ll kv sy))
(defun df (v) (coerce v 'df))
(defun ff (v) (coerce v 'ff))
(defun in (v) (coerce v 'in))
(defun pn (v) (coerce v 'pn))
(defun ll (v) (coerce v 'll))
(defun kv (v) (values (intern (string-upcase (mkstr v)) :keyword)))
(defun sy (v) (values (intern (string-upcase (mkstr v)) :keyword)))

(defmacro df* (&body body) `(values ,@(mapcar (lambda (v) `(df ,v)) body)))
(defmacro ff* (&body body) `(values ,@(mapcar (lambda (v) `(ff ,v)) body)))
(defmacro in* (&body body) `(values ,@(mapcar (lambda (v) `(in ,v)) body)))
(defmacro kv* (&body body) `(values ,@(mapcar (lambda (v) `(kv ,v)) body)))
(defmacro ll* (&body body) `(values ,@(mapcar (lambda (v) `(ll ,v)) body)))
(defmacro pn* (&body body) `(values ,@(mapcar (lambda (v) `(pn ,v)) body)))
(defmacro sy* (&body body) `(values ,@(mapcar (lambda (v) `(sy ,v)) body)))

(defun type-from-short (ty &optional (missing :nil))
  "select type fom type hint."
  (case (kv ty) (:df (values 'df 'd)) (:d (values 'df 'd))
                (:ff (values 'ff 'f)) (:f (values 'ff 'f))
                (:in (values 'in 'i)) (:i (values 'in 'i))
                (:pn (values 'pn 'p)) (:p (values 'pn 'p))
                (:sy (values 'symbol 's)) (:s (values 'symbol 's))
                (:kv (values 'keyword 's)) (:k (values 'keyword 'k))
                (:ll (values 'list 'l)) (:l (values 'list 'l))
                (:nil (values missing missing))))

(defun type-default (ty &optional (missing :nil))
  "default value for array with elements of type (hint) ty.
eg: 0 0f0 0d0 nil :val"
  (case (kv ty) (:df 0d0) (:d 0d0) (:ff 0f0) (:f 0f0)
                (:in 0) (:i 0) (:pn 0) (:p 0)
                (:ll (list)) (:l (list))
                (:sy 'nil) (:s 'nil) (:kv :nil) (:k :nil)
                (:nil missing) (t missing)))

(defun arrtype (ty &optional (missing :nil))
  "select array type from type hint.
eg: :ff :df 'f 'i"
  (case (kv ty) (:df 'dvec) (:d 'dvec) (:ff 'fvec) (:f 'fvec)
                (:in 'ivec) (:i 'ivec) (:pn 'pvec) (:p 'pvec)
                (:sy 'svec) (:s 'svec) (:kv 'kvec) (:k 'kvec)
                (:ll 'lvec) (:l 'lvec)
                (:nil missing) (t missing)))

(declaim (inline ffl dfl))
(defun ffl (l)
  (declare (list l))
  "return (values (ff a) (ff b) ..) from (list a b ..)."
  (apply #'values (mapcar (lambda (v) (ff v)) l)))
(defun dfl (l)
  (declare (list l))
  "return (values (df a) (df b ..) from (list a b ..)."
  (apply #'values (mapcar (lambda (v) (df v)) l)))


(defun vvsym (type dim symb
              &key pref (sep "")
                   (pkg (etypecase symb (keyword "VEQ")
                                        (symbol (symbol-package symb))
                                        (string "VEQ"))))
  (declare #.*opt* (symbol type))
  "build a symbol with correct name convention.
eg: (vvsym ff 2 :lerp) yields f2lerp."
  (let ((elem (list (case (kv type)
                          (:df :d) (:ff :f) (:in :i) (:pn :p) (:kv :k)
                          (otherwise ""))
                    (if (and dim (> (the pn dim) 1)) dim "")
                    sep
                    symb)))
    (when pref (setf elem (cons pref elem)))
    (values (psymb pkg (string-upcase (apply #'mkstr elem))))))

(defun unpack-vvsym (sym &key (s :!) (niltype :nil) (symout t))
  (declare #.*opt* (symbol sym) ((or string keyword character) s))
  "split names of type f34!var into (values :f var 3 4)"
  (labels ((find-type (p) (if p (kv (car p)) niltype))
           (find-dim (p) (if p (digit-char-p (car p)) 1)))
    (dsb (pref vname) (nilpad 2 (split-substr (mkstr s) (mkstr sym)))
      (unless vname (error "UNPACK-VVSYM missing tail: ~a" sym))
      (mvb (pref-digits pref-chars)
        (if pref (fx-split-str #'digit-char-p (mkstr pref)) (values nil nil))
        (values (the keyword (find-type pref-chars))
                (if symout (psymb (symbol-package sym) (string-upcase vname))
                           (string-upcase vname))
                (the fixnum (find-dim pref-digits)) ; dim
                (the fixnum (find-dim (reverse pref-digits)))))))) ; dimout

