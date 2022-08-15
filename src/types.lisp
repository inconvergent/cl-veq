
(in-package :veq)

(deftype df () `double-float)
(deftype ff () `single-float)
(deftype in () `fixnum)
(deftype pn (&optional (bits 31)) `(unsigned-byte ,bits))
(deftype pos-int (&optional (bits 31)) `(unsigned-byte ,bits)) ; TODO: rename pos-int->pn
(deftype pos-df () `(double-float 0d0 *))
(deftype pos-ff () `(single-float 0f0 *))

(deftype dvec () `(simple-array df))
(deftype fvec () `(simple-array ff))
(deftype ivec () `(simple-array in))
(deftype pvec () `(simple-array pn))

(defmacro df (&body body) `(coerce ,@body 'df))
(defmacro ff (&body body) `(coerce ,@body 'ff))
(defmacro in (&body body) `(coerce ,@body 'in))
(defmacro pn (&body body) `(coerce ,@body 'pn))
(defmacro df* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'df)) body)))
(defmacro ff* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'ff)) body)))
(defmacro in* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'in)) body)))
(defmacro pn* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'pn)) body)))

(defun ffl (l)
  (declare (list l))
  "return (values (ff a) (ff b) ..) from (list a b ..)."
  (apply #'values (mapcar (lambda (v) (ff v)) l)))
(defun dfl (l)
  (declare (list l))
  "return (values (df a) (df b ..) from (list a b ..)."
  (apply #'values (mapcar (lambda (v) (df v)) l)))
; (defun inl (l)
;   (declare #.*opt* (list l))
;   (apply #'values (mapcar (lambda (v) (in v)) l)))

(declaim (df dpi dpii dpi5) (ff fpi fpii fpi5))
(defconstant dpi #.(df pi))
(defconstant dpi5 #.(df (* pi 0.5d0)))
(defconstant dpii #.(df (* pi 2d0)))
(defconstant fpi #.(ff pi))
(defconstant fpi5 #.(ff (* pi 0.5f0)))
(defconstant fpii #.(ff (* pi 2f0)))

