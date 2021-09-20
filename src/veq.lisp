
(in-package :veq)

(defparameter *errmsg* "~%-------------~% error in ~a: ~a ~%~%")

(declaim (list *symbols-map*))
(defvar *symbols-map* '())

(defun veqsymb (dim type fx &key pref)
  (declare (pos-int dim) (symbol type))
  (let ((elem (list (cdr (assoc type `((df . "D") (ff . "F") (in . "I"))))
                    (if (> dim 1) dim "")
                    (string fx))))
    (when pref (setf elem (cons pref elem)))
    (values (intern (apply #'mkstr elem) "VEQ"))))

(defun arrtype (type)
  (declare (symbol type))
  (values (intern (mkstr (cdr (assoc type `((df . "DVEC")
                                            (ff . "FVEC")
                                            (in . "IVEC")))))
                  "VEQ")))

(defun optype (mname)
  (declare (symbol mname))
  (cdr (assoc (char (string mname) 0) `((#\D . df) (#\F . ff) (#\I . in)))))

(defun map-symbol (pair)
  (declare (list pair))
  (export (car pair))
  (push pair *symbols-map*))

(defmacro op ((mname args) &body body)
  (declare (symbol mname) (list args))
  (let* ((declares `(,(optype mname) ,@args))
         (fname (symb "-" mname)))
    `(progn (map-symbol `(,',mname (&body body) `(mvc #',',',fname ,@body)))
            (export ',mname)
            ,@(unless #.*dev* `((declaim (inline ,fname))))
            (defun ,fname ,args (declare ,*opt* ,declares)
                                (progn ,@body)))))

(defmacro ops (&body body)
  `(progn ,@(loop for (args body*) in (group body 2)
                  collect `(op ,args ,body*))))

