
(in-package :veq)

(defparameter *errmsg* "~%-------------~% error in ~a: ~a ~%~%")

(declaim (list *symbols-map*))
(defvar *symbols-map* '())

; TODO
; - lerp end/nonend
; - broadcasts
; - reader macros?


(defun veqsymb (dim type fx &key pref)
  (declare (fixnum dim) (symbol type) (string fx))

  (let ((elem (list (cdr (assoc type `((veq:df . "D")
                                       (veq:ff . "F")
                                       (veq:in . "I"))))
                    (if (> dim 1) dim "")
                    (string fx))))
    (when pref (setf elem (cons pref elem)))
    (values (intern (apply #'mkstr elem) "VEQ"))))

(defun arrtype (type)
  (intern (mkstr (cdr (assoc type `((veq:df . "DVEC")
                                    (veq:ff . "FVEC")
                                    (veq:in . "IVEC")))))
          "VEQ"))

(defun typezero (type)
  (cdr (assoc type `((veq:df . 0d0) (veq:ff . 0f0) (veq:in . 0)))))

(defun optype (mname)
  (declare (symbol mname))
  (cdr (assoc (char (string mname) 0)
              `((#\D . veq:df) (#\F . veq:ff) (#\I . veq:in)))))


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
            (declaim (inline ,fname))
            (defun ,fname ,args (declare ,*opt* ,declares)
                                (progn ,@body)))))


(defmacro ops (&body body)
  `(progn ,@(loop for (args body*) in (group body 2)
                  collect `(op ,args ,body*))))

