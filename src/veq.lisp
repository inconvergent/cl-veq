
(in-package :veq)

(defparameter *errmsg* "~%-------------~% error in ~a: ~a ~%~%")

(declaim (list *symbols-map*))
(defvar *symbols-map* '())

(defmacro veq? ()
  "list all macrolets in veq context"
  `(list ,@(sort (mapcar (lambda (s) (mkstr (car s)))
                         *symbols-map*)
                 #'string-lessp)))

(defun map-symbol (pair)
  (declare #.*opt* (list pair))
  "add pair macrolet pair. see macro.lisp"
  (export (the symbol (car pair)))
  (setf *symbols-map*
        (remove-if (lambda (cand) (eq (car cand) (car pair))) *symbols-map*))
  (push pair *symbols-map*))


(defun veqsymb (dim type symb &key pref)
  (declare #.*opt* (pos-int dim) (symbol type))
  "builld a symbol with correct name convention.
  eg: lerp -> f2lerp"
  (let ((elem (list (cdr (assoc type `((df . "D") (ff . "F")
                                       (in . "I") (nil . ""))))
                    (if (> dim 1) dim "")
                    (mkstr symb))))
    (when pref (setf elem (cons pref elem)))
    (values (intern (string-upcase (apply #'mkstr elem)) "VEQ"))))

(defun arrtype (type)
  (declare #.*opt* (symbol type))
  "use type to select array type"
  (values (intern (string-upcase
                    (mkstr (cdr (assoc type `((df . "DVEC") (ff . "FVEC")
                                              (in . "IVEC") (nil . ""))))))
                  "VEQ")))

(defun optype (symb)
  (declare #.*opt*)
  "use first letter to select type d -> df, f -> ff"
  (cdr (assoc (char (string-upcase (mkstr symb)) 0)
              `((#\D . df) (#\F . ff) (#\I . in)))))

(defmacro op ((mname args) &body body)
  (declare (symbol mname) (list args))
  "build an op. see ops-1.lisp, ops-2.lisp, ..."
  (let* ((declares `(,(optype mname) ,@args))
         (fname (symb "-" mname)))
    `(progn (map-symbol `(,',mname (&body body)
                            `(mvc #',',',fname ,@body)))
            (export ',mname)
            ,@(unless #.*dev* `((declaim (inline ,fname))))
            (defun ,fname ,args (declare ,*opt* ,declares)
                                (progn ,@body)))))

(defun type-placeholder (root type)
  (labels ((repl (symb type)
            (intern (substitute type #\@
                      (string-upcase (mkstr symb))))))

    (cond ((numberp root) (coerce root (optype type)))
          ((symbolp root) (repl root type))
          ((atom root) root)
          (t (cons (type-placeholder (car root) type)
                   (type-placeholder (cdr root) type))))))

(defmacro ops (&body body)
  "build ops in ops-1.lisp, ops-2.lisp, ..."
  `(progn ,@(loop for (args body*) in (group (type-placeholder body #\D) 2)
                  collect `(op ,args ,body*))
          ,@(loop for (args body*) in (group (type-placeholder body #\F) 2)
                  collect `(op ,args ,body*))))

