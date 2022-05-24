
(in-package :veq)

(defparameter *errmsg* "~%-------------~% error in ~a: ~a ~%~%")
(declaim (list *symbols-map* *docstring-map*))
(defvar *symbols-map* (list))


(defun map-symbol (pair)
  (declare #.*opt* (list pair))
  "add pair macrolet pair. see macro.lisp"
  (export (the symbol (car pair)))
  (setf *symbols-map*
        (remove-if (lambda (cand) (eq (car cand) (car pair))) *symbols-map*))
  (push pair *symbols-map*))

(defun optype (symb)
  (declare #.*opt*)
  "use first letter to select type d -> df, f -> ff"
  (cdr (assoc (char (string-upcase (mkstr symb)) 0)
              `((#\D . df) (#\F . ff) (#\I . in)))))

(defmacro op ((mname args) &body body)
  (declare (symbol mname) (list args))
  "build an op. see ops-1.lisp, ops-2.lisp, ..."
  (let* ((declares `(,(optype mname) ,@args))
         (fname (symb "-" mname))
         (docs (format nil "veq context op: ~a~%fxname: ~a~%args: ~a~%body: ~a~%"
                 mname fname args (car body))))
    `(progn (map-symbol `(,',mname (&body body)
                            `(mvc #',',',fname ,@body)))
            (map-docstring ',mname ,docs :nodesc :context)
            (export ',mname)
            ,@(unless #.*dev* `((declaim (inline ,fname))))
            (defun ,fname ,args (declare ,*opt* ,declares)
              ,docs
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
  "used to build ops in ops-1.lisp, ops-2.lisp, ..."
  `(progn ,@(loop for (args body*) in (group (type-placeholder body #\D) 2)
                  collect `(op ,args ,body*))
          ,@(loop for (args body*) in (group (type-placeholder body #\F) 2)
                  collect `(op ,args ,body*))))

