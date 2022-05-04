
(in-package :veq)

(defparameter *errmsg* "~%-------------~% error in ~a: ~a ~%~%")

(declaim (list *symbols-map* *docstring-map*))
(defvar *symbols-map* (list))
(defvar *docstring-map* (list))


(defmacro context? ()
  "list all macrolets in veq context. that is ops available inside vprog,
  fvprogn, vdef, fvdef defined contexts/functions."
  (awg (s)
    `(list (sort (mapcar (lambda (,s) (mkstr (car ,s)))
                               ,*symbols-map*)
                 #'string-lessp))))

(defun select-docs (s)
  (let* ((docs (find-if (lambda (c) (equal s c))
                       *docstring-map*
                       :key #'car))
        (idocs (documentation s 'function))
        (desc (if (find :nodesc docs) nil
                  (with-output-to-string (*standard-output*)
                    (describe s)))))

    (cond (docs (format nil "```~%~a~@[~%~a~]```" (cadr docs) desc))
          (idocs (format nil "```~%~a~@[~%~a~]```" idocs desc))
          (t (format nil "```~%:missing:~%~@[~%~a~]```" desc)))))

(defmacro pckgs ()
  (awg (s)
    `(sort (loop for ,s being the external-symbols of (find-package :veq)
                 collect (list (mkstr ,s) ,s))
           #'string-lessp
           :key #'car)))

(defmacro ext-symbols? (&optional mode)
  "list all external symbols in veq. use :verbose to inlcude docstring.  use
  :pretty to print verbose output to stdout in a readable form."
  (awg (str s)
    (case mode
      (:pretty
        `(loop for (,str ,s) in (pckgs)
               do (format t "~&### ~a~%~%~a~&"
                            ,str (select-docs ,s))))
      (:pairs
        `(loop for (,str ,s) in (pckgs) collect (list ,str (select-docs ,s))))
      (otherwise
        `(loop for (,str ,s) in (pckgs) collect ,str)))))

(defun map-docstring (&rest pair)
  (setf *docstring-map*
      (remove-if (lambda (cand) (equal (car cand) (car pair)))
                 *docstring-map*))
  (push pair *docstring-map*))

(map-docstring 'vref
  (mkstr "use (veq:vref s x) or (:vr s x) to get dim x of symbol s"
         #\Newline
         "in fvdef*, vdef*, def*. see replace-varg for details"
         #\Newline)
  :nodesc)

(map-docstring 'varg
  (mkstr "use (veq:varg n a b ...) or (:vr n a b ...) to represent vectors a,b "
         #\Newline
         "of dim n in fvdef*, vdef*, def*. see replace-varg for details"
         #\Newline)
  :nodesc)


; TODO: clean up this mess
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

; ---------------- REGISTER BASIC OPS

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
                (mkstr mname) (mkstr fname)  (mkstr args ) (mkstr (car body)))))
    `(progn (map-symbol `(,',mname (&body body)
                            `(mvc #',',',fname ,@body)))
            (map-docstring ',mname ,docs :nodesc)
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

