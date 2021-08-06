
(in-package :veq)

; from: http://cl-cookbook.sourceforge.net/os.html
(defun vgetenv (name &optional default)
  #+CMU (let ((x (assoc name ext:*environment-list* :test #'string=)))
          (if x (cdr x) default))
  #-CMU (or #+Allegro (sys:getenv name)
            #+CLISP (ext:getenv name)
            #+ECL (si:getenv name)
            #+SBCL (sb-unix::posix-getenv name)
            #+LISPWORKS (lispworks:environment-variable name)
            default))


(let ((devmode (string-downcase (vgetenv "DEV" ""))))
  (if (> (length devmode) 0)
    (progn
      (defparameter *dev* t)
      (defparameter *opt* '(optimize (safety 2) (speed 1) debug (space 1)
                                     compilation-speed))
      (format t "~%!!!!! VEQ DEVMODE !!!!!~%~%"))
    (progn
      (defparameter *dev* nil)
      (defparameter *opt* '(optimize (safety 1) speed (debug 2) space)))))


(deftype df () `double-float)
(deftype dvec () `(simple-array df))
(deftype ff () `single-float)
(deftype fvec () `(simple-array ff))
(deftype in () `fixnum)
(deftype ivec () `(simple-array in))
(deftype pos-df () `(double-float 0d0 *))
(deftype pos-ff () `(single-float 0f0 *))
(deftype pos-int (&optional (bits 31)) `(unsigned-byte ,bits))

(declaim (ff *eps*))
(defvar *eps* (* 3f0 single-float-epsilon))

(defmacro df (&body body) `(coerce ,@body 'df))
(defmacro ff (&body body) `(coerce ,@body 'ff))
(defmacro in (&body body) `(coerce ,@body 'in))
(defmacro df* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'df)) body)))
(defmacro ff* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'ff)) body)))
(defmacro in* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'in)) body)))


(declaim (df dpi dpii dpi5))
(defconstant dpi #.(coerce pi 'df))
(defconstant dpii #.(coerce (* pi 2d0) 'df))
(defconstant dpi5 #.(coerce (* pi 0.5d0) 'df))

(declaim (ff fpi fpii fpi5))
(defconstant fpi #.(coerce pi 'ff))
(defconstant fpii #.(coerce (* pi 2f0) 'ff))
(defconstant fpi5 #.(coerce (* pi 0.5f0) 'ff))


; from on lisp by pg
(defmacro mac (expr) `(pprint (macroexpand-1 ',expr)))
#+sbcl (defmacro mac* (expr) `(pprint (sb-cltl2:macroexpand-all ',expr)))


; from on lisp by pg
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(abbrev mvc multiple-value-call)
(abbrev mvb multiple-value-bind)
(abbrev dsb destructuring-bind)
(abbrev awg alexandria:with-gensyms)
(abbrev awf alexandria:flatten)

; modified from on lisp by pg
(defun group (source n)
  (if (zerop n) (error "group: zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


; from on lisp by pg
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

; from on lisp by pg
(defun symb (&rest args) (values (intern (apply #'mkstr args))))

; from on lisp by pg
(defun reread (&rest args) (values (read-from-string (apply #'mkstr args))))


(defun -gensyms (name n)
  (declare (symbol name) (fixnum n))
  (loop with name = (string name)
        repeat n
        for x across "XYZWUVPQR"
        collect (gensym (format nil "~a~a-" name x))))


(declaim (inline lst>n))
(defun lst>n (l n)
  (declare (list l) (pos-int n))
  "list is longer than n?"
  (consp (nthcdr n l)))

(declaim (inline last*))
(defun last* (a) (first (last a)))


(defun dupes (lst)
  (cond ((null lst) '())
        ((member (car lst) (cdr lst) :test #'equal) (cons (car lst)
                                                      (dupes (cdr lst))))
        (t (dupes (cdr lst)))))

