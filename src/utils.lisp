
(in-package :veq)

(deftype df () `double-float)
(deftype dvec () `(simple-array df))
(deftype ff () `single-float)
(deftype fvec () `(simple-array ff))
(deftype in () `fixnum)
(deftype ivec () `(simple-array in))
(deftype pos-df () `(double-float 0d0 *))
(deftype pos-ff () `(single-float 0f0 *))
(deftype pos-int (&optional (bits 31)) `(unsigned-byte ,bits))

(defmacro df (&body body) `(coerce ,@body 'df))
(defmacro ff (&body body) `(coerce ,@body 'ff))
(defmacro in (&body body) `(coerce ,@body 'in))
(defmacro df* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'df)) body)))
(defmacro ff* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'ff)) body)))
(defmacro in* (&body body) `(values ,@(mapcar (lambda (v) `(coerce ,v 'in)) body)))

(defun ffl (l)
  (declare (list l))
  "return (values (ff a) (ff b) ..) from (list a b ..)"
  (apply #'values (mapcar (lambda (v) (ff v)) l)))
(defun dfl (l)
  (declare (list l))
  "return (values (df a) (df b ..) from (list a b ..)"
  (apply #'values (mapcar (lambda (v) (df v)) l)))
; (defun inl (l)
;   (declare #.*opt* (list l))
;   (apply #'values (mapcar (lambda (v) (in v)) l)))

(declaim (df dpi dpii dpi5))
(defconstant dpi #.(coerce pi 'df))
(defconstant dpii #.(coerce (* pi 2d0) 'df))
(defconstant dpi5 #.(coerce (* pi 0.5d0) 'df))

(declaim (ff fpi fpii fpi5))
(defconstant fpi #.(coerce pi 'ff))
(defconstant fpii #.(coerce (* pi 2f0) 'ff))
(defconstant fpi5 #.(coerce (* pi 0.5f0) 'ff))


(defun v? (&optional (silent t))
  "get version. use silent to surpress stdout"
  (let ((v (slot-value (asdf:find-system 'weird) 'asdf:version)))
    (unless silent (format t "~%veq version: ~a~%" v))
    v))
(defun d? (f) "describe argument" (describe f))
(defun i? (f) "inspect argument" (inspect f))



; from on lisp by pg
(defmacro mac (expr) `(pprint (macroexpand-1 ',expr)))
#+sbcl (defmacro mac* (expr) `(pprint (sb-cltl2:macroexpand-all ',expr)))

;from on lisp by pg
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

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
  (if (< n 1) (error "group error: group size is smaller than 1"))
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


(defmacro push* (v l)
  (declare (symbol l))
  "push v to list l, and return v"
  (awg (vv) `(let ((,vv ,v)) (push ,vv ,l) ,vv)))


(defun -gensyms (name n)
  (declare (symbol name) (fixnum n))
  (loop with name = (string name)
        repeat n
        for x across "XYZWUVPQR"
        collect (gensym (format nil "~a-~a-" name x))))


(declaim (inline lst>n))
(defun lst>n (l n)
  (declare (list l) (pos-int n))
  "list is longer than n?"
  (consp (nthcdr n l)))

(declaim (inline last*))
(defun last* (a) (first (last a)))


(defun dupes (lst)
  (declare (list lst))
  (cond ((null lst) '())
        ((member (car lst) (cdr lst) :test #'equal) (cons (car lst)
                                                      (dupes (cdr lst))))
        (t (dupes (cdr lst)))))


(defmacro vgrp-mvc ((dim fx) &body body)
  "do (multiple-value-call fx g) where g is groups
   of size dim over the (values ...) returned by body
   "
  (awg (gsfx rest x)
  `(veq:fvprogn
     (labels ((,gsfx (&rest ,rest)
       (apply #'values
         (awf
           (loop for ((:va  ,dim ,x)) in (group ,rest ,dim)
                 collect (veq:lst (veq:mvc ,fx ,x)))))))
       (mvc #',gsfx ,@body)))))

