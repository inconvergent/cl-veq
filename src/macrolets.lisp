
(in-package :veq)


(defmacro define-vprogn ()
  `(defmacro vprogn (&body body)
    "enable veq context inside this progn.
handles propagation and resolution of uses of (varg d var) and (vref var i).
also handles vv macro compiler triggers. see vv macro.

fvprogn is faster, but has some limitations."
    (let ((body* (replace-varg (proc-vv body))))
       `(macrolet ,',*symbols-map* ,@body*))))
(define-vprogn)

(defmacro define-fvprogn ()
  `(defmacro fvprogn (&body body)
    "enable veq context inside this progn.
handles propagation and resolution of uses of (varg d var) and (vref var i).
also handles vv macro compiler triggers. see vv macro.

works the same way as vprogn. but removes all macrolets that are not
directly referenced by a symbol in body. this is faster, but may fail in some
cases where body is complex. in the event of errors try vprogn instead."
    (let ((body* (replace-varg (proc-vv body))))
       `(macrolet ,(filter-macrolets *symbols-map* body*)
                  ,@body*))))
(define-fvprogn)


(defmacro define-vdef ()
  `(defmacro vdef (fname &body body)
    "define function with veq context enabled. see vprogn."
    `(vprogn (defun ,fname ,@body))))
(define-vdef)

(defmacro define-fvdef ()
  `(defmacro fvdef (fname &body body)
     "define function with veq context enabled. see fvprogn."
     `(fvprogn (defun ,fname ,@body))))
(define-fvdef)

(defun get-docs (body)
  "find docstring for body"
  (labels ((r (s) (typecase s (cons (equal (car s) 'declare)))))
    (let ((body (remove-if #'r (cdr body))))
      (typecase (first body) (string (first body)) (t "[none]")))))

; MNAME: my-fx; FNAME: %my-fx
(defun make-wrap-docs (context fname body &aux (docs (get-docs body))
                                                     (args (first body)))
  (format nil "WRAPS: ~a~%ARGS: ~a~%DOCSTRING: ~a
defined via veq:~a" fname args docs context))

(defmacro define-vdef* ()
  `(defmacro vdef* (mname &body body)
    "defines a function named: %mname
and a wrapper macro named: mname
veq context is enabled. uses vprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%mname ...)."
    (let ((fname (symb "%" mname)))
      `(vprogn ; replace internal references to mname
               (defun ,fname ,@(subst fname mname body))
               (defmacro ,mname (&rest rest)
                 ,(make-wrap-docs :vdef* fname body)
                 `(mvc #',',fname ,@rest))))))
(define-vdef*)

(defmacro define-fvdef* ()
  `(defmacro fvdef* (mname &body body)
    "defines a function named: %mname
and a wrapper macro named: mname
veq context is enabled. uses fvprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%mname ...)."
    (let ((fname (symb "%" mname)))
      `(fvprogn ; replace internal references to mname
                (defun ,fname ,@(subst fname mname body))
                (defmacro ,mname (&rest rest)
                  ,(make-wrap-docs :fvdef* fname body)
                  `(mvc #',',fname ,@rest))))))
(define-fvdef*)

(defmacro define-def* ()
  `(defmacro def* (mname &body body)
    "defines a function named: %mname
and a wrapper macro named: mname

the wrapper macro ensures every call to this function is done as
(mvc #'%mname ...)."
    (let ((fname (symb "%" mname)))
      `(progn (defun ,fname ,@body)
              (defmacro ,mname (&rest rest)
                ,(make-wrap-docs :def* fname body)
                `(mvc #',',fname ,@rest))))))
(define-def*)

