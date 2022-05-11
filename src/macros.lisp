
(in-package :veq)


(defmacro define-vprogn ()
  `(defmacro vprogn (&body body)
    "enable veq context inside this progn.
    handles propagation and resolution of uses of (varg d var) and (vref var i).
    fvprogn is faster, but has some limitations."
    `(macrolet ,',*symbols-map* (progn ,@(replace-varg body)))))
(define-vprogn)

(defmacro define-fvprogn ()
  `(defmacro fvprogn (&body body)
    "enable veq context inside this progn.
    handles propagation and resolution of uses of (varg d var) and (vref var i).

    works the same way as vprogn. but removes all macrolets that are not
    directly used in body. this is faster, but may fail in some cases where
    body is complex. in the event of errors try vprogn instead."
    `(macrolet ,(filter-macrolets *symbols-map* body)
       (progn ,@(replace-varg body)))))
(define-fvprogn)


(defmacro define-vdef ()
  `(defmacro vdef (fname &body body)
    "define function with veq context enabled. uses vprogn."
    `(vprogn (defun ,fname ,@body))))
(define-vdef)

(defmacro define-fvdef ()
  `(defmacro fvdef (fname &body body)
     "define function with veq context enabled. uses fvprogn."
     `(fvprogn (defun ,fname ,@body))))
(define-fvdef)

(defmacro define-vdef* ()
  `(defmacro vdef* (mname &body body)
    "defines a function named: %fx
and a wrapper macro named: fx
veq context is enabled. uses vprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...)."
    (let ((fname (symb "%" mname)))
      `(vprogn ; replace internal references to mname
               (defun ,fname ,@(subst fname mname body))
               (defmacro ,mname (&rest rest)
                  ,(format nil "fx: ~a~%macro wrapper: ~a~%
defined veq:vdef*" fname mname)
                 `(mvc #',',fname ,@rest))))))
(define-vdef*)

(defmacro define-fvdef* ()
  `(defmacro fvdef* (mname &body body)
    "defines a function named: %fx
and a wrapper macro named: fx
veq context is enabled. uses fvprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...)."
    (let ((fname (symb "%" mname)))
      `(fvprogn ; replace internal references to mname
                (defun ,fname ,@(subst fname mname body))
                (defmacro ,mname (&rest rest)
                  ,(format nil "fx: ~a~%macro wrapper: ~a~%
defined via veq:fvdef*" fname mname)
                  `(mvc #',',fname ,@rest))))))
(define-fvdef*)

(defmacro define-def* ()
  `(defmacro def* (mname &body body)
    "defines a function named: %fx
and a wrapper macro named: fx

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...)."
    (let ((fname (symb "%" mname)))
      `(progn (defun ,fname ,@body)
              (defmacro ,mname (&rest rest)
                  ,(format nil "fx: ~a~%macro wrapper: ~a~%
defined via veq:def*" fname mname)
                `(mvc #',',fname ,@rest))))))
(define-def*)

