(in-package :veq)

(defmacro define-constants ()
  `(progn (declaim (df dpi dpii dpi25 dpi5) (ff fpi fpii fpi25 fpi5))
          (defconstant dpi (df pi))
          (defconstant dpi5 (df (* pi 0.5d0)))
          (defconstant dpi25 (df (* pi 0.25d0)))
          (defconstant dpii (df (* pi 2d0)))
          (defconstant fpi (ff pi))
          (defconstant fpi5 (ff (* pi 0.5f0)))
          (defconstant fpi25 (ff (* pi 0.25f0)))
          (defconstant fpii (ff (* pi 2f0)))))
(define-constants)

(defmacro mvcgrp ((dim fx) &body body)
  "call fx on body in groups of dim.
ex: (labels ((fx ((:va 3 x)) (fsel (:xy) x)))
      (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
returns: (values 1f0 2f0 4f0 5f0)
ex: (labels ((fx ((:va 3 x)) (fsel (:xz) x)))
      (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
returns: (values 1f0 3f0 4f0 6f0)"
  (awg (gsfx rest x)
    `(fvprogn
       (labels ((,gsfx (&rest ,rest)
         (apply #'values
           (awf (loop for ((:va  ,dim ,x)) in (group ,rest ,dim)
                      collect (lst (mvc ,fx ,x)))))))
         (mvc #',gsfx ,@body)))))

(defmacro mvcmap ((dim fx) &body body)
  "returns (values (fx i) (fx j) ...) for dim values from body."
  (let ((symbs (-gensyms 'mvcmap dim)))
    `(mvb (,@symbs) (~ ,@body)
      (values ,@(mapcar (lambda (s) `(,fx ,s)) symbs)))))

(defmacro vpr (&rest rest)
  "print input code with resulting values, return values."
  (awg (res) `(let ((,res (lst ,@rest)))
                (format t "~& ; ~{~a~^ | ~}~& > ~{~a~^ | ~}~&" ',rest ,res)
                (apply #'values ,res))))
(defmacro vp (&rest rest)
  "print values and return values, return values."
  (awg (res) `(let ((,res (lst ,@rest)))
                (format t "~&; vp: ~{~a~^ | ~}~&" ,res)
                (apply #'values ,res))))

; NOTE: using (lst ...) makes things slower in some cases.  because of the
; consing or because the number of values is unknown?. avoid when possible.
(defmacro lst (&body body)
  "get all (values ... ) in body as a list.
almost like multiple-value-list, except it handles multiple arguments."
  `(mvc #'list (~ ,@body)))
(defmacro from-lst (l)
  "return list as values. equivalent to (values-list ...)."
  `(values-list ,l))
(defmacro ~ (&rest rest)
  "wraps arguments in (mvc #'values ...)."
  `(mvc #'values ,@rest))

(defmacro vnrep (n &rest rest)
  "(~ rest1 rest2 ...)"
  `(veq:~ ,@(loop repeat n collect `(progn ,@rest))))
; TODO: this feels weird
(defmacro vnval (dim &rest rest)
  (awg (v) `(let ((,v (~ ,@rest)))
              (values ,@(loop repeat dim collect v)))))

(defmacro vchain (fxs &rest rest &aux (rest `((~ ,@rest))))
  (loop for f in (reverse fxs) do (setf rest `((mvc ,f ,@rest))))
  `(progn ,@rest))

