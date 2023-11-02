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
    `(fvprogn (labels ((,gsfx (&rest ,rest)
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
                (silent? :rt
                  (format t "~&;; ~{~a~^ | ~}~&>> ~{~a~^ | ~}~&" ',rest ,res))
                (apply #'values ,res))))
(defmacro vp (&rest rest)
  "print values and return values, return values."
  (awg (res) `(let ((,res (lst ,@rest)))
                (silent? :rt (format t "~&>> ~{~a~^ | ~}~&" ,res))
                (apply #'values ,res))))

; (defmacro vpr1 (v &optional (prefix ">> ")) ; TODO: finish this
;   (awg (vpr1) `(let ((,vpr1 ,v))
;                  (silent? :rt (format t "~a ~a" ,prefix ,vpr1))
;                  ,vpr1)))
; (defmacro vp (&rest rest &aux (prefix ">> "))
;   "print values and return values, return values."
;   `(progn (silent? :rt (format t "~&"))
;     (values ,@(loop for v in rest
;                    if (keywordp v)
;                    collect (progn (setf prefix (format nil "~a >> " (string-downcase (mkstr v))))
;                                    `(format t "~&"))
;                    ; collect `(format t "~&~a >>: " ,(string-downcase (mkstr v)))
;                    else collect `(vpr1 ,v ,prefix)
;     ))))

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

(defmacro n~ (n &rest rest) ; make flag to disable this in strict mode?
  (cond ((= (length rest) n) `(values ,@rest))
        (t `(~ ,@rest)))) ; possibly remove other uses of ~?

(defmacro vnrep (n &rest rest)
  (declare (pn n))
  "corresponds to (~ r1 ... rn)"
  `(values ,@(loop repeat n collect `(progn ,@rest))))

(defmacro vnval (n &rest rest)
  (declare (pn n))
  "returns (values v ...), where v is (progn ,@rest) evaluated once."
  (awg (v) `(let ((,v (progn ,@rest)))
              (values ,@(loop repeat n collect v)))))

(defmacro vchain (fxs &rest rest &aux (rest `((~ ,@rest))))
  " chain functions, on all values.
eg: (vchain #'a #'b (values 1 2))
corresponds to: (mvc #'a (mvc #'b (values 1 2)))"
  (loop for f in (reverse fxs) do (setf rest `((mvc ,f ,@rest))))
  `(progn ,@rest))

(defmacro mutate! (vars &body body)
  "ex: (mutate! (a b) (values 1 2))
is equivalent to (mvb (a* b*) (values 1 2) (setf a a* b b*))
where a* and b* are gensyms"
  (let ((vars* (loop for v of-type symbol
                     in (etypecase vars (cons vars) (symbol (list vars)))
                     collect `(,v ,(gensym (string-upcase (mkstr v)))))))
    `(mvb (,@(mapcar #'second vars*)) (progn ,@body)
          (setf ,@(awf vars*)))))

