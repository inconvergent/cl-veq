(in-package :veq)

(defmacro define-constants ()
  `(progn (declaim (df dpi dpii dpi25 dpi5) (ff fpi fpii fpi25 fpi5))
      (defconstant dpi (df pi))            (defconstant dpii (df (* pi 2d0)))
      (defconstant dpi5 (df (* pi 0.5d0))) (defconstant dpi25 (df (* pi 0.25d0)))
      (defconstant fpi (ff pi))            (defconstant fpii (ff (* pi 2f0)))
      (defconstant fpi5 (ff (* pi 0.5f0))) (defconstant fpi25 (ff (* pi 0.25f0)))))
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

(defmacro mvcmap ((dim fx) &body body) "returns (values (fx i) (fx j) ...) for dim values from body."
  (let ((symbs (-gensyms 'mvcmap dim)))
    `(mvb (,@symbs) (~ ,@body) (values ,@(mapcar (lambda (s) `(,fx ,s)) symbs)))))

(defmacro vpr (&rest rest) "print input code with resulting values, return values."
  (awg (res) `(let ((,res (lst ,@rest)))
                (silent? :rt (format t "~&;; ~{~a~^ | ~}~&>> ~{~a~^ | ~}~&" ',rest ,res))
                (apply #'values ,res))))
(defmacro vp (&rest rest) "print values and return values, return values."
  (awg (res) `(let ((,res (lst ,@rest)))
                (silent? :rt (format t "~&>> ~{~a~^ | ~}~&" ,res))
                (apply #'values ,res))))

; NOTE: using (lst ...) makes things slower in some cases.  because of the
; consing or because the number of values is unknown?. avoid when possible.
(defmacro lst (&body body)
  "get all (values ... ) in body as a list.
almost like multiple-value-list, except it handles multiple arguments."
  `(mvc #'list (~ ,@body)))
(defmacro from-lst (l) "return list as values. equivalent to (values-list ...)."
  `(values-list ,l))
(defmacro ~ (&rest rest) "wraps rest in (mvc #'values ...)."
  `(mvc #'values ,@rest))

(defmacro n~ (n &rest rest) "use (~ ...) unless there is exactly n arguments; then just use (values ...)."
  (cond ((= (length rest) n) `(values ,@rest)) ; make flag to disable this in strict mode?
        (t `(~ ,@rest)))) ; possibly remove other uses of ~?

(defmacro vnrep (n &rest rest) (declare (pn n))
  "corresponds to (values [rest n times]). see vnval."
  `(values ,@(loop repeat n collect `(progn ,@rest))))
(defmacro vnval (n &rest rest) (declare (pn n))
  "returns (values v ...), where v is (progn ,@rest) evaluated once. see vnrep."
  (awg (v) `(let ((,v (progn ,@rest)))
              (values ,@(loop repeat n collect v)))))

(defmacro vchain (fxs &rest rest &aux (rest `((~ ,@rest))))
  "chain functions, on all values.
eg: (vchain #'a #'b (values 1 2)) equals: (mvc #'a (mvc #'b (values 1 2)))"
  (loop for f in (reverse fxs) do (setf rest `((mvc ,f ,@rest))))
  `(progn ,@rest))

(defmacro lpos (l &optional (i 0) j)
  "get list of index i or subseq i j from list of lists."
  (awg (a)
    (if j `(mapcar (lambda (,a) (subseq (the list ,a) (the veq:pn ,i) (the veq:pn ,j))) ,l)
          `(mapcar (lambda (,a) (nth ,i (the list ,a))) ,l))))
(defmacro vector-rearrange (a &rest rest)
  "get new vector with elements from a. ex:
(let ((i 3) (v #(0 1 2 3 4 5)))
  (vector-rearrange v 0 1 (0 1) ((print i)) i)) ; #(0 1 0 3 3)"
  (awg (a*)
    `(let ((,a* ,a))
       (concatenate 'vector
         ,@(loop for ind in rest
                 collect (etypecase ind
                           (number `(list (aref ,a* ,ind)))
                           (symbol `(list (aref ,a* ,ind)))
                           (cons (ecase (length ind) (1 `(list (aref ,a* ,@ind)))
                                                     (2 `(subseq ,a* ,@ind))))))))))
(defmacro mutate! (vars &body body)
  "ex: (mutate! (a b) (values 1 2))
is equivalent to (mvb (a* b*) (values 1 2) (setf a a* b b*))
where a* and b* are gensyms"
  (let ((vars* (loop for v of-type symbol
                     in (etypecase vars (cons vars) (symbol (list vars)))
                     collect `(,v ,(gensym (string-upcase (mkstr v)))))))
    `(mvb (,@(mapcar #'second vars*)) (progn ,@body)
          (setf ,@(awf vars*)))))

(defun with-symbs (ss body) (declare (list ss body))
  "bind these symbols outside body and replace inside body. eg:
  (with-symbs `(g ,g ...) (qry g :select ... )) ; equals:
  (let ((gg ,g) ...) (qry gg :select ...))      ; gg is a gensym"
  (let ((s* (loop for (var expr) in (group ss 2) ; gs expr var
                  collect (list (gensym (mkstr var)) expr var))))
    `(let (,@(loop for s in s* collect (subseq s 0 2)))
       (progn ,(replace-pairs body
                 (loop for s in s* collect (list (first s) (third s))))))))

