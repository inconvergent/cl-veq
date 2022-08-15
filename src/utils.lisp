
(in-package :veq)

(defun v? (&optional (silent t))
  "get version. use silent to surpress stdout"
  (let ((v (slot-value (asdf:find-system 'veq) 'asdf:version)))
    (unless silent (format t "~%veq version: ~a~%" v))
    v))
(defun d? (f) "describe argument" (describe f))
(defun i? (f) "inspect argument" (inspect f))

; from on lisp by pg
(defmacro mac (expr)
  "expand macro."
  `(pprint (macroexpand-1 ',expr)))
#+sbcl (defmacro mac* (expr)
         "expand macro all. only in SBCL."
         `(pprint (sb-cltl2:macroexpand-all ',expr)))

;from on lisp by pg
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

; from on lisp by pg
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

; from on lisp by pg
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

; modified from on lisp by pg
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

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

;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
(defun psymb (package &rest args) (values (intern (apply #'mkstr args) package)))
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym (string-upcase (mkstr name)))))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(psymb (symbol-package name) name f) ,gs)))
                     fields)
         ,@body))))

; from on lisp by pg
(defun reread (&rest args) (values (read-from-string (apply #'mkstr args))))

(abbrev mvc multiple-value-call)
(abbrev mvb multiple-value-bind)
(abbrev dsb destructuring-bind)
(abbrev awg with-gensyms)
(abbrev awf flatten)

(defun dotted-listp (l)
  (cond ((null l) nil)
        ((atom l) t)
        (t (dotted-listp (cdr l)))))

(defun veqsymb (dim type symb &key pref)
  (declare #.*opt* (pos-int dim) (symbol type))
  "builld a symbol with correct name convention.
eg: (veqsymb 2 ff \"lerp\") yields f2lerp."
  (let ((elem (list (cdr (assoc type
                           `((df . "D") (ff . "F") (in . "I")
                             (pn . "P") (nil . ""))))
                    (if (> dim 1) dim "")
                    (mkstr symb))))
    (when pref (setf elem (cons pref elem)))
    (values (intern (string-upcase (apply #'mkstr elem)) "VEQ"))))

(defun arrtype (type)
  (declare #.*opt* (symbol type))
  "use type to select array type."
  (values (intern (string-upcase
                    (mkstr (cdr (assoc type
                                  `((df . "DVEC") (ff . "FVEC") (in . "IVEC")
                                    (pn . "PVEC") (nil . ""))))))
                  "VEQ")))

(defmacro push* (v l)
  (declare (symbol l))
  "push v to list l, and return v."
  (awg (vv) `(let ((,vv ,v)) (push ,vv ,l) ,vv)))


(defun -gensyms (name n)
  (declare (symbol name) (fixnum n))
  (loop with name = (string-upcase (string name))
        repeat n
        for x across "XYZWUVPQR"
        collect (gensym (format nil "~a-~a-" name x))))


(declaim (inline lst>n last*))
(defun lst>n (l n)
  (declare (list l) (pos-int n))
  "is list longer than n?"
  (consp (nthcdr n l)))
(defun last* (a)
  (declare (list a))
  "last element of list."
  (first (last a)))

(defun dupes (lst)
  (declare (list lst))
  "finds duplicates in list."
  (cond ((null lst) (list))
        ((member (car lst) (cdr lst) :test #'equal)
           (cons (car lst) (dupes (cdr lst))))
        (t (dupes (cdr lst)))))

(defun split-string (x s &key prune)
  (declare (character x) (string s) (boolean prune))
  (labels
    ((splt (s)
       (loop for c across s for i from 0
             if (equal c x)
             do (return-from splt
                  (cons (subseq s 0 i) (splt (subseq s (1+ i))))))))
    (let ((res (splt (concatenate 'string s (string x)))))
      (if prune (remove-if (lambda (s) (= 0 (length s))) res)
                res))))

(defmacro mvcgrp ((dim fx) &body body)
  "call fx on body in groups of dim.
ex: (labels ((fx ((:va 3 x)) (veq:fxy x)))
      (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
returns: (values 1f0 2f0 4f0 5f0)
ex: (labels ((fx ((:va 3 x)) (veq:fxz x)))
      (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
returns: (values 1f0 3f0 4f0 6f0)"
  (awg (gsfx rest x)
    `(veq:fvprogn
       (labels ((,gsfx (&rest ,rest)
         (apply #'values
           (awf (loop for ((:va  ,dim ,x)) in (group ,rest ,dim)
                      collect (veq:lst (veq:mvc ,fx ,x)))))))
         (mvc #',gsfx ,@body)))))

(defmacro mvcmap ((dim fx) &body body)
  "returns (values (fx i) (fx j) ...) for dim values from body."
  (let ((symbs (-gensyms 'mvcmap dim)))
    `(mvb (,@symbs) (~ ,@body)
      (values ,@(mapcar (lambda (s) `(,fx ,s)) symbs)))))

(defmacro mvcwrap (m fx)
  "define a macro named m so that (m a ...) is equivalent to (mvc #'fx a ...)"
  `(defmacro ,m (&rest args)
    ,(format nil "(mvc #'~a ...)" fx)
    `(mvc #',',fx ,@args)))
(mvcwrap vprod *) ; (* a b c ...)
(mvcwrap vsum +) ; (+ a b c ...)

(defmacro vpr (&rest rest)
  "print (mvc #'list rest) and return (mvc #'values rest)."
  (awg (res) `(let ((,res (lst ,@rest)))
                (format t "~& ; ~{~a~^ | ~}~& > ~{~a~^ | ~}~&" ',rest ,res)
                (apply #'values ,res))))

; NOTE: using (lst ...) makes things slower in some cases.  because of the
; consing or because the number of values is unknown?. avoid when possible.
(defmacro lst (&body body)
  "get all values in body as a list.
almost like multuple-values-list, except it handles multiple arguments."
  `(mvc #'list ,@body))
(defmacro from-lst (l)
  "get values from list. equivalent to (values-list ...)."
  `(values-list ,l))
(defmacro ~ (&rest rest)
  "wraps arguments in (mvc #'values ...)."
  `(mvc #'values ,@rest))

