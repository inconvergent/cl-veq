(in-package :veq)

(defun veqsymb (dim type symb &key pref (pkg "VEQ"))
  (declare #.*opt* (symbol type))
  "build a symbol with correct name convention.
eg: (veqsymb 2 ff :lerp) yields f2lerp."
  (let ((elem (list (case (kv type)
                          (:df :d) (:ff :f) (:in :i) (:pn :p) (:kv :k)
                          (otherwise ""))
                    (if (and dim (> (the pn dim) 1)) dim "")
                    symb)))
    (when pref (setf elem (cons pref elem)))
    (values (intern (string-upcase (apply #'mkstr elem)) (mkstr pkg)))))

; TODO: rename, this isnt actually a veqsymb. bangsymb?
(defun unpack-veqsymb (sym &key (s :!)
                           &aux (l (split-substr (mkstr s) (mkstr sym))))
  (declare (symbol sym) ((or string keyword character) s) (list l))
  "split names of type f3!var into (values :f 3 var)"
  (labels
    ((err (&optional info)
       (error "UNPACK-VEQSYMB: var name must be t[d]!name
eg. f2!var. got: ~a (label: ~a)" sym info))
     (find-dim (pref &aux (i (1- (length pref))))
       (cond ((< i 0) 1)
             ((null pref) 1) ; no dimension
             (#1=(digit-char-p (char pref i)) #1#) ; last element is a num
             ((= 1 (length pref)) 1)
             (t (err :find-dim))))
     (var (pref) (cond ((zerop (length pref)) :nil)
                       ((null pref) :nil)
                       ((digit-char-p (char pref 0)) :nil)
                       (t (kv (char pref 0))))))
    (unless (< 0 (length l) 3) (err :len)) ; bad name
    (when (= 1 (length l)) (setf l (cons nil l))) ; no prefix
    (when (or (null (second l)) (< (length (second l)) 1)) (err :second)) ; bad name
    (dsb (pref vname) l
      (values (the keyword (var pref))
              (the fixnum (find-dim pref))
              (the symbol (symb (string-upcase vname)))))))

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
almost like multiple-values-list, except it handles multiple arguments."
  `(mvc #'list ,@body))
(defmacro from-lst (l)
  "get values from list. equivalent to (values-list ...)."
  `(values-list ,l))
(defmacro ~ (&rest rest)
  "wraps arguments in (mvc #'values ...)."
  `(mvc #'values ,@rest))

; (defmacro chain (fxs &rest rest &aux (rest `((~ ,@rest)))) ; TODO
;   (loop for f in (reverse fxs) do (setf rest `((funcall ,f ,@rest))))
;   `(progn ,@rest))
(defmacro vchain (fxs &rest rest &aux (rest `((~ ,@rest))))
  (loop for f in (reverse fxs) do (setf rest `((mvc ,f ,@rest))))
  `(progn ,@rest))

