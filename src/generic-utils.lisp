(in-package :veq)

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
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s))))
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

(defun match-substr (c s)
  (declare (string c s))
  "returns index where substring c matches s from left to right. otherwise nil."
  (loop with lc = (length c)
        for i from 0 repeat (1+ (- (length s) lc))
        if (string= c s :start2 i :end2 (+ i lc))
        do (return i)))

(declaim (inline last*))
(defun kv (s) (values (intern (string-upcase (mkstr s)) :keyword)))
(defun nth* (l i &optional d &aux (v (nth i l)))
  (declare (list l) (fixnum i))
  (if v v d))
(defun last* (l) (declare (list l)) (first (last l)))
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

(defun lpos (l &optional (i 0) j)
  (if j (mapcar (lambda (a) (subseq a i j)) l)
        (mapcar (lambda (a) (nth i a)) l)))

(defun mapqt (l) (mapcar (lambda (s) `(quote ,s)) l))

(defun undup (e &optional (flatten t))
  (declare (optimize speed))
  (remove-duplicates (if flatten (flatten e) e)))

(defun at-most (n &rest rest)
  (declare (fixnum n))
  (<= (length (remove-if-not #'identity rest)) n))

(abbrev mvc multiple-value-call)
(abbrev mvb multiple-value-bind)
(abbrev dsb destructuring-bind)
(abbrev awg with-gensyms)
(abbrev awf flatten)

(defun dotted-listp (l)
  (cond ((null l) nil)
        ((atom l) t)
        (t (dotted-listp (cdr l)))))

(defmacro push* (v l)
  (declare (symbol l))
  "push v to list l, and return v."
  (awg (vv) `(let ((,vv ,v)) (push ,vv ,l) ,vv)))


(defun -gensyms (name n)
  (declare (symbol name) (fixnum n))
  (loop with name = (string-upcase (string name))
        for x across "XYZWUVPQRSTUVABCDEFGHIJKLMNO" repeat n
        collect (gensym (format nil "~a/~a-" name x))))


(declaim (inline lst>n))
(defun lst>n (l n)
  (declare (list l) (fixnum n))
  "is list longer than n?"
  (consp (nthcdr n l)))

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

; this can probably be improved
(defun split-substr (x s &key prune &aux (lx (length x)))
  (declare (string x s) (boolean prune))
  (labels
    ((lst (s) (typecase s (list s) (t (list s))))
     (splt (s &aux (i (veq::match-substr x s)))
       (if i (cons (subseq s 0 i) (lst (splt (subseq s (+ lx i))))) s)))
    (let ((res (lst (splt s))))
      (if prune (remove-if (lambda (s) (= 0 (length s))) res)
                res))))

(defun strip-chars (s cc)
  (declare (symbol s) (list cc))
  (loop for spec in cc
        do (setf s (apply #'symb (split-substr (mkstr spec) (mkstr s)))))
  s)

(defun edge-chars (ch sym &optional rht &aux (s (mkstr sym)) (c 0))
  (declare (character ch) (symbol sym) (fixnum c))
  "count number of padding characters ch  in sym from the left (or right)
returns (values c sym*), where sym* is sym with the padding characters removed"
  (when rht (setf s (reverse s)))
  (loop repeat (length s) while (equal (char s 0) ch)
        do (setf s (subseq s 1) c (1+ c)))
  (values c (if rht (reverse s) s)))


