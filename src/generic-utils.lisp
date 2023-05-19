(in-package :veq)

(defmacro mac (expr) ; from on lisp by pg
  "expand macro."
  `(pprint (macroexpand-1 ',expr)))
#+sbcl (defmacro mac* (expr)
         "expand macro all. only in SBCL."
         `(pprint (sb-cltl2:macroexpand-all ',expr)))

(defmacro aif (test-form then-form &optional else-form) ;from on lisp by pg
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

(defmacro abbrev (short long) ; from on lisp by pg
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

(defun flatten (x) ; from on lisp by pg
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro with-gensyms (syms &body body) ; modified from on lisp by pg
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

(defun group (source n) ; modified from on lisp by pg
  (if (< n 1) (error "group error: group size is smaller than 1"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

(declaim (inline mkstr))
(defun mkstr (&rest args) ; from on lisp by pg
  (declare (optimize speed (safety 2)))
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(declaim (inline match-substr))
(defun match-substr (sub s)
  (declare (optimize speed (safety 2)) (string sub s))
  "returns index where substring matches s from left to right. otherwise nil."
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from match-substr i)))

(declaim (inline last*))
(defun nth* (l i &optional d &aux (v (nth i l)))
  (declare (list l) (fixnum i))
  (if v v d))
(defun last* (l) (declare (list l)) (first (last l)))

(defun symb (&rest args) ; from on lisp by pg
  (values (intern (apply #'mkstr args))))
(defun psymb (pkg &rest args) ;https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  (declare (optimize speed))
  (values (intern (apply #'mkstr args) pkg)))
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym (string-upcase (mkstr name)))))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(psymb (symbol-package name) name f) ,gs)))
                     fields)
         ,@body))))

(defun reread (&rest args) ; from on lisp by pg
  (values (read-from-string (apply #'mkstr args))))

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
  (declare (optimize speed) (symbol name) (fixnum n))
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
      (if prune (remove-if (lambda (s) (zerop (length s))) res)
                res))))

; this can probably be improved
(defun split-substr (x s &key prune &aux (lx (length x)))
  (declare (optimize speed) (string x s) (boolean prune))
  (labels
    ((lst (s) (typecase s (list s) (t (list s))))
     (splt (s &aux (i (match-substr x s)))
       (if i (cons (subseq s 0 i) (lst (splt (subseq s (+ lx i))))) s)))
    (let ((res (lst (splt s))))
      (if prune (remove-if (lambda (s) (zerop (length s))) res)
                res))))

(defun fx-split-str (fx s)
  (declare (function fx) (string s))
  "split s into list of chars according to fx"
  (loop for c across s if (funcall fx c) collect c into yes
        else collect c into no finally (return (values yes no))))

(defun nilpad (n l &optional (v nil) &aux (n* (length l)))
  (declare (fixnum n n*) (list l))
  "cons v to l intil (length l) >= n"
  (loop repeat (- n n*) do (setf l (cons v l)) finally (return l)))

(defun strcat (s)
  (declare (optimize speed) (list s))
  (apply #'concatenate 'string s))

(defun strip-symbols (name symbs)
  (declare (optimize speed) (string name) (list symbs))
  (loop for c of-type string in symbs
        do (setf name (the string (strcat (split-substr c name)))))
  name)

(defun edge-fx (fx s &optional rht &aux (c 0))
  (declare (optimize speed) (function fx) (string s) (fixnum c) (boolean rht))
  "count number of times fx is t across sym chars from the left (or right)
returns (values c sym*), where sym* is sym with the padding characters removed"
  (when rht (setf s (reverse s)))
  (loop repeat (length s) while (funcall fx (char s 0))
        do (setf s (the string (subseq s 1))) (incf c))
  (values c (if rht (reverse s) s)))

(defun edge-str (ch s &optional rht)
  (declare (optimize speed) (character ch) (string s) (boolean rht))
  "count number of padding characters ch  in s from the left (or right)
returns (values c sym*), where sym* is s with the padding characters removed"
  (edge-fx (lambda (c) (eq ch c)) s rht))

