(in-package :veq)

(defun ensure-filename (fn &optional (postfix "") (silent nil))
  (let ((fn* (mkstr (if fn fn "tmp") postfix)))
    (format (not silent) "~&file: ~a~&" fn*)
    fn*))

(defmacro mac (expr) "expand macro."
  `(silent? :ct (pprint (macroexpand-1 ',expr))))
#+sbcl (defmacro mac* (expr) "expand macro all. only in SBCL."
         `(silent? :ct (pprint (sb-cltl2:macroexpand-all ',expr))))

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args) ,(format nil "alias: ~s~&" long) `(,',long ,@args)))
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form)) (if it ,then-form ,else-form)))

(defun flatten (x)
  (labels ((rec (x acc) (cond ((null x) acc)
                              ((atom x) (cons x acc))
                              (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s) `(,s (gensym ,(symbol-name s))))
                 syms)
     ,@body))

(defun group (l n) (declare (list l) (fixnum n))
  "group l into lists of n elements. see ungroup."
  (if (< n 1) (error "group error: group size is smaller than 1"))
  (labels ((rec (l acc &aux (rest (nthcdr n l)))
             (if (consp rest)
                 (rec rest (cons (subseq l 0 n) acc))
                 (nreverse (cons l acc)))))
    (if l (rec l nil) nil)))
(defun ungroup (l &aux (res (list))) (declare (list l res)) "inverse of group."
  (loop for s in l do (loop for k in s do (push k res)))
  (reverse res))

(declaim (inline mkstr))
(defun mkstr (&rest args) (declare (optimize speed (safety 2)))
  (with-output-to-string (s) (dolist (a args) (princ a s))))

(defun match-substr (sub s) (declare (optimize speed (safety 2)) (string sub s))
  "returns index where substring matches s from left to right. otherwise nil."
  (loop with sub0 of-type character = (char sub 0)
        with lc = (length sub)
        for i from 0 repeat (1+ (- (length s) lc))
        if (and (eq sub0 (char s i)) ; this is more efficient
                (string= sub s :start2 (1+ i) :end2 (+ i lc) :start1 1))
        do (return-from match-substr i)))

(declaim (inline last*))
(defun nth* (l i &optional d &aux (v (nth i l))) (declare (list l) (fixnum i))
  (if v v d))
(defun last* (l) (declare (list l)) (first (last l)))

(defun symb (&rest args) (declare (optimize speed (safety 1)))
  (let ((*print-case* :upcase))
    (values (intern (apply #'mkstr args)))))
(defun psymb (pkg &rest args) ; https://gist.github.com/lispm/6ed292af4118077b140df5d1012ca646
  (declare (optimize speed (safety 1)))
  (let ((*print-case* :upcase))
    (values (intern (apply #'mkstr args) (if pkg pkg :veq)))))
(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym (string-upcase (mkstr name)))))
    `(let ((,gs ,struct)) (let ,(mapcar #'(lambda (f)
                                            `(,f (,(psymb (symbol-package name) name f) ,gs)))
                                        fields)
                            ,@body))))

(defun reread (&rest args) (values (read-from-string (apply #'mkstr args))))
(defun mapqt (l) (mapcar (lambda (s) `(quote ,s)) l))
(defun undup (e &optional (flatten t)) (declare (optimize speed))
  (remove-duplicates (if flatten (flatten e) e)))

(defun at-most (n &rest rest) (declare (fixnum n))
  (<= (length (remove-if-not #'identity rest)) n))

(abbrev mvc multiple-value-call) (abbrev mvb multiple-value-bind)
(abbrev dsb destructuring-bind) (abbrev awg with-gensyms)
(abbrev awf flatten)

(defun dotted-listp (l) ; TODO: rewrite with rec to require first call to be cons
  (cond ((null l) nil) ((atom l) t) (t (dotted-listp (cdr l)))))

(defmacro push* (v l) (declare (symbol l)) "push v to list l, and return v."
  (awg (vv) `(let ((,vv ,v)) (push ,vv ,l) ,vv)))

(defun -gensyms (name n) (declare (optimize speed) (symbol name) (fixnum n))
  (loop with name = (string-upcase (string name))
        for x across "XYZWUVPQRSTUVABCDEFGHIJKLMNO" repeat n
        collect (gensym (format nil "~a/~a-" name x))))

(declaim (inline lst>n))
(defun lst>n (l n) (declare (list l) (fixnum n)) "is list longer than n?"
  (consp (nthcdr n l)))

(defun dupes (lst) (declare (list lst)) "finds duplicates in list."
  (cond ((null lst) (list))
        ((member (car lst) (cdr lst) :test #'equal)
           (cons (car lst) (dupes (cdr lst))))
        (t (dupes (cdr lst)))))

(defun split-string (x s &key prune) ; TODO: collapse split-string, split-substr
  (declare (character x) (string s) (boolean prune))
  (labels ((splt (s) (loop for c across s for i from 0
                           if (equal c x)
                           do (return-from splt
                                (cons (subseq s 0 i) (splt (subseq s (1+ i))))))))
    (let ((res (splt (concatenate 'string s (string x)))))
      (if prune (remove-if (lambda (s) (zerop (length s))) res)
                res))))

(defun split-substr (x s &key prune &aux (lx (length x))) ; TODO: this can probably be improved
  (declare (optimize speed) (string x s) (boolean prune))
  (labels ((lst (s) (typecase s (list s) (t (list s))))
           (splt (s &aux (i (match-substr x s)))
             (if i (cons (subseq s 0 i) (lst (splt (subseq s (+ lx i))))) s)))
    (let ((res (lst (splt s))))
      (if prune (remove-if (lambda (s) (zerop (length s))) res) res))))

(defun fx-split-str (fx s) (declare (function fx) (string s))
  "split s into list of chars according to fx"
  (loop for c across s if (funcall fx c) collect c into yes
        else collect c into no finally (return (values yes no))))

(defun nilpad (n l &optional (v nil) &aux (n* (length l)))
  (declare (fixnum n n*) (list l)) "cons v to l intil (length l) >= n"
  (loop repeat (- n n*) do (setf l (cons v l)) finally (return l)))

(defun strcat (s) (declare (optimize speed) (list s))
  (apply #'concatenate 'string s))

(defun repl (s from to) (declare (string s to from)) "replace from with to in s"
  (let ((s (veq::strcat (mapcar (lambda (s) (mkstr s to))
                                (split-substr from s)))))
    (subseq s 0 (- (length s) (length to)))))

(defun filter-by-predicate (l fx &key (key #'identity))
  (declare (optimize speed (safety 2)) (list l) (function fx key))
  "split l into (values yes no) according to fx"
  (loop for x in l
        if (funcall fx (funcall key x)) collect x into yes
        else collect x into no
        finally (return (values yes no))))

(defun tree-find-all (root fx &optional (res (list)))
  (declare (optimize speed) (function fx) (list res))
  "find all instances where fx is t in root."
  (cond ((funcall fx root) (return-from tree-find-all (cons root res)))
        ((atom root) nil)
        (t (let ((l (tree-find-all (car root) fx res))
                 (r (tree-find-all (cdr root) fx res)))
             (when l (setf res `(,@l ,@res)))
             (when r (setf res `(,@r ,@res))))
           res)))

(defun tree-replace (tree from to &optional (comparefx #'equal))
  "compares tree to from (with comparefx); replaces matches with to."
  (cond ((funcall comparefx tree from) to)
        ((null tree) nil) ((atom tree) tree)
        (t (mapcar (lambda (x) (tree-replace x from to)) tree))))

(defun tree-replace-fx (tree fxmatch fxtx)
  "compares elements with (comparefx); repaces matches with (fxmatch hit)."
  (cond ((funcall fxmatch tree) (tree-replace-fx (funcall fxtx tree) fxmatch fxtx))
        ((null tree) nil) ((atom tree) tree)
        (t (mapcar (lambda (x) (tree-replace-fx x fxmatch fxtx)) tree))))

(defun replace-pairs (body pairs)
  (declare (list body pairs)) "replace ((ato afrom) (bto bfrom) ...) in body."
  (loop for (to from) in pairs do (setf body (tree-replace body from to))) body)

(defun strip-symbols (name symbs)
  (declare (optimize speed) (string name) (list symbs))
  (loop for c of-type string in symbs
        do (setf name (the string (strcat (split-substr c name)))))
  name)

(defun strip-arg-keys (ll kk &aux (ll (group ll 2)))
  "strip keywords in kk from ll where ll is a list of kw function args."
  (ungroup (remove-if (lambda (k) (member k kk :test #'eq)) ll :key #'car)))
(defun get-arg-key (ll k &optional d)
  "get the value of keyword k in ll where ll is a list of kw function args."
  (aif (second (find k (group ll 2) :key #'car)) it d))

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

