
(in-package :veq)


(defun find-all-relevant-symbols (body &aux (ht (make-hash-table :test #'equal)))
  (labels ((nil-or-pckg-name (s &aux (p (symbol-package s)))
             (if (not p) nil (package-name p)))
           (rec (b)
             (cond ((null b) nil)
                   ((and (symbolp b) (equal "VEQ" (mkstr (nil-or-pckg-name b))))
                    (setf (gethash b ht) t))
                   ((consp b) (rec (car b)) (rec (cdr b)))
                   (t nil))))
    (rec body)
    ht))

(defun filter-macrolets (symbols-map body)
  (declare #.*opt* (list symbols-map body))
  "remove macrolet tuples that are not present in body. this speeds up
compilation time considerably, and makes it easier to read output code.

note: it can cause errors in in cases with nested macros."
  (let ((hits (find-all-relevant-symbols body)))
    (declare (hash-table hits))
    (remove-if-not (lambda (s) (declare (symbol s)) (gethash s hits))
                   symbols-map :key #'car)))

(map-docstring 'vref
  "use (veq:vref s x) or (:vr s x) to get dim x of symbol s in vprogn,
fvprogn, fvdef*, vdef*, def*. see replace-varg for implementation details."
  :nodesc :context)
(map-docstring 'varg
  "use (veq:varg dim a b ...) or (:vr dim a b ...) to represent dim vectors a,b
of dim n in vprogn, fvprog, fvdef*, vdef*, def*.  see replace-varg for
implementation details." :nodesc :context)


(defun replace-varg (body &optional (root-rmap (list)) (only nil))
  (declare #.*opt*)
  ; TODO: document only, vskp

  "replace instances of varg/:varg/:va and vref/:vref/:vr with
appropriate symbols for the dimension.

local maps vref/varg maps are propagated forwards in the list so a given
arg/ref should be available under its scope.  it seems to work for all cases
i have tested. but i'm mot sure if this propagation will eventually break
somewhere.

ex:
  (veq:replace-varg '(mvb ((:va 2 x)) (values 1 2)
                       (list (:vr x 1 0))))
  ; will return something like:
  ; (MVB (#:X/X-158 #:X/Y-159) (VALUES 1 2)
  ;      (LIST #:X/Y-159 #:X/X-158))"
  (labels
    ((is-skip (v) (and (consp v) (member (car v) '(vskp) :test #'eq)))
     (is-varg (v) (and (consp v) (member (car v) '(varg :varg :va) :test #'eq)))
     (is-vref (root) (and (listp root) (listp (car root))
                          (member (caar root) '(vref :vref :vr) :test #'eq)))
     (correct-dim (d symbs) (declare (pn d) (list symbs)) (= d (length symbs)))
     (gk (cr rmap) (typecase cr (symbol (assoc cr rmap))))
     (do-list (root rmap)
       (declare (list root rmap))
       (loop with res = (list)
             for v in (reverse root)
             if (is-skip v) do (push (second v) res)
             else if (and (is-varg v) (not only))
             do (loop with (dim . va-symbs) = (cdr v)
                      for name in va-symbs
                      for asc = (cdr (gk name rmap))
                      if (and asc (correct-dim dim asc))
                      collect (list asc) into new
                      else collect (let ((syms (-gensyms name dim)))
                                     (push `(,name . ,syms) rmap)
                                     syms) into new
                      finally (setf res `(,@(awf new) ,@res)))
             else do (push v res)
             finally (return (values res rmap))))
     (get-car-rmap (root rmap)
       (if (and (listp root) (not (dotted-listp root)))
           (do-list root rmap)
           (values root rmap)))
     (do-vref (root rmap)
       ; replace-varg can encounter vrefs that have no match in rmap (eg
       ; in -vmvb). so we need to ignore missing matches for vref.
       ; these vrefs will be replaced at a later time (eg. in the call
       ; to replace-varg in -vlet)
       (aif (gk (cadr root) rmap) ; if ref in rmap
            (mapcar (lambda (i) (nth (the pn i) (cdr it))) ; (cdr it) == symbs
                    (cddr root)) ; inds
            (list root))) ; do nothing
     (rec (root rmap)
       (cond ((atom root) root)
             ((is-vref root) `(,@(do-vref (car root) rmap)
                               ,@(rec (cdr root) rmap)))
             ((and (listp root) (gk (car root) rmap))
               `(,@(cdr (gk (car root) rmap))
                 ,@(rec (cdr root) rmap)))
             (t (mvb (car* rmap) (get-car-rmap (car root) rmap)
                  (cons (rec car* rmap)
                        (rec (cdr root) rmap)))))))
    (rec body root-rmap)))

