
(in-package :veq)

(declaim (inline filter-macrolets))
(defun filter-macrolets (symbols-map body)
  (declare #.*opt* (list symbols-map body))
  "remove macrolet tuples that are not present in body. this speeds up
compilation time considerably, and makes it easier to read output code.

note: it can cause errors in in cases with nested macros."
  (let ((macrolet-symbols-in-body
          (remove-duplicates (remove-if-not #'symbolp (awf body)))))
    (declare (list macrolet-symbols-in-body))
    (remove-if-not (lambda (s) (declare (symbol s))
                               (member s macrolet-symbols-in-body))
                   symbols-map :key #'car)))

(defmacro select-macrolets ((dim type &rest names) &body body)
  `(macrolet (,@(filter-macrolets *symbols-map*
                  (loop for n in names
                        collect (veqsymb dim type n))))
             ,@body))

(map-docstring 'vref
  "use (veq:vref s x) or (:vr s x) to get dim x of symbol s in vprogn,
fvprogn, fvdef*, vdef*, def*. see replace-varg for implementation details."
  :nodesc :context)

(map-docstring 'varg
  "use (veq:varg dim a b ...) or (:vr dim a b ...) to represent dim vectors a,b
of dim n in vprogn, fvprog, fvdef*, vdef*, def*.  see replace-varg for
implementation details." :nodesc :context)

(defun replace-varg (body &optional (root-rmap (list)))
  "replace instances of varg/:varg/:va and vref/:vref/vr with
approprite symbols for the dimension.

local maps vref/varg maps are propagated forwards in the list so a given
arg/ref should be available under its scope.  it seems to work for all cases
i have tested. but i'm mot sure if this propagation will eventually break
somewhere."
  (labels
    ((is-varg (v) (and (consp v) (member (car v) '(varg :varg :va) :test #'eq)))
     (is-vref (root) (and (listp root) (listp (car root))
                          (member (caar root) '(vref :vref :vr) :test #'eq)))
     (correct-dim (d symbs) (= d (length symbs)))
     (do-list (root rmap)
       (declare (list root rmap))
       (loop with res = (list)
             for v in (reverse root)
             if (is-varg v)
             do  (dsb (dim &rest va-symbs) (cdr v)
                  (loop for name in va-symbs
                        collect (let ((asc (cdr (assoc name rmap))))
                                  (if (and asc (correct-dim dim asc))
                                    ; already in rmap, with correct dim
                                    ; use existing gensyms
                                    (list asc)
                                    ; make new gensyms and add to rmap
                                    (let ((syms (-gensyms name dim)))
                                      (push `(,name . ,syms) rmap)
                                      syms))) into new
                        finally (setf res `(,@(awf new) ,@res))))
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
       (aif (assoc (cadr root) rmap) ; if ref in rmap
            (mapcar (lambda (i) (nth (the pos-int i) (cdr it))) ; (cdr it) == symbs
                    (cddr root)) ; inds
            (list root))) ; do nothing
     (walk (root rmap)
       (cond ((atom root) root)
             ((is-vref root) `(,@(do-vref (car root) rmap)
                               ,@(walk (cdr root) rmap)))
             ((and (listp root) (assoc (car root) rmap))
               `(,@(cdr (assoc (car root) rmap))
                 ,@(walk (cdr root) rmap)))
             (t (mvb (car* rmap) (get-car-rmap (car root) rmap)
                  (cons (walk car* rmap)
                        (walk (cdr root) rmap)))))))
    (walk body root-rmap)))

