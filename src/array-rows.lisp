(in-package :veq)


(defmacro make-last (dim type)
  (awg (a)
    `(progn (export ',(veqsymb dim type "$last"))
            (defun ,(veqsymb dim type "$last") (,a)
              (declare #.*opt* (,(arrtype type) ,a))
              ,(format nil "return values from last row of ~ad vector array." dim)
            (,(veqsymb dim type "$") ,a
               (1- (the pn (,(veqsymb dim nil "$num") ,a))))))))
(make-last 1 ff) (make-last 2 ff) (make-last 3 ff) (make-last 4 ff)
(make-last 1 df) (make-last 2 df) (make-last 3 df) (make-last 4 df)

; TODO: protect c
(defun -struct-fields (dim type s c slots)
  `(with-struct
     (,s ,@(mapcar #'symb (remove-if-not #'keywordp slots))) ,c
     (~ ,@(mapcar (lambda (o) (if (not (keywordp o)) o
                                `(,(veqsymb dim type "$") ,(symb o))))
                  slots))))
(defmacro struct-fields (dim type)
  (let* ((mname (veqsymb dim type "$s"))
         (docs (format nil "get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (~a structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are ~a of dim ~a" mname (arrtype type) dim)))
    `(progn (map-docstring ',mname ,docs :nodesc :context)
            (map-symbol `(,',mname (s c &rest rest) ,,docs
                                   (-struct-fields ,,dim ',',type s c rest))))))
(struct-fields 1 ff) (struct-fields 2 ff) (struct-fields 3 ff) (struct-fields 4 ff)
(struct-fields 1 df) (struct-fields 2 df) (struct-fields 3 df) (struct-fields 4 df)


(defmacro -with-arrays ((&key type n (inds nil inds?) itr cnt arr
                              fxs exs nxs start)
                         &body body)
  (declare (list arr fxs exs))
  (awg (n* inds*)
    (let ((itr (if itr itr (gensym "ITR")))
          (cnt (if cnt cnt (gensym "CNT"))))
      (declare (symbol itr cnt))
      (labels ((init-let (a dim &rest rest)
                 (declare (symbol a) (pn dim))
                 (unless a (return-from init-let nil))
                 (if rest `(,a (progn ,@rest))
                          `(,a (,(veqsymb 1 (cadr type) "$MAKE")
                                    :dim ,dim :n ,n*))))
               (arr-info (a)
                 (declare (symbol a))
                 (find-if (lambda (v) (eq (car v) a)) arr))

               (get-dim (a)
                 (declare (symbol a))
                 (the pn (cadr (arr-info a))))

               (symb-to-aref (e i)
                 (declare (symbol e))
                 (loop with hit of-type list = (arr-info e)
                       with dim of-type pn = (cadr hit)
                       with a of-type symbol = (car hit)
                       for j of-type pn from 0 below dim
                       collect `(aref ,a (+ ,j (* ,i ,dim)))))

               (transform-expr (expr i)
                 (declare (cons expr))
                 (loop with res of-type list = (list)
                       for e in expr
                       if (arr-info e)
                       do (setf res `(,@res ,@(symb-to-aref e i)))
                       else do (setf res `(,@res ,e))
                       finally (return res)))

               ; TODO: fix inefficient indexing calc in vaset/no-set?
               (vaset-loop-body (res-arr i expr)
                 (declare (cons expr))
                 `(loop for ,itr of-type pn
                        ,@(if inds? `(in ,inds*) `(from ,start below ,n*))
                        for ,cnt of-type pn from 0
                        ; assign result to res-arr
                        do ($nvset (,res-arr ,(get-dim res-arr)
                                             (* ,(get-dim res-arr) ,i))
                                   ,(transform-expr expr itr))))

               (no-set-loop-body (expr)
                 (declare (cons expr))
                 `(loop for ,itr of-type pn
                        ,@(if inds? `(in ,inds*) `(from ,start below ,n*))
                        for ,cnt of-type pn from 0
                        do ,(transform-expr expr itr))))

      `(let ((,n* ,n) ,@(when inds? `((,inds* ,inds))))
         (declare (pn ,n*) (ignorable ,n*)
                  ,@(when inds? `((list ,inds*))))
         (let ,(mapcar #'(lambda (v) (apply #'init-let v)) arr)
           (declare (,(arrtype (cadr type)) ,@(mapcar #'car arr)))
           (labels ,fxs
             ,@(loop for ex in exs
                     collect (progn
                               (unless (= (length ex) 3)
                                 (error "with arrays error. incorrect exs: ~a " ex))
                               (dsb (a i expr) ex (vaset-loop-body a i expr))))
             ,@(loop for ex in nxs collect (no-set-loop-body ex)))
           ,@body))))))

(defun -with-arr-doc (n)
  (format nil "args: (&key (n 0) inds (start 0) itr cnt arr fxs exs nxs)

n is the number of iterations
start is the first (row) index. then n-1 more.
inds is (row) indices to iterate. replaces n/start
arr is the arrays to be defined/referenced
itr is the symbol representing indices
cnt is the symbol representing iterations from 0
fxs is the labels
exs is the expressions assigned to array
nxs is the expressions with no assignment

ex:

(~a (:n 7 :itr k ; k will be 0, 1, ..., 6
  ; the third form in elements of arr can be empty, a form that will be
  ; executed, or a symbol that refers to an array defined outside of
  ; with-arrays
  :arr ((a 3 (f3$one 7)) ; init a as (f3$one 7)
        (b 3) (c 3)) ; init b,c as (f3$zero 7)
  ; define functions to use in fxs
  :fxs ((cross ((varg 3 v w)) (f3cross v w))
        (init1 (i) (f3~~ (1+ i) (* 2 i) (+ 2 i)))
        (init2 (i) (f3~~ (+ 2 i) (1+ i) (* 2 i))))
  ; perform the calculations
  :exs ((a k (init1 k)) ; init row k of a with init1
        (b k (init2 k)) ; init row k of b with init2
        (c k (cross a b))) ; set row k of c to (cross a b)
  :nxs ((cross a b))); executes the function, but does not assign res anywhere
  ; use the arrays. the last form is returned, as in a progn
  (vpr c))" n))

(map-docstring 'dwith-arrays (-with-arr-doc "dwith-arrays") :nodesc :context)
(map-docstring 'fwith-arrays (-with-arr-doc "fwith-arrays") :nodesc :context)
(map-docstring 'iwith-arrays (-with-arr-doc "pwith-arrays") :nodesc :context)
(map-docstring 'pwith-arrays (-with-arr-doc "iwith-arrays") :nodesc :context)

(mapcar #'map-symbol
  `((dwith-arrays ((&key (n 0) (inds nil inds?) (start 0) itr cnt arr fxs exs nxs)
                    &body body)
      `(-with-arrays (:type 'df :n ,n ,@(when inds? `(:inds ,inds))
                      :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs :nxs ,nxs
                      :exs ,exs :start ,start)
                     ,@body))
    (fwith-arrays ((&key (n 0) (inds nil inds?) (start 0) itr cnt arr fxs exs nxs)
                   &body body)
      `(-with-arrays (:type 'ff :n ,n ,@(when inds? `(:inds ,inds))
                      :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs :nxs ,nxs
                      :exs ,exs :start ,start)
                     ,@body))
    (iwith-arrays ((&key (n 0) (inds nil inds?) (start 0) itr cnt arr fxs exs nxs)
                   &body body)
      `(-with-arrays (:type 'in :n ,n ,@(when inds? `(:inds ,inds))
                      :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs :nxs ,nxs
                      :exs ,exs :start ,start)
                     ,@body))
    (pwith-arrays ((&key (n 0) (inds nil inds?) (start 0) itr cnt arr fxs exs nxs)
                   &body body)
      `(-with-arrays (:type 'pn :n ,n ,@(when inds? `(:inds ,inds))
                      :itr ,itr :cnt ,cnt :arr ,arr :fxs ,fxs :nxs ,nxs
                      :exs ,exs :start ,start)
                     ,@body))))

