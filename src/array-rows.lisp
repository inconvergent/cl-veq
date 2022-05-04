
(in-package :veq)


; (declaim (inline f$last d$last f2$last d2$last f3$last d3$last))
(defun f$last (a) (declare #.*opt* (fvec a)) (-f$ a (1- (the pos-int ($num a)))))
(defun d$last (a) (declare #.*opt* (dvec a)) (-d$ a (1- (the pos-int ($num a)))))
(defun f2$last (a) (declare #.*opt* (fvec a)) (-f2$ a (1- (the pos-int (2$num a)))))
(defun d2$last (a) (declare #.*opt* (dvec a)) (-d2$ a (1- (the pos-int (2$num a)))))
(defun f3$last (a) (declare #.*opt* (fvec a)) (-f3$ a (1- (the pos-int (3$num a)))))
(defun d3$last (a) (declare #.*opt* (dvec a)) (-d3$ a (1- (the pos-int (3$num a)))))
(defun f4$last (a) (declare #.*opt* (fvec a)) (-f4$ a (1- (the pos-int (4$num a)))))
(defun d4$last (a) (declare #.*opt* (dvec a)) (-d4$ a (1- (the pos-int (4$num a)))))


(defun -ind-to-val (type dim a inds)
  "return (values a[i] a[j] ...) for inds = (i j ...)
  inds can be on the form (i j k) or ((i j k))"
  (unless inds (setf inds `(0))) ; defaults to (0)
  (awg (a*) `(let ((,a* ,a))
               (declare (,(arrtype type) ,a*))
               (~ ,@(loop for ind in inds
                          collect `(,(veqsymb dim type "$" :pref "-")
                                     ,a* ,ind))))))

; TODO: with-op; simpler version of with-rows that takes input arrays and
; creates an output array of a given dimension

(defmacro -with-arrays ((&key type n (inds nil inds?) itr cnt arr
                              fxs exs nxs start)
                         &body body)
  (declare (list arr fxs exs))
  "
  n is the number of steps
  inds is indices to iterate. replaces n/start
  start is the first index (then n-1 more)
  arr is the arrays to be defined/references
  itr is the symbol representing indices
  cnt is the symbol representing iterations from 0
  fxs is the labels
  exs is the expressions assigned to array
  nxs is the expressions with no assignment
  "
  ; TODO: handle case where largest inds >= n
  (awg (n* inds*)
    (let ((itr (if itr itr (gensym "ITR")))
          (cnt (if cnt cnt (gensym "CNT"))))
      (declare (symbol itr cnt))
      (labels ((init-let (a dim &rest rest)
                 (declare (symbol a) (pos-int dim))
                 (unless a (return-from init-let nil))
                 (if rest `(,a (progn ,@rest))
                          `(,a (,(veqsymb 1 (cadr type) "$MAKE")
                                    :dim ,dim :n ,n*))))
               (arr-info (a)
                 (declare (symbol a))
                 (find-if (lambda (v) (eq (car v) a)) arr))

               (get-dim (a)
                 (declare (symbol a))
                 (the pos-int (cadr (arr-info a))))

               (symb-to-aref (e i)
                 (declare (symbol e))
                 (loop with hit of-type list = (arr-info e)
                       with dim of-type pos-int = (cadr hit)
                       with a of-type symbol = (car hit)
                       for j of-type pos-int from 0 below dim
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
                 `(loop for ,itr of-type pos-int
                        ,@(if inds? `(in ,inds*) `(from ,start below ,n*))
                        for ,cnt of-type pos-int from 0
                        ; assign result to res-arr
                        do (-vaset (,res-arr ,(get-dim res-arr) ,i)
                                   ,(transform-expr expr itr))))

               (no-set-loop-body (expr)
                 (declare (cons expr))
                 `(loop for ,itr of-type pos-int
                        ,@(if inds? `(in ,inds*) `(from ,start below ,n*))
                        for ,cnt of-type pos-int from 0
                        do ,(transform-expr expr itr))))

      `(let ((,n* ,n) ,@(when inds? `((,inds* ,inds))))
         (declare (pos-int ,n*) (ignorable ,n*)
                  ,@(when inds? `((list ,inds*))))
         (let ,(mapcar #'(lambda (v) (apply #'init-let v)) arr)
           (declare (,(arrtype (cadr type)) ,@(mapcar #'car arr)))
           (labels ,fxs

             ,@(loop for ex in exs collect
                 (progn (unless (= (length ex) 3)
                          (error "with arrays error. incorrect exs: ~a " ex))
                        (dsb (a i expr) ex (vaset-loop-body a i expr))))

             ,@(loop for ex in nxs collect (no-set-loop-body ex)))
           ,@body))))))


; TODO: for all inds?
; TODO: THERE WAS a bug that behaved as follows:
; if expr is a lambda that references either of the arrays passed into
; with-rows via  arrs, then (varg xx) in fxs (below) will replace the name of
; the array.
; ex: shape is defined outside. but would be replaced inside the with-arrays in
; with-rows
; (f2$with-rows (n shape)
;   (lambda (i (varg 2 x))
;     (declare (optimize speed) (ff x))
;     (when (f2segx pt shift (vref pt 1)
;                   x (f2$ shape (mod (1+ i) n)))
;           (incf c))))

(defun -with-rows (n arrs expr &key dim type)
  (declare (pos-int dim) (list arrs))
  "execute function (expr i ax ay az bx by bz ...) for row i and arrays a and b
  (...).  arrs can be one or more arrays."
  (awg (fx itr)
    (replace-varg ; < -- need this because of the (varg ...) below
      `(-with-arrays
         (:type (quote ,type) :n ,n :itr ,itr :start 0
          :arr (,@(mapcar (lambda (a) (list a dim a)) arrs))
          :fxs ,(let ((arr-gensyms (mapcar ; <-- this is because of the bugfix
                                     (lambda (x) (gensym (mkstr x)))
                                     arrs)))
                  `((,fx (,itr (varg ,dim ,@arr-gensyms))
                         (,@expr ,itr ,@arr-gensyms))))
          :nxs ((,fx ,itr ,@arrs)))))))

