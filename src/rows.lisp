
(in-package :veq)


(defun dimaref (a ii dim)
  (declare (symbol a) (pos-int dim))
  (loop for j of-type pos-int from 0 below dim
        collect `(aref ,a (+ ,ii ,j))))


; TODO: for all inds
(defun for-all-rows (n arr expr &key dim)
  (declare (pos-int dim) (list arr))
  "
  "
  (unless (= 1 (length expr))
          (error "for-all-rows error. ~% malformed expr: ~a" expr))
  (when (remove-if #'symbolp arr)
        (error "for-all-rows error. ~% arr must be one or more symbols.~% got: ~a" arr))
  (awg (i ii n*)
    `(let ((,n* ,n))
       (declare (pos-int ,n*))
       (loop for ,i of-type pos-int from 0 below ,n*
             for ,ii of-type pos-int from 0 by ,dim
             do (mvc ,(last* expr) ,i
                     ,@(loop with res = (list)
                             for a of-type symbol in arr
                             do (setf res `(,@res ,@(dimaref a ii dim)))
                             finally (return res)))))))


(defmacro -with-arrays ((&key type n (inds nil inds?) itr cnt arr fxs exs
                              start)
                         &body body)
  (declare (list arr fxs exs))
  ; TODO: handle case where largest inds >= n
  (awg (n* inds* i*)
    (let ((itr (if itr itr (gensym "ITR")))
          (cnt (if cnt cnt (gensym "CNT"))))
      (declare (symbol itr cnt))
      (labels ((init-let (a dim &rest rest)
                 (declare (symbol a) (pos-int dim))
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

               (vaset-loop-body (a i expr)
                 (declare (cons expr) (symbol a))
                 `(loop for ,itr of-type pos-int
                        ,@(if inds? `(in ,inds*) `(from ,start below (+ ,start ,n*)))
                        for ,cnt of-type pos-int from 0
                        ; TODO: fix inefficient indexing calc
                        do (-vaset (,a ,(get-dim a) ,i)
                                     ,(transform-expr expr itr)))))

      `(let ((,n* ,n) ,@(when inds? `((,inds* ,inds))))
         (declare (pos-int ,n*) (ignorable ,n*)
                  ,@(when inds? `((list ,inds*))))
         (let ,(mapcar #'(lambda (v) (apply #'init-let v)) arr)
           (declare (,(arrtype (cadr type)) ,@(mapcar #'car arr)))
           (labels ,fxs
             ,@(loop for ex in exs collect
                 (progn (unless (= (length ex) 3)
                          (error "with arrays error. incorrect exs: ~a " ex))
                        (dsb (a i expr) ex (vaset-loop-body a i expr)))))
           ,@body))))))


(defun -ind-to-val (type dim a rest)
  "
  return (values a[i] a[j] ...) for rest = (i j ...)

  rest can be on the form (i j k) or ((i j k))
  "
  (unless rest (setf rest `(0))) ; defaults to (0)
  (awg (a*)
    `(let ((,a* ,a))
       (declare (,(arrtype type) ,a*))
       (mvc #'values ,@(loop for ind in rest
                             collect `(,(veqsymb dim type "$" :pref "-")
                                        ,a* ,ind))))))


(declaim (inline f$last d$last f2$last d2$last f3$last d3$last))
(defun f$last (a) (declare #.*opt* (fvec a)) (-f$ a (1- (the pos-int (length a)))))
(defun d$last (a) (declare #.*opt* (dvec a)) (-d$ a (1- (the pos-int (length a)))))
(defun f2$last (a) (declare #.*opt* (fvec a)) (-f2$ a (1- (the pos-int (/ (length a) 2)))))
(defun d2$last (a) (declare #.*opt* (dvec a)) (-d2$ a (1- (the pos-int (/ (length a) 2)))))
(defun f3$last (a) (declare #.*opt* (fvec a)) (-f3$ a (1- (the pos-int (/ (length a) 3)))))
(defun d3$last (a) (declare #.*opt* (dvec a)) (-d3$ a (1- (the pos-int (/ (length a) 3)))))

