
(in-package :veq)


(defun dimaref (a ii dim)
  (declare (symbol a) (pos-int dim))
  (loop for j of-type pos-int from 0 below dim
        collect `(aref ,a (+ ,ii ,j))))


; TODO: for all inds
; TODO: limit by n
(defun for-all-rows (n arr expr &key dim)
  (declare (pos-int dim) (list arr))
  "
  "
  (unless (= 1 (length expr))
          (error "for-all-rows error. ~% malformed expr: ~a" expr))
  (when (remove-if #'symbolp arr)
        (error "for-all-rows error. ~% arr must be one or more symbols.~% got: ~a" arr))
  (awg (i ii n* expr*)
    `(let ((,n* ,n))
       (declare (pos-int ,n*))
       (loop for ,i of-type pos-int from 0 below ,n*
             for ,ii of-type pos-int from 0 by ,dim
             do (mvc ,(last* expr) ,i
                     ,@(loop with res = (list)
                             for a of-type symbol in arr
                             do (setf res `(,@res ,@(dimaref a ii dim)))
                             finally (return res)))))))



(defmacro -with-arrays ((&key type n (inds nil inds?) itr cnt arr fxs exs)
                         &body body)
  (declare (list arr fxs exs))
  ; TODO: handle case where inds is longer than actual number of inds
  (awg (n* inds* ii*)
    (let ((itr (if itr itr (gensym "ITR")))
          (cnt (if cnt cnt (gensym "CNT"))))
      (declare (symbol itr cnt))
      (labels ((init-let (arr* dim &rest rest)
                 (declare (symbol arr*) (pos-int dim))
                 (if rest `(,arr* (progn ,@rest))
                          `(,arr* ($ :dim ,dim :n ,n* :type ,type))))
               (get-dim (arr*)
                 (declare (symbol arr*))
                 (cadr (find-if (lambda (v) (eq (car v) arr*)) arr)))

               (symb-or-aref (e ii)
                 (declare (symbol ii))
                 (let ((s (find-if (lambda (v) (eq (car v) e)) arr)))
                   ; arr ii dim
                   (when s (dimaref (car s) ii (cadr s)))))

               (transform-expr (expr ii)
                 (declare (cons expr) (symbol ii))
                 (loop with res of-type list = (list)
                       for e in expr
                       do (let ((ref (symb-or-aref e ii)))
                            (if ref (setf res `(,@res ,@ref))
                                    (setf res `(,@res ,e))))
                       finally (return res)))

               (vaset-loop-body (arr* i expr &aux (dim (get-dim arr*)))
                 (declare (cons expr) (symbol arr*) (pos-int dim))
                 `(loop for ,itr of-type pos-int
                        ,@(if inds? `(in ,inds*)
                                    `(from 0 below ,n*))
                        for ,cnt of-type pos-int from 0
                        do (let ((,ii* (* ,dim ,itr)))
                             (declare (pos-int ,ii*) (ignorable ,ii*))
                             (-vaset (,arr* ,dim ,i)
                                     ,(transform-expr expr ii*))))))

      `(let ((,n* ,n) ,@(when inds? `((,inds* ,inds))))
         (declare (pos-int ,n*)
                  ,@(when inds? `((list ,inds*))))
         (let ,(mapcar #'(lambda (v) (apply #'init-let v)) arr)
           (declare (,(arrtype (cadr type))
                     ,@(mapcar #'car arr)))
           (labels ,fxs ,@(loop for ex in exs
                                collect
                                  (progn
                                    (unless (= (length ex) 3)
                                      (error "with arrays error. incorrect exs: ~a "
                                             ex))
                                    (dsb (arr* i expr) ex
                                      (vaset-loop-body arr* i expr)))))
           ,@body))))))

