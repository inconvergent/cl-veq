
(in-package :veq)

; TODO: optional type?
(defmacro $nvset ((a n &optional (i 0)) &body body)
  (declare (symbol a) (pn n))
  "set n indices in a, from a[i] with n values from body."
  (awg (i*) `(let ((,i* ,i)) (declare (pn ,i*))
               (setf (values ,@(loop for i from 0 below n
                                     collect `(aref ,a (+ ,i* ,i))))
                     (~ ,@body)))))

(defmacro make-$defsetf (dim)
  (awg (a i)
    (let* ((name (veqsymb dim nil :$))
           (docstr (format nil "get: (~a a i) yields (values ...)~%set: (setf (~a a i) (values ...))" name name))
           (gs (loop for i from 0 below dim collect (gensym))))
      `(progn (export ',name)
              (defsetf ,name (,a &optional (,i 0)) (,@gs)
                ,docstr `($nvset (,,a ,,dim (* ,,i ,,dim)) (values ,,@gs)))))))
(make-$defsetf 1) (make-$defsetf 2) (make-$defsetf 3) (make-$defsetf 4)

