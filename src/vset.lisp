
(in-package :veq)


(defun proc-$nvset (a n i body)
  (awg (i*) `(let ((,i* ,i)) (declare (pn ,i*))
               (setf (values ,@(loop for k from 0 below n
                                     collect `(aref ,a (the veq:pn (+ ,i* ,k)))))
                     (progn ,@body)))))
; TODO: optional type?
(defmacro $nvset ((a n &optional (i 0)) &body body)
  (declare (pn n))
  "set n indices in a, from a[i] with n values. body must yield n values"
  (proc-$nvset a n i body))

(defun proc-$rowset (a d i row)
  (unless (and (< 0 d) (<= d (length row)))
    (error "$rowset: incorrect number (~a) of elements in ~a. expected: (<= ~a)"
           (length row) row d))
  (awg (i*) `(let ((,i* ,i)) (declare (pn ,i*))
               (setf ,@(loop for r in row
                             for k from 0 repeat d
                             nconc `((aref ,a (the veq:pn (+ ,i* ,k))) ,r))))))
(defmacro $rowset ((a n &optional (i 0)) &body body)
  (declare (pn n))
  "performs (setf (aref a i) row0 (aref a (1+ i) r1 ...))
n must be less than or equal to (length row)"
  (proc-$rowset a n i body))

(defmacro make-$defsetf (dim)
  (awg (a i)
    (let* ((name (vvsym nil dim :$))
           (docstr (format nil "get: (~a a i) yields (values ...)~%set: (setf (~a a i) (values ...))" name name))
           (gs (loop for i from 0 below dim collect (gensym))))
      `(progn (export ',name)
              (defsetf ,name (,a &optional (,i 0)) (,@gs)
                ,docstr `($nvset (,,a ,,dim (* ,,i ,,dim)) (values ,,@gs)))))))

(make-$defsetf 1) (make-$defsetf 2) (make-$defsetf 3) (make-$defsetf 4)

(defmacro make-struct-vec-getters (cls dim type names)
  (declare (symbol cls) (cons names))
  `(veq:fvprogn
     ,@(loop for name of-type symbol in names
             collect `(defun ,(symb :@ name) (c &optional (i 0))
                        (declare (veq:pn i))
                        ,(format nil "~(get ~a. as ~a values.~)" name dim)
                        (let ((arr (,(symb cls name) c))
                              (i* (* i ,dim)))
                          (declare (,(arrtype type) arr))
                          (values ,@(loop for i from 0 repeat dim
                                          collect `(aref arr (+ i* ,i)))))))))


(defun proc-struct-vec-setter (cls dim type name)
  (awg (arr c i)
    (let* ((field (symb cls name))
           (docstr (format nil "set ~a values in struct field ~a." dim field))
           (gs (loop repeat dim collect (gensym))))
      `(defsetf ,(symb :@ name) (,c &optional (,i 0)) (,@gs) ,docstr
         `(let ((,',arr (,',field ,,c))) (declare (,',(arrtype type) ,',arr))
            ($nvset (,',arr ,,dim (* ,,i ,,dim)) (values ,,@gs)))))))
(defmacro make-struct-vec-setters (cls dim type names)
  `(progn ,@(loop for name in names
                  collect (proc-struct-vec-setter cls dim type name))))

