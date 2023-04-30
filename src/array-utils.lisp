(in-package :veq)

;;;;;;;;;;;;;;;;;; GENERIC INIT ARRAY OF VEC

(defmacro $make (&key (dim 1) (n 1) v (type t))
  "create vector array with size (n dim), and initial value v."
  (awg (v* )
     `(let ((,v* (coerce ,v ',#1=(psymb :veq type))))
        (declare (,#1# ,v*))
        (make-array (the pn (* ,dim ,n))
          :initial-element ,v*
          :element-type ',type
          :adjustable nil))))

(defmacro new-stride ((from to type &optional (v 0)) arr)
  (declare (fixnum from to) (symbol arr))
  "shift arr from stride to stride."
  (unless (> to from 0) (error "NEW-STRIDE: must have (> to from 0)"))
  `(fvprogn
    (let ((n (/ (,(veqsymb 1 type :$num) ,arr) ,from))
          (v* (coerce ,v ',(psymb :veq type))))
      (declare (fixnum n))
      (,(veqsymb 1 type :with-arrays)
        (:n n :cnt c :itr i
         :arr ((arr* ,from ,arr)
               (res ,to (,(veqsymb nil type :$zero) (* ,to n))))
         :fxs ((fx ((:va ,from o)) (values o ,@(loop repeat (- to from)
                                                     collect 'v*))))
         :exs ((res c (fx arr*))))
        res))))

(defmacro define-constr (type)
  (labels ((nm (n) (veqsymb 1 type n)))
    (awg (a l)
      `(progn
        (export ',(nm "$make")) (export ',(nm "$copy"))
        (export ',(nm "_")) (export ',(nm "$~")) (export ',(nm "$_"))
        (defmacro ,(nm "$make") (&key (dim 1) (n 1) (v ,(coerce 0 type)))
          ,(format nil
            "create ~a vector array with size n * dim, and initial value v."
            (arrtype type))
          `($make :dim ,dim :n ,n :v ,v :type ,',type))
        (defun ,(nm "$copy") (,a)
          (declare #.*opt* (,(arrtype type) ,a))
          ,(format nil "copy ~a vector array." (arrtype type))
          (make-array (length ,a) :initial-contents ,a
                      :element-type ',type :adjustable nil))
        (defmacro ,(nm "_") (&body body)
          ,(format nil "create ~a vector array from body: (~a '(a b c ...))."
                  (arrtype type) (nm "_"))
          `(let ((,',l (progn ,@body)))
            (declare (list ,',l))
            (make-array (length ,',l) :initial-contents ,',l
                        :element-type ',',type :adjustable nil)))

        (defmacro ,(nm "$~") ((&optional (dim 1)) &body body)
          (declare (fixnum dim))
          ,(format nil "create ~a vector array from body:
(~a (values ...) (values ...) ...)."
                  (arrtype type) (nm "_"))
          (let ((symbs (loop repeat (length body) collect (gensym))))
            `(veq:fvprogn
               (,',(nm "vlet") (,@(loop for s in symbs
                                   for b in body collect (list s dim b)))
               (,',(nm "_") (list ,@symbs))))))

        (defun ,(nm "$_") (body)
          ,(format nil
            "create ~a vector array from body. where body is a list of lists.
ex: (~a (loop repeat 2 collect `(1f0 2f0)))
ex: (~a '((1f0 2f0) (1f0 2f0)))." (arrtype type) (nm "$_") (nm "$_"))
          (let* ((dim (length (the list (car body))))
                 (arr (make-array (* (length body) dim)
                        :element-type ',type :adjustable nil
                        :initial-element ',(coerce 0 type))))
            (declare (pn dim) (,(arrtype type) arr))
            (loop for r in body for i from 0 by dim
                  do (loop repeat dim
                           for e in r for ii from i
                           do (setf (aref arr ii) e)))
            arr))))))

(define-constr ff) (define-constr df) (define-constr in) (define-constr pn)

;;;;;;;;;;;;;;;;;; INIT ARRAY OF VEC

(defmacro define-arr-num (dim)
  (labels ((nm (n) (veqsymb dim nil n)))
    (awg (a)
      `(progn (export ',(nm "$num"))
              (defun ,(nm "$num") (,a)
                (declare #.*opt* (simple-array ,a))
                ,(format nil "number of elements in ~ad array.~%untyped." dim)
                (the pn (/ (length ,a) ,dim)))))))
(define-arr-num 1) (define-arr-num 2) (define-arr-num 3) (define-arr-num 4)

(defmacro define-arr-util ( dim type )
  (labels ((nm (n) (veqsymb dim type n)))
    (awg (a n)
      `(progn
         (export ',(nm "$num"))
         (defun ,(nm "$num") (,a)
           (declare #.*opt* (,(arrtype type) ,a))
           ,(format nil "number of elements in ~ad array.~%typed." dim)
           (the pn (/ (length ,a) ,dim)))
         (export ',(nm "$one"))
         (defun ,(nm "$one") (&optional (,n 1))
           (declare #.*opt* (pn ,n))
           ,(format nil "make ~ad array of ones.~%typed." dim)
           (,(veqsymb 1 type "$make") :dim ,dim :n ,n :v ,(coerce 1 type)))
         (export ',(nm "$val"))
         (defun ,(nm "$val") (v &optional (,n 1))
           (declare #.*opt* (,type v) (pn ,n))
           ,(format nil "make ~ad array of val.~%typed." dim)
           (,(veqsymb 1 type "$make") :dim ,dim :n ,n :v v))
         (export ',(nm "$zero"))
         (defun ,(nm "$zero") (&optional (,n 1))
           (declare #.*opt* (pn ,n))
           ,(format nil "make ~ad vector array of zeros.~%typed." dim)
           (,(veqsymb 1 type "$make") :dim ,dim :n ,n))))))
(define-arr-util 1 ff) (define-arr-util 2 ff) (define-arr-util 3 ff) (define-arr-util 4 ff)
(define-arr-util 1 df) (define-arr-util 2 df) (define-arr-util 3 df) (define-arr-util 4 df)
(define-arr-util 1 in) (define-arr-util 2 in) (define-arr-util 3 in) (define-arr-util 4 in)
(define-arr-util 1 pn) (define-arr-util 2 pn) (define-arr-util 3 pn) (define-arr-util 4 pn)

;;;;;;;;;;;;;;;;;;;;;; ACCESS

(defmacro -$ (dim a &key inds atype)
  (declare (pn dim) (list inds))
  (unless inds (setf inds '(0)))
  (awg (a*)
    (let ((lets (loop for r in inds
                      collect (let ((gs (gensym)))
                                `((,gs ,(typecase r (fixnum (* dim r))
                                                    (t `(* ,dim ,r))))
                                  ,@(loop for i from 0 below dim
                                          collect `(+ ,gs ,i)))))))
    `(let ((,a* ,a) ,@(loop for ss in lets collect (car ss)))
       (declare ,@(when atype `((,atype ,a*)))
                (pn ,@(loop for ss in lets collect (caar ss))))
       (values ,@(loop with res = (list)
                       for ss in lets
                       do (loop for s in (cdr ss)
                                do (push `(aref ,a* ,s) res))
                       finally (return (reverse res))))))))
(defmacro define-$ ()
  `(progn ,@(loop for (dim type)
                  in (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df
                              1 in 2 in 3 in 4 in 1 pn 2 pn 3 pn 4 pn) 2)
                  collect (let* ((name (veqsymb dim type "$"))
                                 (at (arrtype type))
                                 (docs (format nil
"returns indices [default: 0] from ~ad vector array (~a) as values.
ex: (~a a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension." dim at name)))
                            `(progn (export ',name)
                               (defmacro ,name (a &rest rest) ,docs
                               `(-$ ,,dim ,a :inds ,rest :atype ,',at)))))))
(define-$)

(defmacro $ (a &rest rest) `(-$ 1 ,a :inds ,rest))
(defmacro 2$ (a &rest rest) `(-$ 2 ,a :inds ,rest))
(defmacro 3$ (a &rest rest) `(-$ 3 ,a :inds ,rest))
(defmacro 4$ (a &rest rest) `(-$ 4 ,a :inds ,rest))

