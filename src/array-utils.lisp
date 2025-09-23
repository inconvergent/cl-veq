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

(defmacro $new-stride ((from to type &optional (v 0)) arr)
  (declare (veq:pn from to))
  "shift arr from stride to stride."
  (unless (> to from 0) (error "$NEW-STRIDE: must have (> to from 0)"))
  `(fvprogn ; TODO: to less than from
    (let ((v* (coerce ,v ',(psymb :veq type))))
      (declare (,(psymb :veq type) v*))
      (labels ((fx ((:va ,from x))
                 (values (:vr x ,@(loop for i from 0 repeat from collect i))
                         ,@(loop repeat (- to from) collect 'v*))))
        (,(vvsym type 1 (mkstr from to :_@$fx)) ,arr)))))

(export '$coerce)
(defmacro $coerce (type a) ; TODO: this could be more efficient
  (declare (symbol type))
  "coerce sequence a to vec of type (eg: veq:ff, veq:df)"
  (awg (res a*)
  `(let* ((,a* ,a) (,res ($make :dim 1 :n (length ,a*) :v 0 :type ,type)))
     (declare (,(arrtype type) ,res) (sequence ,a*))
     (typecase ,a* (vector (loop for v across ,a* for i from 0
                                 do #1=(setf (aref ,res i) (,type v))))
                   (list   (loop for v in ,a* for i from 0 do #1#)))
     ,res)))

; tentative / incomplete
; (let ((a (veq:f$~ (6) 1.0 2.0 3.0 4.0 5.0 6.0)))
;  (veq:2$print (veq:$reverse! a :dim 2)))
(export '$reverse!) ; todo alternative to not in-place
(defmacro $reverse! (a &key (dim 1))
  (declare (symbol a) (pn dim))
  "reverse typed array in-place."
  `(let ((n (length ,a)))
    (declare (pn n))
    (when (> n 1)
      (loop for i from 0 by ,dim repeat (floor (floor n ,dim) 2)
        do (progn ,@(loop for k from 0 repeat dim
                      collect `(rotatef (aref ,a (+ i ,k))
                                        (aref ,a (+ (- n i ,dim) ,k)))))))
    ,a))

(defmacro define-constr (type)
  (labels ((nm (n) (vvsym type 1 n)))
    (awg (a l)
      `(progn
        (export ',(nm "$make"))
        (defmacro ,(nm "$make") (&key (dim 1) (n 1) (v ,(coerce 0 type)))
          ,(format nil "create ~a vector array with size n * dim, and initial value v."
                       (arrtype type))
          `($make :dim ,dim :n ,n :v ,v :type ,',type))

        (export ',(nm "$copy"))
        (defun ,(nm "$copy") (a &optional (na (length a)))
          (declare #.*opt* (,(arrtype type) a))
          ,(format nil "copy ~a vector array." (arrtype type))
          (make-array na :initial-contents a :element-type ',type :adjustable nil))

        (export ',(nm "$coerce"))
        (defun ,(nm "$coerce") (a) (declare #.*opt* (sequence a))
          ,(format nil "coerce sequence to ~a." (arrtype type))
          ($coerce ,type a))

        (export ',(nm "$join"))
        (defun ,(nm "$join") (a b &optional (n (+ (length a) (length b)))
                                   (res (make-array n :element-type ',type :adjustable nil)))
          (declare #.*opt* (pn n) (,(arrtype type) a b res))
          ,(format nil "join a, b of type ~a." (arrtype type))
               (loop for i from 0 below (length a) do (setf (aref res i) (aref a i)))
               (loop for i from (length a) below n
                     for j from 0 do (setf (aref res i) (aref b j)))
               res)

        (export ',(nm "_"))
        (defmacro ,(nm "_") (&body body)
          ,(format nil "create ~a vector array from body: (~a '(a b c ...))."
                       (arrtype type) (nm "_"))
          `(let ((,',l (progn ,@body)))
            (declare (list ,',l))
            (make-array (length ,',l) :initial-contents ,',l
                        :element-type ',',type :adjustable nil)))

        (export ',(nm "$~"))
        (defmacro ,(nm "$~") ((&optional (n 1)) &body body)
          (declare (pn n))
          ,(format nil "create ~a vector array from n values in body." (arrtype type))
          (let ((symbs (loop repeat n collect (gensym))))
            `(let ((,',a ($make :n ,n :type ,',type :v (coerce 0 ',',type))))
               (declare (,',(arrtype type) ,',a))
               (mvb (,@symbs) (~ ,@body)
                    (declare (,',type ,@symbs))
                    (setf ,@(loop for i from 0 repeat n for s in symbs
                                  append `((aref ,',a ,i) ,s))))
               ,',a)))

        (export ',(nm "$_"))
        (defun ,(nm "$_") (body)
          ,(format nil "create ~a vector array from body. where body is a list of lists.
ex: (~a (loop repeat 2 collect `(a b)))
ex: (~a '((a b) (c d)))." (arrtype type) (nm "$_") (nm "$_"))
          (let* ((dim (length (the list (car body))))
                 (arr (make-array (* (length body) dim) :element-type ',type
                                  :adjustable nil :initial-element ',(coerce 0 type))))
            (declare (pn dim) (,(arrtype type) arr))
            (loop for r in body for i from 0 by dim
                  do (loop repeat dim for e in r for ii from i
                           do (setf (aref arr ii) e)))
            arr))))))

(define-constr ff) (define-constr df) (define-constr in) (define-constr pn)

;;;;;;;;;;;;;;;;;; INIT ARRAY OF VEC

(defmacro define-arr-num (dim)
  (labels ((nm (n) (vvsym nil dim n)))
    (awg (a)
      `(progn (export ',(nm "$num"))
              (defun ,(nm "$num") (,a)
                (declare #.*opt* (simple-array ,a))
                ,(format nil "number of elements in ~ad array.~%untyped." dim)
                (the pn (/ (length ,a) ,dim)))))))
(define-arr-num 1) (define-arr-num 2) (define-arr-num 3) (define-arr-num 4)

(defmacro define-arr-util ( dim type )
  (labels ((nm (n) (vvsym type dim n)))
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
           (,(vvsym type 1 "$make") :dim ,dim :n ,n :v ,(coerce 1 type)))
         (export ',(nm "$val"))
         (defun ,(nm "$val") (v &optional (,n 1))
           (declare #.*opt* (,type v) (pn ,n))
           ,(format nil "make ~ad array of val.~%typed." dim)
           (,(vvsym type 1 "$make") :dim ,dim :n ,n :v v))
         (export ',(nm "$zero"))
         (defun ,(nm "$zero") (&optional (,n 1))
           (declare #.*opt* (pn ,n))
           ,(format nil "make ~ad vector array of zeros.~%typed." dim)
           (,(vvsym type 1 "$make") :dim ,dim :n ,n))))))
(define-arr-util 1 ff) (define-arr-util 2 ff) (define-arr-util 3 ff) (define-arr-util 4 ff)
(define-arr-util 1 df) (define-arr-util 2 df) (define-arr-util 3 df) (define-arr-util 4 df)
(define-arr-util 1 in) (define-arr-util 2 in) (define-arr-util 3 in) (define-arr-util 4 in)
(define-arr-util 1 pn) (define-arr-util 2 pn) (define-arr-util 3 pn) (define-arr-util 4 pn)

;;;;;;;;;;;;;;;;;;;;;; ACCESS

(defun proc-$ (dim a inds atype)
  (declare (pn dim))
  (awg (a*)
    (let* ((inds (loop for r in inds
                       if (numberp r) collect `(nil ,(* dim r))
                       else collect `(,(gensym "IND") (* ,dim ,r))))
           (unknown (remove-if #'not inds :key #'car)))
      `(let ((,a* ,a) ,@unknown)
         (declare ,@(when atype `((,atype ,a*))) (pn ,@(mapcar #'car unknown)))
         (values
           ,@(loop for (sym ind) in inds
                   if sym nconc (loop for i from 0 repeat dim
                                      collect `(aref ,a* (the veq:pn (+ ,sym ,i))))
                   else nconc (loop for i from 0 repeat dim ; pre calc ind
                                    collect `(aref ,a* ,(+ ind i)))))))))

(defmacro -$ (dim a &key inds atype)
  (declare (pn dim) (list inds))
  (unless inds (setf inds '(0)))
  (proc-$ dim a inds atype))

(defmacro define-$ ()
  `(progn ,@(loop for (dim type)
                  in (group '(1 ff 2 ff 3 ff 4 ff 1 df 2 df 3 df 4 df
                              1 in 2 in 3 in 4 in 1 pn 2 pn 3 pn 4 pn) 2)
                  collect (let* ((name (vvsym type dim "$"))
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

