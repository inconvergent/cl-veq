
(in-package :veq)

; mat * v ; (f3mtv mat x)
(defmacro make-mat-mv (dim type &optional transpose)
  (awg (mm m v)
    (let ((exportname (veqsymb dim type (if transpose "mtv" "mv"))))
      (labels ((stp (i k) (if transpose (+ i (* k dim)) (+ (* i dim) k)))
               (make-row-sum (i)
                `(+ ,@(loop for k from 0 below dim
                            collect `(* (aref ,mm ,(stp i k))
                                        (:vref vv ,k))))))
        `(progn
          (export ',exportname)
          (defmacro ,exportname (,m &rest ,v)
            ,(format nil "~:[mat~;transpose(mat)~] * v. for ~ad matrix and vector." transpose dim)
            `(fvprogn
              (let ((,',mm ,,m))
                (declare (,',(arrtype type) ,',mm))
                (fvlet ((vv ,',dim (mvc #'values ,@,v)))
                       ,',(cons 'values
                                (loop for i from 0 below dim
                                      collect (make-row-sum i))))))))))))

(make-mat-mv 2 ff) (make-mat-mv 3 ff) (make-mat-mv 4 ff)
(make-mat-mv 2 df) (make-mat-mv 3 df) (make-mat-mv 4 df)
(make-mat-mv 2 ff t) (make-mat-mv 3 ff t) (make-mat-mv 4 ff t)
(make-mat-mv 2 df t) (make-mat-mv 3 df t) (make-mat-mv 4 df t)


; eye(n) ; (f3eye n)
(defmacro make-mat-eye (dim type &aux (exportname
                                    (veqsymb dim type "meye")))
  (labels ((eye ()
            (loop for i from 0 below (* dim dim)
                  collect (if (= (mod i dim) (/ (- i (mod i dim)) dim))
                              (symb "V") (coerce 0 type)))))
    `(progn (export ',exportname)
            (defun ,exportname (&optional (v ,(coerce 1 type)))
              (declare #.*opt* (,type v))
              ,(format nil "return ~ad eye matrix." dim)
              (the ,(arrtype type) (make-array ,(* dim dim)
                                     :initial-contents (list ,@(eye))
                                     :adjustable nil
                                     :element-type ',type))))))

(make-mat-eye 2 ff) (make-mat-eye 3 ff) (make-mat-eye 4 ff)
(make-mat-eye 2 df) (make-mat-eye 3 df) (make-mat-eye 4 df)

; transpose inplace: (f3mt! mat)
(defmacro make-mat-transp (dim type)
  (awg (arr a)
    (labels ((matind (i j) (+ (* i dim) j))
             (rotate () (loop with res = (list)
                           for i from 0 below dim
                           do (loop for j from 0 below dim
                                   if (> i j)
                                   do (push
                                        `(rotatef
                                           (aref ,arr ,(matind i j))
                                           (aref ,arr ,(matind j i)))
                                        res))
                          finally (return res))))

      (let ((exportname (veqsymb dim type "mt!")))
        `(progn (export ',exportname)
                (defmacro ,exportname (,a)
                  ,(format nil "transpose ~ad matrix in-place." dim)
                  `(let ((,',arr ,,a))
                     (declare (,',(arrtype type) ,',arr))
                     ,',(cons 'progn (rotate))
                     ,',arr)))))))

(make-mat-transp 2 ff) (make-mat-transp 3 ff) (make-mat-transp 4 ff)
(make-mat-transp 2 df) (make-mat-transp 3 df) (make-mat-transp 4 df)

; mat * mat (f3mm mat mat) (f3mtm mat mat) etc
(defmacro make-mat-mm (dim type &key ta tb)
  (awg (a a* b b* c)
    (labels ((matind (i j) (+ (* i dim) j))
             (ikj (i j) (loop for k from 0 below dim
                              collect
                                `(* (aref ,a ,(if ta (matind k i) (matind i k)))
                                    (aref ,b ,(if tb (matind j k) (matind k j)))))))
      (let ((exportname (veqsymb dim type (format nil "m~:[~;t~]m~:[~;t~]" ta tb))))
        `(fvprogn (export ',exportname)
           (defmacro ,exportname (,a* ,b*)
             ,(format nil
                "multiply ~:[mat~;(transpose mat)~] * ~:[mat~;(transpose mat)~]~%of type: ~a"
                ta tb (arrtype type))
             `(let* ((,',a ,,a*)
                     (,',b ,,b*)
                     (,',c (,',(veqsymb  dim type "$zero") ,,dim)))
                 (declare (,',(arrtype type) ,',a ,',b ,',c))
                ,',(cons 'progn
                     (loop with res = (list)
                           for i from 0 below dim
                           do (loop for j from 0 below dim
                                    do (push `(setf (aref ,c ,(matind i j))
                                                    (+ ,@(ikj i j)))
                                             res))
                           finally (return res)))
                ,',c)))))))
(defmacro make-mat-all-mm ()
  (let ((pairs (group '(2 ff 3 ff 4 ff 2 df 3 df 4 df) 2))
        (transp (group '(nil nil t t nil t t nil) 2)))
    `(progn
       ,@(loop for (dim type) in pairs
               collect `(progn ,@(loop for (ta tb) in transp
                                       collect `(make-mat-mm ,dim ,type
                                                  :ta ,ta :tb ,tb)))))))
(make-mat-all-mm)


; make transpose matrix (mtrans x)
(defmacro make-mat-trans (dim type)
  (let ((exportname (veqsymb dim type "mtrans")))
    `(progn (export ',exportname)
            (fvdef* ,exportname ((varg ,dim x))
              (declare #.*opt* (,type x))
              ,(format nil "make ~ad transpose matrix for moving by x" dim)
              (let ((res (,(veqsymb (1+ dim) type "meye"))))
                (declare (,(arrtype type) res))
                ,@(loop for i from 1 to dim
                        collect `(setf (aref res ,(1- (* (1+ dim) i)))
                                       (vref x ,(1- i))))
                res)))))
(make-mat-trans 2 ff) (make-mat-trans 3 ff)
(make-mat-trans 2 df) (make-mat-trans 3 df)

; make scale matrix (mascale s)
(defmacro make-mat-scale (dim type)
  (let ((exportname (veqsymb dim type "mscale")))
    `(progn (export ',exportname)
            (fvdef* ,exportname ((varg ,dim x))
              (declare #.*opt* (,type x))
              ,(format nil "make ~ad matrix for scaling by x" dim)
              (let ((res (,(veqsymb (1+ dim) type "meye"))))
                (declare (,(arrtype type) res))
                ,@(loop for i from 0 below dim
                        collect `(setf (aref res ,(+ i (* (1+ dim) i)))
                                       (vref x ,i)))
                res)))))
(make-mat-scale 2 ff) (make-mat-scale 3 ff)
(make-mat-scale 2 df) (make-mat-scale 3 df)


; i tried to write these as macro-writing macros.
; and i couldn't get it to work ...
; make rot matrix
(defmacro make-2mrot (type &optional w)
  (let ((exportname (veqsymb 2 type (format nil "mrot~:[*~;~]" w)))
        (z0 (coerce 0 type)) (one (coerce 1 type)))
     `(progn (export ',exportname)
        (def* ,exportname (a)
         (declare #.*opt* (,type a))
         "make 2d rotation matrix for rotating a rads"
         (let ((cosa (cos a)) (sina (sin a)))
           (declare (,type cosa sina))
           (f_ (list cosa (- sina) ,@(if w `(,z0)) sina cosa
                     ,@(if w `(,z0 ,z0 ,z0 ,one)))))))))
(make-2mrot ff) (make-2mrot ff t) (make-2mrot df) (make-2mrot df t)

; make rot matrix
; NOTE: f3mrot* is the 3x3 matrix version. f3mrot is 4x4 matrix, with w
(defmacro make-3mrot (type &optional w)
  (let ((exportname (veqsymb 3 type (format nil "mrot~:[*~;~]" w)))
        (z0 (coerce 0 type)) (one (coerce 1 type)))
    `(progn (export ',exportname)
       (def* ,exportname (a x y z)
         (declare #.*opt* (,type a x y z))
         "make 3d rotation matrix for rotating a rad around unit vector (x y z)"
         (let* ((coa (cos a)) (sia (sin a)) (1-coa (- ,one coa)))
           (declare (,type coa sia 1-coa))
           (f_ (list (+ coa (* (* x x) 1-coa))
                     (- (* x y 1-coa) (* z sia))
                     (+ (* x z 1-coa) (* y sia)) ,@(if w `(,z0))

                     (+ (* y x 1-coa) (* z sia))
                     (+ coa (* (* y y) 1-coa))
                     (- (* y z 1-coa) (* x sia)) ,@(if w `(,z0))

                     (- (* z x 1-coa) (* y sia))
                     (+ (* z y 1-coa) (* x sia))
                     (+ coa (* (* z z) 1-coa))
                     ,@(if w `(,z0 ,z0 ,z0 ,z0 ,one)))))))))
(make-3mrot ff) (make-3mrot ff t) (make-3mrot df) (make-3mrot df t)

