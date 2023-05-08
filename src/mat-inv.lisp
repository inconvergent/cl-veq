
(in-package :veq)

(defmacro make-2minv (type)
  (let ((exportname (vvsym type 2 "minv")))
    `(progn (export ',exportname)
       (defun ,exportname (a)
         (declare #.*opt* (,(arrtype type) a))
         "invert 2x2 matrix. non-destructive."
         (let ((inv (/ (- (* (aref a 0) (aref a 3))
                          (* (aref a 1) (aref a 2))))))
           (declare (,type inv))
           (,(vvsym type 1 :$~) (4)
             (* inv (aref a 3)) (* inv (- (aref a 1)))
             (* inv (- (aref a 2))) (* inv (aref a 0))))))))
(make-2minv ff) (make-2minv df)


(defmacro make-3minv (type)
  (awg (a a00 a01 a02 a10 a11 a12 a20 a21 a22 inv)
    (let ((exportname (vvsym type 3 "minv")))
      `(progn (export ',exportname)
         (defun ,exportname (,a)
           (declare #.*opt* (,(arrtype type) ,a))
           "invert 3x3 matrix. non-destructive."
           (let* ((,a00 (aref ,a 0)) (,a01 (aref ,a 1)) (,a02 (aref ,a 2))
                  (,a10 (aref ,a 3)) (,a11 (aref ,a 4)) (,a12 (aref ,a 5))
                  (,a20 (aref ,a 6)) (,a21 (aref ,a 7)) (,a22 (aref ,a 8))
                  (,inv (/ (+ (- (* ,a00 (- (* ,a11 ,a22) (* ,a21 ,a12)))
                                 (* ,a01 (- (* ,a10 ,a22) (* ,a12 ,a20))))
                                 (* ,a02 (- (* ,a10 ,a21) (* ,a11 ,a20)))))))
             (declare (,type ,inv ,a00 ,a01 ,a02 ,a10 ,a11 ,a12 ,a20 ,a21 ,a22 ))
             (,(vvsym type 1 :$~) (9)
               (* (- (* ,a11 ,a22) (* ,a21 ,a12)) ,inv)
               (* (- (* ,a02 ,a21) (* ,a01 ,a22)) ,inv)
               (* (- (* ,a01 ,a12) (* ,a02 ,a11)) ,inv)
               (* (- (* ,a12 ,a20) (* ,a10 ,a22)) ,inv)
               (* (- (* ,a00 ,a22) (* ,a02 ,a20)) ,inv)
               (* (- (* ,a10 ,a02) (* ,a00 ,a12)) ,inv)
               (* (- (* ,a10 ,a21) (* ,a20 ,a11)) ,inv)
               (* (- (* ,a20 ,a01) (* ,a00 ,a21)) ,inv)
               (* (- (* ,a00 ,a11) (* ,a10 ,a01)) ,inv))))))))
(make-3minv ff) (make-3minv df)


; index hell has been shamelessly copied from:
; https://github.com/Shinmera/3d-matrices/blob/master/ops.lisp
(defmacro make-4minv (type)
  (awg (a a00 a01 a02 a03 a10 a11 a12 a13 a20 a21 a22 a23 a30 a31 a32 a33 inv)
    (let ((exportname (vvsym type 4 "minv")))
      `(progn (export ',exportname)
         (defun ,exportname (,a)
           (declare #.*opt* (,(arrtype type) ,a))
           "invert 4x4 matrix. non-destructive."
           (let* ((,a00 (aref ,a 0)) (,a01 (aref ,a 1)) (,a02 (aref ,a 2)) (,a03 (aref ,a 3))
                  (,a10 (aref ,a 4)) (,a11 (aref ,a 5)) (,a12 (aref ,a 6)) (,a13 (aref ,a 7))
                  (,a20 (aref ,a 8)) (,a21 (aref ,a 9)) (,a22 (aref ,a 10)) (,a23 (aref ,a 11))
                  (,a30 (aref ,a 12)) (,a31 (aref ,a 13)) (,a32 (aref ,a 14)) (,a33 (aref ,a 15))
                  (,inv (/ (- (+ (* ,a03 ,a12 ,a21 ,a30) (* ,a00 ,a11 ,a22 ,a33)
                                 (* ,a01 ,a13 ,a22 ,a30) (* ,a02 ,a11 ,a23 ,a30)
                                 (* ,a02 ,a13 ,a20 ,a31) (* ,a03 ,a10 ,a22 ,a31)
                                 (* ,a00 ,a12 ,a23 ,a31) (* ,a03 ,a11 ,a20 ,a32)
                                 (* ,a00 ,a13 ,a21 ,a32) (* ,a01 ,a10 ,a23 ,a32)
                                 (* ,a01 ,a12 ,a20 ,a33) (* ,a02 ,a10 ,a21 ,a33))
                              (+ (* ,a02 ,a13 ,a21 ,a30) (* ,a03 ,a11 ,a22 ,a30)
                                 (* ,a01 ,a12 ,a23 ,a30) (* ,a03 ,a12 ,a20 ,a31)
                                 (* ,a00 ,a13 ,a22 ,a31) (* ,a02 ,a10 ,a23 ,a31)
                                 (* ,a01 ,a13 ,a20 ,a32) (* ,a03 ,a10 ,a21 ,a32)
                                 (* ,a00 ,a11 ,a23 ,a32) (* ,a02 ,a11 ,a20 ,a33)
                                 (* ,a00 ,a12 ,a21 ,a33) (* ,a01 ,a10 ,a22 ,a33))))))
             (declare (,type ,inv ,a00 ,a01 ,a02 ,a03 ,a10 ,a11 ,a12 ,a13
                                  ,a20 ,a21 ,a22 ,a23 ,a30 ,a31 ,a32 ,a33))
             (,(vvsym type 1 :$~) (16)
               (* ,inv (- (+ (* ,a12 ,a23 ,a31) (* ,a13 ,a21 ,a32) (* ,a11 ,a22 ,a33))
                          (+ (* ,a13 ,a22 ,a31) (* ,a11 ,a23 ,a32) (* ,a12 ,a21 ,a33))))
               (* ,inv (- (+ (* ,a03 ,a22 ,a31) (* ,a01 ,a23 ,a32) (* ,a02 ,a21 ,a33))
                          (+ (* ,a02 ,a23 ,a31) (* ,a03 ,a21 ,a32) (* ,a01 ,a22 ,a33))))
               (* ,inv (- (+ (* ,a02 ,a13 ,a31) (* ,a03 ,a11 ,a32) (* ,a01 ,a12 ,a33))
                          (+ (* ,a03 ,a12 ,a31) (* ,a01 ,a13 ,a32) (* ,a02 ,a11 ,a33))))
               (* ,inv (- (+ (* ,a03 ,a12 ,a21) (* ,a01 ,a13 ,a22) (* ,a02 ,a11 ,a23))
                          (+ (* ,a02 ,a13 ,a21) (* ,a03 ,a11 ,a22) (* ,a01 ,a12 ,a23))))
               (* ,inv (- (+ (* ,a13 ,a22 ,a30) (* ,a10 ,a23 ,a32) (* ,a12 ,a20 ,a33))
                          (+ (* ,a12 ,a23 ,a30) (* ,a13 ,a20 ,a32) (* ,a10 ,a22 ,a33))))
               (* ,inv (- (+ (* ,a02 ,a23 ,a30) (* ,a03 ,a20 ,a32) (* ,a00 ,a22 ,a33))
                          (+ (* ,a03 ,a22 ,a30) (* ,a00 ,a23 ,a32) (* ,a02 ,a20 ,a33))))
               (* ,inv (- (+ (* ,a03 ,a12 ,a30) (* ,a00 ,a13 ,a32) (* ,a02 ,a10 ,a33))
                          (+ (* ,a02 ,a13 ,a30) (* ,a03 ,a10 ,a32) (* ,a00 ,a12 ,a33))))
               (* ,inv (- (+ (* ,a02 ,a13 ,a20) (* ,a03 ,a10 ,a22) (* ,a00 ,a12 ,a23))
                          (+ (* ,a03 ,a12 ,a20) (* ,a00 ,a13 ,a22) (* ,a02 ,a10 ,a23))))
               (* ,inv (- (+ (* ,a11 ,a23 ,a30) (* ,a13 ,a20 ,a31) (* ,a10 ,a21 ,a33))
                          (+ (* ,a13 ,a21 ,a30) (* ,a10 ,a23 ,a31) (* ,a11 ,a20 ,a33))))
               (* ,inv (- (+ (* ,a03 ,a21 ,a30) (* ,a00 ,a23 ,a31) (* ,a01 ,a20 ,a33))
                          (+ (* ,a01 ,a23 ,a30) (* ,a03 ,a20 ,a31) (* ,a00 ,a21 ,a33))))
               (* ,inv (- (+ (* ,a01 ,a13 ,a30) (* ,a03 ,a10 ,a31) (* ,a00 ,a11 ,a33))
                          (+ (* ,a03 ,a11 ,a30) (* ,a00 ,a13 ,a31) (* ,a01 ,a10 ,a33))))
               (* ,inv (- (+ (* ,a03 ,a11 ,a20) (* ,a00 ,a13 ,a21) (* ,a01 ,a10 ,a23))
                          (+ (* ,a01 ,a13 ,a20) (* ,a03 ,a10 ,a21) (* ,a00 ,a11 ,a23))))
               (* ,inv (- (+ (* ,a12 ,a21 ,a30) (* ,a10 ,a22 ,a31) (* ,a11 ,a20 ,a32))
                          (+ (* ,a11 ,a22 ,a30) (* ,a12 ,a20 ,a31) (* ,a10 ,a21 ,a32))))
               (* ,inv (- (+ (* ,a01 ,a22 ,a30) (* ,a02 ,a20 ,a31) (* ,a00 ,a21 ,a32))
                          (+ (* ,a02 ,a21 ,a30) (* ,a00 ,a22 ,a31) (* ,a01 ,a20 ,a32))))
               (* ,inv (- (+ (* ,a02 ,a11 ,a30) (* ,a00 ,a12 ,a31) (* ,a01 ,a10 ,a32))
                          (+ (* ,a01 ,a12 ,a30) (* ,a02 ,a10 ,a31) (* ,a00 ,a11 ,a32))))
               (* ,inv (- (+ (* ,a01 ,a12 ,a20) (* ,a02 ,a10 ,a21) (* ,a00 ,a11 ,a22))
                          (+ (* ,a02 ,a11 ,a20) (* ,a00 ,a12 ,a21) (* ,a01 ,a10 ,a22)))))))))) )
(make-4minv ff) (make-4minv df)

