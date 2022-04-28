
(in-package #:veq-tests)

(plan 6)

(subtest "array macro"
  (veq:fvprogn
    (is (veq:d2$zero 2) #(0d0 0d0 0d0 0d0) :test #'equalp)
    (is (veq:d2$zero) #(0d0 0d0) :test #'equalp)
    (is (veq:f2$one 2) #(1f0 1f0 1f0 1f0) :test #'equalp)

    (let ((a (veq:d2$point 100.2d0 2d0)))
      (is a #(100.2d0 2d0) :test #'equalp)
      (is (veq:lst (veq:d2+ (veq:d2$ a) 2d0 3d0)) '(102.2d0 5d0)))

    (let ((a (veq:f2$point 100.2f0 2f0)))
      (is a #(100.2f0 2f0) :test #'equalp)
      (is (veq:lst (veq:f2+ (veq:f2$ a) 2f0 3f0)) '(102.2f0 5f0)))

    (is (veq:d$_ (loop for i from 0 below 3
                      collect (list (veq:df i)
                                    (veq:df (1+ i)))))
        #(0d0 1d0 1d0 2d0 2d0 3d0)
        :test #'equalp)

    (is (veq:d_ '(1d0 2d0 3d0 4d0)) #(1d0 2d0 3d0 4d0) :test #'equalp)
    (let ((a (list 1d0 2d0))) (is (veq:d_ a) #(1d0 2d0) :test #'equalp))))


(subtest "lerp"
  (veq:fvprogn

    (is (veq:f3$lspace 5 (veq:f3 1f0 2f0 -1f0) (veq:f3rep 2f0))
        #(1.0 2.0 -1.0 1.25 2.0 -0.25 1.5 2.0 0.5 1.75 2.0 1.25 2.0 2.0 2.0)
        :test #'equalp)

    (let ((a (veq:f$make :dim 3 :n 5 :v 0f0)))
      (veq:f3$fxlspace (5 (veq:f3 2f0 -4f0 8f0) (veq:f3rep 2f0))
                       (lambda (i x y z) (veq:3$vset (a i) (values x y z))))
      (is a #(2.0 -4.0 8.0 2.0 -2.5 6.5 2.0 -1.0 5.0 2.0 0.5 3.5 2.0 2.0 2.0)
          :test #'equalp))

    (let ((a (veq:f$make :dim 3 :n 5 :v 0f0)))
      (veq:f3$fxlspace (5 (veq:f3 2f0 -4f0 8f0) (veq:f3rep 2f0) :end nil)
                       (lambda (i (:va 3 x)) (veq:3$vset (a i) (values x))))
      (is a #(2.0 -4.0 8.0 2.0 -2.8 6.8 2.0 -1.5999999 5.6 2.0 -0.39999986
              4.3999996 2.0 0.8000002 3.1999998)
          :test #'equalp))))


(subtest "mima"
  (veq:fvprogn

    (let ((a (veq:f_ '(-1f0 2f0 3f0 4f0 5f0 8f0)))
          (b (veq:d_ '(-1d0 2d0 3d0 4d0 5d0 8d0))))

      (is (veq:lst (veq:f$mima a :n 6)) '(-1.0 8.0))
      (is (veq:lst (veq:d$mima b :n 6)) '(-1d0 8d0))
      (is (veq:lst (veq:f2$mima a)) '(-1.0 5.0 2.0 8.0))
      (is (veq:lst (veq:d2$mima b :n 3)) '(-1d0 5d0 2d0 8d0))
      (is (veq:lst (veq:f3$mima a)) '(-1.0 4.0 2.0 5.0 3.0 8.0))
      (is (veq:lst (veq:d3$mima b)) '(-1.0d0 4.0d0 2.0d0 5.0d0 3.0d0 8.0d0))

      (is (veq:lst (veq:f3$mima (veq:f_ '( 0.1    10.088 0.1
                                           -10.0 -10.9   0.1
                                           10.0  -10.0   0.2
                                           0f0 0f0 0f0)) :n 3))
                   '(-10.0 10.0 -10.9 10.088 0.1 0.2))



      )))

(subtest "take"
  (veq:fvprogn
    (let ((a (veq:f$_ '((1f0 2f0) (3f0 4f0) (5f0 6f0)
                        (7f0 7f0) (8f0 9f0) (10f0 11f0)))))
      (is (veq:f2$take a (list 2 4 5)) #(5.0 6.0 8.0 9.0 10.0 11.0)
          :test #'equalp))))

(subtest "reduce"
  (is (veq:lst (veq:f3$sum (veq:f_ '(1f0 2f0 3f0 3f0 2f0 1f0))))
      '(4.0 4.0 4.0))
  (is (veq:f$sum (veq:f_ '(1f0 2f0 3f0 3f0 2f0 1f0))) 12.0))


(subtest "broadcast"
  (veq:fvprogn

    (let ((z (veq:f2$zero 3)))
      (is (veq:f2$+ z 3f0 1f0)
          #(3f0 1f0 3f0 1f0 3f0 1f0) :test #'equalp)
      (is z #(0f0 0f0 0f0 0f0 0f0 0f0) :test #'equalp))

    (let ((z (veq:f2$zero 3)))
      (is (veq:f2$+! z 3f0 1f0)
          #(3f0 1f0 3f0 1f0 3f0 1f0) :test #'equalp)
      (is z #(3f0 1f0 3f0 1f0 3f0 1f0) :test #'equalp))

    (is (veq:f2$len (veq:f2$+ (veq:f2$zero 3) 3f0 1f0))
        #(3.1622777 3.1622777 3.1622777)
        :test #'equalp)

    (is (veq:f2$+ (veq:f2$zero 3) (veq:f2rep 3f0))
        #(3f0 3f0 3f0 3f0 3f0 3f0)
        :test #'equalp)

    (is (veq:f2$- (veq:f2$zero 3) (veq:f2rep 3f0))
        #(-3f0 -3f0 -3f0 -3f0 -3f0 -3f0)
        :test #'equalp)

    (is (veq:f$cos-sin (veq:f$lspace 4 0f0 veq:fpii))
        #(1.0 0.0 -0.50000006 0.8660254 -0.4999999 -0.86602545 1.0
          1.7484555e-7)
        :test #'equalp)))

(unless (finalize) (error "error in arr tests"))

