
(in-package #:veq-tests)

(plan 4)

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
                       (lambda (i (:va 3 x)) (setf (veq:3$ a i) (values x))))
      (is a #(2.0 -4.0 8.0 2.0 -2.5 6.5 2.0 -1.0 5.0 2.0 0.5 3.5 2.0 2.0 2.0)
          :test #'equalp))

    (let ((a (veq:f$make :dim 3 :n 5 :v 0f0)))
      (veq:f3$fxlspace (5 (veq:f3 2f0 -4f0 8f0) (veq:f3rep 2f0) :end nil)
                       (lambda (i (:va 3 x)) (setf (veq:3$ a i) (values x))))
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
                   '(-10.0 10.0 -10.9 10.088 0.1 0.2)))))

(subtest "shapes"
  (veq:fvprogn
    (is (veq:f2$rect 3f0 4f0) #(3.0 -4.0 3.0 4.0 -3.0 4.0 -3.0 -4.0) :test #'equalp)
    (is (veq:f2$polygon 5 23f0)
        #(23.0 0.0 7.1073904 21.8743 -18.607393 13.519059
          -18.60739 -13.519063 7.1073937 -21.874298)
        :test #'equalp)
    (is (veq:f2$center (veq:f2$polygon 5 23f0))
        #(25.196304 9.536743e-7 9.303694 21.874302 -16.41109 13.51906
          -16.411087 -13.519062 9.303698 -21.874298)
        :test #'equalp)
    (is (veq:f2$circ 4f0)
      #(4.0 0.0 3.541824 1.8588928 2.2722588 3.2919354 0.48214626 3.9708354
        -1.4184198 3.7400649 -2.994043 2.6524906 -3.8837676 0.9572618 -3.8837671
        -0.95726335 -2.9940426 -2.652491 -1.4184183 -3.7400653 0.48214698
        -3.9708354 2.27226 -3.291935 3.541825 -1.8588911)
        :test #'equalp)))


(unless (finalize) (error "error in arr tests"))

