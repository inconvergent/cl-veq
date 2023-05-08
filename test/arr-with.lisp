
(in-package #:veq-tests)

(plan 2)

(subtest "for all rows"
  (veq:fvprogn

    (let* ((n 4) (a (veq:f3$zero n)) (b (veq:f3$zero n)) (c (veq:f3$zero n)))
      (loop for i from 0 below n
            do (setf (veq:3$ a i) (veq:f3~ (+ i 10) (1+ i) (* 2 i))
                     (veq:3$ b i) (veq:f3~ (- i) (+ 3 i) (/ (+ i 3) 2))))
      (f3%@$fx a ((i (:va 3 v))
                  (setf (veq:3$ c i) (veq:f3cross v (veq:f3$ b i)))))

      (is c #(1.5 -15.0 30.0 -4.0 -24.0 46.0 -12.5 -38.0 66.0 -24.0 -57.0 90.0)
          :test #'equalp)
      (is (veq:3$to-list c)
          '((1.5 -15.0 30.0) (-4.0 -24.0 46.0)
            (-12.5 -38.0 66.0) (-24.0 -57.0 90.0))))))


(unless (finalize) (error "error in arr-with tests"))

