
(in-package #:veq-tests)

(plan 2)

(subtest "for all rows"
  (veq:fvprogn

    (let* ((n 4) (a (veq:f3$zero n)) (b (veq:f3$zero n)) (c (veq:f3$zero n)))
      (loop for i from 0 below n
            do (veq:3$vset (a i) (veq:f3~ (+ i 10) (1+ i) (* 2 i)))
               (veq:3$vset (b i) (veq:f3~ (- i) (+ 3 i) (/ (+ i 3) 2))))

      (labels ((cross (i (veq:varg 3 v w))
                 (veq:3$vset (c i) (veq:f3cross v w))))
        (veq:f3$with-rows (n a b) cross))
        ; TODO: make a test based on error in with-rows
        ; (veq:f3$with-rows (n a c)
        ;   (lambda (i (veq:varg 3 v w))
        ;     (veq:3$vset (c i) (veq:f3+ v w))))

      (is c #(1.5 -15.0 30.0 -4.0 -24.0 46.0 -12.5 -38.0 66.0 -24.0 -57.0 90.0)
          :test #'equalp)
      (is (veq:3$to-list c)
          '((1.5 -15.0 30.0) (-4.0 -24.0 46.0)
            (-12.5 -38.0 66.0) (-24.0 -57.0 90.0))))))

(subtest "with arrays"
  (veq:fvprogn

    (veq:fwith-arrays (:n 7 :itr k
      :arr ((a 3 (veq:f3$one 7)) (b 3) (c 3))
      :fxs ((cross ((veq:varg 3 v w)) (veq:f3cross v w))
            (init1 (i) (veq:f3~ (1+ i) (* 2 i) (+ 2 i)))
            (init2 (i) (veq:f3~ (+ 2 i) (1+ i) (* 2 i))))
      :exs ((a k (init1 k))
            (b k (init2 k))
            (c k (cross a b))))
      (is c #(-2.0 4.0 1.0 -2.0 5.0 -2.0 4.0 4.0 -7.0 16.0 1.0 -14.0 34.0 -4.0
              -23.0 58.0 -11.0 -34.0 88.0 -20.0 -47.0)
          :test #'equalp))

    (veq:fwith-arrays (:n 7 :itr k
      :arr ((two 2) (three 3))
      :fxs ((init-three (i) (veq:f3~ (+ 2 i) (1+ i) (+ 100 (* 2 i))))
            (three-to-two ((:varg 3 v))
                            (declare (ignore (veq:vref v 1)))
                            (veq:f2 (veq:vref v 0 2))))
      :exs ((three k (init-three k))
            (two k (three-to-two three))))
      (is two
          #(2.0 100.0 3.0 102.0 4.0 104.0 5.0 106.0 6.0 108.0 7.0 110.0 8.0
            112.0)
          :test #'equalp))

    (veq:fwith-arrays (:n 4 :itr k :cnt c :start 2
      :arr ((aa 3 (veq:f3$zero 4)))
      :fxs ((init (i j) (veq:f3~ (+ 2 i) (+ 1000 j) (+ 100 (* 2 i)))))
      :exs ((aa k (init c k))))
      (is aa
          #(0.0 0.0 0.0 0.0 0.0 0.0 2.0 1002.0 100.0 3.0 1003.0 102.0)
          :test #'equalp))))

(unless (finalize) (error "error in arr-with tests"))

