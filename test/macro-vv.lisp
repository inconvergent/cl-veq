(in-package #:veq-tests)

(plan 3)

(subtest "vv" (veq:fvprogn

  (is (i!@+ 1 2) 3)
  (is (i!@+. 1 2) 3)
  (is (i!@+.. 1 2 3) 6)
  (is (i!@+... 1 2 3 4) 10)

  (is (veq:lst (i2!@+ 1 2 3 4)) '(4 6))
  (is (veq:lst (i2!@+. 1 2 3)) '(4 5))
  (is (veq:lst (i2!@.+ 1 2 3)) '(3 4))
  (is (veq:lst (i2!@+.. 1 2 3 4)) '(8 9))
  (is (veq:lst (i2!@..+ 1 2 3 4)) '(6 7))
  (is (veq:lst (i2!@+... 1 2 3 4 5)) '(13 14))
  (is (veq:lst (i2!@...+ 1 2 3 4 5)) '(10 11))

  (is (veq:lst (i3!@+ 1 2 3 4 5 6)) '(5 7 9))
  (is (veq:lst (i3!@+. 1 2 3 4)) '(5 6 7))
  (is (veq:lst (i3!@.+ 1 2 3 4)) '(3 4 5))
  (is (veq:lst (i3!@+.. 1 2 3 4 5)) '(10 11 12))
  (is (veq:lst (i3!@..+ 1 2 3 4 5)) '(6 7 8))

  (is (veq:lst (i3!@+... 1 2 3 4 5 6)) '(16 17 18))
  (is (veq:lst (i3!@...+ 1 2 3 4 5 6)) '(10 11 12))

  (is (veq:lst (i2.@abs -2 -3)) '(2 3))
  (is (veq:lst (i3.@abs -2 -3 -4 -5)) '(2 3 4))

  (is (4_@list 6 7 9 :xx) '(6 7 9 :xx))
  (is (3_@list 6 7 9 :yy) '(6 7 9))

  (is-arr (i!@$+ (veq:i$line 1 2) 3) #(4 5))
  (is-arr (i!@$+. (veq:i$line 1 2) 3) #(4 5))
  (is-arr (i!@$+.. (veq:i$line 1 2) 3 4) #(8 9))

  (is-arr (i2!@$+ (veq:i2$line 1 2 3 4) 5 6) #(6 8 8 10))
  (is-arr (i2!@$+. (veq:i2$line 1 2 3 4) 5) #(6 7 8 9))
  (is-arr (i2!@$+.. (veq:i2$line 1 2 3 4) 5 6) #(12 13 14 15))

  (is-arr (i3!@$+ (veq:i3$line 1 2 3 4 5 6) 7 8 9) #(8 10 12 11 13 15))
  (is-arr (i3!@$+. (veq:i3$line 1 2 3 4 5 6) 7) #(8 9 10 11 12 13))
  (is-arr (i3!@$+.. (veq:i3$line 1 2 3 4 5 6) 7 8) #(16 17 18 19 20 21))

  (is-arr (i_@$abs (veq:i_ '(-1 -2 -3 -4 -5 -6))) #(1 2 3 4 5 6))

  (is-arr (i2.@$abs! (?@ (veq:i_ '(-1 -2 -3 -4 -5 -6)) 2)) #(-1 -2 -3 -4 5 6))
  (is-arr (i2.@$abs  (?@ (veq:i_ '(-1 -2 -3 -4 -5 -6)) 2)) #(5 6))
  (is-arr (i2.@$abs  (?@ (i2!@$+ (veq:i_ '(-1 -2 -3 -4 -5 -6)) 100 1000) 2)) #(95 994))

  (is (veq:lst (veq:xlet ((f3!res (veq:f3 1f0 2f0 3f0)))
                 (f3!@expt. (f3!@./ 1f0 (f3!@+ res 4f0 7f0 9f0)) 2f0)))
      '(0.040000003 0.012345679 0.006944445))

  (is (veq:lst (veq:xlet ((f3!res (veq:f3 1f0 2f0 3f0)))
                 (f3!@expt. (f3!@/. (f3!@+ res 4f0 7f0 9f0) 2f0) 2f0)))
      '(6.25 20.25 36.0))

  (is (f!@+ (f2!@* 1f0 2f0 (values 3f0 4f0))) 11f0)
  (is (f!@+ (f2!@* 1f0 (values 2f0 3f0) 4f0 :xxx)) 11f0)

  (veq:xlet ((v1 (veq:f_ '(1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0)))
             (v2 (veq:f_ '(11f0 12f0 13f0 14f0 15f0 16f0 17f0 18f0)))
             (v3 (veq:d_ '(21d0 22d0 23d0 24d0 25d0 26d0 27d0 28d0 29d0))))

    (is-arr (f2!@$expt. v1 2f0) #(1.0 4.0 9.0 16.0 25.0 36.0 49.0 64.0))
    (is-arr v1 #(1f0 2f0 3f0 4f0 5f0 6f0 7f0 8f0))

    (is-arr (f2!@$+ v2 (f2!@+. 1f0 2f0 3f0)) #(15.0 17.0 17.0 19.0 19.0 21.0 21.0 23.0))
    (is-arr v2 #(11f0 12f0 13f0 14f0 15f0 16f0 17f0 18f0))

    (is-arr (f2!@$expt.! v1 2f0) #(1.0 4.0 9.0 16.0 25.0 36.0 49.0 64.0))
    (is-arr v1 #(1.0 4.0 9.0 16.0 25.0 36.0 49.0 64.0))

    (is-arr (f2!@$+! v2 (f2!@+. 1f0 2f0 3f0)) #(15.0 17.0 17.0 19.0 19.0 21.0 21.0 23.0))
    (is-arr v2 #(15.0 17.0 17.0 19.0 19.0 21.0 21.0 23.0))

    (is-arr (d3!@$+ v3 (d3!@+. 1d0 2d0 3d0 -10d0))
            #(12d0 14d0 16d0 15d0 17d0 19d0 18d0 20d0 22d0))

    (is-arr (d3!@$+ v3 (d3!@+.. 1d0 2d0 3d0 -10d0 -100d0))
            #(-88.0d0 -86.0d0 -84.0d0 -85.0d0 -83.0d0 -81.0d0 -82.0d0 -80.0d0 -78.0d0)))

    ; no ?@ modifiers
  (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
    (is-arr (2!@$+ a (veq:vnrep 2 (incf i))) #(1 2 1 2 1 2 1 2 1 2))
    (is-arr a 5zero)
    (is-arr (2!@$+! a (veq:vnrep 2 (incf i))) #(3 4 3 4 3 4 3 4 3 4))
    (is-arr a #(3 4 3 4 3 4 3 4 3 4))
    (is-arr (2!@$+$ a a) #(6 8 6 8 6 8 6 8 6 8)))

  ; ?@ modifiers regular ...
  (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
    (is-arr (2!@$+ a (?@ (veq:vnrep 2 (incf i)))) #(1 2 3 4 5 6 7 8 9 10))
    (is-arr a 5zero)
    (is-arr (2!@$+! a (?@ (veq:vnrep 2 (incf i)))) #(11 12 13 14 15 16 17 18 19 20))
    (is-arr a #(11 12 13 14 15 16 17 18 19 20))

    ; slice both sides
    (is-arr (2!@$+$! (?@ (veq:f_ '(1f0 2f0 3f0 4f0 5f0 6f0)) 2)
                     (?@ (veq:f_ '(10f0 20f0 30f0 40f0 50f0 60f0)) 0))
            #(1.0 2.0 3.0 4.0 15.0 26.0))
    (is-arr (2!@$+$! (?@ (veq:f_ '(1f0 2f0 3f0 4f0 5f0 6f0)) 0 1)
                     (?@ (veq:f_ '(10f0 20f0 30f0 40f0 50f0 60f0)) 2))
            #(51.0 62.0 3.0 4.0 5.0 6.0)))

  ; right dot
  (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
    (is-arr (2!@$+. a (incf i)) #(1 1 1 1 1 1 1 1 1 1))
    (is-arr a 5zero)
    (is-arr (2!@$+.! a (?@ (incf i))) #(2 2 3 3 4 4 5 5 6 6))
    (is-arr a #(2 2 3 3 4 4 5 5 6 6)))))

(subtest "vv 2" (veq:fvprogn

  (let ((sqrt2/2 (/ (sqrt 2f0) 2f0))
        (pts #((7 2) (3 4))))
    (labels ((r1 (xy) (veq:dsb (x y) xy (list y (- x))))
             (r2 (xy) (veq:dsb (x y) xy (list (- x) (- y))))
             (r45 (xy) (veq:dsb (x y) xy `(,(round (* (- x y) sqrt2/2))
                                           ,(round (* (+ x y) sqrt2/2)))))
              (rn (xy &optional (n 0))
                (veq:dsb (x y) xy (loop repeat n for (x* y*) = (r45 (list x y))
                                        do (setf x x* y y* ))
                                  (list x y))))
      (is-arr (_@$r1 pts) #((2 -7) (4 -3)))
      (is-arr (l_@$r2 pts) #((-7 -2) (-3 -4)))
      (is-arr (_@$rn. pts 3) #((-6 4) (-5 -1)))))

  (labels ((fx (x y z) (values (+ z x) (+ (* 10 z) y))))
    (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
      (is-arr (2_@$fx. a (?@ (incf i))) #(1 10 2 20 3 30 4 40 5 50))
      (is-arr a 5zero)
      (is-arr (2_@$fx.! a (?@ (incf i))) #(6 60 7 70 8 80 9 90 10 100))
      (is-arr a #(6 60 7 70 8 80 9 90 10 100))
      (is-arr (2_@$fx. a (o?@ o)) #(6 60 8 80 10 100 12 120 14 140))
      (is-arr (2_@$fx. a (ioioo?@ ioioo)) #(6 60 8 80 10 100 12 120 14 140))))

  (labels ((proj (x y z) (values z y x)))
    (is-arr (32_@$proj (veq:i3$line 1 2 3 4 5 6)) #(3 2 6 5)))
  (labels ((fx (x y) (values (+ x y))))
    (is-arr (31!@$fx (veq:f3$line 1f0 2f0 3f0 4f0 5f0 6f0) 10f0 20f0 30f0)
            #(11.0 14.0)))

  (is-arr (veq::12_@$fcos-sin (veq:f2$point veq:fpi veq:fpi5))
          #(-1.0 -8.742278e-8 -4.371139e-8 1.0))

  (is (veq::31_@$f3len (veq:f3$line 1f0 2f0 3f0 4f0 1f0 7f0))
      #(3.7416575 8.124039)
      :test #'equalp)))

(subtest "vv 3" (veq:fvprogn
  (veq:fvprogn
    (let ((a (veq:f_ '(1f0 2f0 3f0 4f0 5f0 6f0 7f0 7f0 8f0 9f0 10f0 11f0))))
      (is-arr (veq::f2_@$f2id (l?@ a (list 2 4 5))) #(5.0 6.0 8.0 9.0 10.0 11.0))

      (is-arr (f2!@$+$ (l?@ a '(2 3 4)) (l?@ a '(1 2 3)))
              #(8.0 10.0 12.0 13.0 15.0 16.0))

      (is-arr (f2!@$+$ (l?@ a '(1 2 3)) (?@ a 1))
              #(6.0 8.0 10.0 12.0 14.0 14.0))

      (is-arr (f2!@$+$! (l?@ a '(1 2 3)) (l?@ a '(3 2 1)))
              #(1.0 2.0 10.0 11.0 10.0 12.0 17.0 18.0 8.0 9.0 10.0 11.0))

      (is-arr (let ((i 0.2))
                (f2!@$+.! (?@ (veq:f2$zero 5) 1) (?@ (incf i 0.1))))
              #(0.0 0.0 0.3 0.3 0.4 0.4 0.5 0.5 0.6 0.6))

      (is-arr (labels ((fx (x y) (values x y)))
                (2_@$fx (i?@ (veq:i_ (list 1 2 3 4 5 6 7 8 9 10))
                             (veq:i_ (list 1 2 1 4)))))
              #(3 4 5 6 3 4 9 10))

      (is-arr (labels ((fx (x y) (values x y)))
                (2_@$fx (v?@ (veq:i_ (list 1 2 3 4 5 6 7 8 9 10))
                             #(1 2 1 4))))
              #(3 4 5 6 3 4 9 10))

      (is (veq:lst (i2r@$+ (?@ (veq:i$_ (loop for v from 0 below 10 by 2
                                              collect (list v (1+ v))))
                               2)))
          '(18 21))
      (is (veq:lst (f3r@$+ (veq:f_ '(1f0 2f0 3f0 3f0 2f0 11f0)))) '(4.0 4.0 14.0))
      (is (veq:lst (fr@$+ (veq:f_ '(1f0 2f0 3f0 3f0 2f0 1f0)))) '(12.0))))))

(subtest "vv 3" (veq:fvprogn
  (let ((res (list)))
     (is-arr (2%@$fx #(1 2 3 4) ((i x y) (push (list i :xy x y) res)))
             #(((0 :XY 1 2)) NIL ((1 :XY 3 4) (0 :XY 1 2)) NIL))

     (is res '((1 :XY 3 4) (0 :XY 1 2)))

     (is-arr (2x@$fx #(1 2 3 4) ((i x y) (push (list i :xy x y) res)))
             nil))

  (is (2%@fx 1 2 ((x y) (list :xy x y))) (list :xy 1 2))
  (is (2%@fx 1 2 (((:va 2 x)) (list :xy x))) (list :xy 1 2))
  (is (2%@fx (2!@+ 1 2 3 4) ((x y) (list :xy x y))) (list :xy 4 6))

  ))

(unless (finalize) (error "error in vv tests"))

