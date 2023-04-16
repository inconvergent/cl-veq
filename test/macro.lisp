
(in-package #:veq-tests)

(plan 3)

(subtest "macro" (veq:vprogn
  (labels ((fx1 (h (:va 3 a)) (declare (ignore h))
             (veq:f3+ a 1f0 2f0 0f0)))
    (is (veq:lst (fx1 0f0 1f0 1f0 2f0)) '(2f0 3f0 2f0)))

  (veq:mvb (x (veq:varg 2 a)) (veq:f3 4f0 3f0 1f0)
    (declare (ignore x))
    (veq:mvb ((veq:varg 2 a) y) (veq:f3 8f0 5f0 17f0)
      (declare (ignore y))
      (is (veq:lst a) '(8f0 5f0)))
    (is (veq:lst a) '(3f0 1f0)))

  (veq:mvb ((veq:varg 3 a)) (veq:f3 1f0 2f0 3f0)
    (is (veq:lst (veq:vref a 2 0 1)) '(3f0 1f0 2f0)))

  (veq:f3let ((a (veq:f3 1f0 2f0 3f0)))
    (is (veq:lst a) '(1f0 2f0 3f0))
    (is (:vr a 2) 3f0)
    (is (list (:vref a 0 1)) (list 1f0 2f0)))

  (veq:fvlet ((a 3 (veq:f3 1f0 2f0 3f0))
              (b 2 (veq:f2 5f0 -1f0)))
    (is (veq:lst a b) '(1f0 2f0 3f0 5f0 -1f0)))

  (let ((a 1f0))
    (is (veq:lst (veq:f2rep (incf a))) '(2f0 3f0))
    (is (veq:lst (veq:f3rep (incf a))) '(4f0 5f0 6f0))
    (is (veq:lst (veq:f2val (incf a))) '(7f0 7f0)))

  (veq:f2let ((a (veq:f2 1f0 2f0))
              (b (veq:f2 10 20))
              (a (veq:f2+ a b)))

    (is (veq:lst a) (list 11f0 22f0))
    (is (veq:lst b) (list 10f0 20f0)))))

(subtest "xlet" (veq:fvprogn
  (veq:xlet ((d2!a (veq:f2 1f0 2f0))
             (b :kkkk)
             (x 7d0)
             (2!dd (values :a :b))
             (i8!c (values 1 2 3 4 5 6 7 8))
             (f2!hh (veq:f2 11f0 12f0))
             (f3!gg (veq:~ hh 99f0))
             (f!uu 13f0))
     (declare (veq:ff a) (symbol b) (keyword dd))
     (is (veq:lst a b x dd c hh uu gg)
         '(1.0 2.0 :KKKK 7.0d0 :A :B 1 2 3 4 5 6 7 8
           11.0 12.0 13.0 11.0 12.0 99.0)))))

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

  (is-arr (i!@$+  (veq:i$line 1 2) 3) #(4 5))
  (is-arr (i!@$+. (veq:i$line 1 2) 3) #(4 5))
  (is-arr (i!@$+.. (veq:i$line 1 2) 3 4) #(8 9))

  (is-arr (i2!@$+ (veq:i2$line 1 2 3 4) 5 6) #(6 8 8 10))
  (is-arr (i2!@$+. (veq:i2$line 1 2 3 4) 5) #(6 7 8 9))
  (is-arr (i2!@$+.. (veq:i2$line 1 2 3 4) 5 6) #(12 13 14 15))

  (is-arr (i3!@$+ (veq:i3$line 1 2 3 4 5 6) 7 8 9) #(8 10 12 11 13 15))
  (is-arr (i3!@$+. (veq:i3$line 1 2 3 4 5 6) 7) #(8 9 10 11 12 13))
  (is-arr (i3!@$+.. (veq:i3$line 1 2 3 4 5 6) 7 8) #(16 17 18 19 20 21))

  (is-arr (i_@$abs (veq:i_ '(-1 -2 -3 -4 -5 -6))) #(1 2 3 4 5 6))

  (is-arr (i2.@$abs! (@ (veq:i_ '(-1 -2 -3 -4 -5 -6)) 2)) #(-1 -2 -3 -4 5 6))
  (is-arr (i2.@$abs  (@ (veq:i_ '(-1 -2 -3 -4 -5 -6)) 2)) #(5 6))

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
            #(-88.0d0 -86.0d0 -84.0d0 -85.0d0 -83.0d0 -81.0d0 -82.0d0 -80.0d0 -78.0d0))

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

    ; no ?@ modifiers
    (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
      (is (2!@$+ a (veq:vnrep 2 (incf i))) #(1 2 1 2 1 2 1 2 1 2) :test #'equalp)
      (is a 5zero :test #'equalp)
      (is (2!@$+! a (veq:vnrep 2 (incf i))) #(3 4 3 4 3 4 3 4 3 4) :test #'equalp)
      (is a #(3 4 3 4 3 4 3 4 3 4) :test #'equalp))

    ; ?@ modifiers regular ...
    (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
      (is (2!@$+ a (?@ (veq:vnrep 2 (incf i)))) #(1 2 3 4 5 6 7 8 9 10)  :test #'equalp)
      (is a 5zero :test #'equalp)
      (is (2!@$+! a (?@ (veq:vnrep 2 (incf i)))) #(11 12 13 14 15 16 17 18 19 20) :test #'equalp)
      (is a #(11 12 13 14 15 16 17 18 19 20) :test #'equalp))

    ; right dot
    (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
      (is (2!@$+. a (incf i)) #(1 1 1 1 1 1 1 1 1 1) :test #'equalp) ;=HERER
      (is a 5zero :test #'equalp)
      (is (2!@$+.! a (?@ (incf i))) #(2 2 3 3 4 4 5 5 6 6) :test #'equalp)
      (is a #(2 2 3 3 4 4 5 5 6 6) :test #'equalp))

    ; _ right dot
    (labels ((fx (x y z) (values (+ z x) (+ (* 10 z) y))))

      (let ((a (veq:i2$zero 5)) (i 0) (5zero (veq:i2$zero 5)))
        (is (2_@$fx. a (?@ (incf i))) #(1 10 2 20 3 30 4 40 5 50) :test #'equalp)
        (is a 5zero :test #'equalp)
        (is (2_@$fx.! a (?@ (incf i))) #(6 60 7 70 8 80 9 90 10 100) :test #'equalp)
        (is a #(6 60 7 70 8 80 9 90 10 100) :test #'equalp)
        ))
    )))

(unless (finalize) (error "error in macro tests"))

