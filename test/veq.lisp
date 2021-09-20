

(in-package #:veq-tests)

(plan 12)

(subtest "2d double ops"
  (veq:vprogn

    (is (veq:lst (veq:d2+ 1d0 2d0 (veq:d2< 1d0 3d0))) '(2d0 5d0))
    (is (veq:lst (veq:d2* 1d0 2d0 (veq:d2< 1d0 3d0))) '(1d0 6d0))
    (is (veq:lst (veq:d2- 1d0 2d0 (veq:d2< 1d0 3d0))) '(0d0 -1d0))
    (is (veq:lst (veq:d2/ 1d0 2d0 (veq:d2< 1d0 3d0))) `(1d0 ,(/ 2d0 3d0)))
    (is (veq:lst (veq:d2i- 1d0 2d0 (veq:d2< 1d0 3d0))) '(0d0 1d0))
    (is (veq:lst (veq:d2i/ 1d0 2d0 (veq:d2< 1d0 3d0))) `(1d0 ,(/ 3d0 2d0)))

    (is (veq:lst (veq:d2^ (veq:d2< 1d0 2d0) 3d0)) `(1d0 8d0))
    (is (veq:lst (veq:d2^ 1d0 2d0 3d0)) `(1d0 8d0))

    (is (let ((a 0.4d0)
              (d -1.1d0))
          (veq:lst (veq:d2+ (veq:d2< a d)
                            (veq:d2+ 3d0 4d0
                                     (veq:d2< 4d0 0d0)))))
        '(7.4d0 2.9d0))

    (is (veq:lst (veq:d2norm (veq:d2< 3.0d0 0.0d0))) '(1d0 0d0))
    (is (veq:lst (veq:d2- (veq:d2< 1.0d0 2.0d0) (veq:d2< 2d0 3d0))) '(-1.d0 -1.d0))
    (is (veq:lst (veq:d2+ (veq:d2< 1.0d0 2.0d0) (veq:d2< 2d0 3d0))) '(3.0d0 5.0d0))
    ; (is (veq:d2nsub (veq:d2< 1.0d0 2.0d0) (veq:d2< 2.0d0 10.0d0))
    ;          (veq:d2< -0.12403473458920847d0 -0.9922778767136677d0))
    (is (veq:d2len2 (veq:d2< 1.0d0 2.0d0)) 5d0)
    (is (veq:d2len (veq:d2< 1.0d0 2.0d0)) 2.23606797749979d0)
    (is (veq:d2len (veq:d2< 1.0d0 2.0d0)) 2.23606797749979d0)
    (is (veq:d2dst (veq:d2< 1.0d0 2.0d0) (veq:d2< 1.0d0 3.0d0)) 1.0d0)
    (is (veq:lst (veq:d2mid (veq:d2< 1.0d0 2.0d0) (veq:d2< 3.0d0 4.0d0))) '(2.0d0 3.0d0))
    ; (is (veq:d2lsum (list (veq:d2< 1.0d0 2.0d0) (veq:d2< 0.5d0 4.32d20)))
    ;          (veq:d2< 1.5d0 6.32d20))
    (is (veq:lst (veq:d2lerp (veq:d2< 33d0 88d0) (veq:d2< 32d0 733d0) 0.34d0)) `(32.66d0 307.3d0))
    (is (veq:d2cross (veq:d2< 1d0 2d0) (veq:d2< 3d0 -7.1d0)) -13.1d0)

    (is (veq:lst (veq:d2rot 7d0 2d0 3.24d0)) '(-6.7696361357765955d0 -2.6780639628634217d0))))

(subtest "2d single ops"
  (veq:vprogn

    (is (veq:lst (veq:f2+ 1f0 2f0 (veq:f2< 1f0 3f0))) '(2f0 5f0))
    (is (veq:lst (veq:f2* 1f0 2f0 (veq:f2< 1f0 3f0))) '(1f0 6f0))
    (is (veq:lst (veq:f2- 1f0 2f0 (veq:f2< 1f0 3f0))) '(0f0 -1f0))
    (is (veq:lst (veq:f2/ 1f0 2f0 (veq:f2< 1f0 3f0))) `(1f0 ,(/ 2f0 3f0)))
    (is (veq:lst (veq:f2i- 1f0 2f0 (veq:f2< 1f0 3f0))) '(0f0 1f0))
    (is (veq:lst (veq:f2i/ 1f0 2f0 (veq:f2< 1f0 3f0))) `(1f0 ,(/ 3f0 2f0)))

    (is (veq:lst (veq:f2^ (veq:f2< 1f0 2f0) 3f0)) `(1f0 8f0))
    (is (veq:lst (veq:f2^ 1f0 2f0 3f0)) `(1f0 8f0))

    (is (let ((a 0.4f0)
              (d -1.1f0))
          (veq:lst (veq:f2+ (veq:f2< a d)
                            (veq:f2+ 3f0 4f0
                                     (veq:f2< 4f0 0f0)))))
        '(7.4f0 2.9f0))

    (is (veq:lst (veq:f2norm (veq:f2< 3.0f0 0.0f0))) '(1f0 0f0))
    (is (veq:lst (veq:f2- (veq:f2< 1.0f0 2.0f0) (veq:f2< 2f0 3f0))) '(-1.f0 -1.f0))
    (is (veq:lst (veq:f2+ (veq:f2< 1.0f0 2.0f0) (veq:f2< 2f0 3f0))) '(3.0f0 5.0f0))
    ; (is (veq:f2nsub (veq:f2< 1.0f0 2.0f0) (veq:f2< 2.0f0 10.0f0))
    ;          (veq:f2< -0.12403473458920847f0 -0.9922778767136677f0))
    (is  (veq:f2len2 (veq:f2< 1.0f0 2.0f0)) 5f0)
    (is  (veq:f2len (veq:f2< 1.0f0 2.0f0)) 2.23606797749979f0)
    (is  (veq:f2len (veq:f2< 1.0f0 2.0f0)) 2.23606797749979f0)
    (is  (veq:f2dst (veq:f2< 1.0f0 2.0f0) (veq:f2< 1.0f0 3.0f0)) 1.0f0)
    (is (veq:lst (veq:f2mid (veq:f2< 1.0f0 2.0f0) (veq:f2< 3.0f0 4.0f0))) '(2.0f0 3.0f0))
    ; (is (veq:f2lsum (list (veq:f2< 1.0f0 2.0f0) (veq:f2< 0.5f0 4.32f20)))
    ;          (veq:f2< 1.5f0 6.32f20))
    (is (veq:lst (veq:f2lerp (veq:f2< 33f0 88f0) (veq:f2< 32f0 733f0) 0.34f0)) `(32.66f0 307.3f0))
    (is (veq:f2cross (veq:f2< 1f0 2f0) (veq:f2< 3f0 -7.1f0)) -13.1f0)
    (is (veq:lst (veq:f2rot 7f0 2f0 3.24f0)) '(-6.769636 -2.6780639))))

(subtest "3d double ops"
  (veq:vprogn

    (is (veq:lst (veq:d3cross 1d0 2d0 3d0 3d0 1d0 5d0 )) '(7.0d0 4.0d0 -5.0d0))
    (is (veq:lst (veq:d3rots (veq:d3< 1d0 2d0 3d0) (veq:d3< 3d0 1d0 5d0)
                             0.43d0 (veq:d3< 3d0 2d0 1d0)))
        '(3.1082211094100303d0 -4.3057958375669125d0 5.4723581107104895d0))
    (is (veq:lst (veq:d3rot (veq:d3< 1d0 2d0 3d0) (veq:d3< 3d0 1d0 5d0) 0.43d0))
        '(3.452925152177305d0 1.9711332961352255d0 13.9146762936822d0))))

(subtest "3d single ops"
  (veq:vprogn

    (is (veq:lst (veq:f3cross 1f0 2f0 3f0 3f0 1f0 5f0 )) '(7.0f0 4.0f0 -5.0f0))
    (is (veq:lst (veq:f3rots (veq:f3< 1f0 2f0 3f0) (veq:f3< 3f0 1f0 5f0)
                             0.43f0 (veq:f3< 3f0 2f0 1f0)))
        '(3.1082208 -4.3057957 5.4723577))
    (is (veq:lst (veq:f3rot (veq:f3< 1f0 2f0 3f0) (veq:f3< 3f0 1f0 5f0) 0.43f0))
        '(3.4529245 1.971133 13.914675))))

(subtest "array macro"
  (veq:vprogn
    (is (veq:d2$zero 2) #(0d0 0d0 0d0 0d0) :test #'equalp)
    (is (veq:d2$zero) #(0d0 0d0) :test #'equalp)
    (is (veq:f2$one 2) #(1f0 1f0 1f0 1f0) :test #'equalp)

    (let ((a (veq:d$_ '((100.2d0 2d0)))))
      (is a '#(100.2d0 2d0) :test #'equalp)
      (is (veq:lst (veq:d2+ (veq:d2$ a) 2d0 3d0)) '(102.2d0 5d0)))

    (let ((a (veq:f$_ '((100.2f0 2f0)))))
      (is a '#(100.2f0 2f0) :test #'equalp)
      (is (veq:lst (veq:f2+ (veq:f2$ a) 2f0 3f0)) '(102.2f0 5f0)))

    (is (veq:d$_ (loop for i from 0 below 3
                      collect (list (veq:df i)
                                    (veq:df (1+ i)))))
        #(0d0 1d0 1d0 2d0 2d0 3d0)
        :test #'equalp)

    (is (veq:d_ '(1d0 2d0 3d0 4d0)) #(1d0 2d0 3d0 4d0) :test #'equalp)
    (let ((a (list 1d0 2d0))) (is (veq:d_ a) #(1d0 2d0) :test #'equalp))))


(subtest "macro"
  (veq:vprogn

    (labels ((fx1 (h (veq:varg 3 a)) (declare (ignore h))
               (veq:f3+ a 1f0 2f0 0f0)))
      (is (veq:lst (fx1 0f0 1f0 1f0 2f0)) '(2f0 3f0 2f0)))

    (veq:mvb (x (veq:varg 2 a)) (veq:f3< 4f0 3f0 1f0)
      (declare (ignore x))
      (veq:mvb ((veq:varg 2 a) y) (veq:f3< 8f0 5f0 17f0)
        (declare (ignore y))
        (is (veq:lst a) '(8f0 5f0)))
      (is (veq:lst a) '(3f0 1f0)))

    (veq:mvb ((veq:varg 3 a)) (veq:f3< 1f0 2f0 3f0)
      (is (veq:lst (veq:vref a 2 0 1)) '(3f0 1f0 2f0)))

    (veq:f3let ((a (veq:f3< 1f0 2f0 3f0)))
      (is (veq:lst a) '(1f0 2f0 3f0))
      (is (veq:vref a 2) 3f0)
      (is (list (veq:vref a 0 1)) (list 1f0 2f0)))

    (veq:fvlet ((a 3 (veq:f3< 1f0 2f0 3f0))
                (b 2 (veq:f2< 5f0 -1f0)))
      (is (veq:lst a b) '(1f0 2f0 3f0 5f0 -1f0)))


    (let ((a 1f0))
      (is (veq:lst (veq:f2rep (incf a))) '(2f0 3f0))
      (is (veq:lst (veq:f3rep (incf a))) '(4f0 5f0 6f0))
      (is (veq:lst (veq:f2rep* (incf a))) '(7f0 7f0)))))

(subtest "lerp"
  (veq:vprogn

    (is (veq:f3lspace (5 (veq:f3< 1f0 2f0 -1f0) (veq:f3rep 2f0)))
        #(1.0 2.0 -1.0 1.25 2.0 -0.25 1.5 2.0 0.5 1.75 2.0 1.25 2.0 2.0 2.0)
        :test #'equalp)

    (is (veq:f3lspace (5 (veq:f3< 1f0 5f0 7f0) (veq:f3rep 2f0) )
                      (veq:f$make :dim 3 :n 5 :v 0f0)
                      (lambda (i x y z) (veq:3vaset (veq::arr i) (values x y (* z z)))))
         #(1.0 5.0 49.0 1.25 4.25 33.0625 1.5 3.5 20.25 1.75 2.75 10.5625 2.0
           2.0 4.0)
        :test #'equalp)

    (let ((a (veq:f$make :dim 3 :n 5 :v 0f0)))
      (veq:f3lspace (5 (veq:f3< 2f0 -4f0 8f0) (veq:f3rep 2f0))
                    (lambda (i x y z) (veq:3vaset (a i) (values x y z))))
      (is a #(2.0 -4.0 8.0 2.0 -2.5 6.5 2.0 -1.0 5.0 2.0 0.5 3.5 2.0 2.0 2.0)
          :test #'equalp))

    (let ((a (veq:f$make :dim 3 :n 5 :v 0f0)))
      (veq:f3lspace (5 (veq:f3< 2f0 -4f0 8f0) (veq:f3rep 2f0) :end nil)
                    (lambda (i x y z) (veq:3vaset (a i) (values x y z))))
      (is a #(2.0 -4.0 8.0 2.0 -2.8 6.8 2.0 -1.5999999 5.6 2.0 -0.39999986
              4.3999996 2.0 0.8000002 3.1999998)
          :test #'equalp))))

(subtest "mima"
  (veq:vprogn

    (let ((a #(-1f0 2f0 3f0 4f0 5f0 8f0))
          (b #(-1d0 2d0 3d0 4d0 5d0 8d0)))

      (is (veq:lst (veq:f$mima 6 a)) '(-1.0 8.0))
      (is (veq:lst (veq:d$mima 6 b)) '(-1d0 8d0))
      (is (veq:lst (veq:f2$mima 3 a)) '(-1.0 5.0 2.0 8.0))
      (is (veq:lst (veq:d2$mima 3 b)) '(-1d0 5d0 2d0 8d0))
      (is (veq:lst (veq:f3$mima 2 a)) '(-1.0 4.0 2.0 5.0 3.0 8.0))
      (is (veq:lst (veq:d3$mima 2 b)) '(-1.0d0 4.0d0 2.0d0 5.0d0 3.0d0 8.0d0)))))

(subtest "for all rows"
  (veq:vprogn

    (let ((a (veq:f3$zero 3)) (b (veq:f3$zero 3)) (c (veq:f3$zero 3)))
      (loop for i from 0 below 3
            do (veq:3vaset (a i) (veq:f3<* i (1+ i) (* 2 i)))
               (veq:3vaset (b i) (veq:f3<* 1 (+ 3 i) (/ i 2))))

      (labels ((cross (i (veq:varg 3 v w))
                 (veq:3vaset (c i) (veq:f3cross v w))))

        (veq:3with-rows (3 a b) #'cross))

      (is c #(0.0 0.0 -1.0 -7.0 1.5 2.0 -17.0 2.0 7.0) :test #'equalp)
      (is (veq:3to-list c)
          '((0.0 0.0 -1.0) (-7.0 1.5 2.0) (-17.0 2.0 7.0))))))

(subtest "with arrays"
  (veq:vprogn

    (veq:fwith-arrays (:n 7 :itr k
      :arr ((a 3 (veq:f3$one 7)) (b 3) (c 3))
      :fxs ((cross ((veq:varg 3 v w)) (veq:f3cross v w))
            (init1 (i) (veq:f3<* (1+ i) (* 2 i) (+ 2 i)))
            (init2 (i) (veq:f3<* (+ 2 i) (1+ i) (* 2 i))))
      :exs ((a k (init1 k))
            (b k (init2 k))
            (c k (cross a b))))
      (is c #(-2.0 4.0 1.0 -2.0 5.0 -2.0 4.0 4.0 -7.0 16.0 1.0 -14.0 34.0 -4.0
              -23.0 58.0 -11.0 -34.0 88.0 -20.0 -47.0)
          :test #'equalp))

    (veq:fwith-arrays (:n 7 :itr k
      :arr ((two 2) (three 3))
      :fxs ((init-three (i) (veq:f3<* (+ 2 i) (1+ i) (+ 100 (* 2 i))))
            (three-to-two ((veq:varg 3 v))
                            (declare (ignore (veq:vref v 1)))
                            (veq:f2< (veq:vref v 0 2))))
      :exs ((three k (init-three k))
            (two k (three-to-two three))))
      (is two
          #(2.0 100.0 3.0 102.0 4.0 104.0 5.0 106.0 6.0 108.0 7.0 110.0 8.0
            112.0)
          :test #'equalp))

    (veq:fwith-arrays (:n 4 :itr k :cnt c :start 2
      :arr ((aa 3 (veq:f3$zero 4)))
      :fxs ((init (i j) (veq:f3<* (+ 2 i) (+ 1000 j) (+ 100 (* 2 i)))))
      :exs ((aa k (init c k))))
      (is aa
          #(0.0 0.0 0.0 0.0 0.0 0.0 2.0 1002.0 100.0 3.0 1003.0 102.0)
          :test #'equalp))))

(subtest "take"
  (veq:vprogn
    (let ((a (veq:f$_ '((1f0 2f0) (3f0 4f0) (5f0 6f0)
                        (7f0 7f0) (8f0 9f0) (10f0 11f0)))))
      (is (veq:f2$take a (list 2 4 5)) #(5.0 6.0 8.0 9.0 10.0 11.0)
          :test #'equalp))))

(subtest "select-dim"
  (veq:vprogn
    (is (veq:lst (veq:zyx (values 1 2 3))) (list 3 2 1))
    (is (veq:lst (veq:yx (values 1 2 3))) (list 2 1))
    ))

(unless (finalize) (error "error in veq"))

