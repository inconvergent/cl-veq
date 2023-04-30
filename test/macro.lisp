(in-package #:veq-tests)

(plan 2)

(subtest "macro" (veq:fvprogn
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

(unless (finalize) (error "error in macro tests"))

