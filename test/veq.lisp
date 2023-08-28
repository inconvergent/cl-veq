


(in-package #:veq-tests)

(plan 7)

(subtest "utils"

  (is (veq::strip-symbols (veq::mkstr 'name!@abc) (mapcar #'veq::mkstr '(:!@ :hi))) "NAMEABC")
  (is (veq::strip-symbols (veq::mkstr 'name!abc) (mapcar #'veq::mkstr '(:! :hi))) "NAMEABC")
  (is (veq::strip-symbols (veq::mkstr 'name!abc?) (mapcar #'veq::mkstr '(:! :? :hi))) "NAMEABC")

  (is (veq:lst (veq::edge-fx (lambda (c) (equal #\F c)) (veq::mkstr 'ffffhiii))) '(4 "HIII"))
  (is (veq:lst (veq::edge-str #\F (veq::mkstr 'ffffhiii))) '(4 "HIII"))

  (is (veq:lst (veq:~ :a)) '(:a))
  (is (veq:lst (values 3 2) (values 7 8)) '(3 2 7 8))
  (is (veq:lst (veq:~ (values 3 2) (values 7 8))) '(3 2 7 8))

  (is (veq:vvsym :ff 3 :xxy) 'veq::f3xxy)
  (is (veq:vvsym :ff 3 :xxy :sep :!) 'veq::f3!xxy)

  (is (veq:lst (veq:unpack-vvsym 'f1!abc :s :!)) '(:F ABC 1 1))
  (is (veq:lst (veq:unpack-vvsym 'f!abc :s :!)) '(:F ABC 1 1))
  ; (is (veq:lst (veq:unpack-vvsym '!abc :s :!)) '(:nil ABC 1 1))
  (is (veq:lst (veq:unpack-vvsym 'abc :s :!)) '(:nil ABC 1 1))

  (is (veq:lst (veq:unpack-vvsym 'f1!@abc :s :!@)) '(:F ABC 1 1))
  (is (veq:lst (veq:unpack-vvsym 'f!@abc :s :!@)) '(:F ABC 1 1))
  ; (is (veq:lst (veq:unpack-vvsym '!@abc :s :!@)) '(:nil ABC 1 1))

  ; (is (veq:lst (veq:unpack-vvsym '3!abc :s :!)) '(:nil ABC 3 3))
  (is (veq:lst (veq:unpack-vvsym 's3!abc :s :!)) '(:s ABC 3 3))

  (is (veq:lst (veq:unpack-vvsym '33!abc :s :!)) '(:nil ABC 3 3))
  (is (veq:lst (veq:unpack-vvsym '32!abc :s :!)) '(:nil ABC 3 2))
  (is (veq:lst (veq:unpack-vvsym '12!abc :s :!)) '(:nil ABC 1 2))

  (is (veq:lst (veq:unpack-vvsym 's33!abc :s :!)) '(:s ABC 3 3))
  (is (veq:lst (veq:unpack-vvsym 's32!abc :s :!)) '(:s ABC 3 2))
  (is (veq:lst (veq:unpack-vvsym 's12!abc :s :!)) '(:s ABC 1 2)))

(subtest "2d double ops"
  (veq:fvprogn

    (is (veq:lst (veq:d2 1d0 2d0)) '(1d0 2d0))
    (is (veq:lst (veq:d2~ 1 2d0)) '(1d0 2d0))

    (is (veq:lst (d2!@+  1d0 2d0 (veq:d2 1d0 3d0))) '(2d0 5d0))
    (is (veq:lst (d2!@* 1d0 2d0 (veq:d2 1d0 3d0))) '(1d0 6d0))
    (is (veq:lst (d2!@- 1d0 2d0 (veq:d2 1d0 3d0))) '(0d0 -1d0))
    (is (veq:lst (d2!@/ 1d0 2d0 (veq:d2 1d0 3d0))) `(1d0 ,(/ 2d0 3d0)))
    (is (veq:lst (veq:d2i- 1d0 2d0 (veq:d2 1d0 3d0))) '(0d0 1d0))
    (is (veq:lst (veq:d2i/ 1d0 2d0 (veq:d2 1d0 3d0))) `(1d0 ,(/ 3d0 2d0)))

    (is (veq:lst (d2!@expt. (veq:d2 1d0 2d0) 3d0)) `(1d0 8d0))
    (is (veq:lst (d2!@expt. 1d0 2d0 3d0)) `(1d0 8d0))

    (is (veq:lst (d2!@expt. (veq:d2 1d0 2d0) 3d0)) `(1d0 8d0))
    (is (veq:lst (d2!@expt. 1d0 2d0 3d0)) `(1d0 8d0))

    (is (veq:lst (d2!@+ 1d0 2d0 (veq:d2 1d0 3d0))) '(2d0 5d0))
    (is (veq:lst (d2!@* 1d0 2d0 (veq:d2 1d0 3d0))) '(1d0 6d0))
    (is (veq:lst (d2!@- 1d0 2d0 (veq:d2 1d0 3d0))) '(0d0 -1d0))
    (is (veq:lst (d2!@/ 1d0 2d0 (veq:d2 1d0 3d0))) `(1d0 ,(/ 2d0 3d0)))
    ; (is (veq:lst (d2!@i- 1d0 2d0 (veq:d2 1d0 3d0))) '(0d0 1d0))
    ; (is (veq:lst (d2!@i/ 1d0 2d0 (veq:d2 1d0 3d0))) `(1d0 ,(/ 3d0 2d0)))

    (is (let ((a 0.4d0)
              (d -1.1d0))
          (veq:lst (d2!@+ (veq:d2 a d) (d2!@+ 3d0 4d0 (veq:d2 4d0 0d0)))))
        '(7.4d0 2.9d0))

    (is (veq:lst (veq:d2norm (veq:d2 3.0d0 0.0d0))) '(1d0 0d0))
    (is (veq:lst (d2!@- (veq:d2 1.0d0 2.0d0) (veq:d2 2d0 3d0))) '(-1.d0 -1.d0))
    (is (veq:lst (d2!@+ (veq:d2 1.0d0 2.0d0) (veq:d2 2d0 3d0))) '(3.0d0 5.0d0))
    ; (is (veq:d2nsub (veq:d2 1.0d0 2.0d0) (veq:d2 2.0d0 10.0d0))
    ;          (veq:d2 -0.12403473458920847d0 -0.9922778767136677d0))
    (is (veq:d2len2 (veq:d2 1.0d0 2.0d0)) 5d0)
    (is (veq:d2len (veq:d2 1.0d0 2.0d0)) 2.23606797749979d0)
    (is (veq:d2len (veq:d2 1.0d0 2.0d0)) 2.23606797749979d0)
    (is (veq:d2dst (veq:d2 1.0d0 2.0d0) (veq:d2 1.0d0 3.0d0)) 1.0d0)
    (is (veq:lst (veq:d2mid (veq:d2 1.0d0 2.0d0) (veq:d2 3.0d0 4.0d0)))
        '(2.0d0 3.0d0))
    ; (is (veq:d2lsum (list (veq:d2 1.0d0 2.0d0) (veq:d2 0.5d0 4.32d20)))
    ;          (veq:d2 1.5d0 6.32d20))
    (is (veq:lst (veq:d2lerp (veq:d2 33d0 88d0) (veq:d2 32d0 733d0) 0.34d0))
        `(32.66d0 307.3d0))
    (is (veq:d2cross (veq:d2 1d0 2d0) (veq:d2 3d0 -7.1d0)) -13.1d0)

    (is (veq:lst (veq:d2rot 7d0 2d0 3.24d0))
        '(-6.7696361357765955d0 -2.6780639628634217d0))))

(subtest "2d single ops"
  (veq:fvprogn

    (is (veq:lst (veq:f2 1f0 2f0)) '(1f0 2f0))
    (is (veq:lst (veq:f2~ 1 2)) '(1f0 2f0))

    (is (veq:lst (f2!@+ 1f0 2f0 (veq:f2 1f0 3f0))) '(2f0 5f0))
    (is (veq:lst (f2!@* 1f0 2f0 (veq:f2 1f0 3f0))) '(1f0 6f0))
    (is (veq:lst (f2!@- 1f0 2f0 (veq:f2 1f0 3f0))) '(0f0 -1f0))
    (is (veq:lst (f2!@/ 1f0 2f0 (veq:f2 1f0 3f0))) `(1f0 ,(/ 2f0 3f0)))
    (is (veq:lst (veq:f2i- 1f0 2f0 (veq:f2 1f0 3f0))) '(0f0 1f0))
    (is (veq:lst (veq:f2i/ 1f0 2f0 (veq:f2 1f0 3f0))) `(1f0 ,(/ 3f0 2f0)))

    (is (veq:lst (f2!@+ 1f0 2f0 (veq:f2 1f0 3f0))) '(2f0 5f0))
    (is (veq:lst (f2!@* 1f0 2f0 (veq:f2 1f0 3f0))) '(1f0 6f0))
    (is (veq:lst (f2!@- 1f0 2f0 (veq:f2 1f0 3f0))) '(0f0 -1f0))
    (is (veq:lst (f2!@/ 1f0 2f0 (veq:f2 1f0 3f0))) `(1f0 ,(/ 2f0 3f0)))
    ; (is (veq:lst (f2!@i- 1f0 2f0 (veq:f2 1f0 3f0))) '(0f0 1f0))
    ; (is (veq:lst (f2!@i/ 1f0 2f0 (veq:f2 1f0 3f0))) `(1f0 ,(/ 3f0 2f0)))

    (is (veq:lst (f2!@expt. (veq:f2 1f0 2f0) 3f0)) `(1f0 8f0))
    (is (veq:lst (f2!@expt. 1f0 2f0 3f0)) `(1f0 8f0))

    (is (let ((a 0.4f0)
              (d -1.1f0))
          (veq:lst (f2!@+ (veq:f2 a d) (f2!@+ 3f0 4f0 (veq:f2 4f0 0f0)))))
        '(7.4f0 2.9f0))

    (is (veq:lst (veq:f2norm (veq:f2 3.0f0 0.0f0))) '(1f0 0f0))
    (is (veq:lst (f2!@- (veq:f2 1.0f0 2.0f0) (veq:f2 2f0 3f0))) '(-1.f0 -1.f0))
    (is (veq:lst (f2!@+ (veq:f2 1.0f0 2.0f0) (veq:f2 2f0 3f0))) '(3.0f0 5.0f0))
    ; (is (veq:f2nsub (veq:f2 1.0f0 2.0f0) (veq:f2 2.0f0 10.0f0))
    ;          (veq:f2 -0.12403473458920847f0 -0.9922778767136677f0))
    (is  (veq:f2len2 (veq:f2 1.0f0 2.0f0)) 5f0)
    (is  (veq:f2len (veq:f2 1.0f0 2.0f0)) 2.23606797749979f0)
    (is  (veq:f2len (veq:f2 1.0f0 2.0f0)) 2.23606797749979f0)
    (is  (veq:f2dst (veq:f2 1.0f0 2.0f0) (veq:f2 1.0f0 3.0f0)) 1.0f0)
    (is (veq:lst (veq:f2mid (veq:f2 1.0f0 2.0f0) (veq:f2 3.0f0 4.0f0)))
        '(2.0f0 3.0f0))
    ; (is (veq:f2lsum (list (veq:f2 1.0f0 2.0f0) (veq:f2 0.5f0 4.32f20)))
    ;          (veq:f2 1.5f0 6.32f20))
    (is (veq:lst (veq:f2lerp (veq:f2 33f0 88f0) (veq:f2 32f0 733f0) 0.34f0))
        `(32.66f0 307.3f0))
    (is (veq:f2cross (veq:f2 1f0 2f0) (veq:f2 3f0 -7.1f0)) -13.1f0)
    (is (veq:lst (veq:f2rot 7f0 2f0 3.24f0)) '(-6.769636 -2.6780639))))

(subtest "3d double ops"
  (veq:fvprogn

    (is (veq:lst (veq:d3cross 1d0 2d0 3d0 3d0 1d0 5d0 )) '(7.0d0 4.0d0 -5.0d0))
    (is (veq:lst (veq:d3rots (veq:d3 1d0 2d0 3d0) (veq:d3 3d0 1d0 5d0)
                             0.43d0 (veq:d3 3d0 2d0 1d0)))
        '(3.1082211094100303d0 -4.3057958375669125d0 5.4723581107104895d0))
    (is (veq:lst (veq:d3rot (veq:d3 1d0 2d0 3d0) (veq:d3 3d0 1d0 5d0) 0.43d0))
        '(3.452925152177305d0 1.9711332961352255d0 13.9146762936822d0))))

(subtest "3d single ops"
  (veq:fvprogn

    (is (veq:lst (veq:f3cross 1f0 2f0 3f0 3f0 1f0 5f0 )) '(7.0f0 4.0f0 -5.0f0))
    (is (veq:lst (veq:f3rots (veq:f3 1f0 2f0 3f0) (veq:f3 3f0 1f0 5f0)
                             0.43f0 (veq:f3 3f0 2f0 1f0)))
        '(3.1082208 -4.3057957 5.4723577))
    (is (veq:lst (veq:f3rot (veq:f3 1f0 2f0 3f0) (veq:f3 3f0 1f0 5f0) 0.43f0))
        '(3.4529245 1.971133 13.914675))))


(subtest "select-dim"
  (veq:fvprogn
    (is (veq:lst (veq:fsel (:zy 0) (f3!@+ 1f0 2f0 3f0 2f0 3f0 3f0)))
        (list 6f0 5f0 3f0))
    (is (veq:lst (veq:dsel (1 :x) (values 1d0 2d0 3d0))) (list 2d0 1d0))
    (is (veq:lst (veq:fsel (:w :wxy) (values 1f0 2f0 3f0 4f0))) (list 4f0 4f0 1f0 2f0))))

(subtest "vset"
  (veq:fvprogn
    (veq:fvlet ((a 2 (veq:f2 0f0 0f0))
                (b 3 (veq:f3 1f0 2f0 3f0)))
      (setf (veq:f2 a) (values 2f0 99f0)
            (veq:f3 b) (values -7f0 33f0 3330f0)
            )

    (is (veq:lst a) (list 2f0 99f0))
    (is (veq:lst b) (list -7f0 33f0 3330f0)))))

(unless (finalize) (error "error in veq tests"))

