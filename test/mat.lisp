
(in-package #:veq-tests)

(plan 1)

(subtest "mat"
  (veq:fvprogn
    (is (veq:lst (veq:f3mtv (veq:f_ `(1f0 2f0 4f0 3f0 3f0 5f0 4f0 3f0 3f0))
                            (veq:f3+ 1f0 2f0 4f0 1f0 2f0 4f0)))
        '(46f0 40f0 52f0))

    (is (veq:lst (veq:f3mv (veq:f_ `(1f0 2f0 4f0 3f0 3f0 5f0 4f0 3f0 3f0))
                           (veq:f3+ 1f0 2f0 4f0 1f0 2f0 4f0)))
        '(42f0 58f0 44f0))

    (is (veq:lst (veq:f2mv (veq:f_ `(1f0 2f0 3f0 3f0))
                           (veq:f2- 1f0 2f0 2f0 4f0)))
        '(-5f0 -9f0))

    (is (veq:lst (veq:f4mv (veq:f_ `(1f0 2f0 4f0 6f0 3f0 3f0 5f0 9f0
                                     4f0 3f0 3f0 6f0 -1f0 3f0 -3f0 8f0))
                           (veq:f4* 1f0 2f0 -4f0 1f0 2f0 4f0 8f0 1f0)))
        '(-104f0 -121f0 -58f0 126f0))

    (is (veq:lst (veq:f4mv (veq:f4meye)
                           (veq:f4* 1f0 2f0 -4f0 1f0 2f0 4f0 8f0 1f0)))
        '(2f0 8f0 -32f0 1f0))

    (is (veq:lst (veq:f4mv (veq:f4meye 3f0)
                           (veq:f4* 1f0 2f0 -4f0 1f0 2f0 4f0 8f0 1f0)))
        '(6f0 24f0 -96f0 3f0))

    (is (veq:f4mt! (veq:f_ `(1f0 2f0 4f0 6f0 3f0 3f0 5f0 9f0
                             4f0 3f0 3f0 6f0 -1f0 3f0 -3f0 8f0)))
        #(1f0 3f0 4f0 -1f0 2f0 3f0 3f0 3f0
          4f0 5f0 3f0 -3f0 6f0 9f0 6f0 8f0)
        :test #'equalp)

    (is (veq:f3mm (veq:f_ `(1f0 2f0 4f0 2f0 2f0 3f0 3f0 3f0 4f0))
                  (veq:f_ `(3f0 2f0 4f0 3f0 1f0 2f0 3f0 3f0 1f0)))
        #(21f0 16f0 12f0 21f0 15f0 15f0 30f0 21f0 22f0)
        :test #'equalp)

    (is (veq:f3mtm (veq:f_ `(1f0 2f0 4f0 2f0 2f0 3f0 3f0 3f0 4f0))
                  (veq:f_ `(3f0 2f0 4f0 3f0 1f0 2f0 3f0 3f0 1f0)))
         #(18f0 13f0 11f0 21f0 15f0 15f0 33f0 23f0 26f0)
         :test #'equalp)

    (is (veq:f3mmt (veq:f_ `(1f0 2f0 4f0 2f0 2f0 3f0 3f0 3f0 4f0))
                   (veq:f_ `(3f0 2f0 4f0 3f0 1f0 2f0 3f0 3f0 1f0)))
         #(23f0 13f0 13f0 22f0 14f0 15f0 31f0 20f0 22f0)
         :test #'equalp)

    (is (veq:f3mtmt (veq:f_ `(1f0 2f0 4f0 2f0 2f0 3f0 3f0 3f0 4f0))
                    (veq:f_ `(3f0 2f0 4f0 3f0 1f0 2f0 3f0 3f0 1f0)))
         #(19f0 11f0 12f0 22f0 14f0 15f0 34f0 23f0 25f0)
         :test #'equalp)

    (is (veq:f3mrot* 26.0 (veq:f3norm 0.3 0.4 -2.3))
        #(0.6526553 0.752802 0.08561626 -0.73750615
          0.6571166 -0.15582836 -0.17356777 0.038559683 0.9840667)
         :test #'equalp)

    (is (veq:f3mrot 26.0 (veq:f3norm 0.3  0.4 -2.3))
        #(0.6526553 0.752802 0.08561626 0.0 -0.73750615 0.6571166 -0.15582836 0.0
          -0.17356777 0.038559683 0.9840667 0.0 0.0 0.0 0.0 1.0)
         :test #'equalp)

    (is (veq:d2minv (veq:d_ '(1d0 2d0 3d0 31d0)))
        #(1.24d00 -0.08d0 -0.12d0 0.04d0 )
         :test #'equalp)

    (is (veq:d3minv (veq:d_ '(1d0 2d0 77d0 3d0 3d0 21d0 -1.2d0 7d0 2d0)))
        #(-0.08339247693399575d0 0.3164182635438846d0 -0.11178140525195175d0
            -0.018452803406671398d0 0.05583155902531346d0 0.1242015613910575d0
            0.01454932576295245d0 -0.0055594984622663835d0 -0.00177430801987225d0)
         :test #'equalp)

    (is (veq:f4minv (veq:f_ '(1f0 2f0 77f0 7f0 3f0 3f0 21f0 -1f0
                              -1.2 7f0 2f0 3f0 0f0 1f0 -1f0 -10.1f0)))
        #(-0.08739566 0.32386506 -0.096500464 -0.12130059 -0.017844949 0.05470082
            0.121881254 0.018418644 0.014880082 -0.006174776 -0.00303687 0.01002225
            -0.0032401022 0.006027287 0.012368131 -0.09817857)
         :test #'equalp)))

(unless (finalize) (error "error mat tests"))

