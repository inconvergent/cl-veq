#!/usr/bin/sbcl --script

(load "../helpers/load")

(in-package :veq)

; (vpr ...) is a convenience macro used to print.
; (vpr a b) also returns (values a b) which means it can be used for debugging
; directly in code


; use vdef to enable veq. vdef* does the same, but will also create a macro so
; that calling: (vpr-some-values ...)
; will be equivalent to: (multiple-value-call #'vpr-some-values ...)
(vdef* print-some-values ((varg 2 a))
  ; (varg a 2) means that a is a 2d vector, all instances of a in the remaining
  ; code will be replaced with two gensyms
  (vpr (list a))
  ;> (5.0 9.0)

  ; use vref to access individual values of the vector a
  (vpr (/ (vref a 1 0)))
  ;> 1.8
  )


(defun main ()
  (vprogn ; enable veq inside this progn

    ; ---- BASIC OPERATIONS ----

    (vpr (f2+ 1f0 2f0 3f0 4f0))
    ;> (4.0 6.0)

    ; in actuality it returns the result is (values 4.0 6.0)
    ; this means that if you want to use output from veq outside of
    ; veq functionality you may have to use multiple-value-bind or
    ; multiple-value-call to access all values. eg:

    ; (multiple-value-bind (a b) (f2+ 1f0 2f0 3f0 4f0)
    ;   (list a b))
    ; yields '(4.0 6.0)

    ;> (a b) as output represents (values a b) in many of the remaining examples

    (vpr (f2- 1f0 2f0 3f0 4f0))
    ;> (-2.0 -2.0)

    (vpr (f2/ 1f0 2f0 3f0 4f0))
    ;> (0.33333334 0.5)

    (vpr (f2* 1f0 2f0 3f0 4f0))
    ;> (3.0 8.0)

    (vpr (f2scale 1f0 2f0 3f0))
    ;> (3.0 6.0)

    (vpr (f2iscale 1f0 2f0 3f0))
    ;> (0.33333334 0.6666667)

    (vpr (f2neg 1f0 2f0))
    ;> (-1.0 -2.0)

    (vpr (f2abs -1f0 2f0))
    ;> (1.0 2.0)

    (vpr (f2dst 1f0 2f0 3f0 4f0))
    ;> 2.828427

    (vpr (f2len 1f0 2f0))
    ;> 2.236068

    (vpr (f2norm 1f0 2f0))
    ;> (0.4472136 0.8944272)

    (vpr (f2. 1f0 2f0 3f0 4f0))
    ;> 11.0

    (vpr (f2cross 1f0 2f0 3f0 4f0))
    ;> -2.0

    ; single float operations are prefixed with f, double float functions are
    ; prefixed with d. eg. (d2+ 1d0 4d0 4d0 3d0)

    ; 1d and 3d operations look like:
    (vpr (fabs -3f0)) ;> 3.0
    ; and
    (vpr (f3+ 1f0 2f0 3f0 4f0 5f0 6f0)) ;> (5f0 7f0 9f0)
    ; respectively.

    ; see /src/ops-1.lisp, /src/ops-2.lisp and /src/ops-3.lisp
    ; for all available operations


    ; call print-some-values with two 2d vectors [1f0 2f0] and [4f0 7f0]
    (print-some-values (f2+ 1f0 2f0 4f0 7f0))

    ; equivalent to the above. f2< is avaliable in the event that the
    ; syntax requires that the values are a single form
    (print-some-values (f2+ (f2< 1d0 2f0) 4f0 7f0))

    ; such as when using the available lets:
    (f2let ((a (f2< 3f0 32f0))
            (b (f2+ a 3f0 2f0)))
      (vpr (list a b))) ;> (list 3.0 32.0 6.0 34.0)

    ; in practice veq will translate this into something similar to:
    ; (alexandria:with-gensyms (ax ay bx bz)
    ;   `(multiple-value-bind (,ax ,ay) (values 3f0 32f0)
    ;     (multiple-value-bind (,bx ,by) (f2+ ,ax ,ay 3f0 2f0)
    ;       (list ,ax ,ay ,bx ,by)))))

    ; there is also a version of let that accepts different dimensions for each
    ; binding:
    (fvlet ((a 2 (f2< 3f0 32f0))
            (b 3 (f3+ a 0f0 3f0 2f0 4f0))
            (c 3 (f3+ (vref a 0) 3f0 (vref b 2)
                      1f0 (vref b 1 1))))
      (vpr (list a b))
      ;> (3.0 32.0 6.0 34.0 4.0)

      (f2vset a (f2< 4f0 4f0)) ; set 2d vector a
      (setf (vref b 2) 999f0) ; set z value of 3d vector b

      (vpr (list a b)))
      ;> (4.0 4.0 6.0 34.0 999.0)

    ; NOTE: that all fxlet behave like native lisp let*. currently there is
    ; no veq alternative to plain let


    ; ---- ARRAYS OF VECTORS ----

    (let ((a (f$_ `((3f0 3f0))))
          (line (f$_ `((3f0 40f0) (7f0 3f0))))
          (loop-arr (f$_ (loop for v from 0 below 6
                              collect (list (ff v) (ff (1+ v)))))))

      ; convenience function to print arrays of vectors:
      (2$print a)

      (vpr (f2. (f2$ a 0) (f2$ line 1)))
      ;> 30.0
      (vpr (f2cross (f2$ line 0 1)))
      ;> -271.0

      (vpr (f2$take loop-arr (list 1 2)))
      ;> #(1.0 2.0 2.0 3.0)

      (vpr loop-arr)
      ;> #(0.0 1.0 1.0 2.0 2.0 3.0 3.0 4.0 4.0 5.0 5.0 6.0)

      )
    ) )

(main)
