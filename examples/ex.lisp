#!/usr/local/bin/sbcl --script

(load "~/quicklisp/setup.lisp")
(ql:quickload :veq)

(in-package :veq)

; (vpr ...) is a convenience macro used to print. (vpr a b) also returns
; (values a b) which means it can be used for debugging directly in code

(fvprogn ; enable veq inside this progn

  ; ---- BASIC OPERATIONS ----

  ; TODO: rewrite this to describe vv macro

  (vpr (f2!@+ 1f0 2f0 3f0 4f0))
  ;> (4.0 6.0)

  ; actually it returns (values 4.0 6.0). this means that if you want to use
  ; the output from veq outside of veq, you may have to use multiple-value-bind
  ; or multiple-value-call to access all values. eg:

  ; (multiple-value-bind (a b) (f2+ 1f0 2f0 3f0 4f0)
  ;   (list a b))
  ;> '(4.0 6.0)

  ;> (a b) as output represents (values a b) in many of the remaining examples

  (vpr (f2!@- 1f0 2f0 3f0 4f0)) ;> (-2.0 -2.0)

  (vpr (f2!@/ 1f0 2f0 3f0 4f0)) ;> (0.33333334 0.5)

  (vpr (f2!@* 1f0 2f0 3f0 4f0)) ;> (3.0 8.0)

  (vpr (f2!@*. 1f0 2f0 3f0)) ;> (3.0 6.0)

  (vpr (f2iscale 1f0 2f0 3f0)) ;> (0.33333334 0.6666667)

  (vpr (f2.@- 1f0 2f0)) ;> (-1.0 -2.0)

  (vpr (f2.@abs -1f0 2f0)) ;> (1.0 2.0)

  (vpr (f2dst 1f0 2f0 3f0 4f0)) ;> 2.828427

  (vpr (f2len 1f0 2f0)) ;> 2.236068

  (vpr (f2norm 1f0 2f0)) ;> (0.4472136 0.8944272)

  (vpr (f2dot 1f0 2f0 3f0 4f0)) ;> 11.0

  (vpr (f2cross 1f0 2f0 3f0 4f0)) ;> -2.0

  (vpr (f2rep 1f0)) ;> (1f0 1f0)

  ; equivalent to (multiple-value-bind #'list (f2rep 1f0)):
  (vpr (lst (f2rep 1f0))) ;> '(1f0 1f0)

  ; single float operations are prefixed with f, double float functions are
  ; prefixed with d. eg. (d2+ 1d0 4d0 4d0 3d0)

  ; 1d and 3d operations look like:
  (vpr (f.@abs -3f0)) ;> 3.0
  ; and
  (vpr (f3!@+ 1f0 2f0 3f0 4f0 5f0 6f0))) ;> (5f0 7f0 9f0)
  ; respectively.

  ; see /src/ops-1.lisp, /src/ops-2.lisp and /src/ops-3.lisp and
  ; the docs for vv macro for all available operations. most typical
  ; vector operations have been removed as functions from veq and should be
  ; done using the vv macro. the vv DSL generates better, more efficient, code
  ; and it is more lightweight and practial.


; ---- FUNCTIONS ----

; use (vdef fx (...) ...) to create a function with veq enabled. vdef* does the
; same, but will wrap fx in a macro so that calling: (fx ...) will be
; equivalent to: (multiple-value-call #'fx ...).

(fvdef* print-some-values ((varg 2 a))
  ; (varg a 2) means that a is a 2d vector, all instances of a in the remaining
  ; code will be replaced with two gensyms
  (vpr (list a)) ;> (5.0 9.0)

  ; use vref to access individual values of the vector a
  (vpr (/ (vref a 1 0)))) ; (/ 5.0 9.0)
  ;> 1.8

; when using (vdef* fx (...) ...), the function will be named %fx. that is, you
; can call the function directly with (%fx ...). moreover, (vdef* fx ... ) will
; rename all fx symbols inside the definition to %fx. this is relevant when you
; want to use (return-from %fx ...).

(fvprogn

  (format t "~%------------------~%~%")
  ; call print-some-values with two 2d vectors [1f0 2f0] and [4f0 7f0]
  (print-some-values (f2!@+ 1f0 2f0 4f0 7f0))

  ; this is equivalent to the above. f2 is avaliable in the event that the
  ; context requires that the vector is a single form
  (print-some-values (f2!@+ (f2 1d0 2f0) 4f0 7f0))

  ; ---- LET ----

  ; such as when using the available lets:
  (f2let ((a (f2 3f0 32f0)) ; you can also just write (values 3f0 32f0)
          (b (f2!@+ a 3f0 2f0)))
    (vpr (list a b))) ;> (list 3.0 32.0 6.0 34.0)

  ; in practice veq will translate this into something similar to:
  ; (alexandria:with-gensyms (ax ay bx bz)
  ;   `(multiple-value-bind (,ax ,ay) (values 3f0 32f0)
  ;     (multiple-value-bind (,bx ,by) (f2!@+ ,ax ,ay 3f0 2f0)
  ;       (list ,ax ,ay ,bx ,by)))))

  (format t "~%------------------~%~%")
  ; there is also a let that accepts different dimensions for each binding:
  (fvlet ((a 2 (f2 3f0 32f0))
          (b 3 (f3!@+ a 0f0 3f0 2f0 4f0))
          (c 3 (f3!@+ (vref a 0) 3f0 (vref b 2)
                    1f0 (vref b 1 1)))
          (d 1 (f 3f0)))

    (vpr (list a b d)) ;> (3.0 32.0 6.0 34.0 4.0 3f0)

    (setf (veq:f2 a) (f2 4f0 4f0)) ; set 2d vector a
    (setf (vref b 2) 999f0) ; set z value of 3d vector b

    (vpr (list a b))) ;> (4.0 4.0 6.0 34.0 999.0)

  ; NOTE: that all fxlet behave like native lisp let*. currently there is no
  ; veq alternative to plain let.
  )

(fvprogn

  ; ---- ARRAYS OF VECTORS ----

  (format t "~%------------------~%~%")
  (let ((a (f2$point 3f0 3f0))
        (line (f2$line 3f0 40f0 7f0 3f0))
        (line* (f2$line 1f0 2f0 3f0 4f0))
        (b (f$_ (loop for v from 0 below 6
                      collect (list (ff v) (ff (1+ v)))))))

    ; convenience function to print arrays of vectors:
    (2$print line) ; returns a

    (vpr (f2dot (f2$ a 0) (f2$ line 1))) ; dot product
    ;> 30.0

    (vpr (f2cross (f2$ line 0 1))) ; cross product
    ;> -271.0
    ; equivalent to:
    (vpr (f2cross (f2$ line 0) (f2$ line 1))) ; cross product
    ;> -271.0

    (vpr b)
    ;> #(0.0 1.0 1.0 2.0 2.0 3.0 3.0 4.0 4.0 5.0 5.0 6.0)

    (vp :take (veq::f2_@$f2id (l?@ b (list 1 2)))) ; select rows (vectors)
    ;> #(1.0 2.0 2.0 3.0)

  (format t "~%------------------~%~%")
    (let ((a (f3$zero 3)) (b (f3$zero 3)) (c (f3$zero 3)))

      ; set values of a and b
      (loop for i from 0 below 3
               ; set row i of a and b
               ; f3~ is like f3, but it will coerce the type to float
            do (setf (veq:3$ a i) (f3~ i (1+ i) (* 2 i))
                     (veq:3$ b i) (f3~ 1 (+ 3 i) (/ i 2))))

      ; execute cross on rows of a and b
      (f3x@$fx a ((i (:va 3 v))
                  (setf (veq:3$ c i) (f3cross v (veq:f3$ b i)))))

      (3$print c)
      ;> #(0.0 0.0 -1.0 -7.0 1.5 2.0 -17.0 2.0 7.0)
      (vpr (3$to-list c))
      ;> '((0.0 0.0 -1.0) (-7.0 1.5 2.0) (-17.0 2.0 7.0))

      ; add 1 to every row of a
      (print (f3!@$+ a (f3rep 1f0))) ;> #(1.0 2.0 1.0 2.0 3.0 3.0 3.0 4.0 5.0)

      ; divide every row by [1.0 2.0 3.0]
      (print (f3!@$/ a 1f0 2f0 3f0))

      (print (f31_@$f3len c)) ;> #(1.0 7.4330344 18.493242)

      (3$print (f.@$- (f31_@$f3len c))) ;> #(-1.0 -7.4330344 -18.493242)

      ($print (f.@$abs (f.@$- (f31_@$f3len c)))) ;> #(1.0 7.4330344 18.493242)

      (print (f12_@$fcos-sin (f$lspace 4 0f0 fpii))) )
      ;> #(1.0 0.0 -0.50000006 0.8660254 -0.4999999 -0.86602545 1.0 1.7484555e-7)
      ))
