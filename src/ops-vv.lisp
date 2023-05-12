(in-package :veq)

; -- MVC ------------------------------------------------------------------------

; (m@fx ...) -> (mvc fx (mvc #'values ...))
(defun procm@fx (b p) `(mvc #',(gk p :fx*) (~ ,@(cdr b))))

; -- 1ARY -----------------------------------------------------------------------

; (2.@abs -1 -2) -> (abs -1) (abs -1) -> 1 2
(defun proc.@fx (b p &aux (dim (gk p :dim)))
  (declare (optimize speed) (pn dim))
  (-vmvb* (gk p :ty) dim 'arg `(~ ,@(cdr b))
    `((values ,@(loop for d of-type pn from 0 repeat dim
                      collect `(,(gk p :fx*) (:vr arg ,d)))))))

; -- NARY -----------------------------------------------------------------------

; (labels ((fx (x y) (values y x))) (i2_@fx 1 2)) -> (fx 1 2) -> 2 1
(defun proc_@fx (b p &aux (dim (gk p :dim)))
  (declare (optimize speed) (pn dim))
  (-vmvb* (gk p :ty) dim 'arg `(~ ,@(cdr b))
    `((,(gk p :fx*) (:vr arg ,@(loop for d of-type pn from 0 repeat dim collect d))))))


(defun proc%@fx (b p)
  (declare (optimize speed))
  `(labels ((,(gk p :fx*) ,@(first (last b))))
     ,(-vmvb* (gk p :ty) (gk p :dim) 'lft `(~ ,@(cdr (butlast b)))
              `((,(gk p :fx*) lft)))))

; -- VEC ------------------------------------------------------------------------

; row wise element pair wise
(defun proc!@fx (b p &aux (dim (gk p :dim)))
  (declare (optimize speed) (pn dim))
  (-vmvb* (gk p :ty) (the pn (* 2 dim)) 'arg `(~ ,@(cdr b))
    `((values
        ,@(loop for d of-type pn from 0 repeat dim
                collect `(,(gk p :fx*) (:vr arg ,d) (:vr arg ,(+ d dim))))))))

(defun proc!@.fx (b p &aux (dim (gk p :dim)) (dots (gk p :dots)))
  (declare (optimize speed) (pn dim dots))
  (-vmvb* (gk p :ty) (the pn (+ dim dots)) 'arg `(~ ,@(cdr b))
    `((values
        ,@(loop with lhs = (nvrs 'arg 0 dots)
                for d of-type pn from 0 repeat dim
                collect `(,(gk p :fx*) ,@lhs (:vr arg ,(+ dots d))))))))

(defun proc!@fx. (b p &aux (dim (gk p :dim)) (dots (gk p :dots)))
  (declare (optimize speed) (pn dim dots))
  (-vmvb* (gk p :ty) (+ dim dots) 'arg `(~ ,@(cdr b))
    `((values ,@(loop with rhs = (nvrs 'arg dim dots)
                      for d of-type pn from 0 repeat dim
                      collect `(,(gk p :fx*) (:vr arg ,d) ,@rhs))))))

; -- ARRAY LEFT MAP -------------------------------------------------------------

(defun fx@conf (p b &aux (dim (gk p :dim)))
  (declare (list p b) (pn dim))
  (dsb (args . body) (replace-varg (car (gk p :rht)))
    (declare (ignore body))
    (let ((l (length args)))
      (unless (or (<= dim l (1+ dim)))
              (error "x@ / %@: incorrect number of args in ~%~a.
expecting ~a or ~a, got: ~a)" b dim (1+ dim) l))
      (values `((:fx@-ind . ,(cond ((= l dim) `(lft))
                                   ((= l (1+ dim)) `(ind lft))))
                ,@p) b))))


(defun proc%@$fx (b p &aux (dimout (gk p :dimout)))
  (declare (optimize speed) (pn dimout))
  (let ((p (vchain (#'fx@conf #'lconf #'tailconf) p b)))
  `(labels ((,(gk p :fx*) ,@(car (gk p :rht))))
     (loop ,@(vec-select-itr p)
           do ,(-vmvb* (gk p :ty) (gk p :dim) 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,dimout
                             (* ,(gk p :itr-out-sym) ,dimout))
                           (,(gk p :fx*) ,@(gk p :fx@-ind t)))))
           finally (return ,(gk p :out-sym))))))


(defun procx@$fx (b p)
  (declare (optimize speed))
  (let ((p (vchain (#'fx@conf #'niloutconf #'lconf #'tailconf) p b)))
    `(labels ((,(gk p :fx*) ,@(car (gk p :rht))))
       (loop ,@(vec-select-itr p)
             do ,(-vmvb* (gk p :ty) (gk p :dim) 'lft ($row p :itr-lft-sym :lft-sym)
                         `((,(gk p :fx*) ,@(gk p :fx@-ind t))))))))

; -- ARRAY LEFT REDUCE ----------------------------------------------------------

(defun procr@$fx (b p &aux (dim (gk p :dim)) (dimout (gk p :dimout)))
  (declare (optimize speed) (pn dim dimout))
  (let ((p (vchain (#'niloutconf #'lconf) p b))
        (d0 (type-default (gk p :ty) 0)))
    (-vmvb* (gk p :ty) dim 'agg `(values ,@(loop repeat dimout
                                                 collect d0))
     `((loop ,@(vec-select-itr p)
             do ,(-vmvb* (gk p :ty) dim 'lft ($row p :itr-lft-sym :lft-sym)
                   `((setf ,@(loop for i of-type pn from 0 below dimout
                                   nconc `((:vr agg ,i)
                                           (,(gk p :fx*) (:vr agg ,i) (:vr lft ,i)))))))
             finally (return (values agg)))))))


; -- ARRAY LEFT 1ARY ------------------------------------------------------------

; (labels ((fx (x) (abs x)))
;   (2.@$fx (veq:i2$line -1 -2 -3 -4)))
(defun proc.@$fx (b p &aux (dim (gk p :dim)) (dimout (gk p :dimout)))
  (declare (optimize speed) (pn dim dimout))
  (let ((p (vchain (#'lconf) p b))
        (row `(values ,@(loop for d of-type pn from 0 repeat dim
                              collect `(,(gk p :fx*) (:vr lft ,d))))))
    `(loop ,@(vec-select-itr p)
           do ,(-vmvb* (gk p :ty) dim 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,dimout
                            (* ,(gk p :itr-out-sym) ,dimout))
                           ,row)))
           finally (return ,(gk p :out-sym)))))

; -- ARRAY LEFT NARY ------------------------------------------------------------

; (labels ((fx (x y) (values y x))) (2_@$fx (veq:i2$line 1 2 3 4))) -> #(2 1 4 3)
(defun proc_@$fx (b p &aux (dim (gk p :dim)) (dimout (gk p :dimout)))
  (declare (optimize speed) (pn dim dimout))
  (let ((p (vchain (#'lconf) p b))
        (row `(,(gk p :fx* ) ,@(loop for d of-type pn from 0 repeat dim
                                     collect `(:vr lft ,d)))))
    `(loop ,@(vec-select-itr p)
           do ,(-vmvb* (gk p :ty) dim 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,dimout
                            (* ,(gk p :itr-out-sym) ,dimout))
                           ,row)))
           finally (return ,(gk p :out-sym)))))

; (labels ((fx (x y z) (values y (+ x z)))) (2_@$fx. (veq:i2$line 1 2 3 4) 10)) -> #(2 11 4 13)
(defun proc_@$fx. (b p &aux (dim (gk p :dim)) (dimout (gk p :dimout))
                            (ty (gk p :ty)))
  (declare (optimize speed) (pn dim dimout))
  (let* ((p (vchain (#'lconf #'tailconf) p b))
         (rhs (nvrs 'rht 0 (gk p :dots)))
         (row `(,(gk p :fx* )
                ,@(loop for d of-type pn from 0 repeat dim collect `(:vr lft ,d))
                ,@rhs))
         (ret `(return ,(gk p :out-sym)))
         (rht `(~ ,@(gk p :rht)))
         (inner (-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                  `(($nvset (,(gk p :out-sym) ,dimout
                             (* ,(gk p :itr-out-sym) ,dimout))
                            ,row)))))
     (if (gk p :@modrht t)
         `(loop ,@(vec-select-itr p)
                do ,(-vmvb* ty (gk p :dots) 'rht rht `(,inner)) finally ,ret)
         (-vmvb* ty (gk p :dots) 'rht rht
           `((loop  ,@(vec-select-itr p)
                   do ,inner finally ,ret))))))

; -- ARRAY LEFT vec -------------------------------------------------------------

(defun proc!@$fx (b p &aux (dim (gk p :dim)) (dimout (gk p :dimout)) (ty (gk p :ty)))
  (declare (optimize speed) (pn dim dimout))
  (let* ((p (vchain (#'lconf #'tailconf) p b))
         (row (loop for d of-type pn from 0 repeat dim
                    collect `(,(gk p :fx*) (:vr lft ,d) (:vr rht ,d))))
         (inner (-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                  `(($nvset (,(gk p :out-sym) ,dimout
                             (* ,(gk p :itr-out-sym) ,dimout))
                            (values ,@row)))))
         (rht `(~ ,@(gk p :rht)))
         (ret `(return ,(gk p :out-sym))))
    (if (gk p :@modrht t)
        `(loop ,@(vec-select-itr p)
               do ,(-vmvb* ty dim 'rht rht `(,inner)) finally ,ret)
        (-vmvb* ty dim 'rht rht
          `((loop ,@(vec-select-itr p) do ,inner finally ,ret))))))

(defun proc!@$fx. (b p &aux (dim (gk p :dim)) (dimout (gk p :dimout))
                            (ty (gk p :ty)))
  (declare (optimize speed) (pn dim dimout) )
  (let* ((p (vchain (#'lconf #'tailconf) p b))
        (row (loop with rhs = (nvrs 'rht 0 (gk p :dots))
                   for d of-type pn from 0 repeat dim
                   collect `(,(gk p :fx*) (:vr lft ,d) ,@rhs)))
        (inner (-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                    `(($nvset (,(gk p :out-sym) ,dimout
                               (* ,(gk p :itr-out-sym) ,dimout))
                              (values ,@row)))))
        (rht `(~ ,@(gk p :rht)))
        (ret `(return ,(gk p :out-sym))))

    (if (gk p :@modrht t)
        `(loop ,@(vec-select-itr p)
               do ,(-vmvb* ty (gk p :dots) 'rht rht `(,inner)) finally ,ret)
        (-vmvb* ty (gk p :dots) 'rht rht
          `((loop ,@(vec-select-itr p) do ,inner finally ,ret))))))

; -- ARRAYS LEFT RIGHT vec ------------------------------------------------------

(defun proc!@$fx$ (b p &aux (dim (gk p :dim)) (dimout (gk p :dimout))
                            (ty (gk p :ty)) (aty (gk p :aty)))
  (declare (optimize speed) (pn dim dimout))
  (let ((p (vchain (#'lconf #'rconf) p b))
        (row (loop for d of-type pn from 0 repeat dim
                   collect `(,(gk p :fx*) (:vr lft ,d) (:vr rht ,d)))))
    `(loop with ,(gk p :rht-sym) of-type ,aty = ,(gk p :rht)
          ,@(vec-select-itr p)
           do ,(-vmvb* ty dim 'rht ($row p :itr-rht-sym :rht-sym)
                 `(,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                      `(($nvset (,(gk p :out-sym) ,dimout
                                 (* ,(gk p :itr-out-sym) ,dimout))
                                (values ,@row))))))
           finally (return ,(gk p :out-sym)))))

(defun vv-proc (body)
  (declare (optimize speed (safety 2)))
  (labels
    ((err (p b msg) (error (format nil "VV: ~a, for: ~s~%in: ~s " msg (gk p :fx) b)))
     (tx-m@ (b &aux (p (vvconf b :vv-sym #.(mkstr *vv-m@*)))) (procm@fx b p)) ; m@
     (tx-r@ (b &aux (p (vvconf b :vv-sym #.(mkstr *vv-r@*)))) (procr@$fx b p)) ; r@

     (tx-!@ (b &aux (p (vvconf b)))
       (cond
         ((> (the pn (gk p :arrs)) 1) (err p b "!@: invalid input, too many $"))
         ((gk+ p :.l :.r) (err p b "!@: vec broadcasting on both sides"))
         ((gk+ p :.l :$r) (err p b "!@: .fx$ not implemented"))

         ((gk+ p :$l :$r)
          (unless (= 3 (length b)) (err p b "!@: bad # of elements"))
          (proc!@$fx$ b p))

         ((gk+ p :$l :.r)
          (unless (> (length b) 2) (err p b "!@: missing vecs"))
          (proc!@$fx. b p))
         ((gk+ p :$r) (err p b "!@: fx$ not implemented"))

         ((gk+ p :$l)
          (unless (> (length b) 2) (err p b "!@: missing vecs"))
          (proc!@$fx b p))
         ((gk+ p :.r) (proc!@fx. b p))
         ((gk+ p :.l) (proc!@.fx b p))

         ((gk0 p :$l :$r :.l :.r) (proc!@fx b p))
         (t (err p b "!@: unexpected input"))))

     (tx-_@ (b &aux (p (vvconf b :vv-sym #.(mkstr *vv-_@*))))
       (cond
         ((> (the pn (gk p :arrs)) 1) (err p b "_@: invalid input, too many $"))
         ((gk+ p :.l :.r) (err p b "_@: vec broadcasting on both sides"))

         ((gk+ p :$l :.r) (proc_@$fx. b p))

         ((gk+ p :$l)
          (unless (= 2 (length b)) (err p b "_@: bad # of elements"))
          (proc_@$fx b p))

         ((gk0 p :$l :$r :.l :.r) (proc_@fx b p))
         (t (err p b "_@: unexpected input"))))

     (tx-.@ (b &aux (p (vvconf b :vv-sym #.(mkstr *vv-.@*))))
       (cond
         ((gk0 p :$l :$r :.l :.r) (proc.@fx b p))

         ((gk+ p :$l)
          (unless (= 2 (length b)) (err p b ".@: bad # of elements"))
          (proc.@$fx b p))

         (t (err p b ".@: unexpected input"))))

     (tx-x@ (b &aux (p (vvconf b :vv-sym #.(mkstr *vv-x@*)))) (procx@$fx b p))
     (tx-%@ (b &aux (p (vvconf b :vv-sym #.(mkstr *vv-%@*))))
        (cond
          ((not (gk0 p :$r :.r :.l)) (err p b "%@: bad configuration"))

          ((gk+ p :$l)
           (unless (= (length b) 3) (err p b "%@: bad number of elements") )
           (proc%@$fx b p))

          (t (proc%@fx b p))))

     (rec (b) ; this messy, but much faster to define s as late as possible,
              ; and once only (before it was hidden in fxs)
       (cond ((or (null b) (atom b)) (return-from rec b))
             ((not (and (listp b) (symbolp (car b))))
              (return-from rec (cons (rec (car b)) (rec (cdr b))))))

       (let ((s (mkstr (car b))))
         (declare (string s))
         (cond ((match-substr #.(mkstr *vv-!@*) s) (rec (tx-!@ b)))
               ((match-substr #.(mkstr *vv-_@*) s) (rec (tx-_@ b)))
               ((match-substr #.(mkstr *vv-.@*) s) (rec (tx-.@ b)))
               ((match-substr #.(mkstr *vv-r@*) s) (rec (tx-r@ b)))
               ((match-substr #.(mkstr *vv-%@*) s) (rec (tx-%@ b)))
               ((match-substr #.(mkstr *vv-x@*) s) (rec (tx-x@ b)))
               ((match-substr #.(mkstr *vv-m@*) s) (rec (tx-m@ b)))
               (t (cons (rec (car b)) (rec (cdr b))))))))
    (rec body)))

(defmacro define-vv-macro ()
  (let ((mname 'vv)
        (docs
"the vv macro implements a DSL for manipulating packs of values and/or row
vectors. it is called as a part of vprogn, fvprogn, vdef and fvdef. but can
also be used explicitly via the (vv ...) macro or (veq::vv-proc ...) function.

you can read about the motivation behind vv at:
https://inconvergent.net/2023/a-vector-dsl/

the DSL uses triggers to broadcast a function (symbol name) or code across
packs of 1-9 values and/or rows of 1-9 item vectors.

; lets start with a simple example

  (i2!@+ 1 2 3 4) ; -> (values (+ 1 3) (+ 2 4))

; where:
;  - i is the type (fixnum)
;  - 2 is the dimension / size of the value packs
;  - !@ is the trigger for this particular mode; and
;  - + is the function name

; i2!@+ requires 4 values, but it is agnostic to the grouping of the values in
; the input

  (i2!@+ (values 1 2) 3 4)       ; -> (values (+ 1 3) (+ 2 4))
  (i2!@+ 1 (values 1 2 3 4))     ; -> (values (+ 1 3) (+ 2 4))
  (i2!@+ 1 (values 2 3) (+ 7 8)) ; -> (values (+ 1 3) (+ 2 (+ 7 8)))

; here are other possible configurations for !@

  ; same behaviouir as above
  (2!@*  1 2 3 4)  ; -> (values (* 1 3) (* 2 4))
  ; project last value
  (2!@*. 1 2 3)    ; -> (values (* 1 3) (* 2 3))
  ; project first value
  (2!@.* 1 2 3)    ; -> (values (* 1 2) (* 1 3))
  ; project two values on r side
  (2!@*.. 1 2 3 4) ; -> (values (* 1 3 4) (* 2 3 4))

; several modes also support arrays ($)

  ; array on l side
  (2!@$* #(1 2 3 4) 5 6) ; -> #((* 1 5) (* 2 6)
                         ;      (* 3 5) (* 4 6))
  (2!@$*. #(1 2 3 4) 5)  ; -> #((* 1 5) (* 2 5)
                         ;      (* 3 5) (* 4 5))
  ; array on both sides
  (2!@$+$! #(1 2 3 4) #(10 20 30 40)) ; -> #(11 22 33 44)
  ; array on r side only is currently not supported

; expressions can be nested, so a 2d dot product might look like this

  (2_@+ (2!@* (values 1 2)
              (values 3 4))) ; -> (+ (* 1 3) (* 2 4))

; here we introduce another mode (_@) that will call the function on the input
; values. a simpler example

  (2_@+ (values 2 3)) ; -> 5

; so, if you have two arrays, you can do row-wise dot products in a similar way

  (21_@$+ (2!@$*$ #(2 2 3 4)
                  #(4 3 2 1))) ; -> #(14 10)

; notice that 21_@$+ has two dimension digits. the second digit is the expected
; output dimension. as such this command will reduce the number of columns from
; 2 to 1. by default the output dim is the same as the input dim. output
; dimension is supported on all array ($) modes.

; here are some slightly more involved examples using the _@ mode

  (labels ((swap (x y) (values y x))
           (do-thing (x y z) (values (+ y z) (+ x z))))
    ; swap columns on each row
    (2_@$swap #(2 1 4 3))       ; -> #(1 2 3 4)
    ; swap columns on each row
    (2_@$swap (?@ #(2 1 4 3)))  ; -> #(1 2 3 4)
    ; swap columns in place (!) on all rows except 0
    (2_@$swap! (?@ #(2 1 4 3 6 5) 1)) ; -> #(2 1 3 4 5 6)
    ; project 5 and call do-thing on each row
    (2_@$do-thing. #(1 2 3 4) 5)) ; -> #((+ 2 5) (+ 1 5)
                                  ;      (+ 4 5) (+ 3 5))

; as opposed to calling a function on rows or values, you can use .@ to call a function
on all elements

  (2.@abs -1 -2)           ; -> (values (abs -1) (abs -2))

; or with arrays

  (2.@$abs #(-1 -2 -3 -4)) ; -> #((abs -1) (abs -2)
                           ;      (abs -3) (abs -4))

; slicing will still work as in the previous examples

  (2.@$abs! (?@ #(-1 -2 -3 -4) 1)) ; -> #(-1 -2 (abs -3) (abs -4))

  (m@list 1 (values 2 3) 4) ; -> '(1 2 3 4)

  ; prints array in two rows with row number:
  (2x@$fx #(1 2 3 4) ((i x y) (print (list i :xy x y)))) ; -> nil
  ; > (0 :xy 1 2)
  ; > (1 :xy 3 4)

  ; fcos-sin returns points on a circle at 10 angles,
  ; then scales the circle by 8f0
  ; f12 means that fcos-sin takes 1 value and returns 2 values (per row)
  (f2!@$*. (veq::f12_@$fcos-sin ; must use :: if outside veq
             (veq:f$lspace 10 0f0 veq:fpii))
             8f0) ; -> #(8.0 0.0 ...)

  ; sum all rows:
  (2r@$+ #(0 1 2 3 4 5 6 7 8 9))        ; -> (values (+ 0 2 4 6 8) (+ 1 3 5 7 9))
  ; sum all rows from row 2:
  (2r@$+ (?@ #(0 1 2 3 4 5 6 7 8 9) 2)) ; -> (values (+ 4 6 8) (+ 5 7 9))
  ; sum rows 0 1 3 from vector with indices:
  (2r@$+ (v?@ #(0 1 2 3 4 5 6 7 8 9) #(0 1 3)))  ; -> (values (+ 0 2 6) (+ 1 3 7))
  ; or, using an index list:
  (2r@$+ (l?@ #(0 1 2 3 4 5 6 7 8 9) '(0 1 3))) ; -> (values (+ 0 2 6) (+ 1 3 7))

MODES

the current modes and corresponding triggers are as follows:

  -- d!@: call fx on pairs of values and/or rows of values from arrays

   - d!@fx:   d values, d values
   - d!@fx.:  d values, [number of dots] values
   - d!@$fx:  d array, d values
   - d!@$fx$: d array, d array
   - d!@$fx.: d array, [number of dots] values

  -- _@: call fx on n values, or rows of values from array

   - d_@fx:   d values
   - d_@$fx:  d array
   - d_@$fx.: d array, [number of dots] values

  -- d.@: call fx on individual elements from values, rows of values from array

   - d.@fx:  d values
   - d.@$fx: d array

the following modes have more specific behaviour:

  - d%@$fx: map fx across rows of values from array. see below.
  - dr@$fx: reduce rows with fx vertically. see below.

  - m@fx: translates to (mvc #'fx ...)

  - ?@ is a modifier used to alter the behaviour of a specific mode.


MODE MODIFIERS: ARRAY SLICING/INDEXING

modes that involve arrays ($) support slicing rows by wrapping the array in

  (?@ arr from [to])

for ivec, pvec, list or vector indices, append i, p, l or v respective to the
modifier. here are some examples of behaviour. the types are described below.

  (labels ((take2 ((:va 2 x)) (values x)))

    ; slice from index
    (2_@$take2 (?@ #(1 2 3 4 5 6 7 8) 2))                ; -> #(5 6 7 8)
    ; slice to from index
    (2_@$take2 (?@ #(1 2 3 4 5 6 7 8) 2 3))              ; -> #(5 6)

    ; vector index
    (2_@$take2 (v?@ #(1 2 3 4 5 6 7 8) #(0 3)))          ; -> #(1 2 7 8)
    ; list index
    (2_@$take2 (l?@ #(1 2 3 4 5 6 7 8) '(0 3)))          ; -> #(1 2 7 8)

    ; individual modifiers can be used for l or r side
    (2!@$+$ (?@ #(1 2 3 4) 1) (?@ #(10 20 30 40) 0 1))   ; -> #(13 24)

    ; if the operation is in-place (!) the l array will
    ; be changed, by the l index
    (2!@$+$! (?@ #(1 2 3 4) 1) (?@ #(10 20 30 40) 0 1))) ; -> #(1 2 13 24)


TYPES

all vv vv expressions (except m@ where it does not make sense) can be explicity
typed. the supported types, wtih corresponding array type are as follows

 - f: veq:ff, single-float; veq:fvec
 - d: veq:df, double-float; veq:dvec
 - i: veq:in, fixnum, veq:ivec
 - p: veq:pn, (unsigned-byte 31); veq:pvec
 - s: veq:sy, symbol; veq:svec
 - k: veq:kv, keyword; veq:kvec
 - l: veq:ll, list; veq:lvec
 - none; vector

fvec, ivec, etc are simple-arrays with the coresponding type. ie. veq:fvec
means (simple-array veq:ff). when type is omitted, the code will be more
forgiving, but less efficeint. in which case the corresponding array type is
'vector.


INSPECT CODE

the code that is actually generated is usually a little more involved than what
these examples imply. in order to see the expanded code, the easiset is to wrap
the code in veq:mac, which displays the code after it has been expanded:

  (veq:fvprogn (veq:mac (2!@$*. #(1 2 3 4) 5)))

alternatively, use:

  (print (veq::vv-proc '(2!@$*. #(1 2 3 4) 5)))
"
          ))
    `(progn (export ',mname)
            (map-docstring ',mname ,docs :nodesc)
            (defmacro ,mname (&body body) ,docs
              `(progn ,@(replace-varg (vv-proc body)))))))
(define-vv-macro)

