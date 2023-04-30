(in-package :veq)

; TODO: x@ count lambda arguments. compare to current dim?
; TODO: use lambda forms instead of ?@ modifier for _@ dotmodes?

; -- MVC ------------------------------------------------------------------------

; (m@fx ...) -> (mvc fx (mvc #'values ...))
(defun procm@fx (rec b p) `(mvc #',(gk p :fx*) ,(rec-cdr rec b)))

; -- 1ARY -----------------------------------------------------------------------

; (2.@abs -1 -2) -> (abs -1) (abs -1) -> 1 2
(defun proc.@fx (rec b p &aux (dim (gk p :dim)))
  (-vmvb* (gk p :ty) dim 'arg (rec-cdr rec b)
    `((values ,@(loop for d from 0 repeat dim
                      collect `(,(gk p :fx*) (:vr arg ,d)))))))

; -- NARY -----------------------------------------------------------------------

; (labels ((fx (x y) (values y x))) (i2_@fx 1 2)) -> (fx 1 2) -> 2 1
(defun proc_@fx (rec b p &aux (dim (gk p :dim)))
  (-vmvb* (gk p :ty) dim 'arg (rec-cdr rec b)
    `((,(gk p :fx*) (:vr arg ,@(loop for d from 0 repeat dim collect d))))))


(defun proc%@fx (rec b p)
  `(labels ((,(gk p :fx*) ,@(first (last b))))
     ,(-vmvb* (gk p :ty) (gk p :dim) 'lft (rec-cdr rec (butlast b))
              `((,(gk p :fx*) lft)))))

; -- VEC ------------------------------------------------------------------------

; row wise element pair wise
(defun proc!@fx (rec b p &aux (dim (gk p :dim)))
  (-vmvb* (gk p :ty) (* 2 dim) 'arg (rec-cdr rec b)
    `((values
        ,@(loop for d from 0 repeat dim
                collect `(,(gk p :fx*) (:vr arg ,d) (:vr arg ,(+ d dim))))))))

(defun proc!@.fx (rec b p &aux (dim (gk p :dim)))
  (-vmvb* (gk p :ty) (+ dim (gk p :dots)) 'arg (rec-cdr rec b)
    `((values
        ,@(loop with lhs = (nvrs 'arg 0 (gk p :dots))
                for d from 0 repeat dim
                collect `(,(gk p :fx*) ,@lhs (:vr arg ,(+ (gk p :dots) d))))))))

(defun proc!@fx. (rec b p &aux (dim (gk p :dim)) (dots (gk p :dots)))
  (-vmvb* (gk p :ty) (+ dim dots) 'arg (rec-cdr rec b)
    `((values ,@(loop with rhs = (nvrs 'arg dim dots)
                      for d from 0 repeat dim
                      collect `(,(gk p :fx*) (:vr arg ,d) ,@rhs))))))

; -- ARRAY LEFT MAP -------------------------------------------------------------

; HERE
(defun proc%@$fx (rec b p)
  (let ((p (vchain (#'lconf #'tailconf) p b)))
    `(labels ((,(gk p :fx*) ,@(car (gk p :rht))))
       (loop with ,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft))
             ,@(vec-select-itr p)
             do ,(-vmvb* (gk p :ty) (gk p :dim) 'lft ($row p :itr-lft-sym :lft-sym)
                   `(($nvset (,(gk p :out-sym) ,(gk p :dimout)
                               (* ,(gk p :itr-out-sym) ,(gk p :dimout)))
                             (,(gk p :fx*) veq::ind lft))))
             finally (return ,(gk p :out-sym))))))

(defun procx@$fx (rec b p)
  (let ((p (vchain (#'niloutconf #'lconf #'tailconf) p b)))
    `(labels ((,(gk p :fx*) ,@(car (gk p :rht))))
       (loop with ,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft))
             ,@(vec-select-itr p)
             do ,(-vmvb* (gk p :ty) (gk p :dim) 'lft ($row p :itr-lft-sym :lft-sym)
                         `((,(gk p :fx*) veq::ind lft)))))))

; -- ARRAY LEFT REDUCE ----------------------------------------------------------

(defun procr@$fx (rec b p &aux (dim (gk p :dim)))
  (let ((p (vchain (#'niloutconf #'lconf) p b))
        (d0 (type-default (gk p :ty) 0)))
    (-vmvb* (gk p :ty) dim 'agg `(values ,@(loop repeat (gk p :dimout)
                                                 collect d0))
     `((loop with ,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft))
             ,@(vec-select-itr p)
             do ,(-vmvb* (gk p :ty) dim 'lft ($row p :itr-lft-sym :lft-sym)
                   `((setf ,@(loop for i from 0 below (gk p :dimout)
                                   nconc `((:vr agg ,i)
                                           (,(gk p :fx*) (:vr agg ,i) (:vr lft ,i)))))))
             finally (return (values agg)))))))


; -- ARRAY LEFT 1ARY ------------------------------------------------------------

; (labels ((fx (x) (abs x)))
;   (2.@$fx (veq:i2$line -1 -2 -3 -4)))
(defun proc.@$fx (rec b p &aux (dim (gk p :dim)))
  (let ((p (vchain (#'lconf) p b))
        (row `(values ,@(loop for d from 0 repeat dim
                              collect `(,(gk p :fx*) (:vr lft ,d))))))
    `(loop with ,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft))
           ,@(vec-select-itr p)
           do ,(-vmvb* (gk p :ty) dim 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,(gk p :dimout)
                            (* ,(gk p :itr-out-sym) ,(gk p :dimout)))
                           ,row)))
           finally (return ,(gk p :out-sym)))))

; -- ARRAY LEFT NARY ------------------------------------------------------------

; (labels ((fx (x y) (values y x))) (2_@$fx (veq:i2$line 1 2 3 4))) -> #(2 1 4 3)
(defun proc_@$fx (rec b p &aux (dim (gk p :dim)))
  (let ((p (vchain (#'lconf) p b))
        (row `(,(gk p :fx* ) ,@(loop for d from 0 repeat dim
                                     collect `(:vr lft ,d)))))
    `(loop with ,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft))
           ,@(vec-select-itr p)
           do ,(-vmvb* (gk p :ty) dim 'lft ($row p :itr-lft-sym :lft-sym)
                 `(($nvset (,(gk p :out-sym) ,(gk p :dimout)
                            (* ,(gk p :itr-out-sym) ,(gk p :dimout)))
                           ,row)))
           finally (return ,(gk p :out-sym)))))

; (labels ((fx (x y z) (values y (+ x z)))) (2_@$fx. (veq:i2$line 1 2 3 4) 10)) -> #(2 11 4 13)
(defun proc_@$fx. (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)))
  (let* ((p (vchain (#'lconf #'tailconf) p b))
         (rhs (nvrs 'rht 0 (gk p :dots)))
         (row `(,(gk p :fx* )
                ,@(loop for d from 0 repeat dim collect `(:vr lft ,d))
                ,@rhs))
         (ret `(return ,(gk p :out-sym)))
         (rht `(~ ,@(funcall rec (gk p :rht))))
         (with `(,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft))))
         (inner (-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                  `(($nvset (,(gk p :out-sym) ,(gk p :dimout)
                             (* ,(gk p :itr-out-sym) ,(gk p :dimout)))
                            ,row)))))
     (if (gk p :@modrht t)
         `(loop with ,@with ,@(vec-select-itr p)
                do ,(-vmvb* ty (gk p :dots) 'rht rht `(,inner)) finally ,ret)
         (-vmvb* ty (gk p :dots) 'rht rht
           `((loop with ,@with ,@(vec-select-itr p)
                   do ,inner finally ,ret))))))

; -- ARRAY LEFT vec -------------------------------------------------------------

(defun proc!@$fx (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)))
  (let* ((p (vchain (#'lconf #'tailconf) p b))
         (row (loop for d from 0 repeat dim
                    collect `(,(gk p :fx*) (:vr lft ,d) (:vr rht ,d))))
         (inner (-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                  `(($nvset (,(gk p :out-sym) ,(gk p :dimout)
                             (* ,(gk p :itr-out-sym) ,(gk p :dimout)))
                            (values ,@row)))))
         (rht `(~ ,@(funcall rec (gk p :rht))))
         (ret `(return ,(gk p :out-sym)))
         (with `(,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft)))))
    (if (gk p :@modrht t)
        `(loop with ,@with ,@(vec-select-itr p)
               do ,(-vmvb* ty dim 'rht rht `(,inner)) finally ,ret)
        (-vmvb* ty dim 'rht rht
          `((loop with ,@with ,@(vec-select-itr p) do ,inner finally ,ret))))))

(defun proc!@$fx. (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)))
  (let* ((p (vchain (#'lconf #'tailconf) p b))
        (row (loop with rhs = (nvrs 'rht 0 (gk p :dots))
                   for d from 0 repeat dim
                   collect `(,(gk p :fx*) (:vr lft ,d) ,@rhs)))
        (inner (-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                    `(($nvset (,(gk p :out-sym) ,(gk p :dimout)
                               (* ,(gk p :itr-out-sym) ,(gk p :dimout)))
                              (values ,@row)))))
        (with `(,(gk p :lft-sym) of-type ,(gk p :aty) = ,(funcall rec (gk p :lft))))
        (rht `(~ ,@(funcall rec (gk p :rht))))
        (ret `(return ,(gk p :out-sym))))

    (if (gk p :@modrht t)
        `(loop with ,@with ,@(vec-select-itr p)
               do ,(-vmvb* ty (gk p :dots) 'rht rht `(,inner)) finally ,ret)
        (-vmvb* ty (gk p :dots) 'rht rht
          `((loop with ,@with ,@(vec-select-itr p) do ,inner finally ,ret))))))

; -- ARRAYS LEFT RIGHT vec ------------------------------------------------------

(defun proc!@$fx$ (rec b p &aux (dim (gk p :dim)) (ty (gk p :ty)) (aty (gk p :aty)))
  (let ((p (vchain (#'lconf #'rconf) p b))
        (row (loop for d from 0 repeat dim
                   collect `(,(gk p :fx*) (:vr lft ,d) (:vr rht ,d)))))
    `(loop with ,(gk p :rht-sym) of-type ,aty = ,(funcall rec (gk p :rht))
           with ,(gk p :lft-sym) of-type ,aty = ,(funcall rec (gk p :lft))
          ,@(vec-select-itr p)
           do ,(-vmvb* ty dim 'rht ($row p :itr-rht-sym :rht-sym)
                 `(,(-vmvb* ty dim 'lft ($row p :itr-lft-sym :lft-sym)
                      `(($nvset (,(gk p :out-sym) ,(gk p :dimout)
                                 (* ,(gk p :itr-out-sym) ,(gk p :dimout)))
                                (values ,@row))))))
           finally (return ,(gk p :out-sym)))))

(defun vv-proc (body)
  (labels
    ((err (p b msg) (error (format nil "VV: ~a, for: ~s~%in: ~s " msg (gk p :fx) b)))
    ; TODO: config tests for array positions etc
    (tx-mvc (b &aux (p (vvconf b :vv-sym *vv-mvc*))) (procm@fx #'rec b p)) ; m@
    (tx-red (b &aux (p (vvconf b :vv-sym *vv-red*))) (procr@$fx #'rec b p)) ; r@
    (tx-map (b &aux (p (vvconf b :vv-sym *vv-map*))) ; %@
      (cond
        ((not (gk0 p :$r :.r :.l)) (err p b "%@: bad configuration"))

        ((gk+ p :$l)
         (unless (= (length b) 3) (err p b "%@: bad number of elements") )
         (proc%@$fx #'rec b p))

        (t (proc%@fx #'rec b p))))

    (tx-xmap (b &aux (p (vvconf b :vv-sym *vv-xmap*))) ; x@
      (procx@$fx #'rec b p))

    (tx-1ary (b &aux (p (vvconf b :vv-sym *vv-1ary*))) ; .@
      (cond
        ((gk0 p :$l :$r :.l :.r) (proc.@fx #'rec b p))

        ((gk+ p :$l)
         (unless (= 2 (length b)) (err p b ".@: bad # of elements"))
         (proc.@$fx #'rec b p))

        (t (err p b ".@: unexpected input"))))

    (tx-nary (b &aux (p (vvconf b :vv-sym *vv-nary*))) ; _@
      (cond
        ((> (gk p :arrs) 1) (err p b "_@: invalid input, too many $"))
        ((gk+ p :.l :.r) (err p b "_@: vec broadcasting on both sides"))

        ((gk+ p :$l :.r)
         ; (unless (> (length b) 2) (err p b "!@: missing vecs"))
         (proc_@$fx. #'rec b p))

        ((gk+ p :$l)
         (unless (= 2 (length b)) (err p b "_@: bad # of elements"))
         (proc_@$fx #'rec b p))

        ((gk0 p :$l :$r :.l :.r) (proc_@fx #'rec b p))
        (t (err p b "_@: unexpected input"))))

    (tx-vec (b &aux (p (vvconf b))) ; !@
      (cond
        ((> (gk p :arrs) 1) (err p b "!@: invalid input, too many $"))
        ((gk+ p :.l :.r) (err p b "!@: vec broadcasting on both sides"))
        ((gk+ p :.l :$r) (err p b "!@: not implemented")) ; skip .fx$

        ((gk+ p :$l :$r)
         (unless (= 3 (length b)) (err p b "!@: bad # of elements"))
         (proc!@$fx$ #'rec b p))

        ((gk+ p :$l :.r)
         (unless (> (length b) 2) (err p b "!@: missing vecs"))
         (proc!@$fx. #'rec b p))
        ((gk+ p :$r) (err p b "!@: not implemented")) ; skip fx$

        ((gk+ p :$l)
         (unless (> (length b) 2) (err p b "!@: missing vecs"))
         (proc!@$fx #'rec b p))
        ((gk+ p :.r) (proc!@fx. #'rec b p))
        ((gk+ p :.l) (proc!@.fx #'rec b p))

        ((gk0 p :$l :$r :.l :.r) (proc!@fx #'rec b p))
        (t (err p b "!@: unexpected input")))) ; not sure if this can happen

    (rec (b)
      (cond ((atom b) b)
            ((car-match-modifier *vv-sym* b) (tx-vec b))
            ((car-match-modifier *vv-1ary* b) (tx-1ary b))
            ((car-match-modifier *vv-nary* b) (tx-nary b))
            ((car-match-modifier *vv-mvc* b) (tx-mvc b))
            ((car-match-modifier *vv-red* b) (tx-red b))
            ((car-match-modifier *vv-map* b) (tx-map b))
            ((car-match-modifier *vv-xmap* b) (tx-xmap b))
            ((consp b) (cons (rec (car b)) (rec (cdr b))))
            (t (error "VV: unexpected expr in: ~a" b))))) ; sbcl says this cant happen
    (rec body)))

(defmacro define-vv-macro ()
  (let ((mname 'vv)
        (docs
"the vv macro implements a DSL for manipulating packs of values and/or row
vectors. it is called as a part of vprogn, fvprogn, vdef and fvdef. but can
also be used explicitly via the (vv ...) macro or (veq::vv-proc ...) function.

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

