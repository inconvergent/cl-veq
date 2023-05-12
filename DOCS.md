# VEQ DOCUMENTATION

### Explanation

#### FVPROGN

All symbols marked with `:fvprogn:` are only valid inside a veq context.  veq
context can be initiated using `vprogn`, `fvprogn`, `vdef`, `fvdef`,
`vdef*` or `fvdef*`. See further documentation under the respective symbols
below.

See [examples](/examples/ex.lisp) for working examples of some use.

#### Names and Types

Symols prefixed with `f` pertain to type `ff`, short for `single-float`.
The corresponding vector array type is `fvec`, short for `(simple-array ff)`.

Symols prefixed with `d` pertain to type `df`, short for `double-float`.
The corresponding vector array type is `dvec`, short for `(simple-array df)`.

Symbols with `$` in the name pertain to vector arrays.

Symbols postfixed with `!` are destructive or in-place. Usually on the first
argument.


#### Abbreviations

`dsb` is short for `destructuring-bind`.

`mvb` is short for `multiple-value-bind`.

`mvc` is short for `multiple-value-call`.


### Symbols

#### $

```
 ; VEQ:$
 ;   [symbol]
 ;
 ; $ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF $) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: ($ a i) yields (values ...)
 ;     set: (setf ($ a i) (values ...))
 ;   Source file: src/vset.lisp
```

#### $COPY

```
:none:

 ; VEQ:$COPY
 ;   [symbol]
```

#### $MAKE

```
 ; VEQ:$MAKE
 ;   [symbol]
 ;
 ; $MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) V (TYPE T))
 ;   Documentation:
 ;     create vector array with size (n dim), and initial value v.
 ;   Source file: src/array-utils.lisp
```

#### $NUM

```
 ; VEQ:$NUM
 ;   [symbol]
 ;
 ; $NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SIMPLE-ARRAY)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 1d array.
 ;     untyped.
 ;   Source file: src/array-utils.lisp
```

#### $NVSET

```
 ; VEQ:$NVSET
 ;   [symbol]
 ;
 ; $NVSET names a macro:
 ;   Lambda-list: ((A N &OPTIONAL (I 0)) &BODY BODY)
 ;   Documentation:
 ;     set n indices in a, from a[i] with n values from body.
 ;   Source file: src/vset.lisp
```

#### $PRINT

```
 ; VEQ:$PRINT
 ;   [symbol]
 ;
 ; $PRINT names a compiled function:
 ;   Lambda-list: (A &KEY (DIM 1) (START 0) N (S T))
 ;   Derived type: (FUNCTION
 ;                  (SIMPLE-ARRAY &KEY (:DIM (UNSIGNED-BYTE 31))
 ;                   (:START FIXNUM) (:N T) (:S T))
 ;                  (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
 ;   Documentation:
 ;     pretty print n, or all, rows from vector array of dim.
 ;     start at row (start 0).
 ;     negative start counts backwards from the last row
 ;     use s to overrid output stream.
 ;   Source file: src/array-print.lisp
```

#### $TO-LIST

```
 ; VEQ:$TO-LIST
 ;   [symbol]
 ;
 ; $TO-LIST names a compiled function:
 ;   Lambda-list: (A &KEY (DIM 1))
 ;   Derived type: (FUNCTION (SIMPLE-ARRAY &KEY (:DIM (UNSIGNED-BYTE 31)))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return array as a list of lists of length dim.
 ;   Source file: src/array-print.lisp
```

#### \*EPS\*

```
:none:

 ; VEQ:*EPS*
 ;   [symbol]
 ;
 ; *EPS* names a special variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 5.960465e-8
```

#### 2$

```
 ; VEQ:2$
 ;   [symbol]
 ;
 ; 2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF 2$) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: (2$ a i) yields (values ...)
 ;     set: (setf (2$ a i) (values ...))
 ;   Source file: src/vset.lisp
```

#### 2$NUM

```
 ; VEQ:2$NUM
 ;   [symbol]
 ;
 ; 2$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SIMPLE-ARRAY)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 2d array.
 ;     untyped.
 ;   Source file: src/array-utils.lisp
```

#### 2$PRINT

```
 ; VEQ:2$PRINT
 ;   [symbol]
 ;
 ; 2$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY N (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 2d array. returns array.
 ;   Source file: src/array-print.lisp
```

#### 2$TO-LIST

```
 ; VEQ:2$TO-LIST
 ;   [symbol]
 ;
 ; 2$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of lists of length 2.
 ;   Source file: src/array-print.lisp
```

#### 3$

```
 ; VEQ:3$
 ;   [symbol]
 ;
 ; 3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF 3$) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: (3$ a i) yields (values ...)
 ;     set: (setf (3$ a i) (values ...))
 ;   Source file: src/vset.lisp
```

#### 3$NUM

```
 ; VEQ:3$NUM
 ;   [symbol]
 ;
 ; 3$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SIMPLE-ARRAY)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 3d array.
 ;     untyped.
 ;   Source file: src/array-utils.lisp
```

#### 3$PRINT

```
 ; VEQ:3$PRINT
 ;   [symbol]
 ;
 ; 3$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY N (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 3d array. returns array.
 ;   Source file: src/array-print.lisp
```

#### 3$TO-LIST

```
 ; VEQ:3$TO-LIST
 ;   [symbol]
 ;
 ; 3$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of lists of length 3.
 ;   Source file: src/array-print.lisp
```

#### 4$

```
 ; VEQ:4$
 ;   [symbol]
 ;
 ; 4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF 4$) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: (4$ a i) yields (values ...)
 ;     set: (setf (4$ a i) (values ...))
 ;   Source file: src/vset.lisp
```

#### 4$NUM

```
 ; VEQ:4$NUM
 ;   [symbol]
 ;
 ; 4$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SIMPLE-ARRAY)
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 4d array.
 ;     untyped.
 ;   Source file: src/array-utils.lisp
```

#### 4$PRINT

```
 ; VEQ:4$PRINT
 ;   [symbol]
 ;
 ; 4$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY N (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 4d array. returns array.
 ;   Source file: src/array-print.lisp
```

#### 4$TO-LIST

```
 ; VEQ:4$TO-LIST
 ;   [symbol]
 ;
 ; 4$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of lists of length 4.
 ;   Source file: src/array-print.lisp
```

#### ARRTYPE

```
 ; VEQ:ARRTYPE
 ;   [symbol]
 ;
 ; ARRTYPE names a compiled function:
 ;   Lambda-list: (TY &OPTIONAL (MISSING NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     select array type from type hint.
 ;     eg: :ff :df 'f 'i
 ;   Source file: src/types.lisp
```

#### CONTEXT?

```
 ; VEQ:CONTEXT?
 ;   [symbol]
 ;
 ; CONTEXT? names a macro:
 ;   Lambda-list: ()
 ;   Documentation:
 ;     list all macrolet symbols (ie. ops available inside vprog, fvprogn, vdef,
 ;     fvdef defined contexts/functions) and corresponding macro body in veq
 ;     context.
 ;   Source file: src/docs.lisp
```

#### :fvprogn: D

```
strict make 1d vector in veq context.
```

#### D$

```
 ; VEQ:D$
 ;   [symbol]
 ;
 ; D$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 1d vector array (DVEC) as values.
 ;     ex: (D$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### D$_

```
 ; VEQ:D$_
 ;   [symbol]
 ;
 ; D$_ names a compiled function:
 ;   Lambda-list: (BODY)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     create DVEC vector array from body. where body is a list of lists.
 ;     ex: (D$_ (loop repeat 2 collect `(1f0 2f0)))
 ;     ex: (D$_ '((1f0 2f0) (1f0 2f0))).
 ;   Source file: src/array-utils.lisp
```

#### D$COPY

```
 ; VEQ:D$COPY
 ;   [symbol]
 ;
 ; D$COPY names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     copy DVEC vector array.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

#### D$LAST

```
 ; VEQ:D$LAST
 ;   [symbol]
 ;
 ; D$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 1d vector array.
 ;   Source file: src/array-rows.lisp
```

#### D$LINE

```
 ; VEQ:D$LINE
 ;   [symbol]
 ;
 ; D$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D$LINE
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init DVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### D$LSPACE

```
 ; VEQ:D$LSPACE
 ;   [symbol]
 ;
 ; D$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D$LSPACE
 ;     ARGS: (N (VARG 1 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### D$MAKE

```
 ; VEQ:D$MAKE
 ;   [symbol]
 ;
 ; D$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0d0))
 ;   Documentation:
 ;     create DVEC vector array with size n * dim, and initial value v.
 ;   Source file: src/array-utils.lisp
```

#### D$MIMA

```
 ; VEQ:D$MIMA
 ;   [symbol]
 ;
 ; D$MIMA names a compiled function:
 ;   Lambda-list: (A &KEY (N ($NUM A)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 1 array.
 ;     ex: (D$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### D$NUM

```
 ; VEQ:D$NUM
 ;   [symbol]
 ;
 ; D$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 1d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D$ONE

```
 ; VEQ:D$ONE
 ;   [symbol]
 ;
 ; D$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D$POINT

```
 ; VEQ:D$POINT
 ;   [symbol]
 ;
 ; D$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D$POINT
 ;     ARGS: ((VA 1 X))
 ;     DOCSTRING: init DVEC array with 1 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: D$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 1
```

#### D$VAL

```
 ; VEQ:D$VAL
 ;   [symbol]
 ;
 ; D$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D$ZERO

```
 ; VEQ:D$ZERO
 ;   [symbol]
 ;
 ; D$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D$~

```
 ; VEQ:D$~
 ;   [symbol]
 ;
 ; D$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create DVEC vector array from n values in body.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D2

```
strict make 2d vector in veq context.
```

#### D2$

```
 ; VEQ:D2$
 ;   [symbol]
 ;
 ; D2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 2d vector array (DVEC) as values.
 ;     ex: (D2$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D2$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

#### D2$LAST

```
 ; VEQ:D2$LAST
 ;   [symbol]
 ;
 ; D2$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 2d vector array.
 ;   Source file: src/array-rows.lisp
```

#### D2$LINE

```
 ; VEQ:D2$LINE
 ;   [symbol]
 ;
 ; D2$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D2$LINE
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init DVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### D2$LSPACE

```
 ; VEQ:D2$LSPACE
 ;   [symbol]
 ;
 ; D2$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D2$LSPACE
 ;     ARGS: (N (VARG 2 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### D2$MIMA

```
 ; VEQ:D2$MIMA
 ;   [symbol]
 ;
 ; D2$MIMA names a compiled function:
 ;   Lambda-list: (A &KEY (N (2$NUM A)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 2 array.
 ;     ex: (D2$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### D2$NUM

```
 ; VEQ:D2$NUM
 ;   [symbol]
 ;
 ; D2$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 2d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D2$ONE

```
 ; VEQ:D2$ONE
 ;   [symbol]
 ;
 ; D2$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D2$POINT

```
 ; VEQ:D2$POINT
 ;   [symbol]
 ;
 ; D2$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D2$POINT
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init DVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: D2$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D2$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 2
```

#### D2$VAL

```
 ; VEQ:D2$VAL
 ;   [symbol]
 ;
 ; D2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D2$ZERO

```
 ; VEQ:D2$ZERO
 ;   [symbol]
 ;
 ; D2$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D2^

```
veq context op: D2^
fxname: -D2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
```

#### :fvprogn: D2ANGLE

```
veq context op: D2ANGLE
fxname: -D2ANGLE
args: (AX AY)
body (1): (MVC #'ATAN (-D2NORM AY AX)).
```

#### :fvprogn: D2CROSS

```
veq context op: D2CROSS
fxname: -D2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
```

#### :fvprogn: D2DOT

```
veq context op: D2DOT
fxname: -D2DOT
args: (AX AY BX BY)
body (1): (+ (* AX BX) (* AY BY)).
```

#### :fvprogn: D2DST

```
veq context op: D2DST
fxname: -D2DST
args: (AX AY BX BY)
body (1): (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY))))).
```

#### :fvprogn: D2DST2

```
veq context op: D2DST2
fxname: -D2DST2
args: (AX AY BX BY)
body (1): (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY))).
```

#### :fvprogn: D2EXP

```
veq context op: D2EXP
fxname: -D2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
```

#### :fvprogn: D2FLIP

```
veq context op: D2FLIP
fxname: -D2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
```

#### :fvprogn: D2FROM

```
veq context op: D2FROM
fxname: -D2FROM
args: (AX AY BX BY S)
body (2): (VALUES (+ AX (* BX S)) (+ AY (* BY S))).
```

#### :fvprogn: D2I-

```
veq context op: D2I-
fxname: -D2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
```

#### :fvprogn: D2I/

```
veq context op: D2I/
fxname: -D2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
```

#### :fvprogn: D2ID

```
veq context op: D2ID
fxname: -D2ID
args: (AX AY)
body (2): (VALUES AX AY).
```

#### :fvprogn: D2ISCALE

```
veq context op: D2ISCALE
fxname: -D2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
```

#### :fvprogn: D2LEN

```
veq context op: D2LEN
fxname: -D2LEN
args: (AX AY)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE AX AY))))).
```

#### :fvprogn: D2LEN2

```
veq context op: D2LEN2
fxname: -D2LEN2
args: (AX AY)
body (1): (THE POS-DF (MVC #'+ (-D2SQUARE AX AY))).
```

#### :fvprogn: D2LERP

```
veq context op: D2LERP
fxname: -D2LERP
args: (AX AY BX BY S)
body (2): (VALUES (+ AX (* (- BX AX) S)) (+ AY (* (- BY AY) S))).
```

#### :fvprogn: D2LET

```
make 2d let.
ex: (D2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: D2MAX

```
veq context op: D2MAX
fxname: -D2MAX
args: (AX AY)
body (1): (MAX AX AY).
```

#### D2MEYE

```
 ; VEQ:D2MEYE
 ;   [symbol]
 ;
 ; D2MEYE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (V 1.0d0))
 ;   Derived type: (FUNCTION (&OPTIONAL DOUBLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (4)) &OPTIONAL))
 ;   Documentation:
 ;     return 2d eye matrix.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D2MID

```
veq context op: D2MID
fxname: -D2MID
args: (AX AY BX BY)
body (2): (VALUES (* (+ AX BX) 0.5d0) (* (+ AY BY) 0.5d0)).
```

#### :fvprogn: D2MIN

```
veq context op: D2MIN
fxname: -D2MIN
args: (AX AY)
body (1): (MIN AX AY).
```

#### D2MINV

```
 ; VEQ:D2MINV
 ;   [symbol]
 ;
 ; D2MINV names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (4)) &OPTIONAL))
 ;   Documentation:
 ;     invert 2x2 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### D2MM

```
 ; VEQ:D2MM
 ;   [symbol]
 ;
 ; D2MM names a macro:
 ;   Lambda-list: (A*385 B*387)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D2MMT

```
 ; VEQ:D2MMT
 ;   [symbol]
 ;
 ; D2MMT names a macro:
 ;   Lambda-list: (A*449 B*451)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D2MOD

```
veq context op: D2MOD
fxname: -D2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
```

#### D2MROT

```
 ; VEQ:D2MROT
 ;   [symbol]
 ;
 ; D2MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D2MROT
 ;     ARGS: (A)
 ;     DOCSTRING: make 2d rotation matrix for rotating a rads
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### D2MROT\*

```
 ; VEQ:D2MROT*
 ;   [symbol]
 ;
 ; D2MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D2MROT*
 ;     ARGS: (A)
 ;     DOCSTRING: make 2d rotation matrix for rotating a rads
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### D2MSCALE

```
 ; VEQ:D2MSCALE
 ;   [symbol]
 ;
 ; D2MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D2MSCALE
 ;     ARGS: ((VARG 2 X))
 ;     DOCSTRING: make 2d matrix for scaling by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### D2MT!

```
 ; VEQ:D2MT!
 ;   [symbol]
 ;
 ; D2MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 2d matrix in-place.
 ;   Source file: src/mat.lisp
```

#### D2MTM

```
 ; VEQ:D2MTM
 ;   [symbol]
 ;
 ; D2MTM names a macro:
 ;   Lambda-list: (A*481 B*483)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D2MTMT

```
 ; VEQ:D2MTMT
 ;   [symbol]
 ;
 ; D2MTMT names a macro:
 ;   Lambda-list: (A*417 B*419)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D2MTRANS

```
 ; VEQ:D2MTRANS
 ;   [symbol]
 ;
 ; D2MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D2MTRANS
 ;     ARGS: ((VARG 2 X))
 ;     DOCSTRING: make 2d transpose matrix for moving by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### D2MTV

```
 ; VEQ:D2MTV
 ;   [symbol]
 ;
 ; D2MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 2d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### D2MV

```
 ; VEQ:D2MV
 ;   [symbol]
 ;
 ; D2MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 2d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D2NORM

```
veq context op: D2NORM
fxname: -D2NORM
args: (AX AY)
body (2): (MVC #'-D2ISCALE AX AY (MVC #'-D2LEN AX AY)).
```

#### :fvprogn: D2ON-CIRC

```
veq context op: D2ON-CIRC
fxname: -D2ON-CIRC
args: (AX RAD)
body (2): (MVC #'-D2SCALE (-DCOS-SIN (* AX DPII)) RAD).
```

#### :fvprogn: D2ON-CIRC\*

```
veq context op: D2ON-CIRC*
fxname: -D2ON-CIRC*
args: (AX RAD)
body (2): (MVC #'-D2SCALE (-DCOS-SIN AX) RAD).
```

#### :fvprogn: D2PERP

```
veq context op: D2PERP
fxname: -D2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
```

#### :fvprogn: D2PERP\*

```
veq context op: D2PERP*
fxname: -D2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
```

#### :fvprogn: D2REP

```
repeat argument 2d times as values.
ex: (D2REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: D2ROT

```
veq context op: D2ROT
fxname: -D2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE (DF COSA SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
```

#### :fvprogn: D2ROTS

```
veq context op: D2ROTS
fxname: -D2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVB (RX RY) (MVC #'-D2ROT (- AX SX) (- AY SY) ANGLE) (+ SX RX)
               (+ SY RY)).
```

#### :fvprogn: D2SCALE

```
veq context op: D2SCALE
fxname: -D2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
```

#### :fvprogn: D2SQRT

```
veq context op: D2SQRT
fxname: -D2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))).
```

#### :fvprogn: D2SQUARE

```
veq context op: D2SQUARE
fxname: -D2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
```

#### :fvprogn: D2VAL

```
repeat the evaluated argument 2 times as values.
ex: (D2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: D2~

```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: D3

```
strict make 3d vector in veq context.
```

#### D3$

```
 ; VEQ:D3$
 ;   [symbol]
 ;
 ; D3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 3d vector array (DVEC) as values.
 ;     ex: (D3$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D3$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

#### D3$LAST

```
 ; VEQ:D3$LAST
 ;   [symbol]
 ;
 ; D3$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 3d vector array.
 ;   Source file: src/array-rows.lisp
```

#### D3$LINE

```
 ; VEQ:D3$LINE
 ;   [symbol]
 ;
 ; D3$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D3$LINE
 ;     ARGS: ((VA 6 X))
 ;     DOCSTRING: init DVEC array with 6 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### D3$LSPACE

```
 ; VEQ:D3$LSPACE
 ;   [symbol]
 ;
 ; D3$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D3$LSPACE
 ;     ARGS: (N (VARG 3 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### D3$MIMA

```
 ; VEQ:D3$MIMA
 ;   [symbol]
 ;
 ; D3$MIMA names a compiled function:
 ;   Lambda-list: (A &KEY (N (3$NUM A)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 3 array.
 ;     ex: (D3$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### D3$NUM

```
 ; VEQ:D3$NUM
 ;   [symbol]
 ;
 ; D3$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 3d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D3$ONE

```
 ; VEQ:D3$ONE
 ;   [symbol]
 ;
 ; D3$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D3$POINT

```
 ; VEQ:D3$POINT
 ;   [symbol]
 ;
 ; D3$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D3$POINT
 ;     ARGS: ((VA 3 X))
 ;     DOCSTRING: init DVEC array with 3 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: D3$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D3$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 3
```

#### D3$VAL

```
 ; VEQ:D3$VAL
 ;   [symbol]
 ;
 ; D3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D3$ZERO

```
 ; VEQ:D3$ZERO
 ;   [symbol]
 ;
 ; D3$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D3^

```
veq context op: D3^
fxname: -D3^
args: (AX AY AZ S)
body (3): (VALUES (THE DF (EXPT AX S)) (THE DF (EXPT AY S))
                  (THE DF (EXPT AZ S))).
```

#### :fvprogn: D3CROSS

```
veq context op: D3CROSS
fxname: -D3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
```

#### :fvprogn: D3DOT

```
veq context op: D3DOT
fxname: -D3DOT
args: (AX AY AZ BX BY BZ)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :fvprogn: D3DST

```
veq context op: D3DST
fxname: -D3DST
args: (AX AY AZ BX BY BZ)
body (1): (SQRT
           (THE POS-DF (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ))))).
```

#### :fvprogn: D3DST2

```
veq context op: D3DST2
fxname: -D3DST2
args: (AX AY AZ BX BY BZ)
body (1): (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ))).
```

#### :fvprogn: D3EXP

```
veq context op: D3EXP
fxname: -D3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
```

#### :fvprogn: D3FROM

```
veq context op: D3FROM
fxname: -D3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (VALUES (+ AX (* BX S)) (+ AY (* BY S)) (+ AZ (* BZ S))).
```

#### :fvprogn: D3I-

```
veq context op: D3I-
fxname: -D3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
```

#### :fvprogn: D3I/

```
veq context op: D3I/
fxname: -D3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
```

#### :fvprogn: D3ID

```
veq context op: D3ID
fxname: -D3ID
args: (AX AY AZ)
body (3): (VALUES AX AY AZ).
```

#### :fvprogn: D3ISCALE

```
veq context op: D3ISCALE
fxname: -D3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
```

#### :fvprogn: D3LEN

```
veq context op: D3LEN
fxname: -D3LEN
args: (AX AY AZ)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D3SQUARE AX AY AZ))))).
```

#### :fvprogn: D3LEN2

```
veq context op: D3LEN2
fxname: -D3LEN2
args: (AX AY AZ)
body (1): (THE POS-DF (MVC #'+ (-D3SQUARE AX AY AZ))).
```

#### :fvprogn: D3LERP

```
veq context op: D3LERP
fxname: -D3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (VALUES (+ AX (* (- BX AX) S)) (+ AY (* (- BY AY) S))
                  (+ AZ (* (- BZ AZ) S))).
```

#### :fvprogn: D3LET

```
make 3d let.
ex: (D3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: D3MAX

```
veq context op: D3MAX
fxname: -D3MAX
args: (AX AY AZ)
body (1): (MAX AX AY AZ).
```

#### D3MEYE

```
 ; VEQ:D3MEYE
 ;   [symbol]
 ;
 ; D3MEYE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (V 1.0d0))
 ;   Derived type: (FUNCTION (&OPTIONAL DOUBLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (9)) &OPTIONAL))
 ;   Documentation:
 ;     return 3d eye matrix.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D3MID

```
veq context op: D3MID
fxname: -D3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ AX BX) 0.5d0) (* (+ AY BY) 0.5d0) (* (+ AZ BZ) 0.5d0)).
```

#### :fvprogn: D3MIN

```
veq context op: D3MIN
fxname: -D3MIN
args: (AX AY AZ)
body (1): (MIN AX AY AZ).
```

#### D3MINV

```
 ; VEQ:D3MINV
 ;   [symbol]
 ;
 ; D3MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (9)) &OPTIONAL))
 ;   Documentation:
 ;     invert 3x3 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### D3MM

```
 ; VEQ:D3MM
 ;   [symbol]
 ;
 ; D3MM names a macro:
 ;   Lambda-list: (A*513 B*515)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D3MMT

```
 ; VEQ:D3MMT
 ;   [symbol]
 ;
 ; D3MMT names a macro:
 ;   Lambda-list: (A*577 B*579)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D3MOD

```
veq context op: D3MOD
fxname: -D3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
```

#### D3MROT

```
 ; VEQ:D3MROT
 ;   [symbol]
 ;
 ; D3MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D3MROT
 ;     ARGS: (A X Y Z)
 ;     DOCSTRING: make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### D3MROT\*

```
 ; VEQ:D3MROT*
 ;   [symbol]
 ;
 ; D3MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D3MROT*
 ;     ARGS: (A X Y Z)
 ;     DOCSTRING: make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### D3MSCALE

```
 ; VEQ:D3MSCALE
 ;   [symbol]
 ;
 ; D3MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D3MSCALE
 ;     ARGS: ((VARG 3 X))
 ;     DOCSTRING: make 3d matrix for scaling by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### D3MT!

```
 ; VEQ:D3MT!
 ;   [symbol]
 ;
 ; D3MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 3d matrix in-place.
 ;   Source file: src/mat.lisp
```

#### D3MTM

```
 ; VEQ:D3MTM
 ;   [symbol]
 ;
 ; D3MTM names a macro:
 ;   Lambda-list: (A*609 B*611)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D3MTMT

```
 ; VEQ:D3MTMT
 ;   [symbol]
 ;
 ; D3MTMT names a macro:
 ;   Lambda-list: (A*545 B*547)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D3MTRANS

```
 ; VEQ:D3MTRANS
 ;   [symbol]
 ;
 ; D3MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D3MTRANS
 ;     ARGS: ((VARG 3 X))
 ;     DOCSTRING: make 3d transpose matrix for moving by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### D3MTV

```
 ; VEQ:D3MTV
 ;   [symbol]
 ;
 ; D3MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 3d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### D3MV

```
 ; VEQ:D3MV
 ;   [symbol]
 ;
 ; D3MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 3d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D3NORM

```
veq context op: D3NORM
fxname: -D3NORM
args: (AX AY AZ)
body (3): (MVC #'-D3ISCALE AX AY AZ (THE POS-DF (MVC #'-D3LEN AX AY AZ))).
```

#### :fvprogn: D3REP

```
repeat argument 3d times as values.
ex: (D3REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: D3ROT

```
veq context op: D3ROT
fxname: -D3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE (DF COSA))
            (MVC #'-D3FROM
                 (MVC #'-D3FROM (-D3SCALE AX AY AZ COSA)
                      (-D3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-D3DOT NX NY NZ AX AY AZ) (- 1.0d0 COSA)))).
```

#### :fvprogn: D3ROTS

```
veq context op: D3ROTS
fxname: -D3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVB (RX RY RZ)
               (MVC #'-D3ROT (- AX SX) (- AY SY) (- AZ SZ) NX NY NZ A)
               (VALUES (+ (THE DF RX) SX) (+ (THE DF RY) SY)
                       (+ (THE DF RZ) SZ))).
```

#### :fvprogn: D3SCALE

```
veq context op: D3SCALE
fxname: -D3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
```

#### :fvprogn: D3SQRT

```
veq context op: D3SQRT
fxname: -D3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))).
```

#### :fvprogn: D3SQUARE

```
veq context op: D3SQUARE
fxname: -D3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ))).
```

#### :fvprogn: D3VAL

```
repeat the evaluated argument 3 times as values.
ex: (D3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: D3~

```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: D4

```
strict make 4d vector in veq context.
```

#### D4$

```
 ; VEQ:D4$
 ;   [symbol]
 ;
 ; D4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 4d vector array (DVEC) as values.
 ;     ex: (D4$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D4$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

#### D4$LAST

```
 ; VEQ:D4$LAST
 ;   [symbol]
 ;
 ; D4$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 4d vector array.
 ;   Source file: src/array-rows.lisp
```

#### D4$LINE

```
 ; VEQ:D4$LINE
 ;   [symbol]
 ;
 ; D4$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D4$LINE
 ;     ARGS: ((VA 8 X))
 ;     DOCSTRING: init DVEC array with 8 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### D4$LSPACE

```
 ; VEQ:D4$LSPACE
 ;   [symbol]
 ;
 ; D4$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D4$LSPACE
 ;     ARGS: (N (VARG 4 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### D4$NUM

```
 ; VEQ:D4$NUM
 ;   [symbol]
 ;
 ; D4$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 4d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D4$ONE

```
 ; VEQ:D4$ONE
 ;   [symbol]
 ;
 ; D4$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D4$POINT

```
 ; VEQ:D4$POINT
 ;   [symbol]
 ;
 ; D4$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %D4$POINT
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init DVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: D4$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D4$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 4
```

#### D4$VAL

```
 ; VEQ:D4$VAL
 ;   [symbol]
 ;
 ; D4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### D4$ZERO

```
 ; VEQ:D4$ZERO
 ;   [symbol]
 ;
 ; D4$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D4^

```
veq context op: D4^
fxname: -D4^
args: (AX AY AZ AW S)
body (4): (VALUES (THE DF (EXPT AX S)) (THE DF (EXPT AY S))
                  (THE DF (EXPT AZ S)) (THE DF (EXPT AW S))).
```

#### :fvprogn: D4DOT

```
veq context op: D4DOT
fxname: -D4DOT
args: (AX AY AZ AW BX BY BZ BW)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :fvprogn: D4DST

```
veq context op: D4DST
fxname: -D4DST
args: (AX AY AZ AW BX BY BZ BW)
body (1): (SQRT
           (THE POS-DF
                (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))))).
```

#### :fvprogn: D4DST2

```
veq context op: D4DST2
fxname: -D4DST2
args: (AX AY AZ AW BX BY BZ BW)
body (1): (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))).
```

#### :fvprogn: D4EXP

```
veq context op: D4EXP
fxname: -D4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
```

#### :fvprogn: D4FROM

```
veq context op: D4FROM
fxname: -D4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (VALUES (+ AX (* BX S)) (+ AY (* BY S)) (+ AZ (* BZ S))
                  (+ AW (* BW S))).
```

#### :fvprogn: D4I-

```
veq context op: D4I-
fxname: -D4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
```

#### :fvprogn: D4I/

```
veq context op: D4I/
fxname: -D4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
```

#### :fvprogn: D4ID

```
veq context op: D4ID
fxname: -D4ID
args: (AX AY AZ AW)
body (4): (VALUES AX AY AZ AW).
```

#### :fvprogn: D4ISCALE

```
veq context op: D4ISCALE
fxname: -D4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
```

#### :fvprogn: D4LEN

```
veq context op: D4LEN
fxname: -D4LEN
args: (AX AY AZ AW)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D4SQUARE AX AY AZ AW))))).
```

#### :fvprogn: D4LEN2

```
veq context op: D4LEN2
fxname: -D4LEN2
args: (AX AY AZ AW)
body (1): (THE POS-DF (MVC #'+ (-D4SQUARE AX AY AZ AW))).
```

#### :fvprogn: D4LERP

```
veq context op: D4LERP
fxname: -D4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (VALUES (+ AX (* (- BX AX) S)) (+ AY (* (- BY AY) S))
                  (+ AZ (* (- BZ AZ) S)) (+ AW (* (- BW AW) S))).
```

#### :fvprogn: D4LET

```
make 4d let.
ex: (D4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: D4MAX

```
veq context op: D4MAX
fxname: -D4MAX
args: (AX AY AZ AW)
body (1): (MAX AX AY AZ AW).
```

#### D4MEYE

```
 ; VEQ:D4MEYE
 ;   [symbol]
 ;
 ; D4MEYE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (V 1.0d0))
 ;   Derived type: (FUNCTION (&OPTIONAL DOUBLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (16)) &OPTIONAL))
 ;   Documentation:
 ;     return 4d eye matrix.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D4MID

```
veq context op: D4MID
fxname: -D4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ AX BX) 0.5d0) (* (+ AY BY) 0.5d0) (* (+ AZ BZ) 0.5d0)
                  (* (+ AW BW) 0.5d0)).
```

#### :fvprogn: D4MIN

```
veq context op: D4MIN
fxname: -D4MIN
args: (AX AY AZ AW)
body (1): (MIN AX AY AZ AW).
```

#### D4MINV

```
 ; VEQ:D4MINV
 ;   [symbol]
 ;
 ; D4MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (16)) &OPTIONAL))
 ;   Documentation:
 ;     invert 4x4 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### D4MM

```
 ; VEQ:D4MM
 ;   [symbol]
 ;
 ; D4MM names a macro:
 ;   Lambda-list: (A*641 B*643)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D4MMT

```
 ; VEQ:D4MMT
 ;   [symbol]
 ;
 ; D4MMT names a macro:
 ;   Lambda-list: (A*705 B*707)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D4MOD

```
veq context op: D4MOD
fxname: -D4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
```

#### D4MT!

```
 ; VEQ:D4MT!
 ;   [symbol]
 ;
 ; D4MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 4d matrix in-place.
 ;   Source file: src/mat.lisp
```

#### D4MTM

```
 ; VEQ:D4MTM
 ;   [symbol]
 ;
 ; D4MTM names a macro:
 ;   Lambda-list: (A*737 B*739)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D4MTMT

```
 ; VEQ:D4MTMT
 ;   [symbol]
 ;
 ; D4MTMT names a macro:
 ;   Lambda-list: (A*673 B*675)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D4MTV

```
 ; VEQ:D4MTV
 ;   [symbol]
 ;
 ; D4MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 4d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### D4MV

```
 ; VEQ:D4MV
 ;   [symbol]
 ;
 ; D4MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 4d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: D4NORM

```
veq context op: D4NORM
fxname: -D4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-D4ISCALE AX AY AZ AW (THE POS-DF (MVC #'-D4LEN AX AY AZ AW))).
```

#### :fvprogn: D4REP

```
repeat argument 4d times as values.
ex: (D4REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: D4SCALE

```
veq context op: D4SCALE
fxname: -D4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
```

#### :fvprogn: D4SQRT

```
veq context op: D4SQRT
fxname: -D4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))
                  (THE POS-DF (SQRT (THE POS-DF AW)))).
```

#### :fvprogn: D4SQUARE

```
veq context op: D4SQUARE
fxname: -D4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ)) (THE POS-DF (* AW AW))).
```

#### :fvprogn: D4VAL

```
repeat the evaluated argument 4 times as values.
ex: (D4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: D4~

```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### D?

```
 ; VEQ:D?
 ;   [symbol]
 ;
 ; D? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     describe argument
 ;   Source file: src/config.lisp
```

#### :fvprogn: D^

```
veq context op: D^
fxname: -D^
args: (AX S)
body (1): (EXPT AX S).
```

#### D_

```
 ; VEQ:D_
 ;   [symbol]
 ;
 ; D_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create DVEC vector array from body: (D_ '(a b c ...)).
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: DCLAMP

```
veq context op: DCLAMP
fxname: -DCLAMP
args: (X)
body (1): (MIN 1.0d0 (MAX 0.0d0 X)).
```

#### :fvprogn: DCLAMP\*

```
veq context op: DCLAMP*
fxname: -DCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
```

#### :fvprogn: DCOS-SIN

```
veq context op: DCOS-SIN
fxname: -DCOS-SIN
args: (AX)
body (2): (VALUES (COS AX) (SIN AX)).
```

#### :fvprogn: DDEG->RAD

```
veq context op: DDEG->RAD
fxname: -DDEG->RAD
args: (D)
body (1): (* DPI (/ D 180.0d0)).
```

#### DEASE-IN-BACK

```
 ; VEQ:DEASE-IN-BACK
 ;   [symbol]
 ;
 ; DEASE-IN-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.7015800476074219d0))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0d0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-CIRC

```
 ; VEQ:DEASE-IN-CIRC
 ;   [symbol]
 ;
 ; DEASE-IN-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT -0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-CUBIC

```
 ; VEQ:DEASE-IN-CUBIC
 ;   [symbol]
 ;
 ; DEASE-IN-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-ELASTIC

```
 ; VEQ:DEASE-IN-ELASTIC
 ;   [symbol]
 ;
 ; DEASE-IN-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR (COMPLEX DOUBLE-FLOAT)
 ;                       (DOUBLE-FLOAT -1.0d0 1.0d0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X &OPTIONAL (P 0.3) (S NIL))
 ;     body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
 ;             (-
 ;              (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;                 (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-EXP

```
 ; VEQ:DEASE-IN-EXP
 ;   [symbol]
 ;
 ; DEASE-IN-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 9.765625d-4 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-LINEAR

```
 ; VEQ:DEASE-IN-LINEAR
 ;   [symbol]
 ;
 ; DEASE-IN-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-BACK

```
 ; VEQ:DEASE-IN-OUT-BACK
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.7015800476074219d0))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0d0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-CIRC

```
 ; VEQ:DEASE-IN-OUT-CIRC
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT -0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-CUBIC

```
 ; VEQ:DEASE-IN-OUT-CUBIC
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-ELASTIC

```
 ; VEQ:DEASE-IN-OUT-ELASTIC
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR (COMPLEX DOUBLE-FLOAT)
 ;                       (DOUBLE-FLOAT -512.0d0 513.0d0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X &OPTIONAL (P 0.3) (S NIL))
 ;     body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
 ;             (-
 ;              (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;                 (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-EXP

```
 ; VEQ:DEASE-IN-OUT-EXP
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT -511.0d0 512.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-LINEAR

```
 ; VEQ:DEASE-IN-OUT-LINEAR
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-QUART

```
 ; VEQ:DEASE-IN-OUT-QUART
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT -7.0d0 8.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-QUINT

```
 ; VEQ:DEASE-IN-OUT-QUINT
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT -15.0d0 16.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-SIN

```
 ; VEQ:DEASE-IN-OUT-SIN
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (- 1.0d0 (COS (* X DPI5)))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-QUART

```
 ; VEQ:DEASE-IN-QUART
 ;   [symbol]
 ;
 ; DEASE-IN-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 4.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-QUINT

```
 ; VEQ:DEASE-IN-QUINT
 ;   [symbol]
 ;
 ; DEASE-IN-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 5.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-SIN

```
 ; VEQ:DEASE-IN-SIN
 ;   [symbol]
 ;
 ; DEASE-IN-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 0.9999999999999999d0)
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- 1.0d0 (COS (* X DPI5)))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-BACK

```
 ; VEQ:DEASE-OUT-BACK
 ;   [symbol]
 ;
 ; DEASE-OUT-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.7015800476074219d0))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0d0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-CIRC

```
 ; VEQ:DEASE-OUT-CIRC
 ;   [symbol]
 ;
 ; DEASE-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-CUBIC

```
 ; VEQ:DEASE-OUT-CUBIC
 ;   [symbol]
 ;
 ; DEASE-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-ELASTIC

```
 ; VEQ:DEASE-OUT-ELASTIC
 ;   [symbol]
 ;
 ; DEASE-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR (DOUBLE-FLOAT 0.0d0 2.0d0)
 ;                       (COMPLEX DOUBLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X &OPTIONAL (P 0.3) (S NIL))
 ;     body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
 ;             (-
 ;              (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;                 (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-EXP

```
 ; VEQ:DEASE-OUT-EXP
 ;   [symbol]
 ;
 ; DEASE-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 0.9990234375d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-LINEAR

```
 ; VEQ:DEASE-OUT-LINEAR
 ;   [symbol]
 ;
 ; DEASE-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-QUART

```
 ; VEQ:DEASE-OUT-QUART
 ;   [symbol]
 ;
 ; DEASE-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-QUINT

```
 ; VEQ:DEASE-OUT-QUINT
 ;   [symbol]
 ;
 ; DEASE-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-SIN

```
 ; VEQ:DEASE-OUT-SIN
 ;   [symbol]
 ;
 ; DEASE-OUT-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 1.1102230246251565d-16 1.0d0)
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- 1.0d0 (COS (* X DPI5)))
 ;   Source file: src/easing.lisp
```

#### DEF\*

```
 ; VEQ:DEF*
 ;   [symbol]
 ;
 ; DEF* names a macro:
 ;   Lambda-list: (MNAME &BODY BODY)
 ;   Documentation:
 ;     defines a function named: %mname
 ;     and a wrapper macro named: mname
 ;
 ;     the wrapper macro ensures every call to this function is done as
 ;     (mvc #'%mname ...).
 ;   Source file: src/macrolets.lisp
```

#### :fvprogn: DEXP

```
veq context op: DEXP
fxname: -DEXP
args: (AX)
body (1): (VALUES (EXP AX)).
```

#### DF

```
:none:

 ; VEQ:DF
 ;   [symbol]
 ;
 ; DF names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; DF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: DOUBLE-FLOAT
```

#### DF\*

```
:none:

 ; VEQ:DF*
 ;   [symbol]
 ;
 ; DF* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### DFL

```
 ; VEQ:DFL
 ;   [symbol]
 ;
 ; DFL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (df a) (df b ..) from (list a b ..).
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
```

#### :fvprogn: DFROM

```
veq context op: DFROM
fxname: -DFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
```

#### :fvprogn: DI-

```
veq context op: DI-
fxname: -DI-
args: (AX BX)
body (1): (- BX AX).
```

#### :fvprogn: DI/

```
veq context op: DI/
fxname: -DI/
args: (AX BX)
body (1): (/ BX AX).
```

#### :fvprogn: DID

```
veq context op: DID
fxname: -DID
args: (AX)
body (1): (VALUES AX).
```

#### :fvprogn: DISCALE

```
veq context op: DISCALE
fxname: -DISCALE
args: (AX S)
body (1): (/ AX S).
```

#### :fvprogn: DLEN

```
veq context op: DLEN
fxname: -DLEN
args: (AX)
body (1): (THE POS-DF AX).
```

#### :fvprogn: DLEN2

```
veq context op: DLEN2
fxname: -DLEN2
args: (AX)
body (1): (THE POS-DF (MVC #'+ (-DSQUARE AX))).
```

#### :fvprogn: DLERP

```
veq context op: DLERP
fxname: -DLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
```

#### :fvprogn: DMID

```
veq context op: DMID
fxname: -DMID
args: (AX BX)
body (1): (* (+ AX BX) 0.5d0).
```

#### :fvprogn: DMOD

```
veq context op: DMOD
fxname: -DMOD
args: (AX S)
body (1): (MOD AX S).
```

#### DPI

```
:none:

 ; VEQ:DPI
 ;   [symbol]
 ;
 ; DPI names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 3.141592653589793d0
```

#### DPI25

```
:none:

 ; VEQ:DPI25
 ;   [symbol]
 ;
 ; DPI25 names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 0.7853981633974483d0
```

#### DPI5

```
:none:

 ; VEQ:DPI5
 ;   [symbol]
 ;
 ; DPI5 names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 1.5707963267948966d0
```

#### DPII

```
:none:

 ; VEQ:DPII
 ;   [symbol]
 ;
 ; DPII names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 6.283185307179586d0
```

#### :fvprogn: DREP

```
repeat argument 1d times as values.
ex: (DREP (fx)) corresponds to (values (fx) ...).
```

#### DSB

```
:none:

 ; VEQ:DSB
 ;   [symbol]
 ;
 ; DSB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: src/generic-utils.lisp
```

#### :fvprogn: DSCALE

```
veq context op: DSCALE
fxname: -DSCALE
args: (AX S)
body (1): (* AX S).
```

#### DSEL

```
 ; VEQ:DSEL
 ;   [symbol]
 ;
 ; DSEL names a macro:
 ;   Lambda-list: ((&REST DIMS) &BODY BODY)
 ;   Documentation:
 ;     return values from body in order of dims.
 ;     use indices or :x :y :z :w
 ;     ex: (DSEL (:w :zx 0) (values a b c d)) returns: (values d c a a).
 ;   Source file: src/select-dim.lisp
```

#### :fvprogn: DSIN-COS

```
veq context op: DSIN-COS
fxname: -DSIN-COS
args: (AX)
body (2): (VALUES (SIN AX) (COS AX)).
```

#### :fvprogn: DSQRT

```
veq context op: DSQRT
fxname: -DSQRT
args: (AX)
body (1): (THE POS-DF (SQRT (THE POS-DF AX))).
```

#### :fvprogn: DSQUARE

```
veq context op: DSQUARE
fxname: -DSQUARE
args: (AX)
body (1): (* AX AX).
```

#### :fvprogn: DVAL

```
repeat the evaluated argument 1 times as values.
ex: (DVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### DVEC

```
:none:

 ; VEQ:DVEC
 ;   [symbol]
 ;
 ; DVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:DF)
```

#### DVLET

```
:none:

 ; VEQ:DVLET
 ;   [symbol]
```

#### :fvprogn: D~

```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### EXT-SYMBOLS?

```
 ; VEQ:EXT-SYMBOLS?
 ;   [symbol]
 ;
 ; EXT-SYMBOLS? names a macro:
 ;   Lambda-list: (&OPTIONAL MODE)
 ;   Documentation:
 ;     list all external symbols in veq. use :verbose to inlcude docstring.
 ;     use :pretty to print verbose output to stdout in a readable form.
 ;   Source file: src/docs.lisp
```

#### :fvprogn: F

```
strict make 1d vector in veq context.
```

#### F$

```
 ; VEQ:F$
 ;   [symbol]
 ;
 ; F$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 1d vector array (FVEC) as values.
 ;     ex: (F$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### F$_

```
 ; VEQ:F$_
 ;   [symbol]
 ;
 ; F$_ names a compiled function:
 ;   Lambda-list: (BODY)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     create FVEC vector array from body. where body is a list of lists.
 ;     ex: (F$_ (loop repeat 2 collect `(1f0 2f0)))
 ;     ex: (F$_ '((1f0 2f0) (1f0 2f0))).
 ;   Source file: src/array-utils.lisp
```

#### F$COPY

```
 ; VEQ:F$COPY
 ;   [symbol]
 ;
 ; F$COPY names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     copy FVEC vector array.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

#### F$LAST

```
 ; VEQ:F$LAST
 ;   [symbol]
 ;
 ; F$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 1d vector array.
 ;   Source file: src/array-rows.lisp
```

#### F$LINE

```
 ; VEQ:F$LINE
 ;   [symbol]
 ;
 ; F$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F$LINE
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init FVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### F$LSPACE

```
 ; VEQ:F$LSPACE
 ;   [symbol]
 ;
 ; F$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F$LSPACE
 ;     ARGS: (N (VARG 1 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### F$MAKE

```
 ; VEQ:F$MAKE
 ;   [symbol]
 ;
 ; F$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0))
 ;   Documentation:
 ;     create FVEC vector array with size n * dim, and initial value v.
 ;   Source file: src/array-utils.lisp
```

#### F$MIMA

```
 ; VEQ:F$MIMA
 ;   [symbol]
 ;
 ; F$MIMA names a compiled function:
 ;   Lambda-list: (A &KEY (N ($NUM A)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 1 array.
 ;     ex: (F$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### F$NUM

```
 ; VEQ:F$NUM
 ;   [symbol]
 ;
 ; F$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 1d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F$ONE

```
 ; VEQ:F$ONE
 ;   [symbol]
 ;
 ; F$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F$POINT

```
 ; VEQ:F$POINT
 ;   [symbol]
 ;
 ; F$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F$POINT
 ;     ARGS: ((VA 1 X))
 ;     DOCSTRING: init FVEC array with 1 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: F$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 1
```

#### F$VAL

```
 ; VEQ:F$VAL
 ;   [symbol]
 ;
 ; F$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F$ZERO

```
 ; VEQ:F$ZERO
 ;   [symbol]
 ;
 ; F$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F$~

```
 ; VEQ:F$~
 ;   [symbol]
 ;
 ; F$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create FVEC vector array from n values in body.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F2

```
strict make 2d vector in veq context.
```

#### F2$

```
 ; VEQ:F2$
 ;   [symbol]
 ;
 ; F2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 2d vector array (FVEC) as values.
 ;     ex: (F2$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### F2$CENTER

```
 ; VEQ:F2$CENTER
 ;   [symbol]
 ;
 ; F2$CENTER names a compiled function:
 ;   Lambda-list: (ARR)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     center 2d array according to n points in array. n is optional.
 ;   Source file: src/shapes.lisp
```

#### F2$CIRC

```
 ; VEQ:F2$CIRC
 ;   [symbol]
 ;
 ; F2$CIRC names a compiled function:
 ;   Lambda-list: (RAD &OPTIONAL (RS 0.5))
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (SINGLE-FLOAT 0.0)) *)
 ;   Documentation:
 ;     return circle of size rad. (rs 0.5) is vertex density.
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: F2$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

#### F2$LAST

```
 ; VEQ:F2$LAST
 ;   [symbol]
 ;
 ; F2$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 2d vector array.
 ;   Source file: src/array-rows.lisp
```

#### F2$LINE

```
 ; VEQ:F2$LINE
 ;   [symbol]
 ;
 ; F2$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2$LINE
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init FVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### F2$LSPACE

```
 ; VEQ:F2$LSPACE
 ;   [symbol]
 ;
 ; F2$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2$LSPACE
 ;     ARGS: (N (VARG 2 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### F2$MIMA

```
 ; VEQ:F2$MIMA
 ;   [symbol]
 ;
 ; F2$MIMA names a compiled function:
 ;   Lambda-list: (A &KEY (N (2$NUM A)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 2 array.
 ;     ex: (F2$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### F2$NUM

```
 ; VEQ:F2$NUM
 ;   [symbol]
 ;
 ; F2$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 2d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F2$ONE

```
 ; VEQ:F2$ONE
 ;   [symbol]
 ;
 ; F2$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F2$POINT

```
 ; VEQ:F2$POINT
 ;   [symbol]
 ;
 ; F2$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2$POINT
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init FVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### F2$POLYGON

```
 ; VEQ:F2$POLYGON
 ;   [symbol]
 ;
 ; F2$POLYGON names a compiled function:
 ;   Lambda-list: (N RAD &OPTIONAL (ROT 0.0) (PIN (/ FPII N)))
 ;   Derived type: (FUNCTION
 ;                  ((UNSIGNED-BYTE 31) SINGLE-FLOAT &OPTIONAL
 ;                   SINGLE-FLOAT SINGLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     return n-polygon of size rad. rotate by (rot 0)
 ;   Source file: src/shapes.lisp
```

#### F2$RECT

```
 ; VEQ:F2$RECT
 ;   [symbol]
 ;
 ; F2$RECT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2$RECT
 ;     ARGS: (W H)
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: F2$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F2$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 2
```

#### F2$SQUARE\*

```
:none:

 ; VEQ:F2$SQUARE*
 ;   [symbol]
 ;
 ; F2$SQUARE* names a compiled function:
 ;   Lambda-list: (S)
 ;   Derived type: (FUNCTION (SINGLE-FLOAT) *)
 ;   Source file: src/shapes.lisp
```

#### F2$VAL

```
 ; VEQ:F2$VAL
 ;   [symbol]
 ;
 ; F2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F2$ZERO

```
 ; VEQ:F2$ZERO
 ;   [symbol]
 ;
 ; F2$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F2^

```
veq context op: F2^
fxname: -F2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
```

#### :fvprogn: F2ANGLE

```
veq context op: F2ANGLE
fxname: -F2ANGLE
args: (AX AY)
body (1): (MVC #'ATAN (-F2NORM AY AX)).
```

#### :fvprogn: F2CROSS

```
veq context op: F2CROSS
fxname: -F2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
```

#### :fvprogn: F2DOT

```
veq context op: F2DOT
fxname: -F2DOT
args: (AX AY BX BY)
body (1): (+ (* AX BX) (* AY BY)).
```

#### :fvprogn: F2DST

```
veq context op: F2DST
fxname: -F2DST
args: (AX AY BX BY)
body (1): (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY))))).
```

#### :fvprogn: F2DST2

```
veq context op: F2DST2
fxname: -F2DST2
args: (AX AY BX BY)
body (1): (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY))).
```

#### :fvprogn: F2EXP

```
veq context op: F2EXP
fxname: -F2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
```

#### :fvprogn: F2FLIP

```
veq context op: F2FLIP
fxname: -F2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
```

#### :fvprogn: F2FROM

```
veq context op: F2FROM
fxname: -F2FROM
args: (AX AY BX BY S)
body (2): (VALUES (+ AX (* BX S)) (+ AY (* BY S))).
```

#### :fvprogn: F2I-

```
veq context op: F2I-
fxname: -F2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
```

#### :fvprogn: F2I/

```
veq context op: F2I/
fxname: -F2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
```

#### :fvprogn: F2ID

```
veq context op: F2ID
fxname: -F2ID
args: (AX AY)
body (2): (VALUES AX AY).
```

#### F2IN-BBOX

```
 ; VEQ:F2IN-BBOX
 ;   [symbol]
 ;
 ; F2IN-BBOX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2IN-BBOX
 ;     ARGS: ((VARG 2 TOP-LEFT BOTTOM-RIGHT PT))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/checks.lisp
```

#### F2IN-CONCAVE

```
 ; VEQ:F2IN-CONCAVE
 ;   [symbol]
 ;
 ; F2IN-CONCAVE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2IN-CONCAVE
 ;     ARGS: (SHAPE (VARG 2 PT))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/checks.lisp
```

#### F2IN-TRIANGLE

```
 ; VEQ:F2IN-TRIANGLE
 ;   [symbol]
 ;
 ; F2IN-TRIANGLE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2IN-TRIANGLE
 ;     ARGS: ((VARG 2 A B C P))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/checks.lisp
```

#### :fvprogn: F2ISCALE

```
veq context op: F2ISCALE
fxname: -F2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
```

#### :fvprogn: F2LEN

```
veq context op: F2LEN
fxname: -F2LEN
args: (AX AY)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE AX AY))))).
```

#### :fvprogn: F2LEN2

```
veq context op: F2LEN2
fxname: -F2LEN2
args: (AX AY)
body (1): (THE POS-FF (MVC #'+ (-F2SQUARE AX AY))).
```

#### :fvprogn: F2LERP

```
veq context op: F2LERP
fxname: -F2LERP
args: (AX AY BX BY S)
body (2): (VALUES (+ AX (* (- BX AX) S)) (+ AY (* (- BY AY) S))).
```

#### :fvprogn: F2LET

```
make 2d let.
ex: (F2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### F2LSEGX

```
 ; VEQ:F2LSEGX
 ;   [symbol]
 ;
 ; F2LSEGX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2LSEGX
 ;     ARGS: (LINES*)
 ;     DOCSTRING: lines = #( #(ax ay bx by) ... )
 ;
 ;     not entirely slow line-line intersection for all lines. this is faster than
 ;     comparing all lines when lines are short relative to the area that the lines
 ;     cover. it can be improved further by using binary search tree to store
 ;     current state.
 ;     defined via veq:FVDEF*
 ;   Source file: src/checks.lisp
```

#### :fvprogn: F2MAX

```
veq context op: F2MAX
fxname: -F2MAX
args: (AX AY)
body (1): (MAX AX AY).
```

#### F2MEYE

```
 ; VEQ:F2MEYE
 ;   [symbol]
 ;
 ; F2MEYE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (V 1.0))
 ;   Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (4)) &OPTIONAL))
 ;   Documentation:
 ;     return 2d eye matrix.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F2MID

```
veq context op: F2MID
fxname: -F2MID
args: (AX AY BX BY)
body (2): (VALUES (* (+ AX BX) 0.5) (* (+ AY BY) 0.5)).
```

#### :fvprogn: F2MIN

```
veq context op: F2MIN
fxname: -F2MIN
args: (AX AY)
body (1): (MIN AX AY).
```

#### F2MINV

```
 ; VEQ:F2MINV
 ;   [symbol]
 ;
 ; F2MINV names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (4)) &OPTIONAL))
 ;   Documentation:
 ;     invert 2x2 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### F2MM

```
 ; VEQ:F2MM
 ;   [symbol]
 ;
 ; F2MM names a macro:
 ;   Lambda-list: (A*1 B*3)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F2MMT

```
 ; VEQ:F2MMT
 ;   [symbol]
 ;
 ; F2MMT names a macro:
 ;   Lambda-list: (A*65 B*67)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F2MOD

```
veq context op: F2MOD
fxname: -F2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
```

#### F2MROT

```
 ; VEQ:F2MROT
 ;   [symbol]
 ;
 ; F2MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2MROT
 ;     ARGS: (A)
 ;     DOCSTRING: make 2d rotation matrix for rotating a rads
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### F2MROT\*

```
 ; VEQ:F2MROT*
 ;   [symbol]
 ;
 ; F2MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2MROT*
 ;     ARGS: (A)
 ;     DOCSTRING: make 2d rotation matrix for rotating a rads
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### F2MSCALE

```
 ; VEQ:F2MSCALE
 ;   [symbol]
 ;
 ; F2MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2MSCALE
 ;     ARGS: ((VARG 2 X))
 ;     DOCSTRING: make 2d matrix for scaling by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### F2MT!

```
 ; VEQ:F2MT!
 ;   [symbol]
 ;
 ; F2MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 2d matrix in-place.
 ;   Source file: src/mat.lisp
```

#### F2MTM

```
 ; VEQ:F2MTM
 ;   [symbol]
 ;
 ; F2MTM names a macro:
 ;   Lambda-list: (A*97 B*99)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F2MTMT

```
 ; VEQ:F2MTMT
 ;   [symbol]
 ;
 ; F2MTMT names a macro:
 ;   Lambda-list: (A*33 B*35)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F2MTRANS

```
 ; VEQ:F2MTRANS
 ;   [symbol]
 ;
 ; F2MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2MTRANS
 ;     ARGS: ((VARG 2 X))
 ;     DOCSTRING: make 2d transpose matrix for moving by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### F2MTV

```
 ; VEQ:F2MTV
 ;   [symbol]
 ;
 ; F2MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 2d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### F2MV

```
 ; VEQ:F2MV
 ;   [symbol]
 ;
 ; F2MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 2d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F2NORM

```
veq context op: F2NORM
fxname: -F2NORM
args: (AX AY)
body (2): (MVC #'-F2ISCALE AX AY (MVC #'-F2LEN AX AY)).
```

#### :fvprogn: F2ON-CIRC

```
veq context op: F2ON-CIRC
fxname: -F2ON-CIRC
args: (AX RAD)
body (2): (MVC #'-F2SCALE (-FCOS-SIN (* AX FPII)) RAD).
```

#### :fvprogn: F2ON-CIRC\*

```
veq context op: F2ON-CIRC*
fxname: -F2ON-CIRC*
args: (AX RAD)
body (2): (MVC #'-F2SCALE (-FCOS-SIN AX) RAD).
```

#### :fvprogn: F2PERP

```
veq context op: F2PERP
fxname: -F2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
```

#### :fvprogn: F2PERP\*

```
veq context op: F2PERP*
fxname: -F2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
```

#### :fvprogn: F2REP

```
repeat argument 2d times as values.
ex: (F2REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: F2ROT

```
veq context op: F2ROT
fxname: -F2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE (FF COSA SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
```

#### :fvprogn: F2ROTS

```
veq context op: F2ROTS
fxname: -F2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVB (RX RY) (MVC #'-F2ROT (- AX SX) (- AY SY) ANGLE) (+ SX RX)
               (+ SY RY)).
```

#### :fvprogn: F2SCALE

```
veq context op: F2SCALE
fxname: -F2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
```

#### F2SEGDST

```
 ; VEQ:F2SEGDST
 ;   [symbol]
 ;
 ; F2SEGDST names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2SEGDST
 ;     ARGS: ((VARG 2 VA VB V))
 ;     DOCSTRING: find distance between line, (va vb), and v.
 ;     returns (values distance s) where is is the interpolation value that will
 ;     yield the closest point on line.
 ;     defined via veq:FVDEF*
 ;   Source file: src/checks.lisp
```

#### F2SEGX

```
 ; VEQ:F2SEGX
 ;   [symbol]
 ;
 ; F2SEGX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F2SEGX
 ;     ARGS: ((VARG 2 A1 A2 B1 B2))
 ;     DOCSTRING: find intersection between lines (a1 a2), (b1 b2).
 ;     returns isect? p q where p and q is the distance along each line to the
 ;     intersection point
 ;     defined via veq:FVDEF*
 ;   Source file: src/checks.lisp
```

#### :fvprogn: F2SQRT

```
veq context op: F2SQRT
fxname: -F2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))).
```

#### :fvprogn: F2SQUARE

```
veq context op: F2SQUARE
fxname: -F2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
```

#### :fvprogn: F2VAL

```
repeat the evaluated argument 2 times as values.
ex: (F2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: F2~

```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: F3

```
strict make 3d vector in veq context.
```

#### F3$

```
 ; VEQ:F3$
 ;   [symbol]
 ;
 ; F3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 3d vector array (FVEC) as values.
 ;     ex: (F3$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F3$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

#### F3$LAST

```
 ; VEQ:F3$LAST
 ;   [symbol]
 ;
 ; F3$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 3d vector array.
 ;   Source file: src/array-rows.lisp
```

#### F3$LINE

```
 ; VEQ:F3$LINE
 ;   [symbol]
 ;
 ; F3$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3$LINE
 ;     ARGS: ((VA 6 X))
 ;     DOCSTRING: init FVEC array with 6 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### F3$LSPACE

```
 ; VEQ:F3$LSPACE
 ;   [symbol]
 ;
 ; F3$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3$LSPACE
 ;     ARGS: (N (VARG 3 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### F3$MIMA

```
 ; VEQ:F3$MIMA
 ;   [symbol]
 ;
 ; F3$MIMA names a compiled function:
 ;   Lambda-list: (A &KEY (N (3$NUM A)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 3 array.
 ;     ex: (F3$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### F3$NUM

```
 ; VEQ:F3$NUM
 ;   [symbol]
 ;
 ; F3$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 3d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F3$ONE

```
 ; VEQ:F3$ONE
 ;   [symbol]
 ;
 ; F3$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F3$POINT

```
 ; VEQ:F3$POINT
 ;   [symbol]
 ;
 ; F3$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3$POINT
 ;     ARGS: ((VA 3 X))
 ;     DOCSTRING: init FVEC array with 3 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: F3$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F3$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 3
```

#### F3$VAL

```
 ; VEQ:F3$VAL
 ;   [symbol]
 ;
 ; F3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F3$ZERO

```
 ; VEQ:F3$ZERO
 ;   [symbol]
 ;
 ; F3$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F3^

```
veq context op: F3^
fxname: -F3^
args: (AX AY AZ S)
body (3): (VALUES (THE FF (EXPT AX S)) (THE FF (EXPT AY S))
                  (THE FF (EXPT AZ S))).
```

#### :fvprogn: F3CROSS

```
veq context op: F3CROSS
fxname: -F3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
```

#### :fvprogn: F3DOT

```
veq context op: F3DOT
fxname: -F3DOT
args: (AX AY AZ BX BY BZ)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :fvprogn: F3DST

```
veq context op: F3DST
fxname: -F3DST
args: (AX AY AZ BX BY BZ)
body (1): (SQRT
           (THE POS-FF (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ))))).
```

#### :fvprogn: F3DST2

```
veq context op: F3DST2
fxname: -F3DST2
args: (AX AY AZ BX BY BZ)
body (1): (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ))).
```

#### :fvprogn: F3EXP

```
veq context op: F3EXP
fxname: -F3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
```

#### :fvprogn: F3FROM

```
veq context op: F3FROM
fxname: -F3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (VALUES (+ AX (* BX S)) (+ AY (* BY S)) (+ AZ (* BZ S))).
```

#### :fvprogn: F3I-

```
veq context op: F3I-
fxname: -F3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
```

#### :fvprogn: F3I/

```
veq context op: F3I/
fxname: -F3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
```

#### :fvprogn: F3ID

```
veq context op: F3ID
fxname: -F3ID
args: (AX AY AZ)
body (3): (VALUES AX AY AZ).
```

#### :fvprogn: F3ISCALE

```
veq context op: F3ISCALE
fxname: -F3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
```

#### :fvprogn: F3LEN

```
veq context op: F3LEN
fxname: -F3LEN
args: (AX AY AZ)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F3SQUARE AX AY AZ))))).
```

#### :fvprogn: F3LEN2

```
veq context op: F3LEN2
fxname: -F3LEN2
args: (AX AY AZ)
body (1): (THE POS-FF (MVC #'+ (-F3SQUARE AX AY AZ))).
```

#### :fvprogn: F3LERP

```
veq context op: F3LERP
fxname: -F3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (VALUES (+ AX (* (- BX AX) S)) (+ AY (* (- BY AY) S))
                  (+ AZ (* (- BZ AZ) S))).
```

#### :fvprogn: F3LET

```
make 3d let.
ex: (F3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: F3MAX

```
veq context op: F3MAX
fxname: -F3MAX
args: (AX AY AZ)
body (1): (MAX AX AY AZ).
```

#### F3MEYE

```
 ; VEQ:F3MEYE
 ;   [symbol]
 ;
 ; F3MEYE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (V 1.0))
 ;   Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (9)) &OPTIONAL))
 ;   Documentation:
 ;     return 3d eye matrix.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F3MID

```
veq context op: F3MID
fxname: -F3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ AX BX) 0.5) (* (+ AY BY) 0.5) (* (+ AZ BZ) 0.5)).
```

#### :fvprogn: F3MIN

```
veq context op: F3MIN
fxname: -F3MIN
args: (AX AY AZ)
body (1): (MIN AX AY AZ).
```

#### F3MINV

```
 ; VEQ:F3MINV
 ;   [symbol]
 ;
 ; F3MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (9)) &OPTIONAL))
 ;   Documentation:
 ;     invert 3x3 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### F3MM

```
 ; VEQ:F3MM
 ;   [symbol]
 ;
 ; F3MM names a macro:
 ;   Lambda-list: (A*129 B*131)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F3MMT

```
 ; VEQ:F3MMT
 ;   [symbol]
 ;
 ; F3MMT names a macro:
 ;   Lambda-list: (A*193 B*195)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F3MOD

```
veq context op: F3MOD
fxname: -F3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
```

#### F3MROT

```
 ; VEQ:F3MROT
 ;   [symbol]
 ;
 ; F3MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3MROT
 ;     ARGS: (A X Y Z)
 ;     DOCSTRING: make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### F3MROT\*

```
 ; VEQ:F3MROT*
 ;   [symbol]
 ;
 ; F3MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3MROT*
 ;     ARGS: (A X Y Z)
 ;     DOCSTRING: make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;     defined via veq:DEF*
 ;   Source file: src/mat.lisp
```

#### F3MSCALE

```
 ; VEQ:F3MSCALE
 ;   [symbol]
 ;
 ; F3MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3MSCALE
 ;     ARGS: ((VARG 3 X))
 ;     DOCSTRING: make 3d matrix for scaling by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### F3MT!

```
 ; VEQ:F3MT!
 ;   [symbol]
 ;
 ; F3MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 3d matrix in-place.
 ;   Source file: src/mat.lisp
```

#### F3MTM

```
 ; VEQ:F3MTM
 ;   [symbol]
 ;
 ; F3MTM names a macro:
 ;   Lambda-list: (A*225 B*227)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F3MTMT

```
 ; VEQ:F3MTMT
 ;   [symbol]
 ;
 ; F3MTMT names a macro:
 ;   Lambda-list: (A*161 B*163)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F3MTRANS

```
 ; VEQ:F3MTRANS
 ;   [symbol]
 ;
 ; F3MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3MTRANS
 ;     ARGS: ((VARG 3 X))
 ;     DOCSTRING: make 3d transpose matrix for moving by x
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat.lisp
```

#### F3MTV

```
 ; VEQ:F3MTV
 ;   [symbol]
 ;
 ; F3MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 3d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### F3MV

```
 ; VEQ:F3MV
 ;   [symbol]
 ;
 ; F3MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 3d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F3NORM

```
veq context op: F3NORM
fxname: -F3NORM
args: (AX AY AZ)
body (3): (MVC #'-F3ISCALE AX AY AZ (THE POS-FF (MVC #'-F3LEN AX AY AZ))).
```

#### F3PLANEX

```
 ; VEQ:F3PLANEX
 ;   [symbol]
 ;
 ; F3PLANEX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F3PLANEX
 ;     ARGS: ((VARG 3 N P A B))
 ;     DOCSTRING: intersection of plane (n:normal, p:point) and line (a b)
 ;     defined via veq:FVDEF*
 ;   Source file: src/checks.lisp
```

#### :fvprogn: F3REP

```
repeat argument 3d times as values.
ex: (F3REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: F3ROT

```
veq context op: F3ROT
fxname: -F3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE (FF COSA))
            (MVC #'-F3FROM
                 (MVC #'-F3FROM (-F3SCALE AX AY AZ COSA)
                      (-F3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-F3DOT NX NY NZ AX AY AZ) (- 1.0 COSA)))).
```

#### :fvprogn: F3ROTS

```
veq context op: F3ROTS
fxname: -F3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVB (RX RY RZ)
               (MVC #'-F3ROT (- AX SX) (- AY SY) (- AZ SZ) NX NY NZ A)
               (VALUES (+ (THE FF RX) SX) (+ (THE FF RY) SY)
                       (+ (THE FF RZ) SZ))).
```

#### :fvprogn: F3SCALE

```
veq context op: F3SCALE
fxname: -F3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
```

#### :fvprogn: F3SQRT

```
veq context op: F3SQRT
fxname: -F3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))).
```

#### :fvprogn: F3SQUARE

```
veq context op: F3SQUARE
fxname: -F3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ))).
```

#### :fvprogn: F3VAL

```
repeat the evaluated argument 3 times as values.
ex: (F3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: F3~

```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: F4

```
strict make 4d vector in veq context.
```

#### F4$

```
 ; VEQ:F4$
 ;   [symbol]
 ;
 ; F4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 4d vector array (FVEC) as values.
 ;     ex: (F4$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F4$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

#### F4$LAST

```
 ; VEQ:F4$LAST
 ;   [symbol]
 ;
 ; F4$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     return values from last row of 4d vector array.
 ;   Source file: src/array-rows.lisp
```

#### F4$LINE

```
 ; VEQ:F4$LINE
 ;   [symbol]
 ;
 ; F4$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F4$LINE
 ;     ARGS: ((VA 8 X))
 ;     DOCSTRING: init FVEC array with 8 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### F4$LSPACE

```
 ; VEQ:F4$LSPACE
 ;   [symbol]
 ;
 ; F4$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F4$LSPACE
 ;     ARGS: (N (VARG 4 A B) &KEY (END T))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/fxlspace.lisp
```

#### F4$NUM

```
 ; VEQ:F4$NUM
 ;   [symbol]
 ;
 ; F4$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 4d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F4$ONE

```
 ; VEQ:F4$ONE
 ;   [symbol]
 ;
 ; F4$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F4$POINT

```
 ; VEQ:F4$POINT
 ;   [symbol]
 ;
 ; F4$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %F4$POINT
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init FVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### :fvprogn: F4$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F4$S c structname- :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 4
```

#### F4$VAL

```
 ; VEQ:F4$VAL
 ;   [symbol]
 ;
 ; F4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### F4$ZERO

```
 ; VEQ:F4$ZERO
 ;   [symbol]
 ;
 ; F4$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F4^

```
veq context op: F4^
fxname: -F4^
args: (AX AY AZ AW S)
body (4): (VALUES (THE FF (EXPT AX S)) (THE FF (EXPT AY S))
                  (THE FF (EXPT AZ S)) (THE FF (EXPT AW S))).
```

#### :fvprogn: F4DOT

```
veq context op: F4DOT
fxname: -F4DOT
args: (AX AY AZ AW BX BY BZ BW)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :fvprogn: F4DST

```
veq context op: F4DST
fxname: -F4DST
args: (AX AY AZ AW BX BY BZ BW)
body (1): (SQRT
           (THE POS-FF
                (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))))).
```

#### :fvprogn: F4DST2

```
veq context op: F4DST2
fxname: -F4DST2
args: (AX AY AZ AW BX BY BZ BW)
body (1): (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))).
```

#### :fvprogn: F4EXP

```
veq context op: F4EXP
fxname: -F4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
```

#### :fvprogn: F4FROM

```
veq context op: F4FROM
fxname: -F4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (VALUES (+ AX (* BX S)) (+ AY (* BY S)) (+ AZ (* BZ S))
                  (+ AW (* BW S))).
```

#### :fvprogn: F4I-

```
veq context op: F4I-
fxname: -F4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
```

#### :fvprogn: F4I/

```
veq context op: F4I/
fxname: -F4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
```

#### :fvprogn: F4ID

```
veq context op: F4ID
fxname: -F4ID
args: (AX AY AZ AW)
body (4): (VALUES AX AY AZ AW).
```

#### :fvprogn: F4ISCALE

```
veq context op: F4ISCALE
fxname: -F4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
```

#### :fvprogn: F4LEN

```
veq context op: F4LEN
fxname: -F4LEN
args: (AX AY AZ AW)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F4SQUARE AX AY AZ AW))))).
```

#### :fvprogn: F4LEN2

```
veq context op: F4LEN2
fxname: -F4LEN2
args: (AX AY AZ AW)
body (1): (THE POS-FF (MVC #'+ (-F4SQUARE AX AY AZ AW))).
```

#### :fvprogn: F4LERP

```
veq context op: F4LERP
fxname: -F4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (VALUES (+ AX (* (- BX AX) S)) (+ AY (* (- BY AY) S))
                  (+ AZ (* (- BZ AZ) S)) (+ AW (* (- BW AW) S))).
```

#### :fvprogn: F4LET

```
make 4d let.
ex: (F4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: F4MAX

```
veq context op: F4MAX
fxname: -F4MAX
args: (AX AY AZ AW)
body (1): (MAX AX AY AZ AW).
```

#### F4MEYE

```
 ; VEQ:F4MEYE
 ;   [symbol]
 ;
 ; F4MEYE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (V 1.0))
 ;   Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (16)) &OPTIONAL))
 ;   Documentation:
 ;     return 4d eye matrix.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F4MID

```
veq context op: F4MID
fxname: -F4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ AX BX) 0.5) (* (+ AY BY) 0.5) (* (+ AZ BZ) 0.5)
                  (* (+ AW BW) 0.5)).
```

#### :fvprogn: F4MIN

```
veq context op: F4MIN
fxname: -F4MIN
args: (AX AY AZ AW)
body (1): (MIN AX AY AZ AW).
```

#### F4MINV

```
 ; VEQ:F4MINV
 ;   [symbol]
 ;
 ; F4MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (16)) &OPTIONAL))
 ;   Documentation:
 ;     invert 4x4 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### F4MM

```
 ; VEQ:F4MM
 ;   [symbol]
 ;
 ; F4MM names a macro:
 ;   Lambda-list: (A*257 B*259)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F4MMT

```
 ; VEQ:F4MMT
 ;   [symbol]
 ;
 ; F4MMT names a macro:
 ;   Lambda-list: (A*321 B*323)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F4MOD

```
veq context op: F4MOD
fxname: -F4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
```

#### F4MT!

```
 ; VEQ:F4MT!
 ;   [symbol]
 ;
 ; F4MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 4d matrix in-place.
 ;   Source file: src/mat.lisp
```

#### F4MTM

```
 ; VEQ:F4MTM
 ;   [symbol]
 ;
 ; F4MTM names a macro:
 ;   Lambda-list: (A*353 B*355)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F4MTMT

```
 ; VEQ:F4MTMT
 ;   [symbol]
 ;
 ; F4MTMT names a macro:
 ;   Lambda-list: (A*289 B*291)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F4MTV

```
 ; VEQ:F4MTV
 ;   [symbol]
 ;
 ; F4MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 4d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### F4MV

```
 ; VEQ:F4MV
 ;   [symbol]
 ;
 ; F4MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 4d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :fvprogn: F4NORM

```
veq context op: F4NORM
fxname: -F4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-F4ISCALE AX AY AZ AW (THE POS-FF (MVC #'-F4LEN AX AY AZ AW))).
```

#### :fvprogn: F4REP

```
repeat argument 4d times as values.
ex: (F4REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: F4SCALE

```
veq context op: F4SCALE
fxname: -F4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
```

#### :fvprogn: F4SQRT

```
veq context op: F4SQRT
fxname: -F4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))
                  (THE POS-FF (SQRT (THE POS-FF AW)))).
```

#### :fvprogn: F4SQUARE

```
veq context op: F4SQUARE
fxname: -F4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ)) (THE POS-FF (* AW AW))).
```

#### :fvprogn: F4VAL

```
repeat the evaluated argument 4 times as values.
ex: (F4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: F4~

```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: F^

```
veq context op: F^
fxname: -F^
args: (AX S)
body (1): (EXPT AX S).
```

#### F_

```
 ; VEQ:F_
 ;   [symbol]
 ;
 ; F_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create FVEC vector array from body: (F_ '(a b c ...)).
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: FCLAMP

```
veq context op: FCLAMP
fxname: -FCLAMP
args: (X)
body (1): (MIN 1.0 (MAX 0.0 X)).
```

#### :fvprogn: FCLAMP\*

```
veq context op: FCLAMP*
fxname: -FCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
```

#### :fvprogn: FCOS-SIN

```
veq context op: FCOS-SIN
fxname: -FCOS-SIN
args: (AX)
body (2): (VALUES (COS AX) (SIN AX)).
```

#### :fvprogn: FDEG->RAD

```
veq context op: FDEG->RAD
fxname: -FDEG->RAD
args: (D)
body (1): (* FPI (/ D 180.0)).
```

#### FEASE-IN-BACK

```
 ; VEQ:FEASE-IN-BACK
 ;   [symbol]
 ;
 ; FEASE-IN-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.70158))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
 ;                       (COMPLEX DOUBLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-CIRC

```
 ; VEQ:FEASE-IN-CIRC
 ;   [symbol]
 ;
 ; FEASE-IN-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT -0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-CUBIC

```
 ; VEQ:FEASE-IN-CUBIC
 ;   [symbol]
 ;
 ; FEASE-IN-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-ELASTIC

```
 ; VEQ:FEASE-IN-ELASTIC
 ;   [symbol]
 ;
 ; FEASE-IN-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR (SINGLE-FLOAT -1.0 1.0)
 ;                       (DOUBLE-FLOAT -1.0d0 1.0d0)
 ;                       (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X &OPTIONAL (P 0.3) (S NIL))
 ;     body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
 ;             (-
 ;              (* (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;                 (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-EXP

```
 ; VEQ:FEASE-IN-EXP
 ;   [symbol]
 ;
 ; FEASE-IN-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT 9.765625e-4 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-LINEAR

```
 ; VEQ:FEASE-IN-LINEAR
 ;   [symbol]
 ;
 ; FEASE-IN-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-BACK

```
 ; VEQ:FEASE-IN-OUT-BACK
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.70158))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
 ;                       (COMPLEX DOUBLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-CIRC

```
 ; VEQ:FEASE-IN-OUT-CIRC
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT -0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-CUBIC

```
 ; VEQ:FEASE-IN-OUT-CUBIC
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-ELASTIC

```
 ; VEQ:FEASE-IN-OUT-ELASTIC
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR (SINGLE-FLOAT -512.0 513.0)
 ;                       (DOUBLE-FLOAT -512.0d0 513.0d0)
 ;                       (COMPLEX SINGLE-FLOAT) (COMPLEX DOUBLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X &OPTIONAL (P 0.3) (S NIL))
 ;     body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
 ;             (-
 ;              (* (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;                 (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-EXP

```
 ; VEQ:FEASE-IN-OUT-EXP
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT -511.0 512.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-LINEAR

```
 ; VEQ:FEASE-IN-OUT-LINEAR
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-QUART

```
 ; VEQ:FEASE-IN-OUT-QUART
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT -7.0 8.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-QUINT

```
 ; VEQ:FEASE-IN-OUT-QUINT
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT -15.0 16.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-SIN

```
 ; VEQ:FEASE-IN-OUT-SIN
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (- 1.0 (COS (* X FPI5)))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-QUART

```
 ; VEQ:FEASE-IN-QUART
 ;   [symbol]
 ;
 ; FEASE-IN-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 4.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-QUINT

```
 ; VEQ:FEASE-IN-QUINT
 ;   [symbol]
 ;
 ; FEASE-IN-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 5.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-SIN

```
 ; VEQ:FEASE-IN-SIN
 ;   [symbol]
 ;
 ; FEASE-IN-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- 1.0 (COS (* X FPI5)))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-BACK

```
 ; VEQ:FEASE-OUT-BACK
 ;   [symbol]
 ;
 ; FEASE-OUT-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.70158))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
 ;                       (COMPLEX DOUBLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-CIRC

```
 ; VEQ:FEASE-OUT-CIRC
 ;   [symbol]
 ;
 ; FEASE-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-CUBIC

```
 ; VEQ:FEASE-OUT-CUBIC
 ;   [symbol]
 ;
 ; FEASE-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-ELASTIC

```
 ; VEQ:FEASE-OUT-ELASTIC
 ;   [symbol]
 ;
 ; FEASE-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR (COMPLEX DOUBLE-FLOAT) (COMPLEX SINGLE-FLOAT)
 ;                       (DOUBLE-FLOAT 0.0d0 2.0d0)
 ;                       (SINGLE-FLOAT 0.0 2.0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X &OPTIONAL (P 0.3) (S NIL))
 ;     body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
 ;             (-
 ;              (* (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;                 (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-EXP

```
 ; VEQ:FEASE-OUT-EXP
 ;   [symbol]
 ;
 ; FEASE-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT 0.0 0.99902344) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-LINEAR

```
 ; VEQ:FEASE-OUT-LINEAR
 ;   [symbol]
 ;
 ; FEASE-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-QUART

```
 ; VEQ:FEASE-OUT-QUART
 ;   [symbol]
 ;
 ; FEASE-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-QUINT

```
 ; VEQ:FEASE-OUT-QUINT
 ;   [symbol]
 ;
 ; FEASE-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-SIN

```
 ; VEQ:FEASE-OUT-SIN
 ;   [symbol]
 ;
 ; FEASE-OUT-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- 1.0 (COS (* X FPI5)))
 ;   Source file: src/easing.lisp
```

#### :fvprogn: FEXP

```
veq context op: FEXP
fxname: -FEXP
args: (AX)
body (1): (VALUES (EXP AX)).
```

#### FF

```
:none:

 ; VEQ:FF
 ;   [symbol]
 ;
 ; FF names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; FF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: SINGLE-FLOAT
```

#### FF\*

```
:none:

 ; VEQ:FF*
 ;   [symbol]
 ;
 ; FF* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### FFL

```
 ; VEQ:FFL
 ;   [symbol]
 ;
 ; FFL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (ff a) (ff b) ..) from (list a b ..).
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
```

#### :fvprogn: FFROM

```
veq context op: FFROM
fxname: -FFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
```

#### :fvprogn: FI-

```
veq context op: FI-
fxname: -FI-
args: (AX BX)
body (1): (- BX AX).
```

#### :fvprogn: FI/

```
veq context op: FI/
fxname: -FI/
args: (AX BX)
body (1): (/ BX AX).
```

#### :fvprogn: FID

```
veq context op: FID
fxname: -FID
args: (AX)
body (1): (VALUES AX).
```

#### :fvprogn: FISCALE

```
veq context op: FISCALE
fxname: -FISCALE
args: (AX S)
body (1): (/ AX S).
```

#### :fvprogn: FLEN

```
veq context op: FLEN
fxname: -FLEN
args: (AX)
body (1): (THE POS-FF AX).
```

#### :fvprogn: FLEN2

```
veq context op: FLEN2
fxname: -FLEN2
args: (AX)
body (1): (THE POS-FF (MVC #'+ (-FSQUARE AX))).
```

#### :fvprogn: FLERP

```
veq context op: FLERP
fxname: -FLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
```

#### FMAKE-ORTHO-PROJ-MATRIX

```
 ; VEQ:FMAKE-ORTHO-PROJ-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-ORTHO-PROJ-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %FMAKE-ORTHO-PROJ-MATRIX
 ;     ARGS: (&OPTIONAL (W 1.0) (H W) (N 0.1) (F 100.0))
 ;     DOCSTRING: make orthogonal projection matrix
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat-cam.lisp
```

#### FMAKE-PROJ-MATRIX

```
 ; VEQ:FMAKE-PROJ-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-PROJ-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %FMAKE-PROJ-MATRIX
 ;     ARGS: (&OPTIONAL (W 1.0) (H W) (N 0.1) (F 100.0))
 ;     DOCSTRING: make projection matrix for width, height, near, far
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat-cam.lisp
```

#### FMAKE-VIEW-MATRIX

```
 ; VEQ:FMAKE-VIEW-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-VIEW-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %FMAKE-VIEW-MATRIX
 ;     ARGS: ((VA 3 CAM TARGET UP))
 ;     DOCSTRING: make view matrix for cam (w/up) looking at target
 ;     defined via veq:FVDEF*
 ;   Source file: src/mat-cam.lisp
```

#### :fvprogn: FMID

```
veq context op: FMID
fxname: -FMID
args: (AX BX)
body (1): (* (+ AX BX) 0.5).
```

#### :fvprogn: FMOD

```
veq context op: FMOD
fxname: -FMOD
args: (AX S)
body (1): (MOD AX S).
```

#### FPI

```
:none:

 ; VEQ:FPI
 ;   [symbol]
 ;
 ; FPI names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 3.1415927
```

#### FPI25

```
:none:

 ; VEQ:FPI25
 ;   [symbol]
 ;
 ; FPI25 names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 0.7853982
```

#### FPI5

```
:none:

 ; VEQ:FPI5
 ;   [symbol]
 ;
 ; FPI5 names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 1.5707964
```

#### FPII

```
:none:

 ; VEQ:FPII
 ;   [symbol]
 ;
 ; FPII names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 6.2831855
```

#### :fvprogn: FREP

```
repeat argument 1d times as values.
ex: (FREP (fx)) corresponds to (values (fx) ...).
```

#### FROM-LST

```
 ; VEQ:FROM-LST
 ;   [symbol]
 ;
 ; FROM-LST names a macro:
 ;   Lambda-list: (L)
 ;   Documentation:
 ;     return list as values. equivalent to (values-list ...).
 ;   Source file: src/utils.lisp
```

#### :fvprogn: FSCALE

```
veq context op: FSCALE
fxname: -FSCALE
args: (AX S)
body (1): (* AX S).
```

#### FSEL

```
 ; VEQ:FSEL
 ;   [symbol]
 ;
 ; FSEL names a macro:
 ;   Lambda-list: ((&REST DIMS) &BODY BODY)
 ;   Documentation:
 ;     return values from body in order of dims.
 ;     use indices or :x :y :z :w
 ;     ex: (FSEL (:w :zx 0) (values a b c d)) returns: (values d c a a).
 ;   Source file: src/select-dim.lisp
```

#### :fvprogn: FSIN-COS

```
veq context op: FSIN-COS
fxname: -FSIN-COS
args: (AX)
body (2): (VALUES (SIN AX) (COS AX)).
```

#### :fvprogn: FSQRT

```
veq context op: FSQRT
fxname: -FSQRT
args: (AX)
body (1): (THE POS-FF (SQRT (THE POS-FF AX))).
```

#### :fvprogn: FSQUARE

```
veq context op: FSQUARE
fxname: -FSQUARE
args: (AX)
body (1): (* AX AX).
```

#### :fvprogn: FVAL

```
repeat the evaluated argument 1 times as values.
ex: (FVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### FVDEF

```
 ; VEQ:FVDEF
 ;   [symbol]
 ;
 ; FVDEF names a macro:
 ;   Lambda-list: (FNAME &BODY BODY)
 ;   Documentation:
 ;     define function with veq context enabled. see fvprogn.
 ;   Source file: src/macrolets.lisp
```

#### FVDEF\*

```
 ; VEQ:FVDEF*
 ;   [symbol]
 ;
 ; FVDEF* names a macro:
 ;   Lambda-list: (MNAME &BODY BODY)
 ;   Documentation:
 ;     defines a function named: %mname
 ;     and a wrapper macro named: mname
 ;     veq context is enabled. uses fvprogn.
 ;
 ;     the wrapper macro ensures every call to this function is done as
 ;     (mvc #'%mname ...).
 ;   Source file: src/macrolets.lisp
```

#### FVEC

```
:none:

 ; VEQ:FVEC
 ;   [symbol]
 ;
 ; FVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:FF)
```

#### FVLET

```
:none:

 ; VEQ:FVLET
 ;   [symbol]
```

#### FVPROGN

```
 ; VEQ:FVPROGN
 ;   [symbol]
 ;
 ; FVPROGN names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     enable veq context inside this progn.
 ;     handles propagation and resolution of uses of (varg d var) and (vref var i).
 ;     also handles vv macro compiler triggers. see vv macro.
 ;
 ;     works the same way as vprogn. but removes all macrolets that are not
 ;     directly referenced by a symbol in body. this is faster, but may fail in some
 ;     cases where body is complex. in the event of errors try vprogn instead.
 ;   Source file: src/macrolets.lisp
```

#### :fvprogn: F~

```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: I

```
strict make 1d vector in veq context.
```

#### I$

```
 ; VEQ:I$
 ;   [symbol]
 ;
 ; I$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 1d vector array (IVEC) as values.
 ;     ex: (I$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### I$_

```
 ; VEQ:I$_
 ;   [symbol]
 ;
 ; I$_ names a compiled function:
 ;   Lambda-list: (BODY)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     create IVEC vector array from body. where body is a list of lists.
 ;     ex: (I$_ (loop repeat 2 collect `(1f0 2f0)))
 ;     ex: (I$_ '((1f0 2f0) (1f0 2f0))).
 ;   Source file: src/array-utils.lisp
```

#### I$COPY

```
 ; VEQ:I$COPY
 ;   [symbol]
 ;
 ; I$COPY names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY FIXNUM))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     copy IVEC vector array.
 ;   Source file: src/array-utils.lisp
```

#### I$LINE

```
 ; VEQ:I$LINE
 ;   [symbol]
 ;
 ; I$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I$LINE
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init IVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I$MAKE

```
 ; VEQ:I$MAKE
 ;   [symbol]
 ;
 ; I$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0))
 ;   Documentation:
 ;     create IVEC vector array with size n * dim, and initial value v.
 ;   Source file: src/array-utils.lisp
```

#### I$NUM

```
 ; VEQ:I$NUM
 ;   [symbol]
 ;
 ; I$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY FIXNUM))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 1d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I$ONE

```
 ; VEQ:I$ONE
 ;   [symbol]
 ;
 ; I$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I$POINT

```
 ; VEQ:I$POINT
 ;   [symbol]
 ;
 ; I$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I$POINT
 ;     ARGS: ((VA 1 X))
 ;     DOCSTRING: init IVEC array with 1 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I$VAL

```
 ; VEQ:I$VAL
 ;   [symbol]
 ;
 ; I$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (FIXNUM &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I$ZERO

```
 ; VEQ:I$ZERO
 ;   [symbol]
 ;
 ; I$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I$~

```
 ; VEQ:I$~
 ;   [symbol]
 ;
 ; I$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create IVEC vector array from n values in body.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: I2

```
strict make 2d vector in veq context.
```

#### I2$

```
 ; VEQ:I2$
 ;   [symbol]
 ;
 ; I2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 2d vector array (IVEC) as values.
 ;     ex: (I2$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### I2$LINE

```
 ; VEQ:I2$LINE
 ;   [symbol]
 ;
 ; I2$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I2$LINE
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init IVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I2$NUM

```
 ; VEQ:I2$NUM
 ;   [symbol]
 ;
 ; I2$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY FIXNUM))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 2d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I2$ONE

```
 ; VEQ:I2$ONE
 ;   [symbol]
 ;
 ; I2$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I2$POINT

```
 ; VEQ:I2$POINT
 ;   [symbol]
 ;
 ; I2$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I2$POINT
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init IVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I2$VAL

```
 ; VEQ:I2$VAL
 ;   [symbol]
 ;
 ; I2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (FIXNUM &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I2$ZERO

```
 ; VEQ:I2$ZERO
 ;   [symbol]
 ;
 ; I2$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: I2LET

```
make 2d let.
ex: (I2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: I2REP

```
repeat argument 2d times as values.
ex: (I2REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: I2VAL

```
repeat the evaluated argument 2 times as values.
ex: (I2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: I2~

```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: I3

```
strict make 3d vector in veq context.
```

#### I3$

```
 ; VEQ:I3$
 ;   [symbol]
 ;
 ; I3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 3d vector array (IVEC) as values.
 ;     ex: (I3$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### I3$LINE

```
 ; VEQ:I3$LINE
 ;   [symbol]
 ;
 ; I3$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I3$LINE
 ;     ARGS: ((VA 6 X))
 ;     DOCSTRING: init IVEC array with 6 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I3$NUM

```
 ; VEQ:I3$NUM
 ;   [symbol]
 ;
 ; I3$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY FIXNUM))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 3d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I3$ONE

```
 ; VEQ:I3$ONE
 ;   [symbol]
 ;
 ; I3$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I3$POINT

```
 ; VEQ:I3$POINT
 ;   [symbol]
 ;
 ; I3$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I3$POINT
 ;     ARGS: ((VA 3 X))
 ;     DOCSTRING: init IVEC array with 3 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I3$VAL

```
 ; VEQ:I3$VAL
 ;   [symbol]
 ;
 ; I3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (FIXNUM &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I3$ZERO

```
 ; VEQ:I3$ZERO
 ;   [symbol]
 ;
 ; I3$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: I3LET

```
make 3d let.
ex: (I3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: I3REP

```
repeat argument 3d times as values.
ex: (I3REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: I3VAL

```
repeat the evaluated argument 3 times as values.
ex: (I3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: I3~

```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: I4

```
strict make 4d vector in veq context.
```

#### I4$

```
 ; VEQ:I4$
 ;   [symbol]
 ;
 ; I4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 4d vector array (IVEC) as values.
 ;     ex: (I4$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### I4$LINE

```
 ; VEQ:I4$LINE
 ;   [symbol]
 ;
 ; I4$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I4$LINE
 ;     ARGS: ((VA 8 X))
 ;     DOCSTRING: init IVEC array with 8 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I4$NUM

```
 ; VEQ:I4$NUM
 ;   [symbol]
 ;
 ; I4$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY FIXNUM))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 4d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I4$ONE

```
 ; VEQ:I4$ONE
 ;   [symbol]
 ;
 ; I4$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I4$POINT

```
 ; VEQ:I4$POINT
 ;   [symbol]
 ;
 ; I4$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %I4$POINT
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init IVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### I4$VAL

```
 ; VEQ:I4$VAL
 ;   [symbol]
 ;
 ; I4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (FIXNUM &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### I4$ZERO

```
 ; VEQ:I4$ZERO
 ;   [symbol]
 ;
 ; I4$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: I4LET

```
make 4d let.
ex: (I4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: I4REP

```
repeat argument 4d times as values.
ex: (I4REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: I4VAL

```
repeat the evaluated argument 4 times as values.
ex: (I4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: I4~

```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### I?

```
 ; VEQ:I?
 ;   [symbol]
 ;
 ; I? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     inspect argument
 ;   Source file: src/config.lisp
```

#### I_

```
 ; VEQ:I_
 ;   [symbol]
 ;
 ; I_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create IVEC vector array from body: (I_ '(a b c ...)).
 ;   Source file: src/array-utils.lisp
```

#### IN

```
:none:

 ; VEQ:IN
 ;   [symbol]
 ;
 ; IN names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES FIXNUM &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; IN names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: FIXNUM
```

#### IN\*

```
:none:

 ; VEQ:IN*
 ;   [symbol]
 ;
 ; IN* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### :fvprogn: IREP

```
repeat argument 1d times as values.
ex: (IREP (fx)) corresponds to (values (fx) ...).
```

#### ISEL

```
 ; VEQ:ISEL
 ;   [symbol]
 ;
 ; ISEL names a macro:
 ;   Lambda-list: ((&REST DIMS) &BODY BODY)
 ;   Documentation:
 ;     return values from body in order of dims.
 ;     use indices or :x :y :z :w
 ;     ex: (ISEL (:w :zx 0) (values a b c d)) returns: (values d c a a).
 ;   Source file: src/select-dim.lisp
```

#### :fvprogn: IVAL

```
repeat the evaluated argument 1 times as values.
ex: (IVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### IVEC

```
:none:

 ; VEQ:IVEC
 ;   [symbol]
 ;
 ; IVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:IN)
```

#### IVLET

```
:none:

 ; VEQ:IVLET
 ;   [symbol]
```

#### :fvprogn: I~

```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### KV

```
:none:

 ; VEQ:KV
 ;   [symbol]
 ;
 ; KV names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES SYMBOL &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; KV names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: KEYWORD
```

#### KV\*

```
:none:

 ; VEQ:KV*
 ;   [symbol]
 ;
 ; KV* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### KVEC

```
:none:

 ; VEQ:KVEC
 ;   [symbol]
 ;
 ; KVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:KV)
```

#### LL

```
:none:

 ; VEQ:LL
 ;   [symbol]
 ;
 ; LL names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES LIST &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; LL names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: LIST
```

#### LL\*

```
:none:

 ; VEQ:LL*
 ;   [symbol]
 ;
 ; LL* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### LST

```
 ; VEQ:LST
 ;   [symbol]
 ;
 ; LST names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     get all (values ... ) in body as a list.
 ;     almost like multiple-value-list, except it handles multiple arguments.
 ;   Source file: src/utils.lisp
```

#### LVEC

```
:none:

 ; VEQ:LVEC
 ;   [symbol]
 ;
 ; LVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:LL)
```

#### MAC

```
 ; VEQ:MAC
 ;   [symbol]
 ;
 ; MAC names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     expand macro.
 ;   Source file: src/generic-utils.lisp
```

#### MAC\*

```
 ; VEQ:MAC*
 ;   [symbol]
 ;
 ; MAC* names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     expand macro all. only in SBCL.
 ;   Source file: src/generic-utils.lisp
```

#### MVB

```
:none:

 ; VEQ:MVB
 ;   [symbol]
 ;
 ; MVB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: src/generic-utils.lisp
```

#### MVC

```
:none:

 ; VEQ:MVC
 ;   [symbol]
 ;
 ; MVC names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: src/generic-utils.lisp
```

#### MVCGRP

```
 ; VEQ:MVCGRP
 ;   [symbol]
 ;
 ; MVCGRP names a macro:
 ;   Lambda-list: ((DIM FX) &BODY BODY)
 ;   Documentation:
 ;     call fx on body in groups of dim.
 ;     ex: (labels ((fx ((:va 3 x)) (fsel (:xy) x)))
 ;           (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
 ;     returns: (values 1f0 2f0 4f0 5f0)
 ;     ex: (labels ((fx ((:va 3 x)) (fsel (:xz) x)))
 ;           (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
 ;     returns: (values 1f0 3f0 4f0 6f0)
 ;   Source file: src/utils.lisp
```

#### MVCMAP

```
 ; VEQ:MVCMAP
 ;   [symbol]
 ;
 ; MVCMAP names a macro:
 ;   Lambda-list: ((DIM FX) &BODY BODY)
 ;   Documentation:
 ;     returns (values (fx i) (fx j) ...) for dim values from body.
 ;   Source file: src/utils.lisp
```

#### MVCWRAP

```
:none:

 ; VEQ:MVCWRAP
 ;   [symbol]
```

#### NEW-STRIDE

```
 ; VEQ:NEW-STRIDE
 ;   [symbol]
 ;
 ; NEW-STRIDE names a macro:
 ;   Lambda-list: ((FROM TO TYPE &OPTIONAL (V 0)) ARR)
 ;   Documentation:
 ;     shift arr from stride to stride.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: P

```
strict make 1d vector in veq context.
```

#### P$

```
 ; VEQ:P$
 ;   [symbol]
 ;
 ; P$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 1d vector array (PVEC) as values.
 ;     ex: (P$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### P$_

```
 ; VEQ:P$_
 ;   [symbol]
 ;
 ; P$_ names a compiled function:
 ;   Lambda-list: (BODY)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     create PVEC vector array from body. where body is a list of lists.
 ;     ex: (P$_ (loop repeat 2 collect `(1f0 2f0)))
 ;     ex: (P$_ '((1f0 2f0) (1f0 2f0))).
 ;   Source file: src/array-utils.lisp
```

#### P$COPY

```
 ; VEQ:P$COPY
 ;   [symbol]
 ;
 ; P$COPY names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 31)))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     copy PVEC vector array.
 ;   Source file: src/array-utils.lisp
```

#### P$LINE

```
 ; VEQ:P$LINE
 ;   [symbol]
 ;
 ; P$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P$LINE
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init PVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P$MAKE

```
 ; VEQ:P$MAKE
 ;   [symbol]
 ;
 ; P$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0))
 ;   Documentation:
 ;     create PVEC vector array with size n * dim, and initial value v.
 ;   Source file: src/array-utils.lisp
```

#### P$NUM

```
 ; VEQ:P$NUM
 ;   [symbol]
 ;
 ; P$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 31)))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 1d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P$ONE

```
 ; VEQ:P$ONE
 ;   [symbol]
 ;
 ; P$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P$POINT

```
 ; VEQ:P$POINT
 ;   [symbol]
 ;
 ; P$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P$POINT
 ;     ARGS: ((VA 1 X))
 ;     DOCSTRING: init PVEC array with 1 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P$VAL

```
 ; VEQ:P$VAL
 ;   [symbol]
 ;
 ; P$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION
 ;                  ((UNSIGNED-BYTE 31) &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P$ZERO

```
 ; VEQ:P$ZERO
 ;   [symbol]
 ;
 ; P$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 1d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P$~

```
 ; VEQ:P$~
 ;   [symbol]
 ;
 ; P$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create PVEC vector array from n values in body.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: P2

```
strict make 2d vector in veq context.
```

#### P2$

```
 ; VEQ:P2$
 ;   [symbol]
 ;
 ; P2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 2d vector array (PVEC) as values.
 ;     ex: (P2$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### P2$LINE

```
 ; VEQ:P2$LINE
 ;   [symbol]
 ;
 ; P2$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P2$LINE
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init PVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P2$NUM

```
 ; VEQ:P2$NUM
 ;   [symbol]
 ;
 ; P2$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 31)))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 2d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P2$ONE

```
 ; VEQ:P2$ONE
 ;   [symbol]
 ;
 ; P2$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P2$POINT

```
 ; VEQ:P2$POINT
 ;   [symbol]
 ;
 ; P2$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P2$POINT
 ;     ARGS: ((VA 2 X))
 ;     DOCSTRING: init PVEC array with 2 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P2$VAL

```
 ; VEQ:P2$VAL
 ;   [symbol]
 ;
 ; P2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION
 ;                  ((UNSIGNED-BYTE 31) &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P2$ZERO

```
 ; VEQ:P2$ZERO
 ;   [symbol]
 ;
 ; P2$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 2d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: P2LET

```
make 2d let.
ex: (P2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: P2REP

```
repeat argument 2d times as values.
ex: (P2REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: P2VAL

```
repeat the evaluated argument 2 times as values.
ex: (P2VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: P2~

```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: P3

```
strict make 3d vector in veq context.
```

#### P3$

```
 ; VEQ:P3$
 ;   [symbol]
 ;
 ; P3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 3d vector array (PVEC) as values.
 ;     ex: (P3$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### P3$LINE

```
 ; VEQ:P3$LINE
 ;   [symbol]
 ;
 ; P3$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P3$LINE
 ;     ARGS: ((VA 6 X))
 ;     DOCSTRING: init PVEC array with 6 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P3$NUM

```
 ; VEQ:P3$NUM
 ;   [symbol]
 ;
 ; P3$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 31)))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 3d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P3$ONE

```
 ; VEQ:P3$ONE
 ;   [symbol]
 ;
 ; P3$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P3$POINT

```
 ; VEQ:P3$POINT
 ;   [symbol]
 ;
 ; P3$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P3$POINT
 ;     ARGS: ((VA 3 X))
 ;     DOCSTRING: init PVEC array with 3 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P3$VAL

```
 ; VEQ:P3$VAL
 ;   [symbol]
 ;
 ; P3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION
 ;                  ((UNSIGNED-BYTE 31) &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P3$ZERO

```
 ; VEQ:P3$ZERO
 ;   [symbol]
 ;
 ; P3$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 3d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: P3LET

```
make 3d let.
ex: (P3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: P3REP

```
repeat argument 3d times as values.
ex: (P3REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: P3VAL

```
repeat the evaluated argument 3 times as values.
ex: (P3VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: P3~

```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :fvprogn: P4

```
strict make 4d vector in veq context.
```

#### P4$

```
 ; VEQ:P4$
 ;   [symbol]
 ;
 ; P4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices [default: 0] from 4d vector array (PVEC) as values.
 ;     ex: (P4$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### P4$LINE

```
 ; VEQ:P4$LINE
 ;   [symbol]
 ;
 ; P4$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P4$LINE
 ;     ARGS: ((VA 8 X))
 ;     DOCSTRING: init PVEC array with 8 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P4$NUM

```
 ; VEQ:P4$NUM
 ;   [symbol]
 ;
 ; P4$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 31)))
 ;                  (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Documentation:
 ;     number of elements in 4d array.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P4$ONE

```
 ; VEQ:P4$ONE
 ;   [symbol]
 ;
 ; P4$ONE names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of ones.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P4$POINT

```
 ; VEQ:P4$POINT
 ;   [symbol]
 ;
 ; P4$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %P4$POINT
 ;     ARGS: ((VA 4 X))
 ;     DOCSTRING: init PVEC array with 4 elements.
 ;     defined via veq:FVDEF*
 ;   Source file: src/shapes.lisp
```

#### P4$VAL

```
 ; VEQ:P4$VAL
 ;   [symbol]
 ;
 ; P4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION
 ;                  ((UNSIGNED-BYTE 31) &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### P4$ZERO

```
 ; VEQ:P4$ZERO
 ;   [symbol]
 ;
 ; P4$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 4d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: P4LET

```
make 4d let.
ex: (P4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: P4REP

```
repeat argument 4d times as values.
ex: (P4REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: P4VAL

```
repeat the evaluated argument 4 times as values.
ex: (P4VAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### :fvprogn: P4~

```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### P_

```
 ; VEQ:P_
 ;   [symbol]
 ;
 ; P_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create PVEC vector array from body: (P_ '(a b c ...)).
 ;   Source file: src/array-utils.lisp
```

#### PN

```
:none:

 ; VEQ:PN
 ;   [symbol]
 ;
 ; PN names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; PN names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 31))
 ;   Expansion: (UNSIGNED-BYTE 31)
```

#### PN\*

```
:none:

 ; VEQ:PN*
 ;   [symbol]
 ;
 ; PN* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### POS-DF

```
:none:

 ; VEQ:POS-DF
 ;   [symbol]
 ;
 ; POS-DF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (DOUBLE-FLOAT 0.0d0 *)
```

#### POS-FF

```
:none:

 ; VEQ:POS-FF
 ;   [symbol]
 ;
 ; POS-FF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SINGLE-FLOAT 0.0 *)
```

#### :fvprogn: PREP

```
repeat argument 1d times as values.
ex: (PREP (fx)) corresponds to (values (fx) ...).
```

#### PSEL

```
 ; VEQ:PSEL
 ;   [symbol]
 ;
 ; PSEL names a macro:
 ;   Lambda-list: ((&REST DIMS) &BODY BODY)
 ;   Documentation:
 ;     return values from body in order of dims.
 ;     use indices or :x :y :z :w
 ;     ex: (PSEL (:w :zx 0) (values a b c d)) returns: (values d c a a).
 ;   Source file: src/select-dim.lisp
```

#### :fvprogn: PVAL

```
repeat the evaluated argument 1 times as values.
ex: (PVAL (fx)) corresponds to (let ((v (fx))) (values v ...)).
```

#### PVEC

```
:none:

 ; VEQ:PVEC
 ;   [symbol]
 ;
 ; PVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:PN)
```

#### PVLET

```
:none:

 ; VEQ:PVLET
 ;   [symbol]
```

#### :fvprogn: P~

```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### REPLACE-VARG

```
 ; VEQ:REPLACE-VARG
 ;   [symbol]
 ;
 ; REPLACE-VARG names a compiled function:
 ;   Lambda-list: (BODY &OPTIONAL (ROOT-RMAP (LIST)))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     replace instances of varg/:varg/:va and vref/:vref/:vr with
 ;     appropriate symbols for the dimension.
 ;
 ;     local maps vref/varg maps are propagated forwards in the list so a given
 ;     arg/ref should be available under its scope.  it seems to work for all cases
 ;     i have tested. but i'm mot sure if this propagation will eventually break
 ;     somewhere.
 ;
 ;     ex:
 ;       (print (veq:replace-varg '(mvb ((:va 2 x)) (values 1 2)
 ;                                      (list (:vr x 1 0)))))
 ;       ; will print something like:
 ;       ; (MVB (#:X/X-158 #:X/Y-159) (VALUES 1 2)
 ;       ;      (LIST #:X/Y-159 #:X/X-158))
 ;
 ;   Source file: src/macros-helpers.lisp
```

#### SVEC

```
:none:

 ; VEQ:SVEC
 ;   [symbol]
 ;
 ; SVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:SY)
```

#### SY

```
:none:

 ; VEQ:SY
 ;   [symbol]
 ;
 ; SY names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES SYMBOL &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; SY names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: SYMBOL
```

#### SY\*

```
:none:

 ; VEQ:SY*
 ;   [symbol]
 ;
 ; SY* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### TYPE-DEFAULT

```
 ; VEQ:TYPE-DEFAULT
 ;   [symbol]
 ;
 ; TYPE-DEFAULT names a compiled function:
 ;   Lambda-list: (TY &OPTIONAL (MISSING NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     default value for array with elements of type (hint) ty.
 ;     eg: 0 0f0 0d0 nil :val
 ;   Source file: src/types.lisp
```

#### TYPE-FROM-SHORT

```
 ; VEQ:TYPE-FROM-SHORT
 ;   [symbol]
 ;
 ; TYPE-FROM-SHORT names a compiled function:
 ;   Lambda-list: (TY &OPTIONAL (MISSING NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL T))
 ;   Documentation:
 ;     select type fom type hint.
 ;   Source file: src/types.lisp
```

#### UNPACK-VVSYM

```
 ; VEQ:UNPACK-VVSYM
 ;   [symbol]
 ;
 ; UNPACK-VVSYM names a compiled function:
 ;   Lambda-list: (SYM &KEY (S !) (NILTYPE NIL) (SYMOUT T))
 ;   Derived type: (FUNCTION
 ;                  (SYMBOL &KEY (:S (OR STRING KEYWORD CHARACTER))
 ;                   (:NILTYPE T) (:SYMOUT T))
 ;                  (VALUES KEYWORD T (MOD 36) (MOD 36) &OPTIONAL))
 ;   Documentation:
 ;     split names of type f34!var into (values :f var 3 4)
 ;   Source file: src/types.lisp
```

#### V?

```
 ; VEQ:V?
 ;   [symbol]
 ;
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get version. use silent to surpress stdout
 ;   Source file: src/config.lisp
```

#### :fvprogn: VARG

```
use (veq:varg dim a b ...) or (:vr dim a b ...) to represent dim vectors a,b
of dim n in vprogn, fvprog, fvdef*, vdef*, def*.  see replace-varg for
implementation details.
```

#### VCHAIN

```
 ; VEQ:VCHAIN
 ;   [symbol]
 ;
 ; VCHAIN names a macro:
 ;   Lambda-list: (FXS &REST REST)
 ;   Documentation:
 ;      chain functions, on all values.
 ;     eg: (vchain #'a #'b (values 1 2))
 ;     corresponds to: (~ #'a (~ #b (values 1 2)))
 ;   Source file: src/utils.lisp
```

#### VDEF

```
 ; VEQ:VDEF
 ;   [symbol]
 ;
 ; VDEF names a macro:
 ;   Lambda-list: (FNAME &BODY BODY)
 ;   Documentation:
 ;     define function with veq context enabled. see vprogn.
 ;   Source file: src/macrolets.lisp
```

#### VDEF\*

```
 ; VEQ:VDEF*
 ;   [symbol]
 ;
 ; VDEF* names a macro:
 ;   Lambda-list: (MNAME &BODY BODY)
 ;   Documentation:
 ;     defines a function named: %mname
 ;     and a wrapper macro named: mname
 ;     veq context is enabled. uses vprogn.
 ;
 ;     the wrapper macro ensures every call to this function is done as
 ;     (mvc #'%mname ...).
 ;   Source file: src/macrolets.lisp
```

#### VLABELS

```
 ; VEQ:VLABELS
 ;   [symbol]
 ;
 ; VLABELS names a macro:
 ;   Lambda-list: ((&REST LABS) &BODY BODY)
 ;   Documentation:
 ;     wraps labels so that it can be used with implicit mvc. that is, all labels
 ;     are defined as if with def*.
 ;     use %labelname to call the function directly.
 ;   Source file: src/lets.lisp
```

#### VNREP

```
 ; VEQ:VNREP
 ;   [symbol]
 ;
 ; VNREP names a macro:
 ;   Lambda-list: (N &REST REST)
 ;   Documentation:
 ;     corresponds to (~ r1 ... rn)
 ;   Source file: src/utils.lisp
```

#### VNVAL

```
 ; VEQ:VNVAL
 ;   [symbol]
 ;
 ; VNVAL names a macro:
 ;   Lambda-list: (N &REST REST)
 ;   Documentation:
 ;     returns (values v ...), where v is (progn ,@rest) evaluated once.
 ;   Source file: src/utils.lisp
```

#### VP

```
 ; VEQ:VP
 ;   [symbol]
 ;
 ; VP names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     print values and return values, return values.
 ;   Source file: src/utils.lisp
```

#### VPR

```
 ; VEQ:VPR
 ;   [symbol]
 ;
 ; VPR names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     print input code with resulting values, return values.
 ;   Source file: src/utils.lisp
```

#### VPROGN

```
 ; VEQ:VPROGN
 ;   [symbol]
 ;
 ; VPROGN names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     enable veq context inside this progn.
 ;     handles propagation and resolution of uses of (varg d var) and (vref var i).
 ;     also handles vv macro compiler triggers. see vv macro.
 ;
 ;     fvprogn is faster, but has some limitations.
 ;   Source file: src/macrolets.lisp
```

#### :fvprogn: VREF

```
use (veq:vref s x) or (:vr s x) to get dim x of symbol s in vprogn,
fvprogn, fvdef*, vdef*, def*. see replace-varg for implementation details.
```

#### VSEL

```
 ; VEQ:VSEL
 ;   [symbol]
 ;
 ; VSEL names a macro:
 ;   Lambda-list: ((TYPE &REST DIMS) &BODY BODY)
 ;   Documentation:
 ;     return values from body in order of dims.
 ;     use indices or :x :y :z :w
 ;     ex: (VSEL (df :w :zx 0) (values a b c d)) returns: (values d c a a).
 ;   Source file: src/select-dim.lisp
```

#### VV

```
the vv macro implements a DSL for manipulating packs of values and/or row
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
```

#### VVSYM

```
 ; VEQ:VVSYM
 ;   [symbol]
 ;
 ; VVSYM names a compiled function:
 ;   Lambda-list: (TYPE DIM SYMB &KEY PREF (SEP ) (PKG VEQ))
 ;   Derived type: (FUNCTION (SYMBOL T T &KEY (:PREF T) (:SEP T) (:PKG T))
 ;                  (VALUES SYMBOL &OPTIONAL))
 ;   Documentation:
 ;     build a symbol with correct name convention.
 ;     eg: (vvsym ff 2 :lerp) yields f2lerp.
 ;   Source file: src/types.lisp
```

#### XLET

```
xlet is a macro to bind typed values, and other variables:
(veq:xlet ((f2!a (f2 1f0 2f0)) ; 2d veq:ff/float
           (d!b 1d0) ; 1d veq:df/double
           (h :a)
           (i4!c (values 1 2 3 4))) ; 4d veq:in/integer
  (declare (keyword h))
  (do-something a b c h))

names without ! will be treated (mostly) as in a regular let.
declare can be used to declare types.
NOTE: xlet behaves more like CL let* in that bindings are available
immediately.
```

#### ~

```
 ; VEQ:~
 ;   [symbol]
 ;
 ; ~ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     wraps arguments in (mvc #'values ...).
 ;   Source file: src/utils.lisp
```

