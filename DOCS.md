# VEQ DOCUMENTATION

### Explanation

#### FVPROGN

All symbols marked with `:fvprogn:` are only valid inside a veq context.  veq
context can be initiated using `vprogn`, `fvprogn`, `vdef`, `fvdef`,
`vdef*` or `fvdef*`. See further documentation under the respective symbols
below.

See [examples](/examples/ex.lisp) som examples.

#### Names and Types

Symols prefixed with `f` pertain to type `ff`, short for `single-float`.
The corresponding vector array type is `fvec`, short for `(simple-array ff)`.

Symols prefixed with `d` pertain to type `df`, short for `double-float`.
The corresponding vector array type is `dvec`, short for `(simple-array df)`.

Symbols with `$` in the name pertain to vector arrays.

Symbols postfixed with `!` are destructive or in-place. Usually on the first
argument.


#### Abbreviations

 - `dsb` is short for `destructuring-bind`.
 - `mvb` is short for `multiple-value-bind`.
 - `mvc` is short for `multiple-value-call`.

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
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;     set n indices in a, from a[i] with n values. body must yield n values
 ;   Source file: src/vset.lisp
```

#### $PRINT

```
 ; VEQ:$PRINT
 ;   [symbol]
 ;
 ; $PRINT names a compiled function:
 ;   Lambda-list: (A &KEY (DIM 1) (START 0) (N 16) (S T))
 ;   Derived type: (FUNCTION
 ;                  (SIMPLE-ARRAY &KEY (:DIM (UNSIGNED-BYTE 32))
 ;                   (:START (SIGNED-BYTE 32)) (:N (UNSIGNED-BYTE 32))
 ;                   (:S T))
 ;                  (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
 ;   Documentation:
 ;     pretty print n, or all, rows from vector array of dim.
 ;     start at row (start 0).
 ;     negative start counts backwards from the last row
 ;     use s to overrid output stream.
 ;   Source file: src/array-extra.lisp
```

#### $ROWSET

```
 ; VEQ:$ROWSET
 ;   [symbol]
 ;
 ; $ROWSET names a macro:
 ;   Lambda-list: ((A N &OPTIONAL (I 0)) &BODY BODY)
 ;   Documentation:
 ;     performs (setf (aref a i) row0 (aref a (1+ i) r1 ...))
 ;     n must be less than or equal to (length row)
 ;   Source file: src/vset.lisp
```

#### $TO-LIST

```
 ; VEQ:$TO-LIST
 ;   [symbol]
 ;
 ; $TO-LIST names a compiled function:
 ;   Lambda-list: (A &KEY (DIM 1))
 ;   Derived type: (FUNCTION (SIMPLE-ARRAY &KEY (:DIM (UNSIGNED-BYTE 32)))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return array as a list of lists of length dim.
 ;   Source file: src/array-extra.lisp
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
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Lambda-list: (A &KEY (N 16) (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 2d array. returns array.
 ;   Source file: src/array-extra.lisp
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
 ;   Source file: src/array-extra.lisp
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
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Lambda-list: (A &KEY (N 16) (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 3d array. returns array.
 ;   Source file: src/array-extra.lisp
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
 ;   Source file: src/array-extra.lisp
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
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Lambda-list: (A &KEY (N 16) (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 4d array. returns array.
 ;   Source file: src/array-extra.lisp
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
 ;   Source file: src/array-extra.lisp
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
 ;   Lambda-list: (A0 &OPTIONAL (NA (LENGTH A0)))
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
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
 ;   Source file: src/array-extra.lisp
```

#### D$NUM

```
 ; VEQ:D$NUM
 ;   [symbol]
 ;
 ; D$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Source file: src/array-extra.lisp
```

#### D2$NUM

```
 ; VEQ:D2$NUM
 ;   [symbol]
 ;
 ; D2$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D2ANGLE

```
veq context op: d2angle
fxname: -d2angle
args: (ax ay)
body (1): (atan ay ax).
```

#### :fvprogn: D2CROSS

```
veq context op: d2cross
fxname: -d2cross
args: (ax ay bx by)
body (2): (- (* ax by) (* ay bx)).
```

#### :fvprogn: D2DOT

```
veq context op: d2dot
fxname: -d2dot
args: (ax ay bx by)
body (1): (+ (* ax bx) (* ay by)).
```

#### :fvprogn: D2DST

```
veq context op: d2dst
fxname: -d2dst
args: (ax ay bx by)
body (1): (sqrt (the pos-df (mvc #'+ (-d2square (- bx ax) (- by ay))))).
```

#### :fvprogn: D2DST2

```
veq context op: d2dst2
fxname: -d2dst2
args: (ax ay bx by)
body (1): (mvc #'+ (-d2square (- bx ax) (- by ay))).
```

#### :fvprogn: D2FLIP

```
veq context op: d2flip
fxname: -d2flip
args: (ax ay)
body (2): (values ay ax).
```

#### :fvprogn: D2FROM

```
veq context op: d2from
fxname: -d2from
args: (ax ay bx by s)
body (2): (values (+ ax (* bx s)) (+ ay (* by s))).
```

#### :fvprogn: D2I-

```
veq context op: d2i-
fxname: -d2i-
args: (ax ay bx by)
body (2): (values (- bx ax) (- by ay)).
```

#### :fvprogn: D2I/

```
veq context op: d2i/
fxname: -d2i/
args: (ax ay bx by)
body (2): (values (/ bx ax) (/ by ay)).
```

#### :fvprogn: D2ID

```
veq context op: d2id
fxname: -d2id
args: (ax ay)
body (2): (values ax ay).
```

#### :fvprogn: D2ISCALE

```
veq context op: d2iscale
fxname: -d2iscale
args: (ax ay s)
body (2): (values (/ ax s) (/ ay s)).
```

#### :fvprogn: D2LEN

```
veq context op: d2len
fxname: -d2len
args: (ax ay)
body (1): (the pos-df (sqrt (the pos-df (mvc #'+ (-d2square ax ay))))).
```

#### :fvprogn: D2LEN2

```
veq context op: d2len2
fxname: -d2len2
args: (ax ay)
body (1): (the pos-df (mvc #'+ (-d2square ax ay))).
```

#### :fvprogn: D2LERP

```
veq context op: d2lerp
fxname: -d2lerp
args: (ax ay bx by s)
body (2): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))).
```

#### :fvprogn: D2LET

```
make 2d let.
ex: (D2LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: D2MAX

```
veq context op: d2max
fxname: -d2max
args: (ax ay)
body (1): (max ax ay).
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
veq context op: d2mid
fxname: -d2mid
args: (ax ay bx by)
body (2): (values (* (+ ax bx) 0.5d0) (* (+ ay by) 0.5d0)).
```

#### :fvprogn: D2MIN

```
veq context op: d2min
fxname: -d2min
args: (ax ay)
body (1): (min ax ay).
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
veq context op: d2norm
fxname: -d2norm
args: (ax ay)
body (2): (mvc #'-d2iscale ax ay (mvc #'-d2len ax ay)).
```

#### :fvprogn: D2ON-CIRC

```
veq context op: d2on-circ
fxname: -d2on-circ
args: (ax rad)
body (2): (mvc #'-d2scale (-dcos-sin (* ax dpii)) rad).
```

#### :fvprogn: D2ON-CIRC\*

```
veq context op: d2on-circ*
fxname: -d2on-circ*
args: (ax rad)
body (2): (mvc #'-d2scale (-dcos-sin ax) rad).
```

#### :fvprogn: D2PERP

```
veq context op: d2perp
fxname: -d2perp
args: (ax ay)
body (2): (values ay (- ax)).
```

#### :fvprogn: D2PERP\*

```
veq context op: d2perp*
fxname: -d2perp*
args: (ax ay)
body (2): (values (- ay) ax).
```

#### :fvprogn: D2REP

```
repeat argument 2d times as values.
ex: (D2REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: D2ROT

```
veq context op: d2rot
fxname: -d2rot
args: (ax ay angle)
body (2): (let ((cosa (cos angle)) (sina (sin angle)))
            (declare (df cosa sina))
            (values (- (* ax cosa) (* ay sina)) (+ (* ax sina) (* ay cosa)))).
```

#### :fvprogn: D2ROTS

```
veq context op: d2rots
fxname: -d2rots
args: (ax ay angle sx sy)
body (2): (mvb (rx ry) (mvc #'-d2rot (- ax sx) (- ay sy) angle)
               (values (+ sx rx) (+ sy ry))).
```

#### :fvprogn: D2SCALE

```
veq context op: d2scale
fxname: -d2scale
args: (ax ay s)
body (2): (values (* ax s) (* ay s)).
```

#### :fvprogn: D2SQUARE

```
veq context op: d2square
fxname: -d2square
args: (ax ay)
body (2): (values (* ax ax) (* ay ay)).
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
 ;   Source file: src/array-extra.lisp
```

#### D3$NUM

```
 ; VEQ:D3$NUM
 ;   [symbol]
 ;
 ; D3$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D3CROSS

```
veq context op: d3cross
fxname: -d3cross
args: (ax ay az bx by bz)
body (3): (values (- (* ay bz) (* az by)) (- (* az bx) (* ax bz))
                  (- (* ax by) (* ay bx))).
```

#### :fvprogn: D3DOT

```
veq context op: d3dot
fxname: -d3dot
args: (ax ay az bx by bz)
body (1): (+ (* ax bx) (* ay by) (* az bz)).
```

#### :fvprogn: D3DST

```
veq context op: d3dst
fxname: -d3dst
args: (ax ay az bx by bz)
body (1): (sqrt
           (the pos-df (mvc #'+ (-d3square (- bx ax) (- by ay) (- bz az))))).
```

#### :fvprogn: D3DST2

```
veq context op: d3dst2
fxname: -d3dst2
args: (ax ay az bx by bz)
body (1): (mvc #'+ (-d3square (- bx ax) (- by ay) (- bz az))).
```

#### :fvprogn: D3FROM

```
veq context op: d3from
fxname: -d3from
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))).
```

#### :fvprogn: D3I-

```
veq context op: d3i-
fxname: -d3i-
args: (ax ay az bx by bz)
body (3): (values (- bx ax) (- by ay) (- bz az)).
```

#### :fvprogn: D3I/

```
veq context op: d3i/
fxname: -d3i/
args: (ax ay az bx by bz)
body (3): (values (/ bx ax) (/ by ay) (/ bz az)).
```

#### :fvprogn: D3ID

```
veq context op: d3id
fxname: -d3id
args: (ax ay az)
body (3): (values ax ay az).
```

#### :fvprogn: D3ISCALE

```
veq context op: d3iscale
fxname: -d3iscale
args: (ax ay az s)
body (3): (values (/ ax s) (/ ay s) (/ az s)).
```

#### :fvprogn: D3LEN

```
veq context op: d3len
fxname: -d3len
args: (ax ay az)
body (1): (the pos-df (sqrt (the pos-df (mvc #'+ (-d3square ax ay az))))).
```

#### :fvprogn: D3LEN2

```
veq context op: d3len2
fxname: -d3len2
args: (ax ay az)
body (1): (the pos-df (mvc #'+ (-d3square ax ay az))).
```

#### :fvprogn: D3LERP

```
veq context op: d3lerp
fxname: -d3lerp
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s))).
```

#### :fvprogn: D3LET

```
make 3d let.
ex: (D3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: D3MAX

```
veq context op: d3max
fxname: -d3max
args: (ax ay az)
body (1): (max ax ay az).
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
veq context op: d3mid
fxname: -d3mid
args: (ax ay az bx by bz)
body (3): (values (* (+ ax bx) 0.5d0) (* (+ ay by) 0.5d0) (* (+ az bz) 0.5d0)).
```

#### :fvprogn: D3MIN

```
veq context op: d3min
fxname: -d3min
args: (ax ay az)
body (1): (min ax ay az).
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
veq context op: d3norm
fxname: -d3norm
args: (ax ay az)
body (3): (mvc #'-d3iscale ax ay az (the pos-df (mvc #'-d3len ax ay az))).
```

#### :fvprogn: D3REP

```
repeat argument 3d times as values.
ex: (D3REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: D3ROT

```
veq context op: d3rot
fxname: -d3rot
args: (ax ay az nx ny nz a)
body (3): (let ((cosa (cos a)))
            (declare (df cosa))
            (mvc #'-d3from
                 (mvc #'-d3from (-d3scale ax ay az cosa)
                      (-d3cross nx ny nz ax ay az) (sin a))
                 nx ny nz (* (-d3dot nx ny nz ax ay az) (- 1.0d0 cosa)))).
```

#### :fvprogn: D3ROTS

```
veq context op: d3rots
fxname: -d3rots
args: (ax ay az nx ny nz a sx sy sz)
body (3): (mvb (rx ry rz)
               (mvc #'-d3rot (- ax sx) (- ay sy) (- az sz) nx ny nz a)
               (values (+ (the df rx) sx) (+ (the df ry) sy)
                       (+ (the df rz) sz))).
```

#### :fvprogn: D3SCALE

```
veq context op: d3scale
fxname: -d3scale
args: (ax ay az s)
body (3): (values (* ax s) (* ay s) (* az s)).
```

#### :fvprogn: D3SQUARE

```
veq context op: d3square
fxname: -d3square
args: (ax ay az)
body (3): (values (the pos-df (* ax ax)) (the pos-df (* ay ay))
                  (the pos-df (* az az))).
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
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (DOUBLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: D4DOT

```
veq context op: d4dot
fxname: -d4dot
args: (ax ay az aw bx by bz bw)
body (1): (+ (* ax bx) (* ay by) (* az bz) (* aw bw)).
```

#### :fvprogn: D4DST

```
veq context op: d4dst
fxname: -d4dst
args: (ax ay az aw bx by bz bw)
body (1): (sqrt
           (the pos-df
                (mvc #'+ (-d4square (- bx ax) (- by ay) (- bz az) (- bw aw))))).
```

#### :fvprogn: D4DST2

```
veq context op: d4dst2
fxname: -d4dst2
args: (ax ay az aw bx by bz bw)
body (1): (mvc #'+ (-d4square (- bx ax) (- by ay) (- bz az) (- bw aw))).
```

#### :fvprogn: D4FROM

```
veq context op: d4from
fxname: -d4from
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))
                  (+ aw (* bw s))).
```

#### :fvprogn: D4I-

```
veq context op: d4i-
fxname: -d4i-
args: (ax ay az aw bx by bz bw)
body (4): (values (- bx ax) (- by ay) (- bz az) (- bw aw)).
```

#### :fvprogn: D4I/

```
veq context op: d4i/
fxname: -d4i/
args: (ax ay az aw bx by bz bw)
body (4): (values (/ bx ax) (/ by ay) (/ bz az) (/ bw aw)).
```

#### :fvprogn: D4ID

```
veq context op: d4id
fxname: -d4id
args: (ax ay az aw)
body (4): (values ax ay az aw).
```

#### :fvprogn: D4ISCALE

```
veq context op: d4iscale
fxname: -d4iscale
args: (ax ay az aw s)
body (4): (values (/ ax s) (/ ay s) (/ az s) (/ aw s)).
```

#### :fvprogn: D4LEN

```
veq context op: d4len
fxname: -d4len
args: (ax ay az aw)
body (1): (the pos-df (sqrt (the pos-df (mvc #'+ (-d4square ax ay az aw))))).
```

#### :fvprogn: D4LEN2

```
veq context op: d4len2
fxname: -d4len2
args: (ax ay az aw)
body (1): (the pos-df (mvc #'+ (-d4square ax ay az aw))).
```

#### :fvprogn: D4LERP

```
veq context op: d4lerp
fxname: -d4lerp
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s)) (+ aw (* (- bw aw) s))).
```

#### :fvprogn: D4LET

```
make 4d let.
ex: (D4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: D4MAX

```
veq context op: d4max
fxname: -d4max
args: (ax ay az aw)
body (1): (max ax ay az aw).
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
veq context op: d4mid
fxname: -d4mid
args: (ax ay az aw bx by bz bw)
body (4): (values (* (+ ax bx) 0.5d0) (* (+ ay by) 0.5d0) (* (+ az bz) 0.5d0)
                  (* (+ aw bw) 0.5d0)).
```

#### :fvprogn: D4MIN

```
veq context op: d4min
fxname: -d4min
args: (ax ay az aw)
body (1): (min ax ay az aw).
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
veq context op: d4norm
fxname: -d4norm
args: (ax ay az aw)
body (4): (mvc #'-d4iscale ax ay az aw (the pos-df (mvc #'-d4len ax ay az aw))).
```

#### :fvprogn: D4REP

```
repeat argument 4d times as values.
ex: (D4REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: D4SCALE

```
veq context op: d4scale
fxname: -d4scale
args: (ax ay az aw s)
body (4): (values (* ax s) (* ay s) (* az s) (* aw s)).
```

#### :fvprogn: D4SQUARE

```
veq context op: d4square
fxname: -d4square
args: (ax ay az aw)
body (4): (values (the pos-df (* ax ax)) (the pos-df (* ay ay))
                  (the pos-df (* az az)) (the pos-df (* aw aw))).
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

#### DALPHA

```
 ; VEQ:DALPHA
 ;   [symbol]
 ;
 ; DALPHA names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %DALPHA
 ;     ARGS: (X Y &AUX (A (ATAN (- Y) X)))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/extra.lisp
```

#### :fvprogn: DCLAMP

```
veq context op: dclamp
fxname: -dclamp
args: (x)
body (1): (min 1.0d0 (max 0.0d0 x)).
```

#### :fvprogn: DCLAMP\*

```
veq context op: dclamp*
fxname: -dclamp*
args: (x mi ma)
body (1): (min ma (max mi x)).
```

#### :fvprogn: DCOS-SIN

```
veq context op: dcos-sin
fxname: -dcos-sin
args: (ax)
body (2): (values (cos ax) (sin ax)).
```

#### :fvprogn: DDEG->RAD

```
veq context op: ddeg->rad
fxname: -ddeg->rad
args: (d)
body (1): (* dpi (/ d 180.0d0)).
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
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
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
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
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

#### DEPS=

```
:none:

 ; VEQ:DEPS=
 ;   [symbol]
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
veq context op: dfrom
fxname: -dfrom
args: (ax bx s)
body (1): (+ ax (* bx s)).
```

#### :fvprogn: DI-

```
veq context op: di-
fxname: -di-
args: (ax bx)
body (1): (- bx ax).
```

#### :fvprogn: DI/

```
veq context op: di/
fxname: -di/
args: (ax bx)
body (1): (/ bx ax).
```

#### :fvprogn: DID

```
veq context op: did
fxname: -did
args: (ax)
body (1): (values ax).
```

#### :fvprogn: DISCALE

```
veq context op: discale
fxname: -discale
args: (ax s)
body (1): (/ ax s).
```

#### :fvprogn: DLEN

```
veq context op: dlen
fxname: -dlen
args: (ax)
body (1): (the pos-df ax).
```

#### :fvprogn: DLEN2

```
veq context op: dlen2
fxname: -dlen2
args: (ax)
body (1): (the pos-df (mvc #'+ (-dsquare ax))).
```

#### :fvprogn: DLERP

```
veq context op: dlerp
fxname: -dlerp
args: (ax bx s)
body (1): (+ ax (* (- bx ax) s)).
```

#### :fvprogn: DMID

```
veq context op: dmid
fxname: -dmid
args: (ax bx)
body (1): (* (+ ax bx) 0.5d0).
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
veq context op: dscale
fxname: -dscale
args: (ax s)
body (1): (* ax s).
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
veq context op: dsin-cos
fxname: -dsin-cos
args: (ax)
body (2): (values (sin ax) (cos ax)).
```

#### :fvprogn: DSQUARE

```
veq context op: dsquare
fxname: -dsquare
args: (ax)
body (1): (* ax ax).
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
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY VEQ:DF *)
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
 ;   Lambda-list: (A0 &OPTIONAL (NA (LENGTH A0)))
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
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
 ;   Source file: src/array-extra.lisp
```

#### F$NUM

```
 ; VEQ:F$NUM
 ;   [symbol]
 ;
 ; F$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Source file: src/array-extra.lisp
```

#### F2$NUM

```
 ; VEQ:F2$NUM
 ;   [symbol]
 ;
 ; F2$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;                  ((UNSIGNED-BYTE 32) SINGLE-FLOAT &OPTIONAL
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
 ;   Derived type: (FUNCTION (SINGLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (8)) &OPTIONAL))
 ;   Source file: src/shapes.lisp
```

#### F2$VAL

```
 ; VEQ:F2$VAL
 ;   [symbol]
 ;
 ; F2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F2ANGLE

```
veq context op: f2angle
fxname: -f2angle
args: (ax ay)
body (1): (atan ay ax).
```

#### :fvprogn: F2CROSS

```
veq context op: f2cross
fxname: -f2cross
args: (ax ay bx by)
body (2): (- (* ax by) (* ay bx)).
```

#### :fvprogn: F2DOT

```
veq context op: f2dot
fxname: -f2dot
args: (ax ay bx by)
body (1): (+ (* ax bx) (* ay by)).
```

#### :fvprogn: F2DST

```
veq context op: f2dst
fxname: -f2dst
args: (ax ay bx by)
body (1): (sqrt (the pos-ff (mvc #'+ (-f2square (- bx ax) (- by ay))))).
```

#### :fvprogn: F2DST2

```
veq context op: f2dst2
fxname: -f2dst2
args: (ax ay bx by)
body (1): (mvc #'+ (-f2square (- bx ax) (- by ay))).
```

#### :fvprogn: F2FLIP

```
veq context op: f2flip
fxname: -f2flip
args: (ax ay)
body (2): (values ay ax).
```

#### :fvprogn: F2FROM

```
veq context op: f2from
fxname: -f2from
args: (ax ay bx by s)
body (2): (values (+ ax (* bx s)) (+ ay (* by s))).
```

#### :fvprogn: F2I-

```
veq context op: f2i-
fxname: -f2i-
args: (ax ay bx by)
body (2): (values (- bx ax) (- by ay)).
```

#### :fvprogn: F2I/

```
veq context op: f2i/
fxname: -f2i/
args: (ax ay bx by)
body (2): (values (/ bx ax) (/ by ay)).
```

#### :fvprogn: F2ID

```
veq context op: f2id
fxname: -f2id
args: (ax ay)
body (2): (values ax ay).
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
veq context op: f2iscale
fxname: -f2iscale
args: (ax ay s)
body (2): (values (/ ax s) (/ ay s)).
```

#### :fvprogn: F2LEN

```
veq context op: f2len
fxname: -f2len
args: (ax ay)
body (1): (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f2square ax ay))))).
```

#### :fvprogn: F2LEN2

```
veq context op: f2len2
fxname: -f2len2
args: (ax ay)
body (1): (the pos-ff (mvc #'+ (-f2square ax ay))).
```

#### :fvprogn: F2LERP

```
veq context op: f2lerp
fxname: -f2lerp
args: (ax ay bx by s)
body (2): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))).
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
 ; F2LSEGX names a compiled function:
 ;   Lambda-list: (LINES)
 ;   Derived type: (FUNCTION ((VECTOR (SIMPLE-ARRAY SINGLE-FLOAT)))
 ;                  (VALUES (SIMPLE-ARRAY LIST (*)) &OPTIONAL))
 ;   Documentation:
 ;     find all line-line intersections in lines
 ;   Source file: src/checks-sweep.lisp
```

#### :fvprogn: F2MAX

```
veq context op: f2max
fxname: -f2max
args: (ax ay)
body (1): (max ax ay).
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
veq context op: f2mid
fxname: -f2mid
args: (ax ay bx by)
body (2): (values (* (+ ax bx) 0.5) (* (+ ay by) 0.5)).
```

#### :fvprogn: F2MIN

```
veq context op: f2min
fxname: -f2min
args: (ax ay)
body (1): (min ax ay).
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
veq context op: f2norm
fxname: -f2norm
args: (ax ay)
body (2): (mvc #'-f2iscale ax ay (mvc #'-f2len ax ay)).
```

#### :fvprogn: F2ON-CIRC

```
veq context op: f2on-circ
fxname: -f2on-circ
args: (ax rad)
body (2): (mvc #'-f2scale (-fcos-sin (* ax fpii)) rad).
```

#### :fvprogn: F2ON-CIRC\*

```
veq context op: f2on-circ*
fxname: -f2on-circ*
args: (ax rad)
body (2): (mvc #'-f2scale (-fcos-sin ax) rad).
```

#### :fvprogn: F2PERP

```
veq context op: f2perp
fxname: -f2perp
args: (ax ay)
body (2): (values ay (- ax)).
```

#### :fvprogn: F2PERP\*

```
veq context op: f2perp*
fxname: -f2perp*
args: (ax ay)
body (2): (values (- ay) ax).
```

#### :fvprogn: F2REP

```
repeat argument 2d times as values.
ex: (F2REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: F2ROT

```
veq context op: f2rot
fxname: -f2rot
args: (ax ay angle)
body (2): (let ((cosa (cos angle)) (sina (sin angle)))
            (declare (ff cosa sina))
            (values (- (* ax cosa) (* ay sina)) (+ (* ax sina) (* ay cosa)))).
```

#### :fvprogn: F2ROTS

```
veq context op: f2rots
fxname: -f2rots
args: (ax ay angle sx sy)
body (2): (mvb (rx ry) (mvc #'-f2rot (- ax sx) (- ay sy) angle)
               (values (+ sx rx) (+ sy ry))).
```

#### :fvprogn: F2SCALE

```
veq context op: f2scale
fxname: -f2scale
args: (ax ay s)
body (2): (values (* ax s) (* ay s)).
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

#### :fvprogn: F2SQUARE

```
veq context op: f2square
fxname: -f2square
args: (ax ay)
body (2): (values (* ax ax) (* ay ay)).
```

#### F2SSEGX

```
 ; VEQ:F2SSEGX
 ;   [symbol]
 ;
 ; F2SSEGX names a compiled function:
 ;   Lambda-list: (LINES K &OPTIONAL (N (LENGTH LINES)))
 ;   Derived type: (FUNCTION
 ;                  ((VECTOR (SIMPLE-ARRAY SINGLE-FLOAT))
 ;                   (UNSIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY LIST (*)) &OPTIONAL))
 ;   Documentation:
 ;     find all line-line intersections between the first k lines, with the remaining n-k lines
 ;   Source file: src/checks-sweep.lisp
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
 ;   Source file: src/array-extra.lisp
```

#### F3$NUM

```
 ; VEQ:F3$NUM
 ;   [symbol]
 ;
 ; F3$NUM names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F3CROSS

```
veq context op: f3cross
fxname: -f3cross
args: (ax ay az bx by bz)
body (3): (values (- (* ay bz) (* az by)) (- (* az bx) (* ax bz))
                  (- (* ax by) (* ay bx))).
```

#### :fvprogn: F3DOT

```
veq context op: f3dot
fxname: -f3dot
args: (ax ay az bx by bz)
body (1): (+ (* ax bx) (* ay by) (* az bz)).
```

#### :fvprogn: F3DST

```
veq context op: f3dst
fxname: -f3dst
args: (ax ay az bx by bz)
body (1): (sqrt
           (the pos-ff (mvc #'+ (-f3square (- bx ax) (- by ay) (- bz az))))).
```

#### :fvprogn: F3DST2

```
veq context op: f3dst2
fxname: -f3dst2
args: (ax ay az bx by bz)
body (1): (mvc #'+ (-f3square (- bx ax) (- by ay) (- bz az))).
```

#### :fvprogn: F3FROM

```
veq context op: f3from
fxname: -f3from
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))).
```

#### :fvprogn: F3I-

```
veq context op: f3i-
fxname: -f3i-
args: (ax ay az bx by bz)
body (3): (values (- bx ax) (- by ay) (- bz az)).
```

#### :fvprogn: F3I/

```
veq context op: f3i/
fxname: -f3i/
args: (ax ay az bx by bz)
body (3): (values (/ bx ax) (/ by ay) (/ bz az)).
```

#### :fvprogn: F3ID

```
veq context op: f3id
fxname: -f3id
args: (ax ay az)
body (3): (values ax ay az).
```

#### :fvprogn: F3ISCALE

```
veq context op: f3iscale
fxname: -f3iscale
args: (ax ay az s)
body (3): (values (/ ax s) (/ ay s) (/ az s)).
```

#### :fvprogn: F3LEN

```
veq context op: f3len
fxname: -f3len
args: (ax ay az)
body (1): (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f3square ax ay az))))).
```

#### :fvprogn: F3LEN2

```
veq context op: f3len2
fxname: -f3len2
args: (ax ay az)
body (1): (the pos-ff (mvc #'+ (-f3square ax ay az))).
```

#### :fvprogn: F3LERP

```
veq context op: f3lerp
fxname: -f3lerp
args: (ax ay az bx by bz s)
body (3): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s))).
```

#### :fvprogn: F3LET

```
make 3d let.
ex: (F3LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: F3MAX

```
veq context op: f3max
fxname: -f3max
args: (ax ay az)
body (1): (max ax ay az).
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
veq context op: f3mid
fxname: -f3mid
args: (ax ay az bx by bz)
body (3): (values (* (+ ax bx) 0.5) (* (+ ay by) 0.5) (* (+ az bz) 0.5)).
```

#### :fvprogn: F3MIN

```
veq context op: f3min
fxname: -f3min
args: (ax ay az)
body (1): (min ax ay az).
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
veq context op: f3norm
fxname: -f3norm
args: (ax ay az)
body (3): (mvc #'-f3iscale ax ay az (the pos-ff (mvc #'-f3len ax ay az))).
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
veq context op: f3rot
fxname: -f3rot
args: (ax ay az nx ny nz a)
body (3): (let ((cosa (cos a)))
            (declare (ff cosa))
            (mvc #'-f3from
                 (mvc #'-f3from (-f3scale ax ay az cosa)
                      (-f3cross nx ny nz ax ay az) (sin a))
                 nx ny nz (* (-f3dot nx ny nz ax ay az) (- 1.0 cosa)))).
```

#### :fvprogn: F3ROTS

```
veq context op: f3rots
fxname: -f3rots
args: (ax ay az nx ny nz a sx sy sz)
body (3): (mvb (rx ry rz)
               (mvc #'-f3rot (- ax sx) (- ay sy) (- az sz) nx ny nz a)
               (values (+ (the ff rx) sx) (+ (the ff ry) sy)
                       (+ (the ff rz) sz))).
```

#### :fvprogn: F3SCALE

```
veq context op: f3scale
fxname: -f3scale
args: (ax ay az s)
body (3): (values (* ax s) (* ay s) (* az s)).
```

#### :fvprogn: F3SQUARE

```
veq context op: f3square
fxname: -f3square
args: (ax ay az)
body (3): (values (the pos-ff (* ax ax)) (the pos-ff (* ay ay))
                  (the pos-ff (* az az))).
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
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (UNSIGNED-BYTE 32))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d vector array of zeros.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :fvprogn: F4DOT

```
veq context op: f4dot
fxname: -f4dot
args: (ax ay az aw bx by bz bw)
body (1): (+ (* ax bx) (* ay by) (* az bz) (* aw bw)).
```

#### :fvprogn: F4DST

```
veq context op: f4dst
fxname: -f4dst
args: (ax ay az aw bx by bz bw)
body (1): (sqrt
           (the pos-ff
                (mvc #'+ (-f4square (- bx ax) (- by ay) (- bz az) (- bw aw))))).
```

#### :fvprogn: F4DST2

```
veq context op: f4dst2
fxname: -f4dst2
args: (ax ay az aw bx by bz bw)
body (1): (mvc #'+ (-f4square (- bx ax) (- by ay) (- bz az) (- bw aw))).
```

#### :fvprogn: F4FROM

```
veq context op: f4from
fxname: -f4from
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))
                  (+ aw (* bw s))).
```

#### :fvprogn: F4I-

```
veq context op: f4i-
fxname: -f4i-
args: (ax ay az aw bx by bz bw)
body (4): (values (- bx ax) (- by ay) (- bz az) (- bw aw)).
```

#### :fvprogn: F4I/

```
veq context op: f4i/
fxname: -f4i/
args: (ax ay az aw bx by bz bw)
body (4): (values (/ bx ax) (/ by ay) (/ bz az) (/ bw aw)).
```

#### :fvprogn: F4ID

```
veq context op: f4id
fxname: -f4id
args: (ax ay az aw)
body (4): (values ax ay az aw).
```

#### :fvprogn: F4ISCALE

```
veq context op: f4iscale
fxname: -f4iscale
args: (ax ay az aw s)
body (4): (values (/ ax s) (/ ay s) (/ az s) (/ aw s)).
```

#### :fvprogn: F4LEN

```
veq context op: f4len
fxname: -f4len
args: (ax ay az aw)
body (1): (the pos-ff (sqrt (the pos-ff (mvc #'+ (-f4square ax ay az aw))))).
```

#### :fvprogn: F4LEN2

```
veq context op: f4len2
fxname: -f4len2
args: (ax ay az aw)
body (1): (the pos-ff (mvc #'+ (-f4square ax ay az aw))).
```

#### :fvprogn: F4LERP

```
veq context op: f4lerp
fxname: -f4lerp
args: (ax ay az aw bx by bz bw s)
body (4): (values (+ ax (* (- bx ax) s)) (+ ay (* (- by ay) s))
                  (+ az (* (- bz az) s)) (+ aw (* (- bw aw) s))).
```

#### :fvprogn: F4LET

```
make 4d let.
ex: (F4LET ((a (values ...))) ...)
note that this behaves like native lisp let*.
```

#### :fvprogn: F4MAX

```
veq context op: f4max
fxname: -f4max
args: (ax ay az aw)
body (1): (max ax ay az aw).
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
veq context op: f4mid
fxname: -f4mid
args: (ax ay az aw bx by bz bw)
body (4): (values (* (+ ax bx) 0.5) (* (+ ay by) 0.5) (* (+ az bz) 0.5)
                  (* (+ aw bw) 0.5)).
```

#### :fvprogn: F4MIN

```
veq context op: f4min
fxname: -f4min
args: (ax ay az aw)
body (1): (min ax ay az aw).
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
veq context op: f4norm
fxname: -f4norm
args: (ax ay az aw)
body (4): (mvc #'-f4iscale ax ay az aw (the pos-ff (mvc #'-f4len ax ay az aw))).
```

#### :fvprogn: F4REP

```
repeat argument 4d times as values.
ex: (F4REP (fx)) corresponds to (values (fx) ...).
```

#### :fvprogn: F4SCALE

```
veq context op: f4scale
fxname: -f4scale
args: (ax ay az aw s)
body (4): (values (* ax s) (* ay s) (* az s) (* aw s)).
```

#### :fvprogn: F4SQUARE

```
veq context op: f4square
fxname: -f4square
args: (ax ay az aw)
body (4): (values (the pos-ff (* ax ax)) (the pos-ff (* ay ay))
                  (the pos-ff (* az az)) (the pos-ff (* aw aw))).
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

#### FALPHA

```
 ; VEQ:FALPHA
 ;   [symbol]
 ;
 ; FALPHA names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %FALPHA
 ;     ARGS: (X Y &AUX (A (ATAN (- Y) X)))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/extra.lisp
```

#### :fvprogn: FCLAMP

```
veq context op: fclamp
fxname: -fclamp
args: (x)
body (1): (min 1.0 (max 0.0 x)).
```

#### :fvprogn: FCLAMP\*

```
veq context op: fclamp*
fxname: -fclamp*
args: (x mi ma)
body (1): (min ma (max mi x)).
```

#### :fvprogn: FCOS-SIN

```
veq context op: fcos-sin
fxname: -fcos-sin
args: (ax)
body (2): (values (cos ax) (sin ax)).
```

#### :fvprogn: FDEG->RAD

```
veq context op: fdeg->rad
fxname: -fdeg->rad
args: (d)
body (1): (* fpi (/ d 180.0)).
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
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
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
 ;                   (OR (FLOAT -1.0 1.0) (COMPLEX SINGLE-FLOAT)
 ;                       (COMPLEX DOUBLE-FLOAT))
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
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
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
 ;                   (OR (FLOAT -512.0 513.0) (COMPLEX SINGLE-FLOAT)
 ;                       (COMPLEX DOUBLE-FLOAT))
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
 ;                   (OR (FLOAT 0.0 2.0) (COMPLEX DOUBLE-FLOAT)
 ;                       (COMPLEX SINGLE-FLOAT))
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

#### FEPS=

```
 ; VEQ:FEPS=
 ;   [symbol]
 ;
 ; FEPS= names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     WRAPS: %FEPS=
 ;     ARGS: (A B &OPTIONAL (E (* 10.0 *EPS*)))
 ;     DOCSTRING: [none]
 ;     defined via veq:FVDEF*
 ;   Source file: src/extra.lisp
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
veq context op: ffrom
fxname: -ffrom
args: (ax bx s)
body (1): (+ ax (* bx s)).
```

#### :fvprogn: FI-

```
veq context op: fi-
fxname: -fi-
args: (ax bx)
body (1): (- bx ax).
```

#### :fvprogn: FI/

```
veq context op: fi/
fxname: -fi/
args: (ax bx)
body (1): (/ bx ax).
```

#### :fvprogn: FID

```
veq context op: fid
fxname: -fid
args: (ax)
body (1): (values ax).
```

#### :fvprogn: FISCALE

```
veq context op: fiscale
fxname: -fiscale
args: (ax s)
body (1): (/ ax s).
```

#### :fvprogn: FLEN

```
veq context op: flen
fxname: -flen
args: (ax)
body (1): (the pos-ff ax).
```

#### :fvprogn: FLEN2

```
veq context op: flen2
fxname: -flen2
args: (ax)
body (1): (the pos-ff (mvc #'+ (-fsquare ax))).
```

#### :fvprogn: FLERP

```
veq context op: flerp
fxname: -flerp
args: (ax bx s)
body (1): (+ ax (* (- bx ax) s)).
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
veq context op: fmid
fxname: -fmid
args: (ax bx)
body (1): (* (+ ax bx) 0.5).
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
veq context op: fscale
fxname: -fscale
args: (ax s)
body (1): (* ax s).
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
veq context op: fsin-cos
fxname: -fsin-cos
args: (ax)
body (2): (values (sin ax) (cos ax)).
```

#### :fvprogn: FSQUARE

```
veq context op: fsquare
fxname: -fsquare
args: (ax)
body (1): (* ax ax).
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
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY VEQ:FF *)
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

#### GET-ARG-KEY

```
 ; VEQ:GET-ARG-KEY
 ;   [symbol]
 ;
 ; GET-ARG-KEY names a compiled function:
 ;   Lambda-list: (LL K &OPTIONAL D)
 ;   Derived type: (FUNCTION (T T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get the value of keyword k in ll where ll is a list of kw function args.
 ;   Source file: src/generic-utils.lisp
```

#### GROUP

```
 ; VEQ:GROUP
 ;   [symbol]
 ;
 ; GROUP names a compiled function:
 ;   Lambda-list: (L N)
 ;   Derived type: (FUNCTION (LIST FIXNUM) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     group l into lists of n elements. see ungroup.
 ;   Source file: src/generic-utils.lisp
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
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Lambda-list: (A0 &OPTIONAL (NA (LENGTH A0)))
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (SIGNED-BYTE 32)) &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (SIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION
 ;                  ((SIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (SIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION
 ;                  ((SIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (SIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION
 ;                  ((SIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (SIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION
 ;                  ((SIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (T) (VALUES (SIGNED-BYTE 32) &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; IN names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 32))
 ;   Expansion: (SIGNED-BYTE 32)
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
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY VEQ:IN *)
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
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY VEQ:KV *)
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

#### LPOS

```
 ; VEQ:LPOS
 ;   [symbol]
 ;
 ; LPOS names a macro:
 ;   Lambda-list: (L &OPTIONAL (I 0) J)
 ;   Documentation:
 ;     get list of index i or subseq i j from list of lists.
 ;   Source file: src/utils.lisp
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
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY VEQ:LL *)
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

#### MUTATE!

```
 ; VEQ:MUTATE!
 ;   [symbol]
 ;
 ; MUTATE! names a macro:
 ;   Lambda-list: (VARS &BODY BODY)
 ;   Documentation:
 ;     ex: (mutate! (a b) (values 1 2))
 ;     is equivalent to (mvb (a* b*) (values 1 2) (setf a a* b b*))
 ;     where a* and b* are gensyms
 ;   Source file: src/utils.lisp
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
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Lambda-list: (A0 &OPTIONAL (NA (LENGTH A0)))
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY (UNSIGNED-BYTE 32)) &OPTIONAL T)
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32)) &OPTIONAL))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;                  ((UNSIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;                  ((UNSIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;                  ((UNSIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY (UNSIGNED-BYTE 32)))
 ;                  (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;                  ((UNSIGNED-BYTE 32) &OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 32))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
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
 ;   Derived type: (FUNCTION (T) (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: src/types.lisp
 ;
 ; PN names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 32))
 ;   Expansion: (UNSIGNED-BYTE 32)
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

#### PROC-VV

```
:none:

 ; VEQ:PROC-VV
 ;   [symbol]
 ;
 ; PROC-VV names a compiled function:
 ;   Lambda-list: (BODY)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Source file: src/ops-vv.lisp
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
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY VEQ:PN *)
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
 ;   Lambda-list: (BODY &OPTIONAL (ROOT-RMAP (LIST)) (ONLY NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T) (VALUES T &OPTIONAL))
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
 ;       (veq:replace-varg '(mvb ((:va 2 x)) (values 1 2)
 ;                            (list (:vr x 1 0))))
 ;       ; will return something like:
 ;       ; (MVB (#:X/X-158 #:X/Y-159) (VALUES 1 2)
 ;       ;      (LIST #:X/Y-159 #:X/X-158))
 ;   Source file: src/macros-helpers.lisp
```

#### STRIP-ARG-KEYS

```
 ; VEQ:STRIP-ARG-KEYS
 ;   [symbol]
 ;
 ; STRIP-ARG-KEYS names a compiled function:
 ;   Lambda-list: (LL KK &AUX (LL (GROUP LL 2)))
 ;   Derived type: (FUNCTION (T T) *)
 ;   Documentation:
 ;     strip keywords in kk from ll where ll is a list of kw function args.
 ;   Source file: src/generic-utils.lisp
```

#### SVEC

```
:none:

 ; VEQ:SVEC
 ;   [symbol]
 ;
 ; SVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY VEQ:SY *)
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

#### UNGROUP

```
 ; VEQ:UNGROUP
 ;   [symbol]
 ;
 ; UNGROUP names a compiled function:
 ;   Lambda-list: (L &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (LIST) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     inverse of group.
 ;   Source file: src/generic-utils.lisp
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
 ;                  (VALUES KEYWORD T (MOD 10) (MOD 10) &OPTIONAL))
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
 ;     chain functions, on all values.
 ;     eg: (vchain #'a #'b (values 1 2)) equals: (mvc #'a (mvc #'b (values 1 2)))
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

#### VECTOR-REARRANGE

```
 ; VEQ:VECTOR-REARRANGE
 ;   [symbol]
 ;
 ; VECTOR-REARRANGE names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     get new vector with elements from a. ex:
 ;     (let ((i 3) (v #(0 1 2 3 4 5)))
 ;       (vector-rearrange v 0 1 (0 1) ((print i)) i)) ; #(0 1 0 3 3)
 ;   Source file: src/utils.lisp
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
 ;     corresponds to (values [rest n times]). see vnval.
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
 ;     returns (values v ...), where v is (progn ,@rest) evaluated once. see vnrep.
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
# INTRODUCTION

  ; the vv macro implements a DSL for manipulating packs of values (point
  ; vectors) and arrays of point vectors (vecs). it is a core part of veq, and as
  ; such it is already integrated into vprogn, fvprogn, vdef and fvdef. but the
  ; DSL can also be used explicitly via the (vv ...) macro or (proc-vv ...)
  ; function.

  ; you can read a litte more about the motivation behind vv at:
  ; https://inconvergent.net/2023/a-vector-dsl/

  ; the DSL uses triggers to broadcast a function [symbol name] or code across
  ; packs of 1-9 values and/or rows of 1-9 item vectors.

  ; there is a complete list of triggers, modes [see MODES], modifiers and
  ; types [see TYPES] below. but it might be easier to get a sense of how to use
  ; the DSL with som examples.

# EXAMPLES OF USE

  ; the best way to get an idea of how the DSL works might be to see some
  ; examples n the following examples we use ; -> to indicate output. it is not a
  ; part of the DSL. let us start with something simple:

  (i2!@+ 1 2 3 4) ; -> (values (+ 1 3) (+ 2 4))

  ; here:
  ;  - i is the type [integer / fixnum]
  ;  - 2 is the dimension / size of the value packs
  ;  - !@ is the trigger for this particular mode; and
  ;  - + is the function to call. in this case the native +

  ; as you can see this mode corresponds to calling a function on the
  ; corresponding scalars of each dimension of two point vectors. in this case +.

  ; the type is optional, and the function name can be any valid function that
  ; accepts two arguments and returns one scalar. so you can do the following:

  (labels (my-fx (x y) (/ y x))
    ; no type, local function
    (2!@my-fx 1 2 3 4)) ; -> (values (/ 3 1) (/ 2 4))

  ; i2!@+ requires 4 [(* 2 dim)] values, but it is agnostic [unless veq is loaded
  ; in in strict mode] to the grouping of the values in the input:

  (2!@+ 1 (values 1 2 3 4))     ; -> (values (+ 1 3) (+ 2 4))
  (2!@+ 1 (values 2 3) (+ 7 8)) ; -> (values (+ 1 3) (+ 2 (+ 7 8)))
  (3!@+ (values 1 2) 3          ; -> (values (+ 1 4) (+ 2 5) (+ 3 6))
        (values 4 5) 6)

  ; here are other possible configurations for !@:

  ; same behaviour as above for comparison:
  (2!@*  1 2 3 4)  ; -> (values (* 1 3) (* 2 4))
  ; project [.] last value:
  (2!@*. 1 2 9)    ; -> (values (* 1 9) (* 2 9))
  ; project [.] first value
  (2!@.* 1 2 3)    ; -> (values (* 1 2) (* 1 3))
  ; project [.] two values on rhs
  (2!@*.. 1 2 3 4) ; -> (values (* 1 3 4) (* 2 3 4))
  ; aggregate value in-place [!]:
  (veq:xlet ((f2!a (values 2f0 3f0))) ; bind ax = 2f0 and ay = 3f0
    (loop repeat 10 do (2!@+! a 1f0 2f0))
    (values a)) ; -> (values 12f0 23f0)

  ; several modes also work on vecs [$]. vecs are simple-arrays with a
  ; corresponding type [more on types below].

  ; vec on l side
  (2!@$*  #(1 2 3 4) 5 6) ; -> #((* 1 5) (* 2 6)
                          ;      (* 3 5) (* 4 6))
  (2!@$*. #(1 2 3 4) 5)   ; -> #((* 1 5) (* 2 5)
                          ;      (* 3 5) (* 4 5))
  ; vec on both sides
  (2!@$+$! #(1 2 3 4) #(10 20 30 40)) ; -> #(11 22 33 44)

  ; [NOTE: vec on r side only is currently not supported.]

  ; expressions can be arbitrarily nested, so a 2d dot product might look like
  ; this:

  (2_@+ (2!@* (values 1 2)   ; -> (+ (* 1 3) (* 2 4))
              (values 3 4)))

  ; here we introduce another mode (_@) that will call the function [+] with all
  ; values as arguments. in other words it is roughly equivalent to
  ; multiple-value-call [mvc]. here is a naive example:

  (2_@+ (values 2 3)) ; -> 5

  ; so, if you have two vecs, you can do row-wise dot products as above. except
  ; now we use $ to indicate vecs:

  (21_@$+ (2!@$*$ #(2 2 3 4)   ; -> #(14 10)
                  #(4 3 2 1)))

  ; notice that 21_@$+ has two dimension digits. the second digit is the expected
  ; output dimension. as such this command will reduce the number of columns from
  ; 2 to 1. by default the output dim is the same as the input dim. output
  ; dimension is supported on all vec ($) modes.

  ; here are some slightly more involved examples using the _@ mode:

  (labels ((swap (x y) (values y x))
           (do-thing (x y z) (values (+ y z) (+ x z))))

  ; swap columns on each row
  (2_@$swap #(2 1 4 3))       ; -> #(1 2 3 4)

  ; swap columns in place [!] from row 1 and up. the ?@ modifier is used for
  ; indexing on all modes that support vecs [$]
  (2_@$swap! (?@ #(2 1 4 3 6 5) 1)) ; -> #(2 1 3 4 5 6)

  ; project 5 and call do-thing on each row
  (2_@$do-thing. #(1 2 3 4) 5)) ; -> #((+ 2 5) (+ 1 5)
                                ;      (+ 4 5) (+ 3 5))

  ; as opposed to calling a function on values or rows of values, you can use
  ; .@ to call a function individually on all elements:

  (2.@abs -1 -2)           ; -> (values (abs -1) (abs -2))

  ; or with vecs:

  (2.@$abs #(-1 -2 -3 -4)) ; -> #((abs -1) (abs -2)
                           ;      (abs -3) (abs -4))

  ; [NOTE: it might seem unnecessary to to include the dimension indicator here,
  ; and it is in many cases. but including the dimension indicator makes it
  ; possible to do some compile time evaluation. and it will be more efficient in
  ; some cases. in particular when SIMD support is implemented.]

# MORE ADVANCED EXAMPLES

  ; for convenience it is also possible to call lambda forms on vecs. there are
  ; two modes:

  ; x@ calls the lambda form on the right with every row as argument.
  ; this this example will print two rows with row number:
  (2x@$fx #(1 2 3 4) ((i x y) (print (list i :xy x y)))) ; -> nil

  ; %@ calls the lambda form on every row in vec. returns a
  ; new vec, or alters [!] the vec in place:
  (2%@$fx #(1 2 3 4) ((x y) (values y x))) ; -> #(2 1 4 3)

  ; if the lambda form has one more arguemnt than the dimension, the row number
  ; will be passed in as the first arguemnt. eg:
  (2%@$fx #(1 2 3 4) ((i x y) (print i) (values y x))) ; -> #(2 1 4 3)
  ; this applies to x@ as well

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

# SUMMARY OF MODES

most of the modes are introduced with examples above, but here is a summary of
all the modes:

  -- !@: call fx on pairs of values and/or vec rows
   - [f][d]!@fx[!]   : d values; d values
   - [f][d]!@fx.[!]  : d values; [number of dots] values
   - [f][d]!@.fx     : d values; [number of dots] values
   - [f][d]!@$fx[!]  : d vec; d values
   - [f][d]!@$fx[!]. : d vec; [number of dots] values
   - [f][d]!@$fx$[!] : d vec; d vec

  -- _@: call fx on n values, or vec rows
   - [f][d]_@fx   :  d values
   - [f][d]_@$fx  : d vec
   - [f][d]_@$fx. : d vec; [number of dots] values

  -- .@: call fx on individual elements from values, vec rows
   - [f][d].@fx  : d values
   - [f][d].@$fx : d vec

  -- %@: map lambda form across values or vec rows.
         returns new or altered [!] vec:
   - [f][d]%@$fx[!] : d vec

  -- x@: map lambda form across values or rows of values from vec.
         returns nil:
   - [f][d]x@$fx : d vec

  -- r@: reduces rows with fx vertically. [TODO/INCOMPLETE]
   - [f][d]r@$fx : d vec

  -- mvc/funcall
   - m@fx : translates to (mvc #'fx ...)
   - f@fx : translates to (mvc fx ...)

# SUMMARY OF MODIFIERS

  ; ?@ is a modifier used to alter the behaviour of a specific mode. modes that
  ; involve vecs ($) support slicing rows by wrapping the vec in the ?@ modifier:

  (?@ arr from [to])

  ; for ivec, pvec, list or vector indices, append i, p, l or v respectively to the
  ; modifier. here are some examples of behaviour:

  (labels ((take2 (x y) (values x y)))

    ; slice from index:
    (2_@$take2 (?@ #(1 2 3 4 5 6 7 8) 2))                ; -> #(5 6 7 8)
    ; slice to from index:
    (2_@$take2 (?@ #(1 2 3 4 5 6 7 8) 2 3))              ; -> #(5 6)

    ; vector index:
    (2_@$take2 (v?@ #(1 2 3 4 5 6 7 8) #(0 3)))          ; -> #(1 2 7 8)
    ; list index:
    (2_@$take2 (l?@ #(1 2 3 4 5 6 7 8) '(0 3)))          ; -> #(1 2 7 8)

    ; individual modifiers can be used for lhs or rhs:
    (2!@$+$ (?@ #(1 2 3 4) 1) (?@ #(10 20 30 40) 0 1))   ; -> #(13 24)

    ; if the operation is in-place (!) the lhs vec will be changed,
    ; by the lhs index:
    (2!@$+$! (?@ #(1 2 3 4) 1) (?@ #(10 20 30 40) 0 1))) ; -> #(1 2 13 24)

# SUMMARY OF TYPES

  ; all vv expressions [except f@ and m@, where it does not make sense] can be
  ; explicity typed. the supported types, wtih corresponding array type are as
  ; follows:

  -- f: veq:ff, single-float; veq:fvec
  -- d: veq:df, double-float; veq:dvec
  -- i: veq:in, fixnum, veq:ivec
  -- p: veq:pn, (unsigned-byte 31); veq:pvec
  -- s: veq:sy, symbol; veq:svec
  -- k: veq:kv, keyword; veq:kvec
  -- l: veq:ll, list; veq:lvec
  -- none; vector

  ; fvec, ivec, etc are simple-arrays with the coresponding type. ie. veq:fvec
  ; means (simple-array veq:ff). when type is omitted, the code will be more
  ; forgiving, but less efficeint. in which case the corresponding array type is
  ; 'vector.

# INSPECT GENERATED CODE

  ; the code that is actually generated is usually a little more involved than
  ; what these examples imply. in order to see the expanded code use:

  (print (veq:proc-vv '(2!@$*. #(1 2 3 4) 5)))
  ; which prints generated code, or:

  (veq:vvdb (:exec t) (2!@$*. #(1 2 3 4) 5))
  ; which prints and executes generated code. [see vvdb for more details.]
  ; it can also be helful to use (veq:vp ...) or (veq:vpr ...) [see docs.]
  ; vp will print all values, and return all values; while vpr will print
  ; the expression, the resulting values, and then return the values.
```

#### VVDB

```
:none:

 ; VEQ:VVDB
 ;   [symbol]
 ;
 ; VVDB names a macro:
 ;   Lambda-list: ((&KEY (EXEC T) (S T)) &BODY BODY)
 ;   Source file: src/ops-vv.lisp
```

#### VVSYM

```
 ; VEQ:VVSYM
 ;   [symbol]
 ;
 ; VVSYM names a compiled function:
 ;   Lambda-list: (TYPE DIM SYMB &KEY PREF (SEP )
 ;                 (PKG
 ;                  (ETYPECASE SYMB
 ;                    (KEYWORD VEQ)
 ;                    (SYMBOL (SYMBOL-PACKAGE SYMB))
 ;                    (STRING VEQ))))
 ;   Derived type: (FUNCTION (SYMBOL T T &KEY (:PREF T) (:SEP T) (:PKG T))
 ;                  (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     build a symbol with correct name convention.
 ;     eg: (vvsym ff 2 :lerp) yields f2lerp.
 ;   Source file: src/types.lisp
```

#### WITH-SYMBS

```
 ; VEQ:WITH-SYMBS
 ;   [symbol]
 ;
 ; WITH-SYMBS names a compiled function:
 ;   Lambda-list: (SS BODY)
 ;   Derived type: (FUNCTION (LIST LIST) (VALUES CONS &OPTIONAL))
 ;   Documentation:
 ;     bind these symbols outside body and replace inside body. eg:
 ;       (with-symbs `(g ,g ...) (qry g :select ... )) ; equals:
 ;       (let ((gg ,g) ...) (qry gg :select ...))      ; gg is a gensym
 ;   Source file: src/utils.lisp
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
 ;     wraps rest in (mvc #'values ...).
 ;   Source file: src/utils.lisp
```

