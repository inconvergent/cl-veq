# VEQ DOCUMENTATION

### Explanation

#### Context

All symbols marked with `:contex:` are only valid inside a veq context.  veq
context can be initiated using `vprogn`, `fvprogn`, `vdef`, `fvdef`,
`vdef*` or `fvdef*`. See further documentation below.

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
1d vector array setter and getter.
use (setf ($ a i) (list x)) to set a[i].
use ($ a i j ...) to return (values a[i] a[j] ...)

 ; VEQ:$
 ;   [symbol]
 ;
 ; $ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF $) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     1d vector array setter and getter.
 ;     use (setf ($ a i) (list x)) to set a[i].
 ;     use ($ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: src/vset.lisp
```

#### $COPY

```
:missing:todo:

 ; VEQ:$COPY
 ;   [symbol]
```

#### $MAKE

```
create vector array with size (n dim), and initial value v.

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
number of elements in 1d array.
untyped.

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
set n indices in a, from a[i] with n values from body.

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
pretty print n, or all, rows from vector array of dim.
start at row (start 0).
negative start counts backwards from the last row
use s to overrid output stream.

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
return array as a list of lists of length dim.

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

#### :context: $VSET

```
use ($vset (a i) (values ...)) to set a[i] of 1d array.
```

#### \*EPS\*

```
:missing:todo:

 ; VEQ:*EPS*
 ;   [symbol]
 ;
 ; *EPS* names a special variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 5.960465e-8
```

#### 2$

```
2d vector array setter and getter.
use (setf (2$ a i) (list x)) to set a[i].
use (2$ a i j ...) to return (values a[i] a[j] ...)

 ; VEQ:2$
 ;   [symbol]
 ;
 ; 2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF 2$) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     2d vector array setter and getter.
 ;     use (setf (2$ a i) (list x)) to set a[i].
 ;     use (2$ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: src/vset.lisp
```

#### 2$NUM

```
number of elements in 2d array.
untyped.

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
pretty print 2d array. returns array.

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
return array as a list of lists of length 2.

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

#### :context: 2$VSET

```
use (2$vset (a i) (values ...)) to set a[i] of 2d array.
```

#### 3$

```
3d vector array setter and getter.
use (setf (3$ a i) (list x)) to set a[i].
use (3$ a i j ...) to return (values a[i] a[j] ...)

 ; VEQ:3$
 ;   [symbol]
 ;
 ; 3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF 3$) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     3d vector array setter and getter.
 ;     use (setf (3$ a i) (list x)) to set a[i].
 ;     use (3$ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: src/vset.lisp
```

#### 3$NUM

```
number of elements in 3d array.
untyped.

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
pretty print 3d array. returns array.

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
return array as a list of lists of length 3.

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

#### :context: 3$VSET

```
use (3$vset (a i) (values ...)) to set a[i] of 3d array.
```

#### 4$

```
4d vector array setter and getter.
use (setf (4$ a i) (list x)) to set a[i].
use (4$ a i j ...) to return (values a[i] a[j] ...)

 ; VEQ:4$
 ;   [symbol]
 ;
 ; 4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: src/array-utils.lisp
 ;
 ; (SETF 4$) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     4d vector array setter and getter.
 ;     use (setf (4$ a i) (list x)) to set a[i].
 ;     use (4$ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: src/vset.lisp
```

#### 4$NUM

```
number of elements in 4d array.
untyped.

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
pretty print 4d array. returns array.

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
return array as a list of lists of length 4.

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

#### :context: 4$VSET

```
use (4$vset (a i) (values ...)) to set a[i] of 4d array.
```

#### CONTEXT?

```
list all macrolets in veq context. that is ops available inside vprog,
fvprogn, vdef, fvdef defined contexts/functions.

 ; VEQ:CONTEXT?
 ;   [symbol]
 ;
 ; CONTEXT? names a macro:
 ;   Lambda-list: ()
 ;   Documentation:
 ;     list all macrolets in veq context. that is ops available inside vprog,
 ;     fvprogn, vdef, fvdef defined contexts/functions.
 ;   Source file: src/docs.lisp
```

#### :context: D

```
strict make 1d vector in veq context.
```

#### D$

```
returns indices (default 0) from 1d vector array (DVEC) as values.
ex: (D$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:D$
 ;   [symbol]
 ;
 ; D$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 1d vector array (DVEC) as values.
 ;     ex: (D$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: D$\*

```
veq context broadcast op: D$*
fxname: -D*
args: (AX BX)
body (1): (* AX BX).
```

#### :context: D$\*!

```
veq context broadcast op: D$*
fxname: -D*
args: (AX BX)
body (1): (* AX BX).
destructive.
```

#### :context: D$+

```
veq context broadcast op: D$+
fxname: -D+
args: (AX BX)
body (1): (+ AX BX).
```

#### :context: D$+!

```
veq context broadcast op: D$+
fxname: -D+
args: (AX BX)
body (1): (+ AX BX).
destructive.
```

#### :context: D$-

```
veq context broadcast op: D$-
fxname: -D-
args: (AX BX)
body (1): (- AX BX).
```

#### :context: D$-!

```
veq context broadcast op: D$-
fxname: -D-
args: (AX BX)
body (1): (- AX BX).
destructive.
```

#### :context: D$/

```
veq context broadcast op: D$/
fxname: -D/
args: (AX BX)
body (1): (/ AX BX).
```

#### :context: D$/!

```
veq context broadcast op: D$/
fxname: -D/
args: (AX BX)
body (1): (/ AX BX).
destructive.
```

#### :context: D$^

```
veq context broadcast op: D$^
fxname: -D^
args: (AX S)
body (1): (EXPT AX S).
```

#### :context: D$^!

```
veq context broadcast op: D$^
fxname: -D^
args: (AX S)
body (1): (EXPT AX S).
destructive.
```

#### D$_

```
create dvec vector array from body. where body is a list of lists.
ex: (d$_ (loop repeat 2 collect `(1d0 2d0)))
ex: (d$_ '((1d0 2d0) (1d0 2d0))).

 ; VEQ:D$_
 ;   [symbol]
 ;
 ; D$_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create dvec vector array from body. where body is a list of lists.
 ;     ex: (d$_ (loop repeat 2 collect `(1d0 2d0)))
 ;     ex: (d$_ '((1d0 2d0) (1d0 2d0))).
 ;   Source file: src/array-utils.lisp
```

#### :context: D$ABS

```
veq context broadcast op: D$ABS
fxname: -DABS
args: (AX)
body (1): (ABS AX).
```

#### :context: D$ABS!

```
veq context broadcast op: D$ABS
fxname: -DABS
args: (AX)
body (1): (ABS AX).
destructive.
```

#### :context: D$CLAMP

```
veq context broadcast op: D$CLAMP
fxname: -DCLAMP
args: (X)
body (1): (MIN 1.0d0 (MAX 0.0d0 X)).
```

#### :context: D$CLAMP!

```
veq context broadcast op: D$CLAMP
fxname: -DCLAMP
args: (X)
body (1): (MIN 1.0d0 (MAX 0.0d0 X)).
destructive.
```

#### :context: D$CLAMP\*

```
veq context broadcast op: D$CLAMP*
fxname: -DCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
```

#### :context: D$CLAMP\*!

```
veq context broadcast op: D$CLAMP*
fxname: -DCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
destructive.
```

#### D$COPY

```
copy DVEC vector array.

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

#### :context: D$COS-SIN

```
veq context broadcast op: D$COS-SIN
fxname: -DCOS-SIN
args: (AX)
body (2): (VALUES (COS AX) (SIN AX)).
```

#### :context: D$DEG->RAD

```
veq context broadcast op: D$DEG->RAD
fxname: -DDEG->RAD
args: (D)
body (1): (* DPI (/ D 180.0d0)).
```

#### :context: D$DEG->RAD!

```
veq context broadcast op: D$DEG->RAD
fxname: -DDEG->RAD
args: (D)
body (1): (* DPI (/ D 180.0d0)).
destructive.
```

#### :context: D$EXP

```
veq context broadcast op: D$EXP
fxname: -DEXP
args: (AX)
body (1): (VALUES (EXP AX)).
```

#### :context: D$EXP!

```
veq context broadcast op: D$EXP
fxname: -DEXP
args: (AX)
body (1): (VALUES (EXP AX)).
destructive.
```

#### :context: D$FROM

```
veq context broadcast op: D$FROM
fxname: -DFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
```

#### :context: D$FROM!

```
veq context broadcast op: D$FROM
fxname: -DFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
destructive.
```

#### :context: D$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

#### :context: D$I-

```
veq context broadcast op: D$I-
fxname: -DI-
args: (AX BX)
body (1): (- BX AX).
```

#### :context: D$I-!

```
veq context broadcast op: D$I-
fxname: -DI-
args: (AX BX)
body (1): (- BX AX).
destructive.
```

#### :context: D$I/

```
veq context broadcast op: D$I/
fxname: -DI/
args: (AX BX)
body (1): (/ BX AX).
```

#### :context: D$I/!

```
veq context broadcast op: D$I/
fxname: -DI/
args: (AX BX)
body (1): (/ BX AX).
destructive.
```

#### :context: D$ISCALE

```
veq context broadcast op: D$ISCALE
fxname: -DISCALE
args: (AX S)
body (1): (VALUES (/ AX S)).
```

#### :context: D$ISCALE!

```
veq context broadcast op: D$ISCALE
fxname: -DISCALE
args: (AX S)
body (1): (VALUES (/ AX S)).
destructive.
```

#### D$LAST

```
return values from last row of 1d vector array.

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

#### :context: D$LEN

```
veq context broadcast op: D$LEN
fxname: -DLEN
args: (AX)
body (1): (THE POS-DF AX).
```

#### :context: D$LEN!

```
veq context broadcast op: D$LEN
fxname: -DLEN
args: (AX)
body (1): (THE POS-DF AX).
destructive.
```

#### :context: D$LEN2

```
veq context broadcast op: D$LEN2
fxname: -DLEN2
args: (AX)
body (1): (THE POS-DF (MVC #'+ (-DSQUARE AX))).
```

#### :context: D$LEN2!

```
veq context broadcast op: D$LEN2
fxname: -DLEN2
args: (AX)
body (1): (THE POS-DF (MVC #'+ (-DSQUARE AX))).
destructive.
```

#### :context: D$LERP

```
veq context broadcast op: D$LERP
fxname: -DLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
```

#### :context: D$LERP!

```
veq context broadcast op: D$LERP
fxname: -DLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
destructive.
```

#### D$LINE

```
fx: %D$LINE
macro wrapper: D$LINE
defined via veq:def*

 ; VEQ:D$LINE
 ;   [symbol]
 ;
 ; D$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D$LINE
 ;     macro wrapper: D$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### D$LSPACE

```
fx: %D$LSPACE
macro wrapper: D$LSPACE
defined via veq:fvdef*

 ; VEQ:D$LSPACE
 ;   [symbol]
 ;
 ; D$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D$LSPACE
 ;     macro wrapper: D$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### D$MAKE

```
create DVEC vector array with size n * dim, and initial value v.

 ; VEQ:D$MAKE
 ;   [symbol]
 ;
 ; D$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0d0))
 ;   Documentation:
 ;     create DVEC vector array with size n * dim, and initial value v.
 ;   Source file: src/array-utils.lisp
```

#### :context: D$MID

```
veq context broadcast op: D$MID
fxname: -DMID
args: (AX BX)
body (1): (* 0.5d0 (+ AX BX)).
```

#### :context: D$MID!

```
veq context broadcast op: D$MID
fxname: -DMID
args: (AX BX)
body (1): (* 0.5d0 (+ AX BX)).
destructive.
```

#### D$MIMA

```
find min and max for all dimensions of 1 array.
ex: (D$MIMA &key n) returns (values xmin xmax ...).
use n to limit to first n rows.

 ; VEQ:D$MIMA
 ;   [symbol]
 ;
 ; D$MIMA names a compiled function:
 ;   Lambda-list: (A0 &KEY (N ($NUM A0)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 1 array.
 ;     ex: (D$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### :context: D$MOD

```
veq context broadcast op: D$MOD
fxname: -DMOD
args: (AX S)
body (1): (MOD AX S).
```

#### :context: D$MOD!

```
veq context broadcast op: D$MOD
fxname: -DMOD
args: (AX S)
body (1): (MOD AX S).
destructive.
```

#### :context: D$NEG

```
veq context broadcast op: D$NEG
fxname: -DNEG
args: (AX)
body (1): (- AX).
```

#### :context: D$NEG!

```
veq context broadcast op: D$NEG
fxname: -DNEG
args: (AX)
body (1): (- AX).
destructive.
```

#### :context: D$NORM

```
veq context broadcast op: D$NORM
fxname: -DNORM
args: (AX)
body (1): (MVC #'-DISCALE AX (MVC #'-DLEN AX)).
```

#### :context: D$NORM!

```
veq context broadcast op: D$NORM
fxname: -DNORM
args: (AX)
body (1): (MVC #'-DISCALE AX (MVC #'-DLEN AX)).
destructive.
```

#### D$NUM

```
number of elements in 1d array.
typed.

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
make 1d array of ones.
typed.

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
fx: %D$POINT
macro wrapper: D$POINT
defined via veq:def*

 ; VEQ:D$POINT
 ;   [symbol]
 ;
 ; D$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D$POINT
 ;     macro wrapper: D$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: D$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 1
```

#### :context: D$SCALE

```
veq context broadcast op: D$SCALE
fxname: -DSCALE
args: (AX S)
body (1): (VALUES (* AX S)).
```

#### :context: D$SCALE!

```
veq context broadcast op: D$SCALE
fxname: -DSCALE
args: (AX S)
body (1): (VALUES (* AX S)).
destructive.
```

#### :context: D$SIN-COS

```
veq context broadcast op: D$SIN-COS
fxname: -DSIN-COS
args: (AX)
body (2): (VALUES (SIN AX) (COS AX)).
```

#### :context: D$SQRT

```
veq context broadcast op: D$SQRT
fxname: -DSQRT
args: (AX)
body (1): (THE POS-DF (SQRT (THE POS-DF AX))).
```

#### :context: D$SQRT!

```
veq context broadcast op: D$SQRT
fxname: -DSQRT
args: (AX)
body (1): (THE POS-DF (SQRT (THE POS-DF AX))).
destructive.
```

#### :context: D$SQUARE

```
veq context broadcast op: D$SQUARE
fxname: -DSQUARE
args: (AX)
body (1): (* AX AX).
```

#### :context: D$SQUARE!

```
veq context broadcast op: D$SQUARE
fxname: -DSQUARE
args: (AX)
body (1): (* AX AX).
destructive.
```

#### D$SUM

```
sum all rows of 1d array.

 ; VEQ:D$SUM
 ;   [symbol]
 ;
 ; D$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
 ;                  (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 1d array.
 ;   Source file: src/array-reduce.lisp
```

#### D$TAKE

```
returns 1d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:D$TAKE
 ;   [symbol]
 ;
 ; D$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 1d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### D$VAL

```
make 1d array of val.
typed.

 ; VEQ:D$VAL
 ;   [symbol]
 ;
 ; D$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: D$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 1d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### D$ZERO

```
make 1d vector array of zeros.
typed.

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

#### :context: D\*

```
veq context op: D*
fxname: -D*
args: (AX BX)
body (1): (* AX BX).
```

#### :context: D+

```
veq context op: D+
fxname: -D+
args: (AX BX)
body (1): (+ AX BX).
```

#### :context: D-

```
veq context op: D-
fxname: -D-
args: (AX BX)
body (1): (- AX BX).
```

#### :context: D/

```
veq context op: D/
fxname: -D/
args: (AX BX)
body (1): (/ AX BX).
```

#### :context: D2

```
strict make 2d vector in veq context.
```

#### D2$

```
returns indices (default 0) from 2d vector array (DVEC) as values.
ex: (D2$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:D2$
 ;   [symbol]
 ;
 ; D2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 2d vector array (DVEC) as values.
 ;     ex: (D2$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: D2$\*

```
veq context broadcast op: D2$*
fxname: -D2*
args: (AX AY BX BY)
body (2): (VALUES (* AX BX) (* AY BY)).
```

#### :context: D2$\*!

```
veq context broadcast op: D2$*
fxname: -D2*
args: (AX AY BX BY)
body (2): (VALUES (* AX BX) (* AY BY)).
destructive.
```

#### :context: D2$+

```
veq context broadcast op: D2$+
fxname: -D2+
args: (AX AY BX BY)
body (2): (VALUES (+ AX BX) (+ AY BY)).
```

#### :context: D2$+!

```
veq context broadcast op: D2$+
fxname: -D2+
args: (AX AY BX BY)
body (2): (VALUES (+ AX BX) (+ AY BY)).
destructive.
```

#### :context: D2$-

```
veq context broadcast op: D2$-
fxname: -D2-
args: (AX AY BX BY)
body (2): (VALUES (- AX BX) (- AY BY)).
```

#### :context: D2$-!

```
veq context broadcast op: D2$-
fxname: -D2-
args: (AX AY BX BY)
body (2): (VALUES (- AX BX) (- AY BY)).
destructive.
```

#### :context: D2$.

```
veq context broadcast op: D2$.
fxname: -D2.
args: (AX AY BX BY)
body (1): (+ (* AX BX) (* AY BY)).
```

#### :context: D2$/

```
veq context broadcast op: D2$/
fxname: -D2/
args: (AX AY BX BY)
body (2): (VALUES (/ AX BX) (/ AY BY)).
```

#### :context: D2$/!

```
veq context broadcast op: D2$/
fxname: -D2/
args: (AX AY BX BY)
body (2): (VALUES (/ AX BX) (/ AY BY)).
destructive.
```

#### :context: D2$^

```
veq context broadcast op: D2$^
fxname: -D2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
```

#### :context: D2$^!

```
veq context broadcast op: D2$^
fxname: -D2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
destructive.
```

#### :context: D2$ABS

```
veq context broadcast op: D2$ABS
fxname: -D2ABS
args: (AX AY)
body (2): (VALUES (ABS AX) (ABS AY)).
```

#### :context: D2$ABS!

```
veq context broadcast op: D2$ABS
fxname: -D2ABS
args: (AX AY)
body (2): (VALUES (ABS AX) (ABS AY)).
destructive.
```

#### :context: D2$ANGLE

```
veq context broadcast op: D2$ANGLE
fxname: -D2ANGLE
args: (AX AY)
body (1): (MVC #'ATAN (-D2NORM AY AX)).
```

#### :context: D2$CROSS

```
veq context broadcast op: D2$CROSS
fxname: -D2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
```

#### :context: D2$CROSS!

```
veq context broadcast op: D2$CROSS
fxname: -D2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
destructive.
```

#### :context: D2$DST

```
veq context broadcast op: D2$DST
fxname: -D2DST
args: (AX AY BX BY)
body (1): (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY))))).
```

#### :context: D2$DST2

```
veq context broadcast op: D2$DST2
fxname: -D2DST2
args: (AX AY BX BY)
body (1): (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY))).
```

#### :context: D2$EXP

```
veq context broadcast op: D2$EXP
fxname: -D2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
```

#### :context: D2$EXP!

```
veq context broadcast op: D2$EXP
fxname: -D2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
destructive.
```

#### :context: D2$FLIP

```
veq context broadcast op: D2$FLIP
fxname: -D2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
```

#### :context: D2$FLIP!

```
veq context broadcast op: D2$FLIP
fxname: -D2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
destructive.
```

#### :context: D2$FROM

```
veq context broadcast op: D2$FROM
fxname: -D2FROM
args: (AX AY BX BY S)
body (2): (-D2+ AX AY (* BX S) (* BY S)).
```

#### :context: D2$FROM!

```
veq context broadcast op: D2$FROM
fxname: -D2FROM
args: (AX AY BX BY S)
body (2): (-D2+ AX AY (* BX S) (* BY S)).
destructive.
```

#### :context: D2$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

#### :context: D2$I-

```
veq context broadcast op: D2$I-
fxname: -D2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
```

#### :context: D2$I-!

```
veq context broadcast op: D2$I-
fxname: -D2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
destructive.
```

#### :context: D2$I/

```
veq context broadcast op: D2$I/
fxname: -D2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
```

#### :context: D2$I/!

```
veq context broadcast op: D2$I/
fxname: -D2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
destructive.
```

#### :context: D2$ISCALE

```
veq context broadcast op: D2$ISCALE
fxname: -D2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
```

#### :context: D2$ISCALE!

```
veq context broadcast op: D2$ISCALE
fxname: -D2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
destructive.
```

#### D2$LAST

```
return values from last row of 2d vector array.

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

#### :context: D2$LEN

```
veq context broadcast op: D2$LEN
fxname: -D2LEN
args: (AX AY)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE AX AY))))).
```

#### :context: D2$LEN2

```
veq context broadcast op: D2$LEN2
fxname: -D2LEN2
args: (AX AY)
body (1): (THE POS-DF (MVC #'+ (-D2SQUARE AX AY))).
```

#### :context: D2$LERP

```
veq context broadcast op: D2$LERP
fxname: -D2LERP
args: (AX AY BX BY S)
body (2): (-D2+ AX AY (* (- BX AX) S) (* (- BY AY) S)).
```

#### :context: D2$LERP!

```
veq context broadcast op: D2$LERP
fxname: -D2LERP
args: (AX AY BX BY S)
body (2): (-D2+ AX AY (* (- BX AX) S) (* (- BY AY) S)).
destructive.
```

#### D2$LINE

```
fx: %D2$LINE
macro wrapper: D2$LINE
defined via veq:def*

 ; VEQ:D2$LINE
 ;   [symbol]
 ;
 ; D2$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D2$LINE
 ;     macro wrapper: D2$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### D2$LSPACE

```
fx: %D2$LSPACE
macro wrapper: D2$LSPACE
defined via veq:fvdef*

 ; VEQ:D2$LSPACE
 ;   [symbol]
 ;
 ; D2$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D2$LSPACE
 ;     macro wrapper: D2$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### :context: D2$MAX

```
veq context broadcast op: D2$MAX
fxname: -D2MAX
args: (AX AY)
body (1): (MAX AX AY).
```

#### :context: D2$MID

```
veq context broadcast op: D2$MID
fxname: -D2MID
args: (AX AY BX BY)
body (2): (VALUES (* 0.5d0 (+ AX BX)) (* 0.5d0 (+ AY BY))).
```

#### :context: D2$MID!

```
veq context broadcast op: D2$MID
fxname: -D2MID
args: (AX AY BX BY)
body (2): (VALUES (* 0.5d0 (+ AX BX)) (* 0.5d0 (+ AY BY))).
destructive.
```

#### D2$MIMA

```
find min and max for all dimensions of 2 array.
ex: (D2$MIMA &key n) returns (values xmin xmax ...).
use n to limit to first n rows.

 ; VEQ:D2$MIMA
 ;   [symbol]
 ;
 ; D2$MIMA names a compiled function:
 ;   Lambda-list: (A0 &KEY (N (2$NUM A0)) INDS)
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

#### :context: D2$MIN

```
veq context broadcast op: D2$MIN
fxname: -D2MIN
args: (AX AY)
body (1): (MIN AX AY).
```

#### :context: D2$MOD

```
veq context broadcast op: D2$MOD
fxname: -D2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
```

#### :context: D2$MOD!

```
veq context broadcast op: D2$MOD
fxname: -D2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
destructive.
```

#### :context: D2$NEG

```
veq context broadcast op: D2$NEG
fxname: -D2NEG
args: (AX AY)
body (2): (VALUES (- AX) (- AY)).
```

#### :context: D2$NEG!

```
veq context broadcast op: D2$NEG
fxname: -D2NEG
args: (AX AY)
body (2): (VALUES (- AX) (- AY)).
destructive.
```

#### :context: D2$NORM

```
veq context broadcast op: D2$NORM
fxname: -D2NORM
args: (AX AY)
body (2): (MVC #'-D2ISCALE AX AY (MVC #'-D2LEN AX AY)).
```

#### :context: D2$NORM!

```
veq context broadcast op: D2$NORM
fxname: -D2NORM
args: (AX AY)
body (2): (MVC #'-D2ISCALE AX AY (MVC #'-D2LEN AX AY)).
destructive.
```

#### D2$NUM

```
number of elements in 2d array.
typed.

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

#### :context: D2$ON-CIRC

```
veq context broadcast op: D2$ON-CIRC
fxname: -D2ON-CIRC
args: (AX RAD)
body (2): (MVC #'-D2SCALE (-DCOS-SIN (* AX DPII)) RAD).
```

#### :context: D2$ON-CIRC\*

```
veq context broadcast op: D2$ON-CIRC*
fxname: -D2ON-CIRC*
args: (AX RAD)
body (2): (MVC #'-D2SCALE (-DCOS-SIN AX) RAD).
```

#### D2$ONE

```
make 2d array of ones.
typed.

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

#### :context: D2$PERP

```
veq context broadcast op: D2$PERP
fxname: -D2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
```

#### :context: D2$PERP!

```
veq context broadcast op: D2$PERP
fxname: -D2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
destructive.
```

#### :context: D2$PERP\*

```
veq context broadcast op: D2$PERP*
fxname: -D2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
```

#### :context: D2$PERP\*!

```
veq context broadcast op: D2$PERP*
fxname: -D2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
destructive.
```

#### D2$POINT

```
fx: %D2$POINT
macro wrapper: D2$POINT
defined via veq:def*

 ; VEQ:D2$POINT
 ;   [symbol]
 ;
 ; D2$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D2$POINT
 ;     macro wrapper: D2$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: D2$ROT

```
veq context broadcast op: D2$ROT
fxname: -D2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE
             (DF
               COSA
               SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
```

#### :context: D2$ROT!

```
veq context broadcast op: D2$ROT
fxname: -D2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE
             (DF
               COSA
               SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
destructive.
```

#### :context: D2$ROTS

```
veq context broadcast op: D2$ROTS
fxname: -D2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVC #'-D2+ (MVC #'-D2ROT (-D2- AX AY SX SY) ANGLE) SX SY).
```

#### :context: D2$ROTS!

```
veq context broadcast op: D2$ROTS
fxname: -D2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVC #'-D2+ (MVC #'-D2ROT (-D2- AX AY SX SY) ANGLE) SX SY).
destructive.
```

#### :context: D2$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D2$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 2
```

#### :context: D2$SCALE

```
veq context broadcast op: D2$SCALE
fxname: -D2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
```

#### :context: D2$SCALE!

```
veq context broadcast op: D2$SCALE
fxname: -D2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
destructive.
```

#### :context: D2$SQRT

```
veq context broadcast op: D2$SQRT
fxname: -D2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))).
```

#### :context: D2$SQRT!

```
veq context broadcast op: D2$SQRT
fxname: -D2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))).
destructive.
```

#### :context: D2$SQUARE

```
veq context broadcast op: D2$SQUARE
fxname: -D2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
```

#### :context: D2$SQUARE!

```
veq context broadcast op: D2$SQUARE
fxname: -D2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
destructive.
```

#### D2$SUM

```
sum all rows of 2d array.

 ; VEQ:D2$SUM
 ;   [symbol]
 ;
 ; D2$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 2d array.
 ;   Source file: src/array-reduce.lisp
```

#### D2$TAKE

```
returns 2d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:D2$TAKE
 ;   [symbol]
 ;
 ; D2$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 2d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### D2$VAL

```
make 2d array of val.
typed.

 ; VEQ:D2$VAL
 ;   [symbol]
 ;
 ; D2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: D2$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 2d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### D2$ZERO

```
make 2d vector array of zeros.
typed.

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

#### :context: D2\*

```
veq context op: D2*
fxname: -D2*
args: (AX AY BX BY)
body (2): (VALUES (* AX BX) (* AY BY)).
```

#### :context: D2+

```
veq context op: D2+
fxname: -D2+
args: (AX AY BX BY)
body (2): (VALUES (+ AX BX) (+ AY BY)).
```

#### :context: D2-

```
veq context op: D2-
fxname: -D2-
args: (AX AY BX BY)
body (2): (VALUES (- AX BX) (- AY BY)).
```

#### :context: D2.

```
veq context op: D2.
fxname: -D2.
args: (AX AY BX BY)
body (1): (+ (* AX BX) (* AY BY)).
```

#### :context: D2/

```
veq context op: D2/
fxname: -D2/
args: (AX AY BX BY)
body (2): (VALUES (/ AX BX) (/ AY BY)).
```

#### :context: D2^

```
veq context op: D2^
fxname: -D2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
```

#### :context: D2ABS

```
veq context op: D2ABS
fxname: -D2ABS
args: (AX AY)
body (2): (VALUES (ABS AX) (ABS AY)).
```

#### :context: D2ANGLE

```
veq context op: D2ANGLE
fxname: -D2ANGLE
args: (AX AY)
body (1): (MVC #'ATAN (-D2NORM AY AX)).
```

#### :context: D2CROSS

```
veq context op: D2CROSS
fxname: -D2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
```

#### :context: D2DST

```
veq context op: D2DST
fxname: -D2DST
args: (AX AY BX BY)
body (1): (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY))))).
```

#### :context: D2DST2

```
veq context op: D2DST2
fxname: -D2DST2
args: (AX AY BX BY)
body (1): (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY))).
```

#### :context: D2EXP

```
veq context op: D2EXP
fxname: -D2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
```

#### :context: D2FLIP

```
veq context op: D2FLIP
fxname: -D2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
```

#### :context: D2FROM

```
veq context op: D2FROM
fxname: -D2FROM
args: (AX AY BX BY S)
body (2): (-D2+ AX AY (* BX S) (* BY S)).
```

#### :context: D2I-

```
veq context op: D2I-
fxname: -D2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
```

#### :context: D2I/

```
veq context op: D2I/
fxname: -D2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
```

#### :context: D2ISCALE

```
veq context op: D2ISCALE
fxname: -D2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
```

#### :context: D2LEN

```
veq context op: D2LEN
fxname: -D2LEN
args: (AX AY)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE AX AY))))).
```

#### :context: D2LEN2

```
veq context op: D2LEN2
fxname: -D2LEN2
args: (AX AY)
body (1): (THE POS-DF (MVC #'+ (-D2SQUARE AX AY))).
```

#### :context: D2LERP

```
veq context op: D2LERP
fxname: -D2LERP
args: (AX AY BX BY S)
body (2): (-D2+ AX AY (* (- BX AX) S) (* (- BY AY) S)).
```

#### :context: D2LET

```
make 2d let.
ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*.
```

#### :context: D2MAX

```
veq context op: D2MAX
fxname: -D2MAX
args: (AX AY)
body (1): (MAX AX AY).
```

#### D2MEYE

```
return 2d eye matrix.

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

#### :context: D2MID

```
veq context op: D2MID
fxname: -D2MID
args: (AX AY BX BY)
body (2): (VALUES (* 0.5d0 (+ AX BX)) (* 0.5d0 (+ AY BY))).
```

#### :context: D2MIN

```
veq context op: D2MIN
fxname: -D2MIN
args: (AX AY)
body (1): (MIN AX AY).
```

#### D2MINV

```
invert 2x2 matrix. non-destructive.

 ; VEQ:D2MINV
 ;   [symbol]
 ;
 ; D2MINV names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     invert 2x2 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### D2MM

```
multiply mat * mat
of type: DVEC

 ; VEQ:D2MM
 ;   [symbol]
 ;
 ; D2MM names a macro:
 ;   Lambda-list: (A*349 B*351)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D2MMT

```
multiply mat * (transpose mat)
of type: DVEC

 ; VEQ:D2MMT
 ;   [symbol]
 ;
 ; D2MMT names a macro:
 ;   Lambda-list: (A*407 B*409)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### :context: D2MOD

```
veq context op: D2MOD
fxname: -D2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
```

#### D2MROT

```
DOCSTRING for %D2MROT;
make 2d rotation matrix for rotating a rads

 ; VEQ:D2MROT
 ;   [symbol]
 ;
 ; D2MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D2MROT;
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: src/mat.lisp
```

#### D2MROT\*

```
DOCSTRING for %D2MROT*;
make 2d rotation matrix for rotating a rads

 ; VEQ:D2MROT*
 ;   [symbol]
 ;
 ; D2MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D2MROT*;
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: src/mat.lisp
```

#### D2MSCALE

```
DOCSTRING for %D2MSCALE;
make 2d matrix for scaling by x

 ; VEQ:D2MSCALE
 ;   [symbol]
 ;
 ; D2MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D2MSCALE;
 ;     make 2d matrix for scaling by x
 ;   Source file: src/mat.lisp
```

#### D2MT!

```
transpose 2d matrix in-place.

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
multiply (transpose mat) * mat
of type: DVEC

 ; VEQ:D2MTM
 ;   [symbol]
 ;
 ; D2MTM names a macro:
 ;   Lambda-list: (A*436 B*438)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D2MTMT

```
multiply (transpose mat) * (transpose mat)
of type: DVEC

 ; VEQ:D2MTMT
 ;   [symbol]
 ;
 ; D2MTMT names a macro:
 ;   Lambda-list: (A*378 B*380)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D2MTRANS

```
DOCSTRING for %D2MTRANS;
make 2d transpose matrix for moving by x

 ; VEQ:D2MTRANS
 ;   [symbol]
 ;
 ; D2MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D2MTRANS;
 ;     make 2d transpose matrix for moving by x
 ;   Source file: src/mat.lisp
```

#### D2MTV

```
transpose(mat) * v. for 2d matrix and vector.

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
mat * v. for 2d matrix and vector.

 ; VEQ:D2MV
 ;   [symbol]
 ;
 ; D2MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 2d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :context: D2NEG

```
veq context op: D2NEG
fxname: -D2NEG
args: (AX AY)
body (2): (VALUES (- AX) (- AY)).
```

#### :context: D2NORM

```
veq context op: D2NORM
fxname: -D2NORM
args: (AX AY)
body (2): (MVC #'-D2ISCALE AX AY (MVC #'-D2LEN AX AY)).
```

#### :context: D2NSUM

```
make 2d
```

#### :context: D2ON-CIRC

```
veq context op: D2ON-CIRC
fxname: -D2ON-CIRC
args: (AX RAD)
body (2): (MVC #'-D2SCALE (-DCOS-SIN (* AX DPII)) RAD).
```

#### :context: D2ON-CIRC\*

```
veq context op: D2ON-CIRC*
fxname: -D2ON-CIRC*
args: (AX RAD)
body (2): (MVC #'-D2SCALE (-DCOS-SIN AX) RAD).
```

#### :context: D2PERP

```
veq context op: D2PERP
fxname: -D2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
```

#### :context: D2PERP\*

```
veq context op: D2PERP*
fxname: -D2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
```

#### :context: D2REP

```
repeat argument 2d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: D2REP\*

```
repeat the evaluated argument 2 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### :context: D2ROT

```
veq context op: D2ROT
fxname: -D2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE
             (DF
               COSA
               SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
```

#### :context: D2ROTS

```
veq context op: D2ROTS
fxname: -D2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVC #'-D2+ (MVC #'-D2ROT (-D2- AX AY SX SY) ANGLE) SX SY).
```

#### :context: D2SCALE

```
veq context op: D2SCALE
fxname: -D2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
```

#### :context: D2SQRT

```
veq context op: D2SQRT
fxname: -D2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))).
```

#### :context: D2SQUARE

```
veq context op: D2SQUARE
fxname: -D2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
```

#### :context: D2VSET

```
set 2d value.
ex: (D2VSET (a) (fx ...))
where (fx ...) returns 2 values.
```

#### :context: D2~

```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :context: D3

```
strict make 3d vector in veq context.
```

#### D3$

```
returns indices (default 0) from 3d vector array (DVEC) as values.
ex: (D3$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:D3$
 ;   [symbol]
 ;
 ; D3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 3d vector array (DVEC) as values.
 ;     ex: (D3$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: D3$\*

```
veq context broadcast op: D3$*
fxname: -D3*
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: D3$\*!

```
veq context broadcast op: D3$*
fxname: -D3*
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* AX BX) (* AY BY) (* AZ BZ)).
destructive.
```

#### :context: D3$+

```
veq context broadcast op: D3$+
fxname: -D3+
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ)).
```

#### :context: D3$+!

```
veq context broadcast op: D3$+
fxname: -D3+
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ)).
destructive.
```

#### :context: D3$-

```
veq context broadcast op: D3$-
fxname: -D3-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- AX BX) (- AY BY) (- AZ BZ)).
```

#### :context: D3$-!

```
veq context broadcast op: D3$-
fxname: -D3-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- AX BX) (- AY BY) (- AZ BZ)).
destructive.
```

#### :context: D3$.

```
veq context broadcast op: D3$.
fxname: -D3.
args: (AX AY AZ BX BY BZ)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: D3$/

```
veq context broadcast op: D3$/
fxname: -D3/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ)).
```

#### :context: D3$/!

```
veq context broadcast op: D3$/
fxname: -D3/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ)).
destructive.
```

#### :context: D3$^

```
veq context broadcast op: D3$^
fxname: -D3^
args: (AX AY AZ S)
body (3): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S)).
```

#### :context: D3$^!

```
veq context broadcast op: D3$^
fxname: -D3^
args: (AX AY AZ S)
body (3): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S)).
destructive.
```

#### :context: D3$ABS

```
veq context broadcast op: D3$ABS
fxname: -D3ABS
args: (AX AY AZ)
body (3): (VALUES (ABS AX) (ABS AY) (ABS AZ)).
```

#### :context: D3$ABS!

```
veq context broadcast op: D3$ABS
fxname: -D3ABS
args: (AX AY AZ)
body (3): (VALUES (ABS AX) (ABS AY) (ABS AZ)).
destructive.
```

#### :context: D3$CROSS

```
veq context broadcast op: D3$CROSS
fxname: -D3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
```

#### :context: D3$CROSS!

```
veq context broadcast op: D3$CROSS
fxname: -D3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
destructive.
```

#### :context: D3$DST

```
veq context broadcast op: D3$DST
fxname: -D3DST
args: (AX AY AZ BX BY BZ)
body (1): (SQRT
           (THE POS-DF (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ))))).
```

#### :context: D3$DST2

```
veq context broadcast op: D3$DST2
fxname: -D3DST2
args: (AX AY AZ BX BY BZ)
body (1): (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ))).
```

#### :context: D3$EXP

```
veq context broadcast op: D3$EXP
fxname: -D3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
```

#### :context: D3$EXP!

```
veq context broadcast op: D3$EXP
fxname: -D3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
destructive.
```

#### :context: D3$FROM

```
veq context broadcast op: D3$FROM
fxname: -D3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (-D3+ AX AY AZ (* BX S) (* BY S) (* BZ S)).
```

#### :context: D3$FROM!

```
veq context broadcast op: D3$FROM
fxname: -D3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (-D3+ AX AY AZ (* BX S) (* BY S) (* BZ S)).
destructive.
```

#### :context: D3$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

#### :context: D3$I-

```
veq context broadcast op: D3$I-
fxname: -D3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
```

#### :context: D3$I-!

```
veq context broadcast op: D3$I-
fxname: -D3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
destructive.
```

#### :context: D3$I/

```
veq context broadcast op: D3$I/
fxname: -D3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
```

#### :context: D3$I/!

```
veq context broadcast op: D3$I/
fxname: -D3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
destructive.
```

#### :context: D3$ISCALE

```
veq context broadcast op: D3$ISCALE
fxname: -D3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
```

#### :context: D3$ISCALE!

```
veq context broadcast op: D3$ISCALE
fxname: -D3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
destructive.
```

#### D3$LAST

```
return values from last row of 3d vector array.

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

#### :context: D3$LEN

```
veq context broadcast op: D3$LEN
fxname: -D3LEN
args: (AX AY AZ)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D3SQUARE AX AY AZ))))).
```

#### :context: D3$LEN2

```
veq context broadcast op: D3$LEN2
fxname: -D3LEN2
args: (AX AY AZ)
body (1): (THE POS-DF (MVC #'+ (-D3SQUARE AX AY AZ))).
```

#### :context: D3$LERP

```
veq context broadcast op: D3$LERP
fxname: -D3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (-D3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)).
```

#### :context: D3$LERP!

```
veq context broadcast op: D3$LERP
fxname: -D3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (-D3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)).
destructive.
```

#### D3$LINE

```
fx: %D3$LINE
macro wrapper: D3$LINE
defined via veq:def*

 ; VEQ:D3$LINE
 ;   [symbol]
 ;
 ; D3$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D3$LINE
 ;     macro wrapper: D3$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### D3$LSPACE

```
fx: %D3$LSPACE
macro wrapper: D3$LSPACE
defined via veq:fvdef*

 ; VEQ:D3$LSPACE
 ;   [symbol]
 ;
 ; D3$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D3$LSPACE
 ;     macro wrapper: D3$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### :context: D3$MAX

```
veq context broadcast op: D3$MAX
fxname: -D3MAX
args: (AX AY AZ)
body (1): (MAX AX AY AZ).
```

#### :context: D3$MID

```
veq context broadcast op: D3$MID
fxname: -D3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)).
```

#### :context: D3$MID!

```
veq context broadcast op: D3$MID
fxname: -D3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)).
destructive.
```

#### D3$MIMA

```
find min and max for all dimensions of 3 array.
ex: (D3$MIMA &key n) returns (values xmin xmax ...).
use n to limit to first n rows.

 ; VEQ:D3$MIMA
 ;   [symbol]
 ;
 ; D3$MIMA names a compiled function:
 ;   Lambda-list: (A0 &KEY (N (3$NUM A0)) INDS)
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

#### :context: D3$MIN

```
veq context broadcast op: D3$MIN
fxname: -D3MIN
args: (AX AY AZ)
body (1): (MIN AX AY AZ).
```

#### :context: D3$MOD

```
veq context broadcast op: D3$MOD
fxname: -D3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
```

#### :context: D3$MOD!

```
veq context broadcast op: D3$MOD
fxname: -D3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
destructive.
```

#### :context: D3$NEG

```
veq context broadcast op: D3$NEG
fxname: -D3NEG
args: (AX AY AZ)
body (3): (VALUES (- AX) (- AY) (- AZ)).
```

#### :context: D3$NEG!

```
veq context broadcast op: D3$NEG
fxname: -D3NEG
args: (AX AY AZ)
body (3): (VALUES (- AX) (- AY) (- AZ)).
destructive.
```

#### :context: D3$NORM

```
veq context broadcast op: D3$NORM
fxname: -D3NORM
args: (AX AY AZ)
body (3): (MVC #'-D3ISCALE AX AY AZ (THE POS-DF (MVC #'-D3LEN AX AY AZ))).
```

#### :context: D3$NORM!

```
veq context broadcast op: D3$NORM
fxname: -D3NORM
args: (AX AY AZ)
body (3): (MVC #'-D3ISCALE AX AY AZ (THE POS-DF (MVC #'-D3LEN AX AY AZ))).
destructive.
```

#### D3$NUM

```
number of elements in 3d array.
typed.

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
make 3d array of ones.
typed.

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
fx: %D3$POINT
macro wrapper: D3$POINT
defined via veq:def*

 ; VEQ:D3$POINT
 ;   [symbol]
 ;
 ; D3$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D3$POINT
 ;     macro wrapper: D3$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: D3$ROT

```
veq context broadcast op: D3$ROT
fxname: -D3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE
             (DF
               COSA))
            (MVC #'-D3FROM
                 (MVC #'-D3FROM (-D3SCALE AX AY AZ COSA)
                      (-D3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-D3. NX NY NZ AX AY AZ) (- 1.0d0 COSA)))).
```

#### :context: D3$ROT!

```
veq context broadcast op: D3$ROT
fxname: -D3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE
             (DF
               COSA))
            (MVC #'-D3FROM
                 (MVC #'-D3FROM (-D3SCALE AX AY AZ COSA)
                      (-D3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-D3. NX NY NZ AX AY AZ) (- 1.0d0 COSA)))).
destructive.
```

#### :context: D3$ROTS

```
veq context broadcast op: D3$ROTS
fxname: -D3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVC #'-D3+ (MVC #'-D3ROT (-D3- AX AY AZ SX SY SZ) NX NY NZ A) SX SY
               SZ).
```

#### :context: D3$ROTS!

```
veq context broadcast op: D3$ROTS
fxname: -D3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVC #'-D3+ (MVC #'-D3ROT (-D3- AX AY AZ SX SY SZ) NX NY NZ A) SX SY
               SZ).
destructive.
```

#### :context: D3$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D3$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 3
```

#### :context: D3$SCALE

```
veq context broadcast op: D3$SCALE
fxname: -D3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
```

#### :context: D3$SCALE!

```
veq context broadcast op: D3$SCALE
fxname: -D3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
destructive.
```

#### :context: D3$SQRT

```
veq context broadcast op: D3$SQRT
fxname: -D3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))).
```

#### :context: D3$SQRT!

```
veq context broadcast op: D3$SQRT
fxname: -D3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))).
destructive.
```

#### :context: D3$SQUARE

```
veq context broadcast op: D3$SQUARE
fxname: -D3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ))).
```

#### :context: D3$SQUARE!

```
veq context broadcast op: D3$SQUARE
fxname: -D3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ))).
destructive.
```

#### D3$SUM

```
sum all rows of 3d array.

 ; VEQ:D3$SUM
 ;   [symbol]
 ;
 ; D3$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 3d array.
 ;   Source file: src/array-reduce.lisp
```

#### D3$TAKE

```
returns 3d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:D3$TAKE
 ;   [symbol]
 ;
 ; D3$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 3d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### D3$VAL

```
make 3d array of val.
typed.

 ; VEQ:D3$VAL
 ;   [symbol]
 ;
 ; D3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: D3$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 3d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### D3$ZERO

```
make 3d vector array of zeros.
typed.

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

#### :context: D3\*

```
veq context op: D3*
fxname: -D3*
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: D3+

```
veq context op: D3+
fxname: -D3+
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ)).
```

#### :context: D3-

```
veq context op: D3-
fxname: -D3-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- AX BX) (- AY BY) (- AZ BZ)).
```

#### :context: D3.

```
veq context op: D3.
fxname: -D3.
args: (AX AY AZ BX BY BZ)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: D3/

```
veq context op: D3/
fxname: -D3/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ)).
```

#### :context: D3^

```
veq context op: D3^
fxname: -D3^
args: (AX AY AZ S)
body (3): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S)).
```

#### :context: D3ABS

```
veq context op: D3ABS
fxname: -D3ABS
args: (AX AY AZ)
body (3): (VALUES (ABS AX) (ABS AY) (ABS AZ)).
```

#### :context: D3CROSS

```
veq context op: D3CROSS
fxname: -D3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
```

#### :context: D3DST

```
veq context op: D3DST
fxname: -D3DST
args: (AX AY AZ BX BY BZ)
body (1): (SQRT
           (THE POS-DF (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ))))).
```

#### :context: D3DST2

```
veq context op: D3DST2
fxname: -D3DST2
args: (AX AY AZ BX BY BZ)
body (1): (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ))).
```

#### :context: D3EXP

```
veq context op: D3EXP
fxname: -D3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
```

#### :context: D3FROM

```
veq context op: D3FROM
fxname: -D3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (-D3+ AX AY AZ (* BX S) (* BY S) (* BZ S)).
```

#### :context: D3I-

```
veq context op: D3I-
fxname: -D3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
```

#### :context: D3I/

```
veq context op: D3I/
fxname: -D3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
```

#### :context: D3ISCALE

```
veq context op: D3ISCALE
fxname: -D3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
```

#### :context: D3LEN

```
veq context op: D3LEN
fxname: -D3LEN
args: (AX AY AZ)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D3SQUARE AX AY AZ))))).
```

#### :context: D3LEN2

```
veq context op: D3LEN2
fxname: -D3LEN2
args: (AX AY AZ)
body (1): (THE POS-DF (MVC #'+ (-D3SQUARE AX AY AZ))).
```

#### :context: D3LERP

```
veq context op: D3LERP
fxname: -D3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (-D3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)).
```

#### :context: D3LET

```
make 3d let.
ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*.
```

#### :context: D3MAX

```
veq context op: D3MAX
fxname: -D3MAX
args: (AX AY AZ)
body (1): (MAX AX AY AZ).
```

#### D3MEYE

```
return 3d eye matrix.

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

#### :context: D3MID

```
veq context op: D3MID
fxname: -D3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)).
```

#### :context: D3MIN

```
veq context op: D3MIN
fxname: -D3MIN
args: (AX AY AZ)
body (1): (MIN AX AY AZ).
```

#### D3MINV

```
invert 3x3 matrix. non-destructive.

 ; VEQ:D3MINV
 ;   [symbol]
 ;
 ; D3MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     invert 3x3 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### D3MM

```
multiply mat * mat
of type: DVEC

 ; VEQ:D3MM
 ;   [symbol]
 ;
 ; D3MM names a macro:
 ;   Lambda-list: (A*465 B*467)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D3MMT

```
multiply mat * (transpose mat)
of type: DVEC

 ; VEQ:D3MMT
 ;   [symbol]
 ;
 ; D3MMT names a macro:
 ;   Lambda-list: (A*523 B*525)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### :context: D3MOD

```
veq context op: D3MOD
fxname: -D3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
```

#### D3MROT

```
DOCSTRING for %D3MROT;
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:D3MROT
 ;   [symbol]
 ;
 ; D3MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D3MROT;
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: src/mat.lisp
```

#### D3MROT\*

```
DOCSTRING for %D3MROT*;
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:D3MROT*
 ;   [symbol]
 ;
 ; D3MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D3MROT*;
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: src/mat.lisp
```

#### D3MSCALE

```
DOCSTRING for %D3MSCALE;
make 3d matrix for scaling by x

 ; VEQ:D3MSCALE
 ;   [symbol]
 ;
 ; D3MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D3MSCALE;
 ;     make 3d matrix for scaling by x
 ;   Source file: src/mat.lisp
```

#### D3MT!

```
transpose 3d matrix in-place.

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
multiply (transpose mat) * mat
of type: DVEC

 ; VEQ:D3MTM
 ;   [symbol]
 ;
 ; D3MTM names a macro:
 ;   Lambda-list: (A*552 B*554)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D3MTMT

```
multiply (transpose mat) * (transpose mat)
of type: DVEC

 ; VEQ:D3MTMT
 ;   [symbol]
 ;
 ; D3MTMT names a macro:
 ;   Lambda-list: (A*494 B*496)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D3MTRANS

```
DOCSTRING for %D3MTRANS;
make 3d transpose matrix for moving by x

 ; VEQ:D3MTRANS
 ;   [symbol]
 ;
 ; D3MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %D3MTRANS;
 ;     make 3d transpose matrix for moving by x
 ;   Source file: src/mat.lisp
```

#### D3MTV

```
transpose(mat) * v. for 3d matrix and vector.

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
mat * v. for 3d matrix and vector.

 ; VEQ:D3MV
 ;   [symbol]
 ;
 ; D3MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 3d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :context: D3NEG

```
veq context op: D3NEG
fxname: -D3NEG
args: (AX AY AZ)
body (3): (VALUES (- AX) (- AY) (- AZ)).
```

#### :context: D3NORM

```
veq context op: D3NORM
fxname: -D3NORM
args: (AX AY AZ)
body (3): (MVC #'-D3ISCALE AX AY AZ (THE POS-DF (MVC #'-D3LEN AX AY AZ))).
```

#### :context: D3NSUM

```
make 3d
```

#### :context: D3REP

```
repeat argument 3d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: D3REP\*

```
repeat the evaluated argument 3 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### :context: D3ROT

```
veq context op: D3ROT
fxname: -D3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE
             (DF
               COSA))
            (MVC #'-D3FROM
                 (MVC #'-D3FROM (-D3SCALE AX AY AZ COSA)
                      (-D3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-D3. NX NY NZ AX AY AZ) (- 1.0d0 COSA)))).
```

#### :context: D3ROTS

```
veq context op: D3ROTS
fxname: -D3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVC #'-D3+ (MVC #'-D3ROT (-D3- AX AY AZ SX SY SZ) NX NY NZ A) SX SY
               SZ).
```

#### :context: D3SCALE

```
veq context op: D3SCALE
fxname: -D3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
```

#### :context: D3SQRT

```
veq context op: D3SQRT
fxname: -D3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))).
```

#### :context: D3SQUARE

```
veq context op: D3SQUARE
fxname: -D3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ))).
```

#### :context: D3VSET

```
set 3d value.
ex: (D3VSET (a) (fx ...))
where (fx ...) returns 3 values.
```

#### :context: D3~

```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :context: D4

```
strict make 4d vector in veq context.
```

#### D4$

```
returns indices (default 0) from 4d vector array (DVEC) as values.
ex: (D4$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:D4$
 ;   [symbol]
 ;
 ; D4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 4d vector array (DVEC) as values.
 ;     ex: (D4$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: D4$\*

```
veq context broadcast op: D4$*
fxname: -D4*
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: D4$\*!

```
veq context broadcast op: D4$*
fxname: -D4*
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
destructive.
```

#### :context: D4$+

```
veq context broadcast op: D4$+
fxname: -D4+
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW)).
```

#### :context: D4$+!

```
veq context broadcast op: D4$+
fxname: -D4+
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW)).
destructive.
```

#### :context: D4$-

```
veq context broadcast op: D4$-
fxname: -D4-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW)).
```

#### :context: D4$-!

```
veq context broadcast op: D4$-
fxname: -D4-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW)).
destructive.
```

#### :context: D4$.

```
veq context broadcast op: D4$.
fxname: -D4.
args: (AX AY AZ AW BX BY BZ BW)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: D4$/

```
veq context broadcast op: D4$/
fxname: -D4/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW)).
```

#### :context: D4$/!

```
veq context broadcast op: D4$/
fxname: -D4/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW)).
destructive.
```

#### :context: D4$^

```
veq context broadcast op: D4$^
fxname: -D4^
args: (AX AY AZ AW S)
body (4): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S) (EXPT AW S)).
```

#### :context: D4$^!

```
veq context broadcast op: D4$^
fxname: -D4^
args: (AX AY AZ AW S)
body (4): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S) (EXPT AW S)).
destructive.
```

#### :context: D4$ABS

```
veq context broadcast op: D4$ABS
fxname: -D4ABS
args: (AX AY AZ AW)
body (4): (VALUES (ABS AX) (ABS AY) (ABS AZ) (ABS AW)).
```

#### :context: D4$ABS!

```
veq context broadcast op: D4$ABS
fxname: -D4ABS
args: (AX AY AZ AW)
body (4): (VALUES (ABS AX) (ABS AY) (ABS AZ) (ABS AW)).
destructive.
```

#### :context: D4$DST

```
veq context broadcast op: D4$DST
fxname: -D4DST
args: (AX AY AZ AW BX BY BZ BW)
body (1): (SQRT
           (THE POS-DF
                (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))))).
```

#### :context: D4$DST2

```
veq context broadcast op: D4$DST2
fxname: -D4DST2
args: (AX AY AZ AW BX BY BZ BW)
body (1): (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))).
```

#### :context: D4$EXP

```
veq context broadcast op: D4$EXP
fxname: -D4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
```

#### :context: D4$EXP!

```
veq context broadcast op: D4$EXP
fxname: -D4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
destructive.
```

#### :context: D4$FROM

```
veq context broadcast op: D4$FROM
fxname: -D4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-D4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S)).
```

#### :context: D4$FROM!

```
veq context broadcast op: D4$FROM
fxname: -D4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-D4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S)).
destructive.
```

#### :context: D4$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

#### :context: D4$I-

```
veq context broadcast op: D4$I-
fxname: -D4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
```

#### :context: D4$I-!

```
veq context broadcast op: D4$I-
fxname: -D4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
destructive.
```

#### :context: D4$I/

```
veq context broadcast op: D4$I/
fxname: -D4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
```

#### :context: D4$I/!

```
veq context broadcast op: D4$I/
fxname: -D4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
destructive.
```

#### :context: D4$ISCALE

```
veq context broadcast op: D4$ISCALE
fxname: -D4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
```

#### :context: D4$ISCALE!

```
veq context broadcast op: D4$ISCALE
fxname: -D4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
destructive.
```

#### D4$LAST

```
return values from last row of 4d vector array.

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

#### :context: D4$LEN

```
veq context broadcast op: D4$LEN
fxname: -D4LEN
args: (AX AY AZ AW)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D4SQUARE AX AY AZ AW))))).
```

#### :context: D4$LEN2

```
veq context broadcast op: D4$LEN2
fxname: -D4LEN2
args: (AX AY AZ AW)
body (1): (THE POS-DF (MVC #'+ (-D4SQUARE AX AY AZ AW))).
```

#### :context: D4$LERP

```
veq context broadcast op: D4$LERP
fxname: -D4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-D4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
           (* (- BW AW) S)).
```

#### :context: D4$LERP!

```
veq context broadcast op: D4$LERP
fxname: -D4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-D4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
           (* (- BW AW) S)).
destructive.
```

#### D4$LINE

```
fx: %D4$LINE
macro wrapper: D4$LINE
defined via veq:def*

 ; VEQ:D4$LINE
 ;   [symbol]
 ;
 ; D4$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D4$LINE
 ;     macro wrapper: D4$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### D4$LSPACE

```
fx: %D4$LSPACE
macro wrapper: D4$LSPACE
defined via veq:fvdef*

 ; VEQ:D4$LSPACE
 ;   [symbol]
 ;
 ; D4$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D4$LSPACE
 ;     macro wrapper: D4$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### :context: D4$MAX

```
veq context broadcast op: D4$MAX
fxname: -D4MAX
args: (AX AY AZ AW)
body (1): (MAX AX AY AZ AW).
```

#### :context: D4$MID

```
veq context broadcast op: D4$MID
fxname: -D4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)
                  (* (+ BW AW) 0.5d0)).
```

#### :context: D4$MID!

```
veq context broadcast op: D4$MID
fxname: -D4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)
                  (* (+ BW AW) 0.5d0)).
destructive.
```

#### :context: D4$MIN

```
veq context broadcast op: D4$MIN
fxname: -D4MIN
args: (AX AY AZ AW)
body (1): (MIN AX AY AZ AW).
```

#### :context: D4$MOD

```
veq context broadcast op: D4$MOD
fxname: -D4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
```

#### :context: D4$MOD!

```
veq context broadcast op: D4$MOD
fxname: -D4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
destructive.
```

#### :context: D4$NEG

```
veq context broadcast op: D4$NEG
fxname: -D4NEG
args: (AX AY AZ AW)
body (4): (VALUES (- AX) (- AY) (- AZ) (- AW)).
```

#### :context: D4$NEG!

```
veq context broadcast op: D4$NEG
fxname: -D4NEG
args: (AX AY AZ AW)
body (4): (VALUES (- AX) (- AY) (- AZ) (- AW)).
destructive.
```

#### :context: D4$NORM

```
veq context broadcast op: D4$NORM
fxname: -D4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-D4ISCALE AX AY AZ AW (THE POS-DF (MVC #'-D4LEN AX AY AZ AW))).
```

#### :context: D4$NORM!

```
veq context broadcast op: D4$NORM
fxname: -D4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-D4ISCALE AX AY AZ AW (THE POS-DF (MVC #'-D4LEN AX AY AZ AW))).
destructive.
```

#### D4$NUM

```
number of elements in 4d array.
typed.

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
make 4d array of ones.
typed.

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
fx: %D4$POINT
macro wrapper: D4$POINT
defined via veq:def*

 ; VEQ:D4$POINT
 ;   [symbol]
 ;
 ; D4$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %D4$POINT
 ;     macro wrapper: D4$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: D4$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D4$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 4
```

#### :context: D4$SCALE

```
veq context broadcast op: D4$SCALE
fxname: -D4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
```

#### :context: D4$SCALE!

```
veq context broadcast op: D4$SCALE
fxname: -D4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
destructive.
```

#### :context: D4$SQRT

```
veq context broadcast op: D4$SQRT
fxname: -D4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))
                  (THE POS-DF (SQRT (THE POS-DF AW)))).
```

#### :context: D4$SQRT!

```
veq context broadcast op: D4$SQRT
fxname: -D4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))
                  (THE POS-DF (SQRT (THE POS-DF AW)))).
destructive.
```

#### :context: D4$SQUARE

```
veq context broadcast op: D4$SQUARE
fxname: -D4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ)) (THE POS-DF (* AW AW))).
```

#### :context: D4$SQUARE!

```
veq context broadcast op: D4$SQUARE
fxname: -D4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ)) (THE POS-DF (* AW AW))).
destructive.
```

#### D4$SUM

```
sum all rows of 4d array.

 ; VEQ:D4$SUM
 ;   [symbol]
 ;
 ; D4$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 4d array.
 ;   Source file: src/array-reduce.lisp
```

#### D4$TAKE

```
returns 4d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:D4$TAKE
 ;   [symbol]
 ;
 ; D4$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY DOUBLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 4d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### D4$VAL

```
make 4d array of val.
typed.

 ; VEQ:D4$VAL
 ;   [symbol]
 ;
 ; D4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: D4$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 4d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### D4$ZERO

```
make 4d vector array of zeros.
typed.

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

#### :context: D4\*

```
veq context op: D4*
fxname: -D4*
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: D4+

```
veq context op: D4+
fxname: -D4+
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW)).
```

#### :context: D4-

```
veq context op: D4-
fxname: -D4-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW)).
```

#### :context: D4.

```
veq context op: D4.
fxname: -D4.
args: (AX AY AZ AW BX BY BZ BW)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: D4/

```
veq context op: D4/
fxname: -D4/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW)).
```

#### :context: D4^

```
veq context op: D4^
fxname: -D4^
args: (AX AY AZ AW S)
body (4): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S) (EXPT AW S)).
```

#### :context: D4ABS

```
veq context op: D4ABS
fxname: -D4ABS
args: (AX AY AZ AW)
body (4): (VALUES (ABS AX) (ABS AY) (ABS AZ) (ABS AW)).
```

#### :context: D4DST

```
veq context op: D4DST
fxname: -D4DST
args: (AX AY AZ AW BX BY BZ BW)
body (1): (SQRT
           (THE POS-DF
                (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))))).
```

#### :context: D4DST2

```
veq context op: D4DST2
fxname: -D4DST2
args: (AX AY AZ AW BX BY BZ BW)
body (1): (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))).
```

#### :context: D4EXP

```
veq context op: D4EXP
fxname: -D4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
```

#### :context: D4FROM

```
veq context op: D4FROM
fxname: -D4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-D4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S)).
```

#### :context: D4I-

```
veq context op: D4I-
fxname: -D4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
```

#### :context: D4I/

```
veq context op: D4I/
fxname: -D4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
```

#### :context: D4ISCALE

```
veq context op: D4ISCALE
fxname: -D4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
```

#### :context: D4LEN

```
veq context op: D4LEN
fxname: -D4LEN
args: (AX AY AZ AW)
body (1): (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D4SQUARE AX AY AZ AW))))).
```

#### :context: D4LEN2

```
veq context op: D4LEN2
fxname: -D4LEN2
args: (AX AY AZ AW)
body (1): (THE POS-DF (MVC #'+ (-D4SQUARE AX AY AZ AW))).
```

#### :context: D4LERP

```
veq context op: D4LERP
fxname: -D4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-D4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
           (* (- BW AW) S)).
```

#### :context: D4LET

```
make 4d let.
ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*.
```

#### :context: D4MAX

```
veq context op: D4MAX
fxname: -D4MAX
args: (AX AY AZ AW)
body (1): (MAX AX AY AZ AW).
```

#### D4MEYE

```
return 4d eye matrix.

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

#### :context: D4MID

```
veq context op: D4MID
fxname: -D4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)
                  (* (+ BW AW) 0.5d0)).
```

#### :context: D4MIN

```
veq context op: D4MIN
fxname: -D4MIN
args: (AX AY AZ AW)
body (1): (MIN AX AY AZ AW).
```

#### D4MINV

```
invert 4x4 matrix. non-destructive.

 ; VEQ:D4MINV
 ;   [symbol]
 ;
 ; D4MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     invert 4x4 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### D4MM

```
multiply mat * mat
of type: DVEC

 ; VEQ:D4MM
 ;   [symbol]
 ;
 ; D4MM names a macro:
 ;   Lambda-list: (A*581 B*583)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D4MMT

```
multiply mat * (transpose mat)
of type: DVEC

 ; VEQ:D4MMT
 ;   [symbol]
 ;
 ; D4MMT names a macro:
 ;   Lambda-list: (A*639 B*641)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### :context: D4MOD

```
veq context op: D4MOD
fxname: -D4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
```

#### D4MT!

```
transpose 4d matrix in-place.

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
multiply (transpose mat) * mat
of type: DVEC

 ; VEQ:D4MTM
 ;   [symbol]
 ;
 ; D4MTM names a macro:
 ;   Lambda-list: (A*668 B*670)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D4MTMT

```
multiply (transpose mat) * (transpose mat)
of type: DVEC

 ; VEQ:D4MTMT
 ;   [symbol]
 ;
 ; D4MTMT names a macro:
 ;   Lambda-list: (A*610 B*612)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: src/mat.lisp
```

#### D4MTV

```
transpose(mat) * v. for 4d matrix and vector.

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
mat * v. for 4d matrix and vector.

 ; VEQ:D4MV
 ;   [symbol]
 ;
 ; D4MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 4d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :context: D4NEG

```
veq context op: D4NEG
fxname: -D4NEG
args: (AX AY AZ AW)
body (4): (VALUES (- AX) (- AY) (- AZ) (- AW)).
```

#### :context: D4NORM

```
veq context op: D4NORM
fxname: -D4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-D4ISCALE AX AY AZ AW (THE POS-DF (MVC #'-D4LEN AX AY AZ AW))).
```

#### :context: D4NSUM

```
make 4d
```

#### :context: D4REP

```
repeat argument 4d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: D4REP\*

```
repeat the evaluated argument 4 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### :context: D4SCALE

```
veq context op: D4SCALE
fxname: -D4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
```

#### :context: D4SQRT

```
veq context op: D4SQRT
fxname: -D4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (SQRT (THE POS-DF AX)))
                  (THE POS-DF (SQRT (THE POS-DF AY)))
                  (THE POS-DF (SQRT (THE POS-DF AZ)))
                  (THE POS-DF (SQRT (THE POS-DF AW)))).
```

#### :context: D4SQUARE

```
veq context op: D4SQUARE
fxname: -D4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-DF (* AX AX)) (THE POS-DF (* AY AY))
                  (THE POS-DF (* AZ AZ)) (THE POS-DF (* AW AW))).
```

#### :context: D4VSET

```
set 4d value.
ex: (D4VSET (a) (fx ...))
where (fx ...) returns 4 values.
```

#### :context: D4~

```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### D?

```
describe argument

 ; VEQ:D?
 ;   [symbol]
 ;
 ; D? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     describe argument
 ;   Source file: src/utils.lisp
```

#### :context: D^

```
veq context op: D^
fxname: -D^
args: (AX S)
body (1): (EXPT AX S).
```

#### D_

```
create DVEC vector array from body: (D_ '(a b c ...)).

 ; VEQ:D_
 ;   [symbol]
 ;
 ; D_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create DVEC vector array from body: (D_ '(a b c ...)).
 ;   Source file: src/array-utils.lisp
```

#### :context: DABS

```
veq context op: DABS
fxname: -DABS
args: (AX)
body (1): (ABS AX).
```

#### :context: DCLAMP

```
veq context op: DCLAMP
fxname: -DCLAMP
args: (X)
body (1): (MIN 1.0d0 (MAX 0.0d0 X)).
```

#### :context: DCLAMP\*

```
veq context op: DCLAMP*
fxname: -DCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
```

#### :context: DCOS-SIN

```
veq context op: DCOS-SIN
fxname: -DCOS-SIN
args: (AX)
body (2): (VALUES (COS AX) (SIN AX)).
```

#### :context: DDEG->RAD

```
veq context op: DDEG->RAD
fxname: -DDEG->RAD
args: (D)
body (1): (* DPI (/ D 180.0d0)).
```

#### DEASE-IN-BACK

```
ease in:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0d0 S) X) S))

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
ease in:
arg: (X)
body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))

 ; VEQ:DEASE-IN-CIRC
 ;   [symbol]
 ;
 ; DEASE-IN-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES
 ;                   (OR (COMPLEX DOUBLE-FLOAT)
 ;                       (DOUBLE-FLOAT -0.0d0 1.0d0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-CUBIC

```
ease in:
arg: (X)
body: (* X X X)

 ; VEQ:DEASE-IN-CUBIC
 ;   [symbol]
 ;
 ; DEASE-IN-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-ELASTIC

```
ease in:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
        (-
         (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
            (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))

 ; VEQ:DEASE-IN-ELASTIC
 ;   [symbol]
 ;
 ; DEASE-IN-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
 ;                          &OPTIONAL))
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
ease in:
arg: (X)
body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))

 ; VEQ:DEASE-IN-EXP
 ;   [symbol]
 ;
 ; DEASE-IN-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (DOUBLE-FLOAT 0.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-LINEAR

```
ease in:
arg: (X)
body: X

 ; VEQ:DEASE-IN-LINEAR
 ;   [symbol]
 ;
 ; DEASE-IN-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-BACK

```
ease in-out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0d0 S) X) S))

 ; VEQ:DEASE-IN-OUT-BACK
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.7015800476074219d0))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES (OR (COMPLEX DOUBLE-FLOAT) DOUBLE-FLOAT)
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0d0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-CIRC

```
ease in-out:
arg: (X)
body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))

 ; VEQ:DEASE-IN-OUT-CIRC
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES
 ;                   (OR (DOUBLE-FLOAT -0.0d0 1.0d0)
 ;                       (COMPLEX DOUBLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-CUBIC

```
ease in-out:
arg: (X)
body: (* X X X)

 ; VEQ:DEASE-IN-OUT-CUBIC
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-ELASTIC

```
ease in-out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
        (-
         (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
            (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))

 ; VEQ:DEASE-IN-OUT-ELASTIC
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES (OR (COMPLEX DOUBLE-FLOAT) DOUBLE-FLOAT)
 ;                          &OPTIONAL))
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
ease in-out:
arg: (X)
body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))

 ; VEQ:DEASE-IN-OUT-EXP
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-LINEAR

```
ease in-out:
arg: (X)
body: X

 ; VEQ:DEASE-IN-OUT-LINEAR
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-QUART

```
ease in-out:
arg: (X)
body: (EXPT X 4.0d0)

 ; VEQ:DEASE-IN-OUT-QUART
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-QUINT

```
ease in-out:
arg: (X)
body: (EXPT X 5.0d0)

 ; VEQ:DEASE-IN-OUT-QUINT
 ;   [symbol]
 ;
 ; DEASE-IN-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-OUT-SIN

```
ease in-out:
arg: (X)
body: (- 1.0d0 (COS (* X DPI5)))

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
ease in:
arg: (X)
body: (EXPT X 4.0d0)

 ; VEQ:DEASE-IN-QUART
 ;   [symbol]
 ;
 ; DEASE-IN-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 4.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-QUINT

```
ease in:
arg: (X)
body: (EXPT X 5.0d0)

 ; VEQ:DEASE-IN-QUINT
 ;   [symbol]
 ;
 ; DEASE-IN-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 5.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-IN-SIN

```
ease in:
arg: (X)
body: (- 1.0d0 (COS (* X DPI5)))

 ; VEQ:DEASE-IN-SIN
 ;   [symbol]
 ;
 ; DEASE-IN-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT 0.0d0 2.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- 1.0d0 (COS (* X DPI5)))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-BACK

```
ease out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0d0 S) X) S))

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
ease out:
arg: (X)
body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))

 ; VEQ:DEASE-OUT-CIRC
 ;   [symbol]
 ;
 ; DEASE-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES
 ;                   (OR (COMPLEX DOUBLE-FLOAT)
 ;                       (DOUBLE-FLOAT 0.0d0 1.0d0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-CUBIC

```
ease out:
arg: (X)
body: (* X X X)

 ; VEQ:DEASE-OUT-CUBIC
 ;   [symbol]
 ;
 ; DEASE-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-ELASTIC

```
ease out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
        (-
         (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
            (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))

 ; VEQ:DEASE-OUT-ELASTIC
 ;   [symbol]
 ;
 ; DEASE-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
 ;                          &OPTIONAL))
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
ease out:
arg: (X)
body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))

 ; VEQ:DEASE-OUT-EXP
 ;   [symbol]
 ;
 ; DEASE-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (DOUBLE-FLOAT * 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-LINEAR

```
ease out:
arg: (X)
body: X

 ; VEQ:DEASE-OUT-LINEAR
 ;   [symbol]
 ;
 ; DEASE-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-QUART

```
ease out:
arg: (X)
body: (EXPT X 4.0d0)

 ; VEQ:DEASE-OUT-QUART
 ;   [symbol]
 ;
 ; DEASE-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-QUINT

```
ease out:
arg: (X)
body: (EXPT X 5.0d0)

 ; VEQ:DEASE-OUT-QUINT
 ;   [symbol]
 ;
 ; DEASE-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0d0)
 ;   Source file: src/easing.lisp
```

#### DEASE-OUT-SIN

```
ease out:
arg: (X)
body: (- 1.0d0 (COS (* X DPI5)))

 ; VEQ:DEASE-OUT-SIN
 ;   [symbol]
 ;
 ; DEASE-OUT-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (DOUBLE-FLOAT -1.0d0 1.0d0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- 1.0d0 (COS (* X DPI5)))
 ;   Source file: src/easing.lisp
```

#### DEF\*

```
defines a function named: %fx
and a wrapper macro named: fx

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...).

 ; VEQ:DEF*
 ;   [symbol]
 ;
 ; DEF* names a macro:
 ;   Lambda-list: (MNAME &BODY BODY)
 ;   Documentation:
 ;     defines a function named: %fx
 ;     and a wrapper macro named: fx
 ;
 ;     the wrapper macro ensures every call to this function is done as
 ;     (mvc #'%fx ...).
 ;   Source file: src/macrolets.lisp
```

#### :context: DEXP

```
veq context op: DEXP
fxname: -DEXP
args: (AX)
body (1): (VALUES (EXP AX)).
```

#### DF

```
:missing:todo:

 ; VEQ:DF
 ;   [symbol]
 ;
 ; DF names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
 ;
 ; DF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: DOUBLE-FLOAT
```

#### DF\*

```
:missing:todo:

 ; VEQ:DF*
 ;   [symbol]
 ;
 ; DF* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### DFL

```
return (values (df a) (df b ..) from (list a b ..).

 ; VEQ:DFL
 ;   [symbol]
 ;
 ; DFL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (df a) (df b ..) from (list a b ..).
 ;   Source file: src/types.lisp
```

#### :context: DFROM

```
veq context op: DFROM
fxname: -DFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
```

#### :context: DI-

```
veq context op: DI-
fxname: -DI-
args: (AX BX)
body (1): (- BX AX).
```

#### :context: DI/

```
veq context op: DI/
fxname: -DI/
args: (AX BX)
body (1): (/ BX AX).
```

#### :context: DISCALE

```
veq context op: DISCALE
fxname: -DISCALE
args: (AX S)
body (1): (VALUES (/ AX S)).
```

#### :context: DLEN

```
veq context op: DLEN
fxname: -DLEN
args: (AX)
body (1): (THE POS-DF AX).
```

#### :context: DLEN2

```
veq context op: DLEN2
fxname: -DLEN2
args: (AX)
body (1): (THE POS-DF (MVC #'+ (-DSQUARE AX))).
```

#### :context: DLERP

```
veq context op: DLERP
fxname: -DLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
```

#### :context: DMID

```
veq context op: DMID
fxname: -DMID
args: (AX BX)
body (1): (* 0.5d0 (+ AX BX)).
```

#### :context: DMOD

```
veq context op: DMOD
fxname: -DMOD
args: (AX S)
body (1): (MOD AX S).
```

#### :context: DNEG

```
veq context op: DNEG
fxname: -DNEG
args: (AX)
body (1): (- AX).
```

#### :context: DNORM

```
veq context op: DNORM
fxname: -DNORM
args: (AX)
body (1): (MVC #'-DISCALE AX (MVC #'-DLEN AX)).
```

#### :context: DNSUM

```
make 1d
```

#### DPI

```
:missing:todo:

 ; VEQ:DPI
 ;   [symbol]
 ;
 ; DPI names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 3.141592653589793d0
```

#### DPI5

```
:missing:todo:

 ; VEQ:DPI5
 ;   [symbol]
 ;
 ; DPI5 names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 1.5707963267948966d0
```

#### DPII

```
:missing:todo:

 ; VEQ:DPII
 ;   [symbol]
 ;
 ; DPII names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 6.283185307179586d0
```

#### :context: DREP

```
repeat argument 1d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: DREP\*

```
repeat the evaluated argument 1 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### DSB

```
:missing:todo:

 ; VEQ:DSB
 ;   [symbol]
 ;
 ; DSB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: src/utils.lisp
```

#### :context: DSCALE

```
veq context op: DSCALE
fxname: -DSCALE
args: (AX S)
body (1): (VALUES (* AX S)).
```

#### DSEL

```
return values from body in order of dims. use indices or :x :y :z :w
ex: (dsel (:w :zx 0) (values a b c d))
returns (values d c a a).

 ; VEQ:DSEL
 ;   [symbol]
 ;
 ; DSEL names a macro:
 ;   Lambda-list: ((&REST DIMS) &BODY BODY)
 ;   Documentation:
 ;     return values from body in order of dims. use indices or :x :y :z :w
 ;     ex: (dsel (:w :zx 0) (values a b c d))
 ;     returns (values d c a a).
 ;   Source file: src/select-dim.lisp
```

#### :context: DSIN-COS

```
veq context op: DSIN-COS
fxname: -DSIN-COS
args: (AX)
body (2): (VALUES (SIN AX) (COS AX)).
```

#### :context: DSQRT

```
veq context op: DSQRT
fxname: -DSQRT
args: (AX)
body (1): (THE POS-DF (SQRT (THE POS-DF AX))).
```

#### :context: DSQUARE

```
veq context op: DSQUARE
fxname: -DSQUARE
args: (AX)
body (1): (* AX AX).
```

#### DVEC

```
:missing:todo:

 ; VEQ:DVEC
 ;   [symbol]
 ;
 ; DVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:DF)
```

#### DVLET

```
:missing:todo:

 ; VEQ:DVLET
 ;   [symbol]
```

#### :context: DVSET

```
set 1d value.
ex: (DVSET (a) (fx ...))
where (fx ...) returns 1 values.
```

#### :context: DWITH-ARRAYS

```
args: (&key (n 0) inds (start 0) itr cnt arr fxs exs nxs)

n is the number of iterations
start is the first (row) index. then n-1 more.
inds is (row) indices to iterate. replaces n/start
arr is the arrays to be defined/referenced
itr is the symbol representing indices
cnt is the symbol representing iterations from 0
fxs is the labels
exs is the expressions assigned to array
nxs is the expressions with no assignment

ex:

(dwith-arrays (:n 7 :itr k ; k will be 0, 1, ..., 6
  ; the third form in elements of arr can be empty, a form that will be
  ; executed, or a symbol that refers to an array defined outside of
  ; with-arrays
  :arr ((a 3 (f3$one 7)) ; init a as (f3$one 7)
        (b 3) (c 3)) ; init b,c as (f3$zero 7)
  ; define functions to use in fxs
  :fxs ((cross ((varg 3 v w)) (f3cross v w))
        (init1 (i) (f3~ (1+ i) (* 2 i) (+ 2 i)))
        (init2 (i) (f3~ (+ 2 i) (1+ i) (* 2 i))))
  ; perform the calculations
  :exs ((a k (init1 k)) ; init row k of a with init1
        (b k (init2 k)) ; init row k of b with init2
        (c k (cross a b))) ; set row k of c to (cross a b)
  :nxs ((cross a b))); executes the function, but does not assign res anywhere
  ; use the arrays. the last form is returned, as in a progn
  (vpr c))
```

#### :context: D~

```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### EXT-SYMBOLS?

```
list all external symbols in veq. use :verbose to inlcude docstring.
use :pretty to print verbose output to stdout in a readable form.

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

#### :context: F

```
strict make 1d vector in veq context.
```

#### F$

```
returns indices (default 0) from 1d vector array (FVEC) as values.
ex: (F$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:F$
 ;   [symbol]
 ;
 ; F$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 1d vector array (FVEC) as values.
 ;     ex: (F$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: F$\*

```
veq context broadcast op: F$*
fxname: -F*
args: (AX BX)
body (1): (* AX BX).
```

#### :context: F$\*!

```
veq context broadcast op: F$*
fxname: -F*
args: (AX BX)
body (1): (* AX BX).
destructive.
```

#### :context: F$+

```
veq context broadcast op: F$+
fxname: -F+
args: (AX BX)
body (1): (+ AX BX).
```

#### :context: F$+!

```
veq context broadcast op: F$+
fxname: -F+
args: (AX BX)
body (1): (+ AX BX).
destructive.
```

#### :context: F$-

```
veq context broadcast op: F$-
fxname: -F-
args: (AX BX)
body (1): (- AX BX).
```

#### :context: F$-!

```
veq context broadcast op: F$-
fxname: -F-
args: (AX BX)
body (1): (- AX BX).
destructive.
```

#### :context: F$/

```
veq context broadcast op: F$/
fxname: -F/
args: (AX BX)
body (1): (/ AX BX).
```

#### :context: F$/!

```
veq context broadcast op: F$/
fxname: -F/
args: (AX BX)
body (1): (/ AX BX).
destructive.
```

#### :context: F$^

```
veq context broadcast op: F$^
fxname: -F^
args: (AX S)
body (1): (EXPT AX S).
```

#### :context: F$^!

```
veq context broadcast op: F$^
fxname: -F^
args: (AX S)
body (1): (EXPT AX S).
destructive.
```

#### F$_

```
create fvec vector array from body. where body is a list of lists.
ex: (f$_ (loop repeat 2 collect `(1f0 2f0)))
ex: (f$_ '((1f0 2f0) (1f0 2f0))).

 ; VEQ:F$_
 ;   [symbol]
 ;
 ; F$_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create fvec vector array from body. where body is a list of lists.
 ;     ex: (f$_ (loop repeat 2 collect `(1f0 2f0)))
 ;     ex: (f$_ '((1f0 2f0) (1f0 2f0))).
 ;   Source file: src/array-utils.lisp
```

#### :context: F$ABS

```
veq context broadcast op: F$ABS
fxname: -FABS
args: (AX)
body (1): (ABS AX).
```

#### :context: F$ABS!

```
veq context broadcast op: F$ABS
fxname: -FABS
args: (AX)
body (1): (ABS AX).
destructive.
```

#### :context: F$CLAMP

```
veq context broadcast op: F$CLAMP
fxname: -FCLAMP
args: (X)
body (1): (MIN 1.0 (MAX 0.0 X)).
```

#### :context: F$CLAMP!

```
veq context broadcast op: F$CLAMP
fxname: -FCLAMP
args: (X)
body (1): (MIN 1.0 (MAX 0.0 X)).
destructive.
```

#### :context: F$CLAMP\*

```
veq context broadcast op: F$CLAMP*
fxname: -FCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
```

#### :context: F$CLAMP\*!

```
veq context broadcast op: F$CLAMP*
fxname: -FCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
destructive.
```

#### F$COPY

```
copy FVEC vector array.

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

#### :context: F$COS-SIN

```
veq context broadcast op: F$COS-SIN
fxname: -FCOS-SIN
args: (AX)
body (2): (VALUES (COS AX) (SIN AX)).
```

#### :context: F$DEG->RAD

```
veq context broadcast op: F$DEG->RAD
fxname: -FDEG->RAD
args: (D)
body (1): (* FPI (/ D 180.0)).
```

#### :context: F$DEG->RAD!

```
veq context broadcast op: F$DEG->RAD
fxname: -FDEG->RAD
args: (D)
body (1): (* FPI (/ D 180.0)).
destructive.
```

#### :context: F$EXP

```
veq context broadcast op: F$EXP
fxname: -FEXP
args: (AX)
body (1): (VALUES (EXP AX)).
```

#### :context: F$EXP!

```
veq context broadcast op: F$EXP
fxname: -FEXP
args: (AX)
body (1): (VALUES (EXP AX)).
destructive.
```

#### :context: F$FROM

```
veq context broadcast op: F$FROM
fxname: -FFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
```

#### :context: F$FROM!

```
veq context broadcast op: F$FROM
fxname: -FFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
destructive.
```

#### :context: F$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

#### :context: F$I-

```
veq context broadcast op: F$I-
fxname: -FI-
args: (AX BX)
body (1): (- BX AX).
```

#### :context: F$I-!

```
veq context broadcast op: F$I-
fxname: -FI-
args: (AX BX)
body (1): (- BX AX).
destructive.
```

#### :context: F$I/

```
veq context broadcast op: F$I/
fxname: -FI/
args: (AX BX)
body (1): (/ BX AX).
```

#### :context: F$I/!

```
veq context broadcast op: F$I/
fxname: -FI/
args: (AX BX)
body (1): (/ BX AX).
destructive.
```

#### :context: F$ISCALE

```
veq context broadcast op: F$ISCALE
fxname: -FISCALE
args: (AX S)
body (1): (VALUES (/ AX S)).
```

#### :context: F$ISCALE!

```
veq context broadcast op: F$ISCALE
fxname: -FISCALE
args: (AX S)
body (1): (VALUES (/ AX S)).
destructive.
```

#### F$LAST

```
return values from last row of 1d vector array.

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

#### :context: F$LEN

```
veq context broadcast op: F$LEN
fxname: -FLEN
args: (AX)
body (1): (THE POS-FF AX).
```

#### :context: F$LEN!

```
veq context broadcast op: F$LEN
fxname: -FLEN
args: (AX)
body (1): (THE POS-FF AX).
destructive.
```

#### :context: F$LEN2

```
veq context broadcast op: F$LEN2
fxname: -FLEN2
args: (AX)
body (1): (THE POS-FF (MVC #'+ (-FSQUARE AX))).
```

#### :context: F$LEN2!

```
veq context broadcast op: F$LEN2
fxname: -FLEN2
args: (AX)
body (1): (THE POS-FF (MVC #'+ (-FSQUARE AX))).
destructive.
```

#### :context: F$LERP

```
veq context broadcast op: F$LERP
fxname: -FLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
```

#### :context: F$LERP!

```
veq context broadcast op: F$LERP
fxname: -FLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
destructive.
```

#### F$LINE

```
fx: %F$LINE
macro wrapper: F$LINE
defined via veq:def*

 ; VEQ:F$LINE
 ;   [symbol]
 ;
 ; F$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F$LINE
 ;     macro wrapper: F$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### F$LSPACE

```
fx: %F$LSPACE
macro wrapper: F$LSPACE
defined via veq:fvdef*

 ; VEQ:F$LSPACE
 ;   [symbol]
 ;
 ; F$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F$LSPACE
 ;     macro wrapper: F$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### F$MAKE

```
create FVEC vector array with size n * dim, and initial value v.

 ; VEQ:F$MAKE
 ;   [symbol]
 ;
 ; F$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0))
 ;   Documentation:
 ;     create FVEC vector array with size n * dim, and initial value v.
 ;   Source file: src/array-utils.lisp
```

#### :context: F$MID

```
veq context broadcast op: F$MID
fxname: -FMID
args: (AX BX)
body (1): (* 0.5 (+ AX BX)).
```

#### :context: F$MID!

```
veq context broadcast op: F$MID
fxname: -FMID
args: (AX BX)
body (1): (* 0.5 (+ AX BX)).
destructive.
```

#### F$MIMA

```
find min and max for all dimensions of 1 array.
ex: (F$MIMA &key n) returns (values xmin xmax ...).
use n to limit to first n rows.

 ; VEQ:F$MIMA
 ;   [symbol]
 ;
 ; F$MIMA names a compiled function:
 ;   Lambda-list: (A0 &KEY (N ($NUM A0)) INDS)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T) (:INDS T))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     find min and max for all dimensions of 1 array.
 ;     ex: (F$MIMA &key n) returns (values xmin xmax ...).
 ;     use n to limit to first n rows.
 ;   Source file: src/array-mima.lisp
```

#### :context: F$MOD

```
veq context broadcast op: F$MOD
fxname: -FMOD
args: (AX S)
body (1): (MOD AX S).
```

#### :context: F$MOD!

```
veq context broadcast op: F$MOD
fxname: -FMOD
args: (AX S)
body (1): (MOD AX S).
destructive.
```

#### :context: F$NEG

```
veq context broadcast op: F$NEG
fxname: -FNEG
args: (AX)
body (1): (- AX).
```

#### :context: F$NEG!

```
veq context broadcast op: F$NEG
fxname: -FNEG
args: (AX)
body (1): (- AX).
destructive.
```

#### :context: F$NORM

```
veq context broadcast op: F$NORM
fxname: -FNORM
args: (AX)
body (1): (MVC #'-FISCALE AX (MVC #'-FLEN AX)).
```

#### :context: F$NORM!

```
veq context broadcast op: F$NORM
fxname: -FNORM
args: (AX)
body (1): (MVC #'-FISCALE AX (MVC #'-FLEN AX)).
destructive.
```

#### F$NUM

```
number of elements in 1d array.
typed.

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
make 1d array of ones.
typed.

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
fx: %F$POINT
macro wrapper: F$POINT
defined via veq:def*

 ; VEQ:F$POINT
 ;   [symbol]
 ;
 ; F$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F$POINT
 ;     macro wrapper: F$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: F$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 1
```

#### :context: F$SCALE

```
veq context broadcast op: F$SCALE
fxname: -FSCALE
args: (AX S)
body (1): (VALUES (* AX S)).
```

#### :context: F$SCALE!

```
veq context broadcast op: F$SCALE
fxname: -FSCALE
args: (AX S)
body (1): (VALUES (* AX S)).
destructive.
```

#### :context: F$SIN-COS

```
veq context broadcast op: F$SIN-COS
fxname: -FSIN-COS
args: (AX)
body (2): (VALUES (SIN AX) (COS AX)).
```

#### :context: F$SQRT

```
veq context broadcast op: F$SQRT
fxname: -FSQRT
args: (AX)
body (1): (THE POS-FF (SQRT (THE POS-FF AX))).
```

#### :context: F$SQRT!

```
veq context broadcast op: F$SQRT
fxname: -FSQRT
args: (AX)
body (1): (THE POS-FF (SQRT (THE POS-FF AX))).
destructive.
```

#### :context: F$SQUARE

```
veq context broadcast op: F$SQUARE
fxname: -FSQUARE
args: (AX)
body (1): (* AX AX).
```

#### :context: F$SQUARE!

```
veq context broadcast op: F$SQUARE
fxname: -FSQUARE
args: (AX)
body (1): (* AX AX).
destructive.
```

#### F$SUM

```
sum all rows of 1d array.

 ; VEQ:F$SUM
 ;   [symbol]
 ;
 ; F$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 1d array.
 ;   Source file: src/array-reduce.lisp
```

#### F$TAKE

```
returns 1d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:F$TAKE
 ;   [symbol]
 ;
 ; F$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 1d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### F$VAL

```
make 1d array of val.
typed.

 ; VEQ:F$VAL
 ;   [symbol]
 ;
 ; F$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: F$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 1d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### F$ZERO

```
make 1d vector array of zeros.
typed.

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

#### :context: F\*

```
veq context op: F*
fxname: -F*
args: (AX BX)
body (1): (* AX BX).
```

#### :context: F+

```
veq context op: F+
fxname: -F+
args: (AX BX)
body (1): (+ AX BX).
```

#### :context: F-

```
veq context op: F-
fxname: -F-
args: (AX BX)
body (1): (- AX BX).
```

#### :context: F/

```
veq context op: F/
fxname: -F/
args: (AX BX)
body (1): (/ AX BX).
```

#### :context: F2

```
strict make 2d vector in veq context.
```

#### F2$

```
returns indices (default 0) from 2d vector array (FVEC) as values.
ex: (F2$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:F2$
 ;   [symbol]
 ;
 ; F2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 2d vector array (FVEC) as values.
 ;     ex: (F2$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: F2$\*

```
veq context broadcast op: F2$*
fxname: -F2*
args: (AX AY BX BY)
body (2): (VALUES (* AX BX) (* AY BY)).
```

#### :context: F2$\*!

```
veq context broadcast op: F2$*
fxname: -F2*
args: (AX AY BX BY)
body (2): (VALUES (* AX BX) (* AY BY)).
destructive.
```

#### :context: F2$+

```
veq context broadcast op: F2$+
fxname: -F2+
args: (AX AY BX BY)
body (2): (VALUES (+ AX BX) (+ AY BY)).
```

#### :context: F2$+!

```
veq context broadcast op: F2$+
fxname: -F2+
args: (AX AY BX BY)
body (2): (VALUES (+ AX BX) (+ AY BY)).
destructive.
```

#### :context: F2$-

```
veq context broadcast op: F2$-
fxname: -F2-
args: (AX AY BX BY)
body (2): (VALUES (- AX BX) (- AY BY)).
```

#### :context: F2$-!

```
veq context broadcast op: F2$-
fxname: -F2-
args: (AX AY BX BY)
body (2): (VALUES (- AX BX) (- AY BY)).
destructive.
```

#### :context: F2$.

```
veq context broadcast op: F2$.
fxname: -F2.
args: (AX AY BX BY)
body (1): (+ (* AX BX) (* AY BY)).
```

#### :context: F2$/

```
veq context broadcast op: F2$/
fxname: -F2/
args: (AX AY BX BY)
body (2): (VALUES (/ AX BX) (/ AY BY)).
```

#### :context: F2$/!

```
veq context broadcast op: F2$/
fxname: -F2/
args: (AX AY BX BY)
body (2): (VALUES (/ AX BX) (/ AY BY)).
destructive.
```

#### :context: F2$^

```
veq context broadcast op: F2$^
fxname: -F2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
```

#### :context: F2$^!

```
veq context broadcast op: F2$^
fxname: -F2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
destructive.
```

#### :context: F2$ABS

```
veq context broadcast op: F2$ABS
fxname: -F2ABS
args: (AX AY)
body (2): (VALUES (ABS AX) (ABS AY)).
```

#### :context: F2$ABS!

```
veq context broadcast op: F2$ABS
fxname: -F2ABS
args: (AX AY)
body (2): (VALUES (ABS AX) (ABS AY)).
destructive.
```

#### :context: F2$ANGLE

```
veq context broadcast op: F2$ANGLE
fxname: -F2ANGLE
args: (AX AY)
body (1): (MVC #'ATAN (-F2NORM AY AX)).
```

#### F2$CENTER

```
DOCSTRING for %F2$CENTER;
center 2d array according to n points in array. n is optional.

 ; VEQ:F2$CENTER
 ;   [symbol]
 ;
 ; F2$CENTER names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2$CENTER;
 ;     center 2d array according to n points in array. n is optional.
 ;   Source file: src/shapes.lisp
```

#### F2$CIRC

```
DOCSTRING for %F2$CIRC;
return circle of size rad. (rs 0.5) is vertex density.

 ; VEQ:F2$CIRC
 ;   [symbol]
 ;
 ; F2$CIRC names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2$CIRC;
 ;     return circle of size rad. (rs 0.5) is vertex density.
 ;   Source file: src/shapes.lisp
```

#### :context: F2$CROSS

```
veq context broadcast op: F2$CROSS
fxname: -F2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
```

#### :context: F2$CROSS!

```
veq context broadcast op: F2$CROSS
fxname: -F2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
destructive.
```

#### :context: F2$DST

```
veq context broadcast op: F2$DST
fxname: -F2DST
args: (AX AY BX BY)
body (1): (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY))))).
```

#### :context: F2$DST2

```
veq context broadcast op: F2$DST2
fxname: -F2DST2
args: (AX AY BX BY)
body (1): (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY))).
```

#### :context: F2$EXP

```
veq context broadcast op: F2$EXP
fxname: -F2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
```

#### :context: F2$EXP!

```
veq context broadcast op: F2$EXP
fxname: -F2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
destructive.
```

#### :context: F2$FLIP

```
veq context broadcast op: F2$FLIP
fxname: -F2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
```

#### :context: F2$FLIP!

```
veq context broadcast op: F2$FLIP
fxname: -F2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
destructive.
```

#### :context: F2$FROM

```
veq context broadcast op: F2$FROM
fxname: -F2FROM
args: (AX AY BX BY S)
body (2): (-F2+ AX AY (* BX S) (* BY S)).
```

#### :context: F2$FROM!

```
veq context broadcast op: F2$FROM
fxname: -F2FROM
args: (AX AY BX BY S)
body (2): (-F2+ AX AY (* BX S) (* BY S)).
destructive.
```

#### :context: F2$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

#### :context: F2$I-

```
veq context broadcast op: F2$I-
fxname: -F2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
```

#### :context: F2$I-!

```
veq context broadcast op: F2$I-
fxname: -F2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
destructive.
```

#### :context: F2$I/

```
veq context broadcast op: F2$I/
fxname: -F2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
```

#### :context: F2$I/!

```
veq context broadcast op: F2$I/
fxname: -F2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
destructive.
```

#### :context: F2$ISCALE

```
veq context broadcast op: F2$ISCALE
fxname: -F2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
```

#### :context: F2$ISCALE!

```
veq context broadcast op: F2$ISCALE
fxname: -F2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
destructive.
```

#### F2$LAST

```
return values from last row of 2d vector array.

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

#### :context: F2$LEN

```
veq context broadcast op: F2$LEN
fxname: -F2LEN
args: (AX AY)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE AX AY))))).
```

#### :context: F2$LEN2

```
veq context broadcast op: F2$LEN2
fxname: -F2LEN2
args: (AX AY)
body (1): (THE POS-FF (MVC #'+ (-F2SQUARE AX AY))).
```

#### :context: F2$LERP

```
veq context broadcast op: F2$LERP
fxname: -F2LERP
args: (AX AY BX BY S)
body (2): (-F2+ AX AY (* (- BX AX) S) (* (- BY AY) S)).
```

#### :context: F2$LERP!

```
veq context broadcast op: F2$LERP
fxname: -F2LERP
args: (AX AY BX BY S)
body (2): (-F2+ AX AY (* (- BX AX) S) (* (- BY AY) S)).
destructive.
```

#### F2$LINE

```
fx: %F2$LINE
macro wrapper: F2$LINE
defined via veq:def*

 ; VEQ:F2$LINE
 ;   [symbol]
 ;
 ; F2$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2$LINE
 ;     macro wrapper: F2$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### F2$LSPACE

```
fx: %F2$LSPACE
macro wrapper: F2$LSPACE
defined via veq:fvdef*

 ; VEQ:F2$LSPACE
 ;   [symbol]
 ;
 ; F2$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2$LSPACE
 ;     macro wrapper: F2$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### :context: F2$MAX

```
veq context broadcast op: F2$MAX
fxname: -F2MAX
args: (AX AY)
body (1): (MAX AX AY).
```

#### :context: F2$MID

```
veq context broadcast op: F2$MID
fxname: -F2MID
args: (AX AY BX BY)
body (2): (VALUES (* 0.5 (+ AX BX)) (* 0.5 (+ AY BY))).
```

#### :context: F2$MID!

```
veq context broadcast op: F2$MID
fxname: -F2MID
args: (AX AY BX BY)
body (2): (VALUES (* 0.5 (+ AX BX)) (* 0.5 (+ AY BY))).
destructive.
```

#### F2$MIMA

```
find min and max for all dimensions of 2 array.
ex: (F2$MIMA &key n) returns (values xmin xmax ...).
use n to limit to first n rows.

 ; VEQ:F2$MIMA
 ;   [symbol]
 ;
 ; F2$MIMA names a compiled function:
 ;   Lambda-list: (A0 &KEY (N (2$NUM A0)) INDS)
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

#### :context: F2$MIN

```
veq context broadcast op: F2$MIN
fxname: -F2MIN
args: (AX AY)
body (1): (MIN AX AY).
```

#### :context: F2$MOD

```
veq context broadcast op: F2$MOD
fxname: -F2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
```

#### :context: F2$MOD!

```
veq context broadcast op: F2$MOD
fxname: -F2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
destructive.
```

#### :context: F2$NEG

```
veq context broadcast op: F2$NEG
fxname: -F2NEG
args: (AX AY)
body (2): (VALUES (- AX) (- AY)).
```

#### :context: F2$NEG!

```
veq context broadcast op: F2$NEG
fxname: -F2NEG
args: (AX AY)
body (2): (VALUES (- AX) (- AY)).
destructive.
```

#### :context: F2$NORM

```
veq context broadcast op: F2$NORM
fxname: -F2NORM
args: (AX AY)
body (2): (MVC #'-F2ISCALE AX AY (MVC #'-F2LEN AX AY)).
```

#### :context: F2$NORM!

```
veq context broadcast op: F2$NORM
fxname: -F2NORM
args: (AX AY)
body (2): (MVC #'-F2ISCALE AX AY (MVC #'-F2LEN AX AY)).
destructive.
```

#### F2$NUM

```
number of elements in 2d array.
typed.

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

#### :context: F2$ON-CIRC

```
veq context broadcast op: F2$ON-CIRC
fxname: -F2ON-CIRC
args: (AX RAD)
body (2): (MVC #'-F2SCALE (-FCOS-SIN (* AX FPII)) RAD).
```

#### :context: F2$ON-CIRC\*

```
veq context broadcast op: F2$ON-CIRC*
fxname: -F2ON-CIRC*
args: (AX RAD)
body (2): (MVC #'-F2SCALE (-FCOS-SIN AX) RAD).
```

#### F2$ONE

```
make 2d array of ones.
typed.

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

#### :context: F2$PERP

```
veq context broadcast op: F2$PERP
fxname: -F2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
```

#### :context: F2$PERP!

```
veq context broadcast op: F2$PERP
fxname: -F2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
destructive.
```

#### :context: F2$PERP\*

```
veq context broadcast op: F2$PERP*
fxname: -F2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
```

#### :context: F2$PERP\*!

```
veq context broadcast op: F2$PERP*
fxname: -F2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
destructive.
```

#### F2$POINT

```
fx: %F2$POINT
macro wrapper: F2$POINT
defined via veq:def*

 ; VEQ:F2$POINT
 ;   [symbol]
 ;
 ; F2$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2$POINT
 ;     macro wrapper: F2$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### F2$POLYGON

```
DOCSTRING for %F2$POLYGON;
return n-polygon of size rad. rotate by (rot 0)

 ; VEQ:F2$POLYGON
 ;   [symbol]
 ;
 ; F2$POLYGON names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2$POLYGON;
 ;     return n-polygon of size rad. rotate by (rot 0)
 ;   Source file: src/shapes.lisp
```

#### F2$RECT

```
fx: %F2$RECT
macro wrapper: F2$RECT
defined via veq:def*

 ; VEQ:F2$RECT
 ;   [symbol]
 ;
 ; F2$RECT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2$RECT
 ;     macro wrapper: F2$RECT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: F2$ROT

```
veq context broadcast op: F2$ROT
fxname: -F2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE
             (FF
               COSA
               SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
```

#### :context: F2$ROT!

```
veq context broadcast op: F2$ROT
fxname: -F2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE
             (FF
               COSA
               SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
destructive.
```

#### :context: F2$ROTS

```
veq context broadcast op: F2$ROTS
fxname: -F2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVC #'-F2+ (MVC #'-F2ROT (-F2- AX AY SX SY) ANGLE) SX SY).
```

#### :context: F2$ROTS!

```
veq context broadcast op: F2$ROTS
fxname: -F2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVC #'-F2+ (MVC #'-F2ROT (-F2- AX AY SX SY) ANGLE) SX SY).
destructive.
```

#### :context: F2$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F2$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 2
```

#### :context: F2$SCALE

```
veq context broadcast op: F2$SCALE
fxname: -F2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
```

#### :context: F2$SCALE!

```
veq context broadcast op: F2$SCALE
fxname: -F2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
destructive.
```

#### :context: F2$SQRT

```
veq context broadcast op: F2$SQRT
fxname: -F2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))).
```

#### :context: F2$SQRT!

```
veq context broadcast op: F2$SQRT
fxname: -F2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))).
destructive.
```

#### :context: F2$SQUARE

```
veq context broadcast op: F2$SQUARE
fxname: -F2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
```

#### :context: F2$SQUARE!

```
veq context broadcast op: F2$SQUARE
fxname: -F2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
destructive.
```

#### F2$SUM

```
sum all rows of 2d array.

 ; VEQ:F2$SUM
 ;   [symbol]
 ;
 ; F2$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 2d array.
 ;   Source file: src/array-reduce.lisp
```

#### F2$TAKE

```
returns 2d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:F2$TAKE
 ;   [symbol]
 ;
 ; F2$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 2d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### F2$VAL

```
make 2d array of val.
typed.

 ; VEQ:F2$VAL
 ;   [symbol]
 ;
 ; F2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: F2$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 2d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### F2$ZERO

```
make 2d vector array of zeros.
typed.

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

#### :context: F2\*

```
veq context op: F2*
fxname: -F2*
args: (AX AY BX BY)
body (2): (VALUES (* AX BX) (* AY BY)).
```

#### :context: F2+

```
veq context op: F2+
fxname: -F2+
args: (AX AY BX BY)
body (2): (VALUES (+ AX BX) (+ AY BY)).
```

#### :context: F2-

```
veq context op: F2-
fxname: -F2-
args: (AX AY BX BY)
body (2): (VALUES (- AX BX) (- AY BY)).
```

#### :context: F2.

```
veq context op: F2.
fxname: -F2.
args: (AX AY BX BY)
body (1): (+ (* AX BX) (* AY BY)).
```

#### :context: F2/

```
veq context op: F2/
fxname: -F2/
args: (AX AY BX BY)
body (2): (VALUES (/ AX BX) (/ AY BY)).
```

#### :context: F2^

```
veq context op: F2^
fxname: -F2^
args: (AX AY S)
body (2): (VALUES (EXPT AX S) (EXPT AY S)).
```

#### :context: F2ABS

```
veq context op: F2ABS
fxname: -F2ABS
args: (AX AY)
body (2): (VALUES (ABS AX) (ABS AY)).
```

#### :context: F2ANGLE

```
veq context op: F2ANGLE
fxname: -F2ANGLE
args: (AX AY)
body (1): (MVC #'ATAN (-F2NORM AY AX)).
```

#### :context: F2CROSS

```
veq context op: F2CROSS
fxname: -F2CROSS
args: (AX AY BX BY)
body (2): (- (* AX BY) (* AY BX)).
```

#### :context: F2DST

```
veq context op: F2DST
fxname: -F2DST
args: (AX AY BX BY)
body (1): (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY))))).
```

#### :context: F2DST2

```
veq context op: F2DST2
fxname: -F2DST2
args: (AX AY BX BY)
body (1): (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY))).
```

#### :context: F2EXP

```
veq context op: F2EXP
fxname: -F2EXP
args: (AX AY)
body (2): (VALUES (EXP AX) (EXP AY)).
```

#### :context: F2FLIP

```
veq context op: F2FLIP
fxname: -F2FLIP
args: (AX AY)
body (2): (VALUES AY AX).
```

#### :context: F2FROM

```
veq context op: F2FROM
fxname: -F2FROM
args: (AX AY BX BY S)
body (2): (-F2+ AX AY (* BX S) (* BY S)).
```

#### :context: F2I-

```
veq context op: F2I-
fxname: -F2I-
args: (AX AY BX BY)
body (2): (VALUES (- BX AX) (- BY AY)).
```

#### :context: F2I/

```
veq context op: F2I/
fxname: -F2I/
args: (AX AY BX BY)
body (2): (VALUES (/ BX AX) (/ BY AY)).
```

#### F2IN-BBOX

```
fx: %F2IN-BBOX
macro wrapper: F2IN-BBOX
defined via veq:fvdef*

 ; VEQ:F2IN-BBOX
 ;   [symbol]
 ;
 ; F2IN-BBOX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2IN-BBOX
 ;     macro wrapper: F2IN-BBOX
 ;     defined via veq:fvdef*
 ;   Source file: src/checks.lisp
```

#### F2IN-CONCAVE

```
fx: %F2IN-CONCAVE
macro wrapper: F2IN-CONCAVE
defined via veq:fvdef*

 ; VEQ:F2IN-CONCAVE
 ;   [symbol]
 ;
 ; F2IN-CONCAVE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2IN-CONCAVE
 ;     macro wrapper: F2IN-CONCAVE
 ;     defined via veq:fvdef*
 ;   Source file: src/checks.lisp
```

#### F2IN-TRIANGLE

```
fx: %F2IN-TRIANGLE
macro wrapper: F2IN-TRIANGLE
defined via veq:fvdef*

 ; VEQ:F2IN-TRIANGLE
 ;   [symbol]
 ;
 ; F2IN-TRIANGLE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2IN-TRIANGLE
 ;     macro wrapper: F2IN-TRIANGLE
 ;     defined via veq:fvdef*
 ;   Source file: src/checks.lisp
```

#### :context: F2ISCALE

```
veq context op: F2ISCALE
fxname: -F2ISCALE
args: (AX AY S)
body (2): (VALUES (/ AX S) (/ AY S)).
```

#### :context: F2LEN

```
veq context op: F2LEN
fxname: -F2LEN
args: (AX AY)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE AX AY))))).
```

#### :context: F2LEN2

```
veq context op: F2LEN2
fxname: -F2LEN2
args: (AX AY)
body (1): (THE POS-FF (MVC #'+ (-F2SQUARE AX AY))).
```

#### :context: F2LERP

```
veq context op: F2LERP
fxname: -F2LERP
args: (AX AY BX BY S)
body (2): (-F2+ AX AY (* (- BX AX) S) (* (- BY AY) S)).
```

#### :context: F2LET

```
make 2d let.
ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*.
```

#### F2LSEGX

```
DOCSTRING for %F2LSEGX;
lines = #( #(ax ay bx by) ... )

not entirely slow line-line intersection for all lines. this is faster than
comparing all lines when lines are short relative to the area that the lines
cover. it can be improved further by using binary search tree to store
current state.

 ; VEQ:F2LSEGX
 ;   [symbol]
 ;
 ; F2LSEGX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2LSEGX;
 ;     lines = #( #(ax ay bx by) ... )
 ;
 ;     not entirely slow line-line intersection for all lines. this is faster than
 ;     comparing all lines when lines are short relative to the area that the lines
 ;     cover. it can be improved further by using binary search tree to store
 ;     current state.
 ;   Source file: src/checks.lisp
```

#### :context: F2MAX

```
veq context op: F2MAX
fxname: -F2MAX
args: (AX AY)
body (1): (MAX AX AY).
```

#### F2MEYE

```
return 2d eye matrix.

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

#### :context: F2MID

```
veq context op: F2MID
fxname: -F2MID
args: (AX AY BX BY)
body (2): (VALUES (* 0.5 (+ AX BX)) (* 0.5 (+ AY BY))).
```

#### :context: F2MIN

```
veq context op: F2MIN
fxname: -F2MIN
args: (AX AY)
body (1): (MIN AX AY).
```

#### F2MINV

```
invert 2x2 matrix. non-destructive.

 ; VEQ:F2MINV
 ;   [symbol]
 ;
 ; F2MINV names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     invert 2x2 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### F2MM

```
multiply mat * mat
of type: FVEC

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
multiply mat * (transpose mat)
of type: FVEC

 ; VEQ:F2MMT
 ;   [symbol]
 ;
 ; F2MMT names a macro:
 ;   Lambda-list: (A*59 B*61)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### :context: F2MOD

```
veq context op: F2MOD
fxname: -F2MOD
args: (AX AY S)
body (2): (VALUES (MOD AX S) (MOD AY S)).
```

#### F2MROT

```
DOCSTRING for %F2MROT;
make 2d rotation matrix for rotating a rads

 ; VEQ:F2MROT
 ;   [symbol]
 ;
 ; F2MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2MROT;
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: src/mat.lisp
```

#### F2MROT\*

```
DOCSTRING for %F2MROT*;
make 2d rotation matrix for rotating a rads

 ; VEQ:F2MROT*
 ;   [symbol]
 ;
 ; F2MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2MROT*;
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: src/mat.lisp
```

#### F2MSCALE

```
DOCSTRING for %F2MSCALE;
make 2d matrix for scaling by x

 ; VEQ:F2MSCALE
 ;   [symbol]
 ;
 ; F2MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2MSCALE;
 ;     make 2d matrix for scaling by x
 ;   Source file: src/mat.lisp
```

#### F2MT!

```
transpose 2d matrix in-place.

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
multiply (transpose mat) * mat
of type: FVEC

 ; VEQ:F2MTM
 ;   [symbol]
 ;
 ; F2MTM names a macro:
 ;   Lambda-list: (A*88 B*90)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F2MTMT

```
multiply (transpose mat) * (transpose mat)
of type: FVEC

 ; VEQ:F2MTMT
 ;   [symbol]
 ;
 ; F2MTMT names a macro:
 ;   Lambda-list: (A*30 B*32)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F2MTRANS

```
DOCSTRING for %F2MTRANS;
make 2d transpose matrix for moving by x

 ; VEQ:F2MTRANS
 ;   [symbol]
 ;
 ; F2MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2MTRANS;
 ;     make 2d transpose matrix for moving by x
 ;   Source file: src/mat.lisp
```

#### F2MTV

```
transpose(mat) * v. for 2d matrix and vector.

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
mat * v. for 2d matrix and vector.

 ; VEQ:F2MV
 ;   [symbol]
 ;
 ; F2MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 2d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :context: F2NEG

```
veq context op: F2NEG
fxname: -F2NEG
args: (AX AY)
body (2): (VALUES (- AX) (- AY)).
```

#### :context: F2NORM

```
veq context op: F2NORM
fxname: -F2NORM
args: (AX AY)
body (2): (MVC #'-F2ISCALE AX AY (MVC #'-F2LEN AX AY)).
```

#### :context: F2NSUM

```
make 2d
```

#### :context: F2ON-CIRC

```
veq context op: F2ON-CIRC
fxname: -F2ON-CIRC
args: (AX RAD)
body (2): (MVC #'-F2SCALE (-FCOS-SIN (* AX FPII)) RAD).
```

#### :context: F2ON-CIRC\*

```
veq context op: F2ON-CIRC*
fxname: -F2ON-CIRC*
args: (AX RAD)
body (2): (MVC #'-F2SCALE (-FCOS-SIN AX) RAD).
```

#### :context: F2PERP

```
veq context op: F2PERP
fxname: -F2PERP
args: (AX AY)
body (2): (VALUES AY (- AX)).
```

#### :context: F2PERP\*

```
veq context op: F2PERP*
fxname: -F2PERP*
args: (AX AY)
body (2): (VALUES (- AY) AX).
```

#### :context: F2REP

```
repeat argument 2d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: F2REP\*

```
repeat the evaluated argument 2 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### :context: F2ROT

```
veq context op: F2ROT
fxname: -F2ROT
args: (AX AY ANGLE)
body (2): (LET ((COSA (COS ANGLE)) (SINA (SIN ANGLE)))
            (DECLARE
             (FF
               COSA
               SINA))
            (VALUES (- (* AX COSA) (* AY SINA)) (+ (* AX SINA) (* AY COSA)))).
```

#### :context: F2ROTS

```
veq context op: F2ROTS
fxname: -F2ROTS
args: (AX AY ANGLE SX SY)
body (2): (MVC #'-F2+ (MVC #'-F2ROT (-F2- AX AY SX SY) ANGLE) SX SY).
```

#### :context: F2SCALE

```
veq context op: F2SCALE
fxname: -F2SCALE
args: (AX AY S)
body (2): (VALUES (* AX S) (* AY S)).
```

#### F2SEGDST

```
fx: %F2SEGDST
macro wrapper: F2SEGDST
defined via veq:fvdef*

 ; VEQ:F2SEGDST
 ;   [symbol]
 ;
 ; F2SEGDST names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2SEGDST
 ;     macro wrapper: F2SEGDST
 ;     defined via veq:fvdef*
 ;   Source file: src/checks.lisp
```

#### F2SEGX

```
DOCSTRING for %F2SEGX;
find intersection between lines (a1 a2), (b1 b2).
returns isect? p q where p and q is the distance along each line to the
intersection point

 ; VEQ:F2SEGX
 ;   [symbol]
 ;
 ; F2SEGX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F2SEGX;
 ;     find intersection between lines (a1 a2), (b1 b2).
 ;     returns isect? p q where p and q is the distance along each line to the
 ;     intersection point
 ;   Source file: src/checks.lisp
```

#### :context: F2SQRT

```
veq context op: F2SQRT
fxname: -F2SQRT
args: (AX AY)
body (2): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))).
```

#### :context: F2SQUARE

```
veq context op: F2SQUARE
fxname: -F2SQUARE
args: (AX AY)
body (2): (VALUES (* AX AX) (* AY AY)).
```

#### :context: F2VSET

```
set 2d value.
ex: (F2VSET (a) (fx ...))
where (fx ...) returns 2 values.
```

#### :context: F2~

```
make 2d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :context: F3

```
strict make 3d vector in veq context.
```

#### F3$

```
returns indices (default 0) from 3d vector array (FVEC) as values.
ex: (F3$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:F3$
 ;   [symbol]
 ;
 ; F3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 3d vector array (FVEC) as values.
 ;     ex: (F3$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: F3$\*

```
veq context broadcast op: F3$*
fxname: -F3*
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: F3$\*!

```
veq context broadcast op: F3$*
fxname: -F3*
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* AX BX) (* AY BY) (* AZ BZ)).
destructive.
```

#### :context: F3$+

```
veq context broadcast op: F3$+
fxname: -F3+
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ)).
```

#### :context: F3$+!

```
veq context broadcast op: F3$+
fxname: -F3+
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ)).
destructive.
```

#### :context: F3$-

```
veq context broadcast op: F3$-
fxname: -F3-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- AX BX) (- AY BY) (- AZ BZ)).
```

#### :context: F3$-!

```
veq context broadcast op: F3$-
fxname: -F3-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- AX BX) (- AY BY) (- AZ BZ)).
destructive.
```

#### :context: F3$.

```
veq context broadcast op: F3$.
fxname: -F3.
args: (AX AY AZ BX BY BZ)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: F3$/

```
veq context broadcast op: F3$/
fxname: -F3/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ)).
```

#### :context: F3$/!

```
veq context broadcast op: F3$/
fxname: -F3/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ)).
destructive.
```

#### :context: F3$^

```
veq context broadcast op: F3$^
fxname: -F3^
args: (AX AY AZ S)
body (3): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S)).
```

#### :context: F3$^!

```
veq context broadcast op: F3$^
fxname: -F3^
args: (AX AY AZ S)
body (3): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S)).
destructive.
```

#### :context: F3$ABS

```
veq context broadcast op: F3$ABS
fxname: -F3ABS
args: (AX AY AZ)
body (3): (VALUES (ABS AX) (ABS AY) (ABS AZ)).
```

#### :context: F3$ABS!

```
veq context broadcast op: F3$ABS
fxname: -F3ABS
args: (AX AY AZ)
body (3): (VALUES (ABS AX) (ABS AY) (ABS AZ)).
destructive.
```

#### :context: F3$CROSS

```
veq context broadcast op: F3$CROSS
fxname: -F3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
```

#### :context: F3$CROSS!

```
veq context broadcast op: F3$CROSS
fxname: -F3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
destructive.
```

#### :context: F3$DST

```
veq context broadcast op: F3$DST
fxname: -F3DST
args: (AX AY AZ BX BY BZ)
body (1): (SQRT
           (THE POS-FF (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ))))).
```

#### :context: F3$DST2

```
veq context broadcast op: F3$DST2
fxname: -F3DST2
args: (AX AY AZ BX BY BZ)
body (1): (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ))).
```

#### :context: F3$EXP

```
veq context broadcast op: F3$EXP
fxname: -F3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
```

#### :context: F3$EXP!

```
veq context broadcast op: F3$EXP
fxname: -F3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
destructive.
```

#### :context: F3$FROM

```
veq context broadcast op: F3$FROM
fxname: -F3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (-F3+ AX AY AZ (* BX S) (* BY S) (* BZ S)).
```

#### :context: F3$FROM!

```
veq context broadcast op: F3$FROM
fxname: -F3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (-F3+ AX AY AZ (* BX S) (* BY S) (* BZ S)).
destructive.
```

#### :context: F3$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

#### :context: F3$I-

```
veq context broadcast op: F3$I-
fxname: -F3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
```

#### :context: F3$I-!

```
veq context broadcast op: F3$I-
fxname: -F3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
destructive.
```

#### :context: F3$I/

```
veq context broadcast op: F3$I/
fxname: -F3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
```

#### :context: F3$I/!

```
veq context broadcast op: F3$I/
fxname: -F3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
destructive.
```

#### :context: F3$ISCALE

```
veq context broadcast op: F3$ISCALE
fxname: -F3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
```

#### :context: F3$ISCALE!

```
veq context broadcast op: F3$ISCALE
fxname: -F3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
destructive.
```

#### F3$LAST

```
return values from last row of 3d vector array.

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

#### :context: F3$LEN

```
veq context broadcast op: F3$LEN
fxname: -F3LEN
args: (AX AY AZ)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F3SQUARE AX AY AZ))))).
```

#### :context: F3$LEN2

```
veq context broadcast op: F3$LEN2
fxname: -F3LEN2
args: (AX AY AZ)
body (1): (THE POS-FF (MVC #'+ (-F3SQUARE AX AY AZ))).
```

#### :context: F3$LERP

```
veq context broadcast op: F3$LERP
fxname: -F3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (-F3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)).
```

#### :context: F3$LERP!

```
veq context broadcast op: F3$LERP
fxname: -F3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (-F3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)).
destructive.
```

#### F3$LINE

```
fx: %F3$LINE
macro wrapper: F3$LINE
defined via veq:def*

 ; VEQ:F3$LINE
 ;   [symbol]
 ;
 ; F3$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F3$LINE
 ;     macro wrapper: F3$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### F3$LSPACE

```
fx: %F3$LSPACE
macro wrapper: F3$LSPACE
defined via veq:fvdef*

 ; VEQ:F3$LSPACE
 ;   [symbol]
 ;
 ; F3$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F3$LSPACE
 ;     macro wrapper: F3$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### :context: F3$MAX

```
veq context broadcast op: F3$MAX
fxname: -F3MAX
args: (AX AY AZ)
body (1): (MAX AX AY AZ).
```

#### :context: F3$MID

```
veq context broadcast op: F3$MID
fxname: -F3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5)).
```

#### :context: F3$MID!

```
veq context broadcast op: F3$MID
fxname: -F3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5)).
destructive.
```

#### F3$MIMA

```
find min and max for all dimensions of 3 array.
ex: (F3$MIMA &key n) returns (values xmin xmax ...).
use n to limit to first n rows.

 ; VEQ:F3$MIMA
 ;   [symbol]
 ;
 ; F3$MIMA names a compiled function:
 ;   Lambda-list: (A0 &KEY (N (3$NUM A0)) INDS)
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

#### :context: F3$MIN

```
veq context broadcast op: F3$MIN
fxname: -F3MIN
args: (AX AY AZ)
body (1): (MIN AX AY AZ).
```

#### :context: F3$MOD

```
veq context broadcast op: F3$MOD
fxname: -F3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
```

#### :context: F3$MOD!

```
veq context broadcast op: F3$MOD
fxname: -F3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
destructive.
```

#### :context: F3$NEG

```
veq context broadcast op: F3$NEG
fxname: -F3NEG
args: (AX AY AZ)
body (3): (VALUES (- AX) (- AY) (- AZ)).
```

#### :context: F3$NEG!

```
veq context broadcast op: F3$NEG
fxname: -F3NEG
args: (AX AY AZ)
body (3): (VALUES (- AX) (- AY) (- AZ)).
destructive.
```

#### :context: F3$NORM

```
veq context broadcast op: F3$NORM
fxname: -F3NORM
args: (AX AY AZ)
body (3): (MVC #'-F3ISCALE AX AY AZ (THE POS-FF (MVC #'-F3LEN AX AY AZ))).
```

#### :context: F3$NORM!

```
veq context broadcast op: F3$NORM
fxname: -F3NORM
args: (AX AY AZ)
body (3): (MVC #'-F3ISCALE AX AY AZ (THE POS-FF (MVC #'-F3LEN AX AY AZ))).
destructive.
```

#### F3$NUM

```
number of elements in 3d array.
typed.

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
make 3d array of ones.
typed.

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
fx: %F3$POINT
macro wrapper: F3$POINT
defined via veq:def*

 ; VEQ:F3$POINT
 ;   [symbol]
 ;
 ; F3$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F3$POINT
 ;     macro wrapper: F3$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: F3$ROT

```
veq context broadcast op: F3$ROT
fxname: -F3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE
             (FF
               COSA))
            (MVC #'-F3FROM
                 (MVC #'-F3FROM (-F3SCALE AX AY AZ COSA)
                      (-F3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-F3. NX NY NZ AX AY AZ) (- 1.0 COSA)))).
```

#### :context: F3$ROT!

```
veq context broadcast op: F3$ROT
fxname: -F3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE
             (FF
               COSA))
            (MVC #'-F3FROM
                 (MVC #'-F3FROM (-F3SCALE AX AY AZ COSA)
                      (-F3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-F3. NX NY NZ AX AY AZ) (- 1.0 COSA)))).
destructive.
```

#### :context: F3$ROTS

```
veq context broadcast op: F3$ROTS
fxname: -F3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVC #'-F3+ (MVC #'-F3ROT (-F3- AX AY AZ SX SY SZ) NX NY NZ A) SX SY
               SZ).
```

#### :context: F3$ROTS!

```
veq context broadcast op: F3$ROTS
fxname: -F3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVC #'-F3+ (MVC #'-F3ROT (-F3- AX AY AZ SX SY SZ) NX NY NZ A) SX SY
               SZ).
destructive.
```

#### :context: F3$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F3$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 3
```

#### :context: F3$SCALE

```
veq context broadcast op: F3$SCALE
fxname: -F3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
```

#### :context: F3$SCALE!

```
veq context broadcast op: F3$SCALE
fxname: -F3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
destructive.
```

#### :context: F3$SQRT

```
veq context broadcast op: F3$SQRT
fxname: -F3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))).
```

#### :context: F3$SQRT!

```
veq context broadcast op: F3$SQRT
fxname: -F3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))).
destructive.
```

#### :context: F3$SQUARE

```
veq context broadcast op: F3$SQUARE
fxname: -F3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ))).
```

#### :context: F3$SQUARE!

```
veq context broadcast op: F3$SQUARE
fxname: -F3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ))).
destructive.
```

#### F3$SUM

```
sum all rows of 3d array.

 ; VEQ:F3$SUM
 ;   [symbol]
 ;
 ; F3$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 3d array.
 ;   Source file: src/array-reduce.lisp
```

#### F3$TAKE

```
returns 3d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:F3$TAKE
 ;   [symbol]
 ;
 ; F3$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 3d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### F3$VAL

```
make 3d array of val.
typed.

 ; VEQ:F3$VAL
 ;   [symbol]
 ;
 ; F3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: F3$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 3d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### F3$ZERO

```
make 3d vector array of zeros.
typed.

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

#### :context: F3\*

```
veq context op: F3*
fxname: -F3*
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: F3+

```
veq context op: F3+
fxname: -F3+
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ)).
```

#### :context: F3-

```
veq context op: F3-
fxname: -F3-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- AX BX) (- AY BY) (- AZ BZ)).
```

#### :context: F3.

```
veq context op: F3.
fxname: -F3.
args: (AX AY AZ BX BY BZ)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ)).
```

#### :context: F3/

```
veq context op: F3/
fxname: -F3/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ)).
```

#### :context: F3^

```
veq context op: F3^
fxname: -F3^
args: (AX AY AZ S)
body (3): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S)).
```

#### :context: F3ABS

```
veq context op: F3ABS
fxname: -F3ABS
args: (AX AY AZ)
body (3): (VALUES (ABS AX) (ABS AY) (ABS AZ)).
```

#### :context: F3CROSS

```
veq context op: F3CROSS
fxname: -F3CROSS
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
                  (- (* AX BY) (* AY BX))).
```

#### :context: F3DST

```
veq context op: F3DST
fxname: -F3DST
args: (AX AY AZ BX BY BZ)
body (1): (SQRT
           (THE POS-FF (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ))))).
```

#### :context: F3DST2

```
veq context op: F3DST2
fxname: -F3DST2
args: (AX AY AZ BX BY BZ)
body (1): (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ))).
```

#### :context: F3EXP

```
veq context op: F3EXP
fxname: -F3EXP
args: (AX AY AZ)
body (3): (VALUES (EXP AX) (EXP AY) (EXP AZ)).
```

#### :context: F3FROM

```
veq context op: F3FROM
fxname: -F3FROM
args: (AX AY AZ BX BY BZ S)
body (3): (-F3+ AX AY AZ (* BX S) (* BY S) (* BZ S)).
```

#### :context: F3I-

```
veq context op: F3I-
fxname: -F3I-
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (- BX AX) (- BY AY) (- BZ AZ)).
```

#### :context: F3I/

```
veq context op: F3I/
fxname: -F3I/
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ)).
```

#### :context: F3ISCALE

```
veq context op: F3ISCALE
fxname: -F3ISCALE
args: (AX AY AZ S)
body (3): (VALUES (/ AX S) (/ AY S) (/ AZ S)).
```

#### :context: F3LEN

```
veq context op: F3LEN
fxname: -F3LEN
args: (AX AY AZ)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F3SQUARE AX AY AZ))))).
```

#### :context: F3LEN2

```
veq context op: F3LEN2
fxname: -F3LEN2
args: (AX AY AZ)
body (1): (THE POS-FF (MVC #'+ (-F3SQUARE AX AY AZ))).
```

#### :context: F3LERP

```
veq context op: F3LERP
fxname: -F3LERP
args: (AX AY AZ BX BY BZ S)
body (3): (-F3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)).
```

#### :context: F3LET

```
make 3d let.
ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*.
```

#### :context: F3MAX

```
veq context op: F3MAX
fxname: -F3MAX
args: (AX AY AZ)
body (1): (MAX AX AY AZ).
```

#### F3MEYE

```
return 3d eye matrix.

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

#### :context: F3MID

```
veq context op: F3MID
fxname: -F3MID
args: (AX AY AZ BX BY BZ)
body (3): (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5)).
```

#### :context: F3MIN

```
veq context op: F3MIN
fxname: -F3MIN
args: (AX AY AZ)
body (1): (MIN AX AY AZ).
```

#### F3MINV

```
invert 3x3 matrix. non-destructive.

 ; VEQ:F3MINV
 ;   [symbol]
 ;
 ; F3MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     invert 3x3 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### F3MM

```
multiply mat * mat
of type: FVEC

 ; VEQ:F3MM
 ;   [symbol]
 ;
 ; F3MM names a macro:
 ;   Lambda-list: (A*117 B*119)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F3MMT

```
multiply mat * (transpose mat)
of type: FVEC

 ; VEQ:F3MMT
 ;   [symbol]
 ;
 ; F3MMT names a macro:
 ;   Lambda-list: (A*175 B*177)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### :context: F3MOD

```
veq context op: F3MOD
fxname: -F3MOD
args: (AX AY AZ S)
body (3): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S)).
```

#### F3MROT

```
DOCSTRING for %F3MROT;
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:F3MROT
 ;   [symbol]
 ;
 ; F3MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F3MROT;
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: src/mat.lisp
```

#### F3MROT\*

```
DOCSTRING for %F3MROT*;
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:F3MROT*
 ;   [symbol]
 ;
 ; F3MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F3MROT*;
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: src/mat.lisp
```

#### F3MSCALE

```
DOCSTRING for %F3MSCALE;
make 3d matrix for scaling by x

 ; VEQ:F3MSCALE
 ;   [symbol]
 ;
 ; F3MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F3MSCALE;
 ;     make 3d matrix for scaling by x
 ;   Source file: src/mat.lisp
```

#### F3MT!

```
transpose 3d matrix in-place.

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
multiply (transpose mat) * mat
of type: FVEC

 ; VEQ:F3MTM
 ;   [symbol]
 ;
 ; F3MTM names a macro:
 ;   Lambda-list: (A*204 B*206)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F3MTMT

```
multiply (transpose mat) * (transpose mat)
of type: FVEC

 ; VEQ:F3MTMT
 ;   [symbol]
 ;
 ; F3MTMT names a macro:
 ;   Lambda-list: (A*146 B*148)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F3MTRANS

```
DOCSTRING for %F3MTRANS;
make 3d transpose matrix for moving by x

 ; VEQ:F3MTRANS
 ;   [symbol]
 ;
 ; F3MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F3MTRANS;
 ;     make 3d transpose matrix for moving by x
 ;   Source file: src/mat.lisp
```

#### F3MTV

```
transpose(mat) * v. for 3d matrix and vector.

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
mat * v. for 3d matrix and vector.

 ; VEQ:F3MV
 ;   [symbol]
 ;
 ; F3MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 3d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :context: F3NEG

```
veq context op: F3NEG
fxname: -F3NEG
args: (AX AY AZ)
body (3): (VALUES (- AX) (- AY) (- AZ)).
```

#### :context: F3NORM

```
veq context op: F3NORM
fxname: -F3NORM
args: (AX AY AZ)
body (3): (MVC #'-F3ISCALE AX AY AZ (THE POS-FF (MVC #'-F3LEN AX AY AZ))).
```

#### :context: F3NSUM

```
make 3d
```

#### F3PLANEX

```
DOCSTRING for %F3PLANEX;
intersection of plane (n:normal, p:point) and line (a b)

 ; VEQ:F3PLANEX
 ;   [symbol]
 ;
 ; F3PLANEX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %F3PLANEX;
 ;     intersection of plane (n:normal, p:point) and line (a b)
 ;   Source file: src/checks.lisp
```

#### :context: F3REP

```
repeat argument 3d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: F3REP\*

```
repeat the evaluated argument 3 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### :context: F3ROT

```
veq context op: F3ROT
fxname: -F3ROT
args: (AX AY AZ NX NY NZ A)
body (3): (LET ((COSA (COS A)))
            (DECLARE
             (FF
               COSA))
            (MVC #'-F3FROM
                 (MVC #'-F3FROM (-F3SCALE AX AY AZ COSA)
                      (-F3CROSS NX NY NZ AX AY AZ) (SIN A))
                 NX NY NZ (* (-F3. NX NY NZ AX AY AZ) (- 1.0 COSA)))).
```

#### :context: F3ROTS

```
veq context op: F3ROTS
fxname: -F3ROTS
args: (AX AY AZ NX NY NZ A SX SY SZ)
body (3): (MVC #'-F3+ (MVC #'-F3ROT (-F3- AX AY AZ SX SY SZ) NX NY NZ A) SX SY
               SZ).
```

#### :context: F3SCALE

```
veq context op: F3SCALE
fxname: -F3SCALE
args: (AX AY AZ S)
body (3): (VALUES (* AX S) (* AY S) (* AZ S)).
```

#### :context: F3SQRT

```
veq context op: F3SQRT
fxname: -F3SQRT
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))).
```

#### :context: F3SQUARE

```
veq context op: F3SQUARE
fxname: -F3SQUARE
args: (AX AY AZ)
body (3): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ))).
```

#### :context: F3VSET

```
set 3d value.
ex: (F3VSET (a) (fx ...))
where (fx ...) returns 3 values.
```

#### :context: F3~

```
make 3d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :context: F4

```
strict make 4d vector in veq context.
```

#### F4$

```
returns indices (default 0) from 4d vector array (FVEC) as values.
ex: (F4$ a i j ...) returns (values a[i] .. a[j] .. ...).
note that the number of values depends on the dimension.

 ; VEQ:F4$
 ;   [symbol]
 ;
 ; F4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Documentation:
 ;     returns indices (default 0) from 4d vector array (FVEC) as values.
 ;     ex: (F4$ a i j ...) returns (values a[i] .. a[j] .. ...).
 ;     note that the number of values depends on the dimension.
 ;   Source file: src/array-utils.lisp
```

#### :context: F4$\*

```
veq context broadcast op: F4$*
fxname: -F4*
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: F4$\*!

```
veq context broadcast op: F4$*
fxname: -F4*
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
destructive.
```

#### :context: F4$+

```
veq context broadcast op: F4$+
fxname: -F4+
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW)).
```

#### :context: F4$+!

```
veq context broadcast op: F4$+
fxname: -F4+
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW)).
destructive.
```

#### :context: F4$-

```
veq context broadcast op: F4$-
fxname: -F4-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW)).
```

#### :context: F4$-!

```
veq context broadcast op: F4$-
fxname: -F4-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW)).
destructive.
```

#### :context: F4$.

```
veq context broadcast op: F4$.
fxname: -F4.
args: (AX AY AZ AW BX BY BZ BW)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: F4$/

```
veq context broadcast op: F4$/
fxname: -F4/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW)).
```

#### :context: F4$/!

```
veq context broadcast op: F4$/
fxname: -F4/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW)).
destructive.
```

#### :context: F4$^

```
veq context broadcast op: F4$^
fxname: -F4^
args: (AX AY AZ AW S)
body (4): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S) (EXPT AW S)).
```

#### :context: F4$^!

```
veq context broadcast op: F4$^
fxname: -F4^
args: (AX AY AZ AW S)
body (4): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S) (EXPT AW S)).
destructive.
```

#### :context: F4$ABS

```
veq context broadcast op: F4$ABS
fxname: -F4ABS
args: (AX AY AZ AW)
body (4): (VALUES (ABS AX) (ABS AY) (ABS AZ) (ABS AW)).
```

#### :context: F4$ABS!

```
veq context broadcast op: F4$ABS
fxname: -F4ABS
args: (AX AY AZ AW)
body (4): (VALUES (ABS AX) (ABS AY) (ABS AZ) (ABS AW)).
destructive.
```

#### :context: F4$DST

```
veq context broadcast op: F4$DST
fxname: -F4DST
args: (AX AY AZ AW BX BY BZ BW)
body (1): (SQRT
           (THE POS-FF
                (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))))).
```

#### :context: F4$DST2

```
veq context broadcast op: F4$DST2
fxname: -F4DST2
args: (AX AY AZ AW BX BY BZ BW)
body (1): (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))).
```

#### :context: F4$EXP

```
veq context broadcast op: F4$EXP
fxname: -F4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
```

#### :context: F4$EXP!

```
veq context broadcast op: F4$EXP
fxname: -F4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
destructive.
```

#### :context: F4$FROM

```
veq context broadcast op: F4$FROM
fxname: -F4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-F4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S)).
```

#### :context: F4$FROM!

```
veq context broadcast op: F4$FROM
fxname: -F4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-F4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S)).
destructive.
```

#### :context: F4$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

#### :context: F4$I-

```
veq context broadcast op: F4$I-
fxname: -F4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
```

#### :context: F4$I-!

```
veq context broadcast op: F4$I-
fxname: -F4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
destructive.
```

#### :context: F4$I/

```
veq context broadcast op: F4$I/
fxname: -F4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
```

#### :context: F4$I/!

```
veq context broadcast op: F4$I/
fxname: -F4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
destructive.
```

#### :context: F4$ISCALE

```
veq context broadcast op: F4$ISCALE
fxname: -F4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
```

#### :context: F4$ISCALE!

```
veq context broadcast op: F4$ISCALE
fxname: -F4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
destructive.
```

#### F4$LAST

```
return values from last row of 4d vector array.

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

#### :context: F4$LEN

```
veq context broadcast op: F4$LEN
fxname: -F4LEN
args: (AX AY AZ AW)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F4SQUARE AX AY AZ AW))))).
```

#### :context: F4$LEN2

```
veq context broadcast op: F4$LEN2
fxname: -F4LEN2
args: (AX AY AZ AW)
body (1): (THE POS-FF (MVC #'+ (-F4SQUARE AX AY AZ AW))).
```

#### :context: F4$LERP

```
veq context broadcast op: F4$LERP
fxname: -F4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-F4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
           (* (- BW AW) S)).
```

#### :context: F4$LERP!

```
veq context broadcast op: F4$LERP
fxname: -F4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-F4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
           (* (- BW AW) S)).
destructive.
```

#### F4$LINE

```
fx: %F4$LINE
macro wrapper: F4$LINE
defined via veq:def*

 ; VEQ:F4$LINE
 ;   [symbol]
 ;
 ; F4$LINE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F4$LINE
 ;     macro wrapper: F4$LINE
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### F4$LSPACE

```
fx: %F4$LSPACE
macro wrapper: F4$LSPACE
defined via veq:fvdef*

 ; VEQ:F4$LSPACE
 ;   [symbol]
 ;
 ; F4$LSPACE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F4$LSPACE
 ;     macro wrapper: F4$LSPACE
 ;     defined via veq:fvdef*
 ;   Source file: src/lspace.lisp
```

#### :context: F4$MAX

```
veq context broadcast op: F4$MAX
fxname: -F4MAX
args: (AX AY AZ AW)
body (1): (MAX AX AY AZ AW).
```

#### :context: F4$MID

```
veq context broadcast op: F4$MID
fxname: -F4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5)
                  (* (+ BW AW) 0.5)).
```

#### :context: F4$MID!

```
veq context broadcast op: F4$MID
fxname: -F4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5)
                  (* (+ BW AW) 0.5)).
destructive.
```

#### :context: F4$MIN

```
veq context broadcast op: F4$MIN
fxname: -F4MIN
args: (AX AY AZ AW)
body (1): (MIN AX AY AZ AW).
```

#### :context: F4$MOD

```
veq context broadcast op: F4$MOD
fxname: -F4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
```

#### :context: F4$MOD!

```
veq context broadcast op: F4$MOD
fxname: -F4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
destructive.
```

#### :context: F4$NEG

```
veq context broadcast op: F4$NEG
fxname: -F4NEG
args: (AX AY AZ AW)
body (4): (VALUES (- AX) (- AY) (- AZ) (- AW)).
```

#### :context: F4$NEG!

```
veq context broadcast op: F4$NEG
fxname: -F4NEG
args: (AX AY AZ AW)
body (4): (VALUES (- AX) (- AY) (- AZ) (- AW)).
destructive.
```

#### :context: F4$NORM

```
veq context broadcast op: F4$NORM
fxname: -F4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-F4ISCALE AX AY AZ AW (THE POS-FF (MVC #'-F4LEN AX AY AZ AW))).
```

#### :context: F4$NORM!

```
veq context broadcast op: F4$NORM
fxname: -F4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-F4ISCALE AX AY AZ AW (THE POS-FF (MVC #'-F4LEN AX AY AZ AW))).
destructive.
```

#### F4$NUM

```
number of elements in 4d array.
typed.

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
make 4d array of ones.
typed.

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
fx: %F4$POINT
macro wrapper: F4$POINT
defined via veq:def*

 ; VEQ:F4$POINT
 ;   [symbol]
 ;
 ; F4$POINT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F4$POINT
 ;     macro wrapper: F4$POINT
 ;     defined via veq:def*
 ;   Source file: src/shapes.lisp
```

#### :context: F4$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F4$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 4
```

#### :context: F4$SCALE

```
veq context broadcast op: F4$SCALE
fxname: -F4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
```

#### :context: F4$SCALE!

```
veq context broadcast op: F4$SCALE
fxname: -F4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
destructive.
```

#### :context: F4$SQRT

```
veq context broadcast op: F4$SQRT
fxname: -F4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))
                  (THE POS-FF (SQRT (THE POS-FF AW)))).
```

#### :context: F4$SQRT!

```
veq context broadcast op: F4$SQRT
fxname: -F4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))
                  (THE POS-FF (SQRT (THE POS-FF AW)))).
destructive.
```

#### :context: F4$SQUARE

```
veq context broadcast op: F4$SQUARE
fxname: -F4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ)) (THE POS-FF (* AW AW))).
```

#### :context: F4$SQUARE!

```
veq context broadcast op: F4$SQUARE
fxname: -F4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ)) (THE POS-FF (* AW AW))).
destructive.
```

#### F4$SUM

```
sum all rows of 4d array.

 ; VEQ:F4$SUM
 ;   [symbol]
 ;
 ; F4$SUM names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     sum all rows of 4d array.
 ;   Source file: src/array-reduce.lisp
```

#### F4$TAKE

```
returns 4d array with rows for inds.
use :res to put result in existing array.

 ; VEQ:F4$TAKE
 ;   [symbol]
 ;
 ; F4$TAKE names a compiled function:
 ;   Lambda-list: (A INDS &KEY RES)
 ;   Derived type: (FUNCTION
 ;                  ((SIMPLE-ARRAY SINGLE-FLOAT) SEQUENCE &KEY (:RES T))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
 ;   Documentation:
 ;     returns 4d array with rows for inds.
 ;     use :res to put result in existing array.
 ;   Source file: src/array-take.lisp
```

#### F4$VAL

```
make 4d array of val.
typed.

 ; VEQ:F4$VAL
 ;   [symbol]
 ;
 ; F4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: F4$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 4d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### F4$ZERO

```
make 4d vector array of zeros.
typed.

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

#### :context: F4\*

```
veq context op: F4*
fxname: -F4*
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: F4+

```
veq context op: F4+
fxname: -F4+
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW)).
```

#### :context: F4-

```
veq context op: F4-
fxname: -F4-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW)).
```

#### :context: F4.

```
veq context op: F4.
fxname: -F4.
args: (AX AY AZ AW BX BY BZ BW)
body (1): (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW)).
```

#### :context: F4/

```
veq context op: F4/
fxname: -F4/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW)).
```

#### :context: F4^

```
veq context op: F4^
fxname: -F4^
args: (AX AY AZ AW S)
body (4): (VALUES (EXPT AX S) (EXPT AY S) (EXPT AZ S) (EXPT AW S)).
```

#### :context: F4ABS

```
veq context op: F4ABS
fxname: -F4ABS
args: (AX AY AZ AW)
body (4): (VALUES (ABS AX) (ABS AY) (ABS AZ) (ABS AW)).
```

#### :context: F4DST

```
veq context op: F4DST
fxname: -F4DST
args: (AX AY AZ AW BX BY BZ BW)
body (1): (SQRT
           (THE POS-FF
                (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))))).
```

#### :context: F4DST2

```
veq context op: F4DST2
fxname: -F4DST2
args: (AX AY AZ AW BX BY BZ BW)
body (1): (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))).
```

#### :context: F4EXP

```
veq context op: F4EXP
fxname: -F4EXP
args: (AX AY AZ AW)
body (4): (VALUES (EXP AX) (EXP AY) (EXP AZ) (EXP AW)).
```

#### :context: F4FROM

```
veq context op: F4FROM
fxname: -F4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-F4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S)).
```

#### :context: F4I-

```
veq context op: F4I-
fxname: -F4I-
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)).
```

#### :context: F4I/

```
veq context op: F4I/
fxname: -F4I/
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW)).
```

#### :context: F4ISCALE

```
veq context op: F4ISCALE
fxname: -F4ISCALE
args: (AX AY AZ AW S)
body (4): (VALUES (/ AX S) (/ AY S) (/ AZ S) (/ AW S)).
```

#### :context: F4LEN

```
veq context op: F4LEN
fxname: -F4LEN
args: (AX AY AZ AW)
body (1): (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F4SQUARE AX AY AZ AW))))).
```

#### :context: F4LEN2

```
veq context op: F4LEN2
fxname: -F4LEN2
args: (AX AY AZ AW)
body (1): (THE POS-FF (MVC #'+ (-F4SQUARE AX AY AZ AW))).
```

#### :context: F4LERP

```
veq context op: F4LERP
fxname: -F4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body (4): (-F4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
           (* (- BW AW) S)).
```

#### :context: F4LET

```
make 4d let.
ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*.
```

#### :context: F4MAX

```
veq context op: F4MAX
fxname: -F4MAX
args: (AX AY AZ AW)
body (1): (MAX AX AY AZ AW).
```

#### F4MEYE

```
return 4d eye matrix.

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

#### :context: F4MID

```
veq context op: F4MID
fxname: -F4MID
args: (AX AY AZ AW BX BY BZ BW)
body (4): (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5)
                  (* (+ BW AW) 0.5)).
```

#### :context: F4MIN

```
veq context op: F4MIN
fxname: -F4MIN
args: (AX AY AZ AW)
body (1): (MIN AX AY AZ AW).
```

#### F4MINV

```
invert 4x4 matrix. non-destructive.

 ; VEQ:F4MINV
 ;   [symbol]
 ;
 ; F4MINV names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     invert 4x4 matrix. non-destructive.
 ;   Source file: src/mat-inv.lisp
```

#### F4MM

```
multiply mat * mat
of type: FVEC

 ; VEQ:F4MM
 ;   [symbol]
 ;
 ; F4MM names a macro:
 ;   Lambda-list: (A*233 B*235)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F4MMT

```
multiply mat * (transpose mat)
of type: FVEC

 ; VEQ:F4MMT
 ;   [symbol]
 ;
 ; F4MMT names a macro:
 ;   Lambda-list: (A*291 B*293)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### :context: F4MOD

```
veq context op: F4MOD
fxname: -F4MOD
args: (AX AY AZ AW S)
body (4): (VALUES (MOD AX S) (MOD AY S) (MOD AZ S) (MOD AW S)).
```

#### F4MT!

```
transpose 4d matrix in-place.

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
multiply (transpose mat) * mat
of type: FVEC

 ; VEQ:F4MTM
 ;   [symbol]
 ;
 ; F4MTM names a macro:
 ;   Lambda-list: (A*320 B*322)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F4MTMT

```
multiply (transpose mat) * (transpose mat)
of type: FVEC

 ; VEQ:F4MTMT
 ;   [symbol]
 ;
 ; F4MTMT names a macro:
 ;   Lambda-list: (A*262 B*264)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: src/mat.lisp
```

#### F4MTV

```
transpose(mat) * v. for 4d matrix and vector.

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
mat * v. for 4d matrix and vector.

 ; VEQ:F4MV
 ;   [symbol]
 ;
 ; F4MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 4d matrix and vector.
 ;   Source file: src/mat.lisp
```

#### :context: F4NEG

```
veq context op: F4NEG
fxname: -F4NEG
args: (AX AY AZ AW)
body (4): (VALUES (- AX) (- AY) (- AZ) (- AW)).
```

#### :context: F4NORM

```
veq context op: F4NORM
fxname: -F4NORM
args: (AX AY AZ AW)
body (4): (MVC #'-F4ISCALE AX AY AZ AW (THE POS-FF (MVC #'-F4LEN AX AY AZ AW))).
```

#### :context: F4NSUM

```
make 4d
```

#### :context: F4REP

```
repeat argument 4d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: F4REP\*

```
repeat the evaluated argument 4 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### :context: F4SCALE

```
veq context op: F4SCALE
fxname: -F4SCALE
args: (AX AY AZ AW S)
body (4): (VALUES (* AX S) (* AY S) (* AZ S) (* AW S)).
```

#### :context: F4SQRT

```
veq context op: F4SQRT
fxname: -F4SQRT
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (SQRT (THE POS-FF AX)))
                  (THE POS-FF (SQRT (THE POS-FF AY)))
                  (THE POS-FF (SQRT (THE POS-FF AZ)))
                  (THE POS-FF (SQRT (THE POS-FF AW)))).
```

#### :context: F4SQUARE

```
veq context op: F4SQUARE
fxname: -F4SQUARE
args: (AX AY AZ AW)
body (4): (VALUES (THE POS-FF (* AX AX)) (THE POS-FF (* AY AY))
                  (THE POS-FF (* AZ AZ)) (THE POS-FF (* AW AW))).
```

#### :context: F4VSET

```
set 4d value.
ex: (F4VSET (a) (fx ...))
where (fx ...) returns 4 values.
```

#### :context: F4~

```
make 4d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### :context: F^

```
veq context op: F^
fxname: -F^
args: (AX S)
body (1): (EXPT AX S).
```

#### F_

```
create FVEC vector array from body: (F_ '(a b c ...)).

 ; VEQ:F_
 ;   [symbol]
 ;
 ; F_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create FVEC vector array from body: (F_ '(a b c ...)).
 ;   Source file: src/array-utils.lisp
```

#### :context: FABS

```
veq context op: FABS
fxname: -FABS
args: (AX)
body (1): (ABS AX).
```

#### :context: FCLAMP

```
veq context op: FCLAMP
fxname: -FCLAMP
args: (X)
body (1): (MIN 1.0 (MAX 0.0 X)).
```

#### :context: FCLAMP\*

```
veq context op: FCLAMP*
fxname: -FCLAMP*
args: (X MI MA)
body (1): (MIN MA (MAX MI X)).
```

#### :context: FCOS-SIN

```
veq context op: FCOS-SIN
fxname: -FCOS-SIN
args: (AX)
body (2): (VALUES (COS AX) (SIN AX)).
```

#### :context: FDEG->RAD

```
veq context op: FDEG->RAD
fxname: -FDEG->RAD
args: (D)
body (1): (* FPI (/ D 180.0)).
```

#### FEASE-IN-BACK

```
ease in:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0 S) X) S))

 ; VEQ:FEASE-IN-BACK
 ;   [symbol]
 ;
 ; FEASE-IN-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.70158))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX DOUBLE-FLOAT)
 ;                       (COMPLEX SINGLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-CIRC

```
ease in:
arg: (X)
body: (- (- (SQRT (- 1.0 (* X X))) 1.0))

 ; VEQ:FEASE-IN-CIRC
 ;   [symbol]
 ;
 ; FEASE-IN-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES
 ;                   (OR (COMPLEX SINGLE-FLOAT) (SINGLE-FLOAT -0.0 1.0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-CUBIC

```
ease in:
arg: (X)
body: (* X X X)

 ; VEQ:FEASE-IN-CUBIC
 ;   [symbol]
 ;
 ; FEASE-IN-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-ELASTIC

```
ease in:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
        (-
         (* (EXPT 2.0 (* 10.0 (- X 1.0)))
            (SIN (/ (* (- (- X 1.0) S) FPII) P)))))

 ; VEQ:FEASE-IN-ELASTIC
 ;   [symbol]
 ;
 ; FEASE-IN-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
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
ease in:
arg: (X)
body: (EXPT 2.0 (* 10.0 (- X 1.0)))

 ; VEQ:FEASE-IN-EXP
 ;   [symbol]
 ;
 ; FEASE-IN-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-LINEAR

```
ease in:
arg: (X)
body: X

 ; VEQ:FEASE-IN-LINEAR
 ;   [symbol]
 ;
 ; FEASE-IN-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-BACK

```
ease in-out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0 S) X) S))

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
ease in-out:
arg: (X)
body: (- (- (SQRT (- 1.0 (* X X))) 1.0))

 ; VEQ:FEASE-IN-OUT-CIRC
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES
 ;                   (OR (SINGLE-FLOAT -0.0 1.0) (COMPLEX SINGLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-CUBIC

```
ease in-out:
arg: (X)
body: (* X X X)

 ; VEQ:FEASE-IN-OUT-CUBIC
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-ELASTIC

```
ease in-out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
        (-
         (* (EXPT 2.0 (* 10.0 (- X 1.0)))
            (SIN (/ (* (- (- X 1.0) S) FPII) P)))))

 ; VEQ:FEASE-IN-OUT-ELASTIC
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX DOUBLE-FLOAT)
 ;                       (COMPLEX SINGLE-FLOAT))
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
ease in-out:
arg: (X)
body: (EXPT 2.0 (* 10.0 (- X 1.0)))

 ; VEQ:FEASE-IN-OUT-EXP
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-LINEAR

```
ease in-out:
arg: (X)
body: X

 ; VEQ:FEASE-IN-OUT-LINEAR
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-QUART

```
ease in-out:
arg: (X)
body: (EXPT X 4.0)

 ; VEQ:FEASE-IN-OUT-QUART
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-QUINT

```
ease in-out:
arg: (X)
body: (EXPT X 5.0)

 ; VEQ:FEASE-IN-OUT-QUINT
 ;   [symbol]
 ;
 ; FEASE-IN-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in-out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-OUT-SIN

```
ease in-out:
arg: (X)
body: (- 1.0 (COS (* X FPI5)))

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
ease in:
arg: (X)
body: (EXPT X 4.0)

 ; VEQ:FEASE-IN-QUART
 ;   [symbol]
 ;
 ; FEASE-IN-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 4.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-QUINT

```
ease in:
arg: (X)
body: (EXPT X 5.0)

 ; VEQ:FEASE-IN-QUINT
 ;   [symbol]
 ;
 ; FEASE-IN-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (EXPT X 5.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-IN-SIN

```
ease in:
arg: (X)
body: (- 1.0 (COS (* X FPI5)))

 ; VEQ:FEASE-IN-SIN
 ;   [symbol]
 ;
 ; FEASE-IN-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 2.0) &OPTIONAL))
 ;   Documentation:
 ;     ease in:
 ;     arg: (X)
 ;     body: (- 1.0 (COS (* X FPI5)))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-BACK

```
ease out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0 S) X) S))

 ; VEQ:FEASE-OUT-BACK
 ;   [symbol]
 ;
 ; FEASE-OUT-BACK names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (S 1.70158))
 ;   Derived type: (FUNCTION (T &OPTIONAL T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX DOUBLE-FLOAT)
 ;                       (COMPLEX SINGLE-FLOAT))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X &OPTIONAL (S 1.70158))
 ;     body: (* X X (- (* (+ 1.0 S) X) S))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-CIRC

```
ease out:
arg: (X)
body: (- (- (SQRT (- 1.0 (* X X))) 1.0))

 ; VEQ:FEASE-OUT-CIRC
 ;   [symbol]
 ;
 ; FEASE-OUT-CIRC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES
 ;                   (OR (COMPLEX SINGLE-FLOAT) (SINGLE-FLOAT 0.0 1.0))
 ;                   &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-CUBIC

```
ease out:
arg: (X)
body: (* X X X)

 ; VEQ:FEASE-OUT-CUBIC
 ;   [symbol]
 ;
 ; FEASE-OUT-CUBIC names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (* X X X)
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-ELASTIC

```
ease out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
        (-
         (* (EXPT 2.0 (* 10.0 (- X 1.0)))
            (SIN (/ (* (- (- X 1.0) S) FPII) P)))))

 ; VEQ:FEASE-OUT-ELASTIC
 ;   [symbol]
 ;
 ; FEASE-OUT-ELASTIC names a compiled function:
 ;   Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T T)
 ;                  (VALUES
 ;                   (OR FLOAT (COMPLEX SINGLE-FLOAT)
 ;                       (COMPLEX DOUBLE-FLOAT))
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
ease out:
arg: (X)
body: (EXPT 2.0 (* 10.0 (- X 1.0)))

 ; VEQ:FEASE-OUT-EXP
 ;   [symbol]
 ;
 ; FEASE-OUT-EXP names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT * 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT 2.0 (* 10.0 (- X 1.0)))
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-LINEAR

```
ease out:
arg: (X)
body: X

 ; VEQ:FEASE-OUT-LINEAR
 ;   [symbol]
 ;
 ; FEASE-OUT-LINEAR names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: X
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-QUART

```
ease out:
arg: (X)
body: (EXPT X 4.0)

 ; VEQ:FEASE-OUT-QUART
 ;   [symbol]
 ;
 ; FEASE-OUT-QUART names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 4.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-QUINT

```
ease out:
arg: (X)
body: (EXPT X 5.0)

 ; VEQ:FEASE-OUT-QUINT
 ;   [symbol]
 ;
 ; FEASE-OUT-QUINT names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (EXPT X 5.0)
 ;   Source file: src/easing.lisp
```

#### FEASE-OUT-SIN

```
ease out:
arg: (X)
body: (- 1.0 (COS (* X FPI5)))

 ; VEQ:FEASE-OUT-SIN
 ;   [symbol]
 ;
 ; FEASE-OUT-SIN names a compiled function:
 ;   Lambda-list: (X)
 ;   Derived type: (FUNCTION (T)
 ;                  (VALUES (SINGLE-FLOAT -1.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     ease out:
 ;     arg: (X)
 ;     body: (- 1.0 (COS (* X FPI5)))
 ;   Source file: src/easing.lisp
```

#### :context: FEXP

```
veq context op: FEXP
fxname: -FEXP
args: (AX)
body (1): (VALUES (EXP AX)).
```

#### FF

```
:missing:todo:

 ; VEQ:FF
 ;   [symbol]
 ;
 ; FF names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
 ;
 ; FF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: SINGLE-FLOAT
```

#### FF\*

```
:missing:todo:

 ; VEQ:FF*
 ;   [symbol]
 ;
 ; FF* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### FFL

```
return (values (ff a) (ff b) ..) from (list a b ..).

 ; VEQ:FFL
 ;   [symbol]
 ;
 ; FFL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (ff a) (ff b) ..) from (list a b ..).
 ;   Source file: src/types.lisp
```

#### :context: FFROM

```
veq context op: FFROM
fxname: -FFROM
args: (AX BX S)
body (1): (+ AX (* BX S)).
```

#### :context: FI-

```
veq context op: FI-
fxname: -FI-
args: (AX BX)
body (1): (- BX AX).
```

#### :context: FI/

```
veq context op: FI/
fxname: -FI/
args: (AX BX)
body (1): (/ BX AX).
```

#### :context: FISCALE

```
veq context op: FISCALE
fxname: -FISCALE
args: (AX S)
body (1): (VALUES (/ AX S)).
```

#### :context: FLEN

```
veq context op: FLEN
fxname: -FLEN
args: (AX)
body (1): (THE POS-FF AX).
```

#### :context: FLEN2

```
veq context op: FLEN2
fxname: -FLEN2
args: (AX)
body (1): (THE POS-FF (MVC #'+ (-FSQUARE AX))).
```

#### :context: FLERP

```
veq context op: FLERP
fxname: -FLERP
args: (AX BX S)
body (1): (+ AX (* (- BX AX) S)).
```

#### FMAKE-ORTHO-PROJ-MATRIX

```
DOCSTRING for %FMAKE-ORTHO-PROJ-MATRIX;
make orthogonal projection matrix

 ; VEQ:FMAKE-ORTHO-PROJ-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-ORTHO-PROJ-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %FMAKE-ORTHO-PROJ-MATRIX;
 ;     make orthogonal projection matrix
 ;   Source file: src/mat-cam.lisp
```

#### FMAKE-PROJ-MATRIX

```
DOCSTRING for %FMAKE-PROJ-MATRIX;
make projection matrix for width, height, near, far

 ; VEQ:FMAKE-PROJ-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-PROJ-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %FMAKE-PROJ-MATRIX;
 ;     make projection matrix for width, height, near, far
 ;   Source file: src/mat-cam.lisp
```

#### FMAKE-VIEW-MATRIX

```
DOCSTRING for %FMAKE-VIEW-MATRIX;
make view matrix for cam (w/up) looking at target

 ; VEQ:FMAKE-VIEW-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-VIEW-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     DOCSTRING for %FMAKE-VIEW-MATRIX;
 ;     make view matrix for cam (w/up) looking at target
 ;   Source file: src/mat-cam.lisp
```

#### :context: FMID

```
veq context op: FMID
fxname: -FMID
args: (AX BX)
body (1): (* 0.5 (+ AX BX)).
```

#### :context: FMOD

```
veq context op: FMOD
fxname: -FMOD
args: (AX S)
body (1): (MOD AX S).
```

#### :context: FNEG

```
veq context op: FNEG
fxname: -FNEG
args: (AX)
body (1): (- AX).
```

#### :context: FNORM

```
veq context op: FNORM
fxname: -FNORM
args: (AX)
body (1): (MVC #'-FISCALE AX (MVC #'-FLEN AX)).
```

#### :context: FNSUM

```
make 1d
```

#### FPI

```
:missing:todo:

 ; VEQ:FPI
 ;   [symbol]
 ;
 ; FPI names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 3.1415927
```

#### FPI5

```
:missing:todo:

 ; VEQ:FPI5
 ;   [symbol]
 ;
 ; FPI5 names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 1.5707964
```

#### FPII

```
:missing:todo:

 ; VEQ:FPII
 ;   [symbol]
 ;
 ; FPII names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 6.2831855
```

#### :context: FREP

```
repeat argument 1d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: FREP\*

```
repeat the evaluated argument 1 times as values.
ex: (f3rep (fx)) corresponds to (let ((v (fx))) (values v v v)).
```

#### FROM-LST

```
get values from list. equivalent to (values-list ...).

 ; VEQ:FROM-LST
 ;   [symbol]
 ;
 ; FROM-LST names a macro:
 ;   Lambda-list: (L)
 ;   Documentation:
 ;     get values from list. equivalent to (values-list ...).
 ;   Source file: src/utils.lisp
```

#### :context: FSCALE

```
veq context op: FSCALE
fxname: -FSCALE
args: (AX S)
body (1): (VALUES (* AX S)).
```

#### FSEL

```
return values from body in order of dims. use indices or :x :y :z :w
ex: (fsel (:w :zx 0) (values a b c d))
returns (values d c a a).

 ; VEQ:FSEL
 ;   [symbol]
 ;
 ; FSEL names a macro:
 ;   Lambda-list: ((&REST DIMS) &BODY BODY)
 ;   Documentation:
 ;     return values from body in order of dims. use indices or :x :y :z :w
 ;     ex: (fsel (:w :zx 0) (values a b c d))
 ;     returns (values d c a a).
 ;   Source file: src/select-dim.lisp
```

#### :context: FSIN-COS

```
veq context op: FSIN-COS
fxname: -FSIN-COS
args: (AX)
body (2): (VALUES (SIN AX) (COS AX)).
```

#### :context: FSQRT

```
veq context op: FSQRT
fxname: -FSQRT
args: (AX)
body (1): (THE POS-FF (SQRT (THE POS-FF AX))).
```

#### :context: FSQUARE

```
veq context op: FSQUARE
fxname: -FSQUARE
args: (AX)
body (1): (* AX AX).
```

#### FVDEF

```
define function with veq context enabled. uses fvprogn.

 ; VEQ:FVDEF
 ;   [symbol]
 ;
 ; FVDEF names a macro:
 ;   Lambda-list: (FNAME &BODY BODY)
 ;   Documentation:
 ;     define function with veq context enabled. uses fvprogn.
 ;   Source file: src/macrolets.lisp
```

#### FVDEF\*

```
defines a function named: %fx
and a wrapper macro named: fx
veq context is enabled. uses fvprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...).

 ; VEQ:FVDEF*
 ;   [symbol]
 ;
 ; FVDEF* names a macro:
 ;   Lambda-list: (MNAME &BODY BODY)
 ;   Documentation:
 ;     defines a function named: %fx
 ;     and a wrapper macro named: fx
 ;     veq context is enabled. uses fvprogn.
 ;
 ;     the wrapper macro ensures every call to this function is done as
 ;     (mvc #'%fx ...).
 ;   Source file: src/macrolets.lisp
```

#### FVEC

```
:missing:todo:

 ; VEQ:FVEC
 ;   [symbol]
 ;
 ; FVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:FF)
```

#### FVLET

```
:missing:todo:

 ; VEQ:FVLET
 ;   [symbol]
```

#### FVPROGN

```
enable veq context inside this progn.
handles propagation and resolution of uses of (varg d var) and (vref var i).

works the same way as vprogn. but removes all macrolets that are not
directly used in body. this is faster, but may fail in some cases where
body is complex. in the event of errors try vprogn instead.

 ; VEQ:FVPROGN
 ;   [symbol]
 ;
 ; FVPROGN names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     enable veq context inside this progn.
 ;     handles propagation and resolution of uses of (varg d var) and (vref var i).
 ;
 ;     works the same way as vprogn. but removes all macrolets that are not
 ;     directly used in body. this is faster, but may fail in some cases where
 ;     body is complex. in the event of errors try vprogn instead.
 ;   Source file: src/macrolets.lisp
```

#### :context: FVSET

```
set 1d value.
ex: (FVSET (a) (fx ...))
where (fx ...) returns 1 values.
```

#### :context: FWITH-ARRAYS

```
args: (&key (n 0) inds (start 0) itr cnt arr fxs exs nxs)

n is the number of iterations
start is the first (row) index. then n-1 more.
inds is (row) indices to iterate. replaces n/start
arr is the arrays to be defined/referenced
itr is the symbol representing indices
cnt is the symbol representing iterations from 0
fxs is the labels
exs is the expressions assigned to array
nxs is the expressions with no assignment

ex:

(fwith-arrays (:n 7 :itr k ; k will be 0, 1, ..., 6
  ; the third form in elements of arr can be empty, a form that will be
  ; executed, or a symbol that refers to an array defined outside of
  ; with-arrays
  :arr ((a 3 (f3$one 7)) ; init a as (f3$one 7)
        (b 3) (c 3)) ; init b,c as (f3$zero 7)
  ; define functions to use in fxs
  :fxs ((cross ((varg 3 v w)) (f3cross v w))
        (init1 (i) (f3~ (1+ i) (* 2 i) (+ 2 i)))
        (init2 (i) (f3~ (+ 2 i) (1+ i) (* 2 i))))
  ; perform the calculations
  :exs ((a k (init1 k)) ; init row k of a with init1
        (b k (init2 k)) ; init row k of b with init2
        (c k (cross a b))) ; set row k of c to (cross a b)
  :nxs ((cross a b))); executes the function, but does not assign res anywhere
  ; use the arrays. the last form is returned, as in a progn
  (vpr c))
```

#### :context: F~

```
make 1d vector in veq context.
wraps body in mvc so that (f3~ 1 (f2~ 2f0 3))
returns (values 1f0 2f0 3f0)
```

#### I$COPY

```
copy IVEC vector array.

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

#### I$MAKE

```
create IVEC vector array with size n * dim, and initial value v.

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
number of elements in 1d array.
typed.

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
make 1d array of ones.
typed.

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

#### I$VAL

```
make 1d array of val.
typed.

 ; VEQ:I$VAL
 ;   [symbol]
 ;
 ; I$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: I$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 1d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### I$ZERO

```
make 1d vector array of zeros.
typed.

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

#### I2$NUM

```
number of elements in 2d array.
typed.

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
make 2d array of ones.
typed.

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

#### I2$VAL

```
make 2d array of val.
typed.

 ; VEQ:I2$VAL
 ;   [symbol]
 ;
 ; I2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: I2$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 2d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### I2$ZERO

```
make 2d vector array of zeros.
typed.

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

#### :context: I2VSET

```
set 2d value.
ex: (I2VSET (a) (fx ...))
where (fx ...) returns 2 values.
```

#### I3$NUM

```
number of elements in 3d array.
typed.

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
make 3d array of ones.
typed.

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

#### I3$VAL

```
make 3d array of val.
typed.

 ; VEQ:I3$VAL
 ;   [symbol]
 ;
 ; I3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: I3$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 3d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### I3$ZERO

```
make 3d vector array of zeros.
typed.

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

#### :context: I3VSET

```
set 3d value.
ex: (I3VSET (a) (fx ...))
where (fx ...) returns 3 values.
```

#### I4$NUM

```
number of elements in 4d array.
typed.

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
make 4d array of ones.
typed.

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

#### I4$VAL

```
make 4d array of val.
typed.

 ; VEQ:I4$VAL
 ;   [symbol]
 ;
 ; I4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY FIXNUM (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: I4$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 4d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### I4$ZERO

```
make 4d vector array of zeros.
typed.

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

#### :context: I4VSET

```
set 4d value.
ex: (I4VSET (a) (fx ...))
where (fx ...) returns 4 values.
```

#### I?

```
inspect argument

 ; VEQ:I?
 ;   [symbol]
 ;
 ; I? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     inspect argument
 ;   Source file: src/utils.lisp
```

#### I_

```
create IVEC vector array from body: (I_ '(a b c ...)).

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
:missing:todo:

 ; VEQ:IN
 ;   [symbol]
 ;
 ; IN names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
 ;
 ; IN names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: FIXNUM
```

#### IN\*

```
:missing:todo:

 ; VEQ:IN*
 ;   [symbol]
 ;
 ; IN* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### IVEC

```
:missing:todo:

 ; VEQ:IVEC
 ;   [symbol]
 ;
 ; IVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:IN)
```

#### :context: IVSET

```
set 1d value.
ex: (IVSET (a) (fx ...))
where (fx ...) returns 1 values.
```

#### :context: IWITH-ARRAYS

```
args: (&key (n 0) inds (start 0) itr cnt arr fxs exs nxs)

n is the number of iterations
start is the first (row) index. then n-1 more.
inds is (row) indices to iterate. replaces n/start
arr is the arrays to be defined/referenced
itr is the symbol representing indices
cnt is the symbol representing iterations from 0
fxs is the labels
exs is the expressions assigned to array
nxs is the expressions with no assignment

ex:

(pwith-arrays (:n 7 :itr k ; k will be 0, 1, ..., 6
  ; the third form in elements of arr can be empty, a form that will be
  ; executed, or a symbol that refers to an array defined outside of
  ; with-arrays
  :arr ((a 3 (f3$one 7)) ; init a as (f3$one 7)
        (b 3) (c 3)) ; init b,c as (f3$zero 7)
  ; define functions to use in fxs
  :fxs ((cross ((varg 3 v w)) (f3cross v w))
        (init1 (i) (f3~ (1+ i) (* 2 i) (+ 2 i)))
        (init2 (i) (f3~ (+ 2 i) (1+ i) (* 2 i))))
  ; perform the calculations
  :exs ((a k (init1 k)) ; init row k of a with init1
        (b k (init2 k)) ; init row k of b with init2
        (c k (cross a b))) ; set row k of c to (cross a b)
  :nxs ((cross a b))); executes the function, but does not assign res anywhere
  ; use the arrays. the last form is returned, as in a progn
  (vpr c))
```

#### LST

```
get all values in body as a list.
almost like multuple-values-list, except it handles multiple arguments.

 ; VEQ:LST
 ;   [symbol]
 ;
 ; LST names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     get all values in body as a list.
 ;     almost like multuple-values-list, except it handles multiple arguments.
 ;   Source file: src/utils.lisp
```

#### MAC

```
expand macro.

 ; VEQ:MAC
 ;   [symbol]
 ;
 ; MAC names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     expand macro.
 ;   Source file: src/utils.lisp
```

#### MAC\*

```
expand macro all. only in SBCL.

 ; VEQ:MAC*
 ;   [symbol]
 ;
 ; MAC* names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     expand macro all. only in SBCL.
 ;   Source file: src/utils.lisp
```

#### MVB

```
:missing:todo:

 ; VEQ:MVB
 ;   [symbol]
 ;
 ; MVB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: src/utils.lisp
```

#### MVC

```
:missing:todo:

 ; VEQ:MVC
 ;   [symbol]
 ;
 ; MVC names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: src/utils.lisp
```

#### MVCGRP

```
call fx on body in groups of dim.
ex: (labels ((fx ((:va 3 x)) (veq:fxy x)))
      (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
returns: (values 1f0 2f0 4f0 5f0)
ex: (labels ((fx ((:va 3 x)) (veq:fxz x)))
      (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
returns: (values 1f0 3f0 4f0 6f0)

 ; VEQ:MVCGRP
 ;   [symbol]
 ;
 ; MVCGRP names a macro:
 ;   Lambda-list: ((DIM FX) &BODY BODY)
 ;   Documentation:
 ;     call fx on body in groups of dim.
 ;     ex: (labels ((fx ((:va 3 x)) (veq:fxy x)))
 ;           (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
 ;     returns: (values 1f0 2f0 4f0 5f0)
 ;     ex: (labels ((fx ((:va 3 x)) (veq:fxz x)))
 ;           (vpr (mvcgrp (3 #'fx) (values 1f0 2f0 3f0 4f0 5f0 6f0))))
 ;     returns: (values 1f0 3f0 4f0 6f0)
 ;   Source file: src/utils.lisp
```

#### MVCMAP

```
returns (values (fx i) (fx j) ...) for dim values from body.

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
define a macro named m so that (m a ...) is equivalent to (mvc #'fx a ...)

 ; VEQ:MVCWRAP
 ;   [symbol]
 ;
 ; MVCWRAP names a macro:
 ;   Lambda-list: (M FX)
 ;   Documentation:
 ;     define a macro named m so that (m a ...) is equivalent to (mvc #'fx a ...)
 ;   Source file: src/utils.lisp
```

#### P$COPY

```
copy PVEC vector array.

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

#### P$MAKE

```
create PVEC vector array with size n * dim, and initial value v.

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
number of elements in 1d array.
typed.

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
make 1d array of ones.
typed.

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

#### P$VAL

```
make 1d array of val.
typed.

 ; VEQ:P$VAL
 ;   [symbol]
 ;
 ; P$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: P$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 1d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### P$ZERO

```
make 1d vector array of zeros.
typed.

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

#### P2$NUM

```
number of elements in 2d array.
typed.

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
make 2d array of ones.
typed.

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

#### P2$VAL

```
make 2d array of val.
typed.

 ; VEQ:P2$VAL
 ;   [symbol]
 ;
 ; P2$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: P2$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 2d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### P2$ZERO

```
make 2d vector array of zeros.
typed.

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

#### P3$NUM

```
number of elements in 3d array.
typed.

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
make 3d array of ones.
typed.

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

#### P3$VAL

```
make 3d array of val.
typed.

 ; VEQ:P3$VAL
 ;   [symbol]
 ;
 ; P3$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: P3$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 3d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### P3$ZERO

```
make 3d vector array of zeros.
typed.

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

#### P4$NUM

```
number of elements in 4d array.
typed.

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
make 4d array of ones.
typed.

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

#### P4$VAL

```
make 4d array of val.
typed.

 ; VEQ:P4$VAL
 ;   [symbol]
 ;
 ; P4$VAL names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 31) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of val.
 ;     typed.
 ;   Source file: src/array-utils.lisp
```

#### :context: P4$WITH-ROWS

```
execute function (expr i ax ay az bx by bz ...) for
row i and 4d arrays a and b (...).  arrs can be one or more arrays.
ex:
  (labels ((cross (i (veq:varg 3 a b))
             (veq:3$vset (c i) (veq:f3cross a b))))
    (veq:f3$with-rows (n a b) cross))
```

#### P4$ZERO

```
make 4d vector array of zeros.
typed.

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

#### P_

```
create PVEC vector array from body: (P_ '(a b c ...)).

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
:missing:todo:

 ; VEQ:PN
 ;   [symbol]
 ;
 ; PN names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
 ;
 ; PN names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 31))
 ;   Expansion: (UNSIGNED-BYTE 31)
```

#### PN\*

```
:missing:todo:

 ; VEQ:PN*
 ;   [symbol]
 ;
 ; PN* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: src/types.lisp
```

#### POS-DF

```
:missing:todo:

 ; VEQ:POS-DF
 ;   [symbol]
 ;
 ; POS-DF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (DOUBLE-FLOAT 0.0d0 *)
```

#### POS-FF

```
:missing:todo:

 ; VEQ:POS-FF
 ;   [symbol]
 ;
 ; POS-FF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SINGLE-FLOAT 0.0 *)
```

#### POS-INT

```
:missing:todo:

 ; VEQ:POS-INT
 ;   [symbol]
 ;
 ; POS-INT names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 31))
 ;   Expansion: (UNSIGNED-BYTE 31)
```

#### PVEC

```
:missing:todo:

 ; VEQ:PVEC
 ;   [symbol]
 ;
 ; PVEC names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SIMPLE-ARRAY VEQ:PN)
```

#### :context: PWITH-ARRAYS

```
args: (&key (n 0) inds (start 0) itr cnt arr fxs exs nxs)

n is the number of iterations
start is the first (row) index. then n-1 more.
inds is (row) indices to iterate. replaces n/start
arr is the arrays to be defined/referenced
itr is the symbol representing indices
cnt is the symbol representing iterations from 0
fxs is the labels
exs is the expressions assigned to array
nxs is the expressions with no assignment

ex:

(iwith-arrays (:n 7 :itr k ; k will be 0, 1, ..., 6
  ; the third form in elements of arr can be empty, a form that will be
  ; executed, or a symbol that refers to an array defined outside of
  ; with-arrays
  :arr ((a 3 (f3$one 7)) ; init a as (f3$one 7)
        (b 3) (c 3)) ; init b,c as (f3$zero 7)
  ; define functions to use in fxs
  :fxs ((cross ((varg 3 v w)) (f3cross v w))
        (init1 (i) (f3~ (1+ i) (* 2 i) (+ 2 i)))
        (init2 (i) (f3~ (+ 2 i) (1+ i) (* 2 i))))
  ; perform the calculations
  :exs ((a k (init1 k)) ; init row k of a with init1
        (b k (init2 k)) ; init row k of b with init2
        (c k (cross a b))) ; set row k of c to (cross a b)
  :nxs ((cross a b))); executes the function, but does not assign res anywhere
  ; use the arrays. the last form is returned, as in a progn
  (vpr c))
```

#### V?

```
get version. use silent to surpress stdout

 ; VEQ:V?
 ;   [symbol]
 ;
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get version. use silent to surpress stdout
 ;   Source file: src/utils.lisp
```

#### :context: VARG

```
use (veq:varg dim a b ...) or (:vr dim a b ...) to represent dim vectors a,b
of dim n in vprogn, fvprog, fvdef*, vdef*, def*.  see replace-varg for
implementation details.
```

#### VDEF

```
define function with veq context enabled. uses vprogn.

 ; VEQ:VDEF
 ;   [symbol]
 ;
 ; VDEF names a macro:
 ;   Lambda-list: (FNAME &BODY BODY)
 ;   Documentation:
 ;     define function with veq context enabled. uses vprogn.
 ;   Source file: src/macrolets.lisp
```

#### VDEF\*

```
defines a function named: %fx
and a wrapper macro named: fx
veq context is enabled. uses vprogn.

the wrapper macro ensures every call to this function is done as
(mvc #'%fx ...).

 ; VEQ:VDEF*
 ;   [symbol]
 ;
 ; VDEF* names a macro:
 ;   Lambda-list: (MNAME &BODY BODY)
 ;   Documentation:
 ;     defines a function named: %fx
 ;     and a wrapper macro named: fx
 ;     veq context is enabled. uses vprogn.
 ;
 ;     the wrapper macro ensures every call to this function is done as
 ;     (mvc #'%fx ...).
 ;   Source file: src/macrolets.lisp
```

#### VLABELS

```
wraps labels so that it can be used with implicit mvc. that is, all labels
are defined as if with def*.
use %labelname to call the function directly.

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

#### VPR

```
print (mvc #'list rest) and return (mvc #'values rest).

 ; VEQ:VPR
 ;   [symbol]
 ;
 ; VPR names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     print (mvc #'list rest) and return (mvc #'values rest).
 ;   Source file: src/utils.lisp
```

#### VPROD

```
(mvc #'* ...)

 ; VEQ:VPROD
 ;   [symbol]
 ;
 ; VPROD names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Documentation:
 ;     (mvc #'* ...)
 ;   Source file: src/utils.lisp
```

#### VPROGN

```
enable veq context inside this progn.
handles propagation and resolution of uses of (varg d var) and (vref var i).
fvprogn is faster, but has some limitations.

 ; VEQ:VPROGN
 ;   [symbol]
 ;
 ; VPROGN names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     enable veq context inside this progn.
 ;     handles propagation and resolution of uses of (varg d var) and (vref var i).
 ;     fvprogn is faster, but has some limitations.
 ;   Source file: src/macrolets.lisp
```

#### :context: VREF

```
use (veq:vref s x) or (:vr s x) to get dim x of symbol s in vprogn,
fvprogn, fvdef*, vdef*, def*. see replace-varg for implementation details.
```

#### VSUM

```
(mvc #'+ ...)

 ; VEQ:VSUM
 ;   [symbol]
 ;
 ; VSUM names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Documentation:
 ;     (mvc #'+ ...)
 ;   Source file: src/utils.lisp
```

#### ~

```
wraps arguments in (mvc #'values ...).

 ; VEQ:~
 ;   [symbol]
 ;
 ; ~ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     wraps arguments in (mvc #'values ...).
 ;   Source file: src/utils.lisp
```

