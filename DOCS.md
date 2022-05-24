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

Symbols postfixed with `!` are destructive or in-place.


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
 ; (SETF $) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     1d vector array setter and getter.
 ;     use (setf ($ a i) (list x)) to set a[i].
 ;     use ($ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: /data/x/veq/src/vset.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### $PRINT

```
pretty print nd array

 ; VEQ:$PRINT
 ;   [symbol]
 ;
 ; $PRINT names a compiled function:
 ;   Lambda-list: (A &KEY N (DIM 1) &AUX
 ;                 (N
 ;                  (IF N
 ;                      N
 ;                      (/ (LENGTH A) DIM))))
 ;   Derived type: (FUNCTION
 ;                  (SIMPLE-ARRAY &KEY (:N T) (:DIM (UNSIGNED-BYTE 31)))
 ;                  (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
 ;   Documentation:
 ;     pretty print nd array
 ;   Source file: /data/x/veq/src/array-print.lisp
```

#### $TO-LIST

```
return array as a list of nd lists

 ; VEQ:$TO-LIST
 ;   [symbol]
 ;
 ; $TO-LIST names a compiled function:
 ;   Lambda-list: (A &KEY (DIM 1))
 ;   Derived type: (FUNCTION (SIMPLE-ARRAY &KEY (:DIM (UNSIGNED-BYTE 31)))
 ;                  (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     return array as a list of nd lists
 ;   Source file: /data/x/veq/src/array-print.lisp
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
 ; (SETF 2$) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     2d vector array setter and getter.
 ;     use (setf (2$ a i) (list x)) to set a[i].
 ;     use (2$ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: /data/x/veq/src/vset.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### 2$PRINT

```
print 2d array

 ; VEQ:2$PRINT
 ;   [symbol]
 ;
 ; 2$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION (T &KEY (:N T)) *)
 ;   Documentation:
 ;     print 2d array
 ;   Source file: /data/x/veq/src/array-print.lisp
```

#### 2$TO-LIST

```
return array as a list of 2d lists

 ; VEQ:2$TO-LIST
 ;   [symbol]
 ;
 ; 2$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of 2d lists
 ;   Source file: /data/x/veq/src/array-print.lisp
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
 ; (SETF 3$) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     3d vector array setter and getter.
 ;     use (setf (3$ a i) (list x)) to set a[i].
 ;     use (3$ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: /data/x/veq/src/vset.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### 3$PRINT

```
print 3d array

 ; VEQ:3$PRINT
 ;   [symbol]
 ;
 ; 3$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION (T &KEY (:N T)) *)
 ;   Documentation:
 ;     print 3d array
 ;   Source file: /data/x/veq/src/array-print.lisp
```

#### 3$TO-LIST

```
return array as a list of 3d lists

 ; VEQ:3$TO-LIST
 ;   [symbol]
 ;
 ; 3$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of 3d lists
 ;   Source file: /data/x/veq/src/array-print.lisp
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
 ; (SETF 4$) has a complex setf-expansion:
 ;   Lambda-list: (A &OPTIONAL (I 0))
 ;   Documentation:
 ;     4d vector array setter and getter.
 ;     use (setf (4$ a i) (list x)) to set a[i].
 ;     use (4$ a i j ...) to return (values a[i] a[j] ...)
 ;   Source file: /data/x/veq/src/vset.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### 4$PRINT

```
print 4d array

 ; VEQ:4$PRINT
 ;   [symbol]
 ;
 ; 4$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY N)
 ;   Derived type: (FUNCTION (T &KEY (:N T)) *)
 ;   Documentation:
 ;     print 4d array
 ;   Source file: /data/x/veq/src/array-print.lisp
```

#### 4$TO-LIST

```
return array as a list of 4d lists

 ; VEQ:4$TO-LIST
 ;   [symbol]
 ;
 ; 4$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of 4d lists
 ;   Source file: /data/x/veq/src/array-print.lisp
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
 ;       fvprogn, vdef, fvdef defined contexts/functions.
 ;   Source file: /data/x/veq/src/docs.lisp
```

#### :context: D

```
make 1d vector in veq context.
strict.
```

#### :context: D$

```
returns values from 1d array.
supports multiple indices. default is 0.
ex: (D$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### D$\*

```
broadcast for fx: -D*
macroname: D$*
ex: (D$* a ...) performs (mvc #'-D* a[i] ...) for every row in a.

 ; VEQ:D$*
 ;   [symbol]
 ;
 ; D$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$*
 ;     broadcast for fx: -D*
 ;     macroname: D$*
 ;     ex: (D$* a ...) performs (mvc #'-D* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$\*!

```
broadcast for fx: -D*
macroname: D$*!
ex: (D$*! a ...) performs (mvc #'-D* a[i] ...) for every row in a.
destructive.

 ; VEQ:D$*!
 ;   [symbol]
 ;
 ; D$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$*!
 ;     broadcast for fx: -D*
 ;     macroname: D$*!
 ;     ex: (D$*! a ...) performs (mvc #'-D* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$+

```
broadcast for fx: -D+
macroname: D$+
ex: (D$+ a ...) performs (mvc #'-D+ a[i] ...) for every row in a.

 ; VEQ:D$+
 ;   [symbol]
 ;
 ; D$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$+
 ;     broadcast for fx: -D+
 ;     macroname: D$+
 ;     ex: (D$+ a ...) performs (mvc #'-D+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$+!

```
broadcast for fx: -D+
macroname: D$+!
ex: (D$+! a ...) performs (mvc #'-D+ a[i] ...) for every row in a.
destructive.

 ; VEQ:D$+!
 ;   [symbol]
 ;
 ; D$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$+!
 ;     broadcast for fx: -D+
 ;     macroname: D$+!
 ;     ex: (D$+! a ...) performs (mvc #'-D+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$-

```
broadcast for fx: -D-
macroname: D$-
ex: (D$- a ...) performs (mvc #'-D- a[i] ...) for every row in a.

 ; VEQ:D$-
 ;   [symbol]
 ;
 ; D$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$-
 ;     broadcast for fx: -D-
 ;     macroname: D$-
 ;     ex: (D$- a ...) performs (mvc #'-D- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$-!

```
broadcast for fx: -D-
macroname: D$-!
ex: (D$-! a ...) performs (mvc #'-D- a[i] ...) for every row in a.
destructive.

 ; VEQ:D$-!
 ;   [symbol]
 ;
 ; D$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$-!
 ;     broadcast for fx: -D-
 ;     macroname: D$-!
 ;     ex: (D$-! a ...) performs (mvc #'-D- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$/

```
broadcast for fx: -D/
macroname: D$/
ex: (D$/ a ...) performs (mvc #'-D/ a[i] ...) for every row in a.

 ; VEQ:D$/
 ;   [symbol]
 ;
 ; D$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$/
 ;     broadcast for fx: -D/
 ;     macroname: D$/
 ;     ex: (D$/ a ...) performs (mvc #'-D/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$/!

```
broadcast for fx: -D/
macroname: D$/!
ex: (D$/! a ...) performs (mvc #'-D/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D$/!
 ;   [symbol]
 ;
 ; D$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$/!
 ;     broadcast for fx: -D/
 ;     macroname: D$/!
 ;     ex: (D$/! a ...) performs (mvc #'-D/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$_

```
create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
   or: ($_ '((1d0 2d0) (1d0 2d0)))

 ; VEQ:D$_
 ;   [symbol]
 ;
 ; D$_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
 ;        or: ($_ '((1d0 2d0) (1d0 2d0)))
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### D$ABS

```
broadcast for fx: -DABS
macroname: D$ABS
ex: (D$ABS a ...) performs (mvc #'-DABS a[i] ...) for every row in a.

 ; VEQ:D$ABS
 ;   [symbol]
 ;
 ; D$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$ABS
 ;     broadcast for fx: -DABS
 ;     macroname: D$ABS
 ;     ex: (D$ABS a ...) performs (mvc #'-DABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$ABS!

```
broadcast for fx: -DABS
macroname: D$ABS!
ex: (D$ABS! a ...) performs (mvc #'-DABS a[i] ...) for every row in a.
destructive.

 ; VEQ:D$ABS!
 ;   [symbol]
 ;
 ; D$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$ABS!
 ;     broadcast for fx: -DABS
 ;     macroname: D$ABS!
 ;     ex: (D$ABS! a ...) performs (mvc #'-DABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$COPY

```
copy array

 ; VEQ:D$COPY
 ;   [symbol]
 ;
 ; D$COPY names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     copy array
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### D$COS-SIN

```
broadcast for fx: -DCOS-SIN
macroname: D$COS-SIN
ex: (D$COS-SIN a ...) performs (mvc #'-DCOS-SIN a[i] ...) for every row in a.

 ; VEQ:D$COS-SIN
 ;   [symbol]
 ;
 ; D$COS-SIN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$COS-SIN
 ;     broadcast for fx: -DCOS-SIN
 ;     macroname: D$COS-SIN
 ;     ex: (D$COS-SIN a ...) performs (mvc #'-DCOS-SIN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$FROM

```
broadcast for fx: -DFROM
macroname: D$FROM
ex: (D$FROM a ...) performs (mvc #'-DFROM a[i] ...) for every row in a.

 ; VEQ:D$FROM
 ;   [symbol]
 ;
 ; D$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$FROM
 ;     broadcast for fx: -DFROM
 ;     macroname: D$FROM
 ;     ex: (D$FROM a ...) performs (mvc #'-DFROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$FROM!

```
broadcast for fx: -DFROM
macroname: D$FROM!
ex: (D$FROM! a ...) performs (mvc #'-DFROM a[i] ...) for every row in a.
destructive.

 ; VEQ:D$FROM!
 ;   [symbol]
 ;
 ; D$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$FROM!
 ;     broadcast for fx: -DFROM
 ;     macroname: D$FROM!
 ;     ex: (D$FROM! a ...) performs (mvc #'-DFROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: D$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

#### D$I-

```
broadcast for fx: -DI-
macroname: D$I-
ex: (D$I- a ...) performs (mvc #'-DI- a[i] ...) for every row in a.

 ; VEQ:D$I-
 ;   [symbol]
 ;
 ; D$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$I-
 ;     broadcast for fx: -DI-
 ;     macroname: D$I-
 ;     ex: (D$I- a ...) performs (mvc #'-DI- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$I-!

```
broadcast for fx: -DI-
macroname: D$I-!
ex: (D$I-! a ...) performs (mvc #'-DI- a[i] ...) for every row in a.
destructive.

 ; VEQ:D$I-!
 ;   [symbol]
 ;
 ; D$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$I-!
 ;     broadcast for fx: -DI-
 ;     macroname: D$I-!
 ;     ex: (D$I-! a ...) performs (mvc #'-DI- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$I/

```
broadcast for fx: -DI/
macroname: D$I/
ex: (D$I/ a ...) performs (mvc #'-DI/ a[i] ...) for every row in a.

 ; VEQ:D$I/
 ;   [symbol]
 ;
 ; D$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$I/
 ;     broadcast for fx: -DI/
 ;     macroname: D$I/
 ;     ex: (D$I/ a ...) performs (mvc #'-DI/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$I/!

```
broadcast for fx: -DI/
macroname: D$I/!
ex: (D$I/! a ...) performs (mvc #'-DI/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D$I/!
 ;   [symbol]
 ;
 ; D$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$I/!
 ;     broadcast for fx: -DI/
 ;     macroname: D$I/!
 ;     ex: (D$I/! a ...) performs (mvc #'-DI/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$ISCALE

```
broadcast for fx: -DISCALE
macroname: D$ISCALE
ex: (D$ISCALE a ...) performs (mvc #'-DISCALE a[i] ...) for every row in a.

 ; VEQ:D$ISCALE
 ;   [symbol]
 ;
 ; D$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$ISCALE
 ;     broadcast for fx: -DISCALE
 ;     macroname: D$ISCALE
 ;     ex: (D$ISCALE a ...) performs (mvc #'-DISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$ISCALE!

```
broadcast for fx: -DISCALE
macroname: D$ISCALE!
ex: (D$ISCALE! a ...) performs (mvc #'-DISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D$ISCALE!
 ;   [symbol]
 ;
 ; D$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$ISCALE!
 ;     broadcast for fx: -DISCALE
 ;     macroname: D$ISCALE!
 ;     ex: (D$ISCALE! a ...) performs (mvc #'-DISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$LAST

```
get last row of 1d array as (values ...)

 ; VEQ:D$LAST
 ;   [symbol]
 ;
 ; D$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get last row of 1d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### D$LEN

```
broadcast for fx: -DLEN
macroname: D$LEN
ex: (D$LEN a ...) performs (mvc #'-DLEN a[i] ...) for every row in a.

 ; VEQ:D$LEN
 ;   [symbol]
 ;
 ; D$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$LEN
 ;     broadcast for fx: -DLEN
 ;     macroname: D$LEN
 ;     ex: (D$LEN a ...) performs (mvc #'-DLEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$LEN2

```
broadcast for fx: -DLEN2
macroname: D$LEN2
ex: (D$LEN2 a ...) performs (mvc #'-DLEN2 a[i] ...) for every row in a.

 ; VEQ:D$LEN2
 ;   [symbol]
 ;
 ; D$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$LEN2
 ;     broadcast for fx: -DLEN2
 ;     macroname: D$LEN2
 ;     ex: (D$LEN2 a ...) performs (mvc #'-DLEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
```

#### D$MAKE

```
 create array with size (n dim), and initial value v

 ; VEQ:D$MAKE
 ;   [symbol]
 ;
 ; D$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0d0))
 ;   Documentation:
 ;      create array with size (n dim), and initial value v
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-mima.lisp
```

#### D$NEG

```
broadcast for fx: -DNEG
macroname: D$NEG
ex: (D$NEG a ...) performs (mvc #'-DNEG a[i] ...) for every row in a.

 ; VEQ:D$NEG
 ;   [symbol]
 ;
 ; D$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$NEG
 ;     broadcast for fx: -DNEG
 ;     macroname: D$NEG
 ;     ex: (D$NEG a ...) performs (mvc #'-DNEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$NEG!

```
broadcast for fx: -DNEG
macroname: D$NEG!
ex: (D$NEG! a ...) performs (mvc #'-DNEG a[i] ...) for every row in a.
destructive.

 ; VEQ:D$NEG!
 ;   [symbol]
 ;
 ; D$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$NEG!
 ;     broadcast for fx: -DNEG
 ;     macroname: D$NEG!
 ;     ex: (D$NEG! a ...) performs (mvc #'-DNEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$NORM

```
broadcast for fx: -DNORM
macroname: D$NORM
ex: (D$NORM a ...) performs (mvc #'-DNORM a[i] ...) for every row in a.

 ; VEQ:D$NORM
 ;   [symbol]
 ;
 ; D$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$NORM
 ;     broadcast for fx: -DNORM
 ;     macroname: D$NORM
 ;     ex: (D$NORM a ...) performs (mvc #'-DNORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$NORM!

```
broadcast for fx: -DNORM
macroname: D$NORM!
ex: (D$NORM! a ...) performs (mvc #'-DNORM a[i] ...) for every row in a.
destructive.

 ; VEQ:D$NORM!
 ;   [symbol]
 ;
 ; D$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$NORM!
 ;     broadcast for fx: -DNORM
 ;     macroname: D$NORM!
 ;     ex: (D$NORM! a ...) performs (mvc #'-DNORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### :context: D$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 1
```

#### D$SCALE

```
broadcast for fx: -DSCALE
macroname: D$SCALE
ex: (D$SCALE a ...) performs (mvc #'-DSCALE a[i] ...) for every row in a.

 ; VEQ:D$SCALE
 ;   [symbol]
 ;
 ; D$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$SCALE
 ;     broadcast for fx: -DSCALE
 ;     macroname: D$SCALE
 ;     ex: (D$SCALE a ...) performs (mvc #'-DSCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D$SCALE!

```
broadcast for fx: -DSCALE
macroname: D$SCALE!
ex: (D$SCALE! a ...) performs (mvc #'-DSCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D$SCALE!
 ;   [symbol]
 ;
 ; D$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D$SCALE!
 ;     broadcast for fx: -DSCALE
 ;     macroname: D$SCALE!
 ;     ex: (D$SCALE! a ...) performs (mvc #'-DSCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### D$TAKE

```
returns 1d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D$WITH-ROWS

```
make 1d
```

#### D$ZERO

```
make 1d array of zeros.
typed.

 ; VEQ:D$ZERO
 ;   [symbol]
 ;
 ; D$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D\*

```
veq context op: D*
fxname: -D*
args: (A B)
body: (* A B)
```

#### :context: D+

```
veq context op: D+
fxname: -D+
args: (A B)
body: (+ A B)
```

#### :context: D-

```
veq context op: D-
fxname: -D-
args: (A B)
body: (- A B)
```

#### :context: D/

```
veq context op: D/
fxname: -D/
args: (A B)
body: (/ A B)
```

#### :context: D2

```
make 2d vector in veq context.
strict.
```

#### :context: D2$

```
returns values from 2d array.
supports multiple indices. default is 0.
ex: (D2$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### D2$\*

```
broadcast for fx: -D2*
macroname: D2$*
ex: (D2$* a ...) performs (mvc #'-D2* a[i] ...) for every row in a.

 ; VEQ:D2$*
 ;   [symbol]
 ;
 ; D2$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$*
 ;     broadcast for fx: -D2*
 ;     macroname: D2$*
 ;     ex: (D2$* a ...) performs (mvc #'-D2* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$\*!

```
broadcast for fx: -D2*
macroname: D2$*!
ex: (D2$*! a ...) performs (mvc #'-D2* a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$*!
 ;   [symbol]
 ;
 ; D2$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$*!
 ;     broadcast for fx: -D2*
 ;     macroname: D2$*!
 ;     ex: (D2$*! a ...) performs (mvc #'-D2* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$+

```
broadcast for fx: -D2+
macroname: D2$+
ex: (D2$+ a ...) performs (mvc #'-D2+ a[i] ...) for every row in a.

 ; VEQ:D2$+
 ;   [symbol]
 ;
 ; D2$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$+
 ;     broadcast for fx: -D2+
 ;     macroname: D2$+
 ;     ex: (D2$+ a ...) performs (mvc #'-D2+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$+!

```
broadcast for fx: -D2+
macroname: D2$+!
ex: (D2$+! a ...) performs (mvc #'-D2+ a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$+!
 ;   [symbol]
 ;
 ; D2$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$+!
 ;     broadcast for fx: -D2+
 ;     macroname: D2$+!
 ;     ex: (D2$+! a ...) performs (mvc #'-D2+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$-

```
broadcast for fx: -D2-
macroname: D2$-
ex: (D2$- a ...) performs (mvc #'-D2- a[i] ...) for every row in a.

 ; VEQ:D2$-
 ;   [symbol]
 ;
 ; D2$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$-
 ;     broadcast for fx: -D2-
 ;     macroname: D2$-
 ;     ex: (D2$- a ...) performs (mvc #'-D2- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$-!

```
broadcast for fx: -D2-
macroname: D2$-!
ex: (D2$-! a ...) performs (mvc #'-D2- a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$-!
 ;   [symbol]
 ;
 ; D2$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$-!
 ;     broadcast for fx: -D2-
 ;     macroname: D2$-!
 ;     ex: (D2$-! a ...) performs (mvc #'-D2- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$/

```
broadcast for fx: -D2/
macroname: D2$/
ex: (D2$/ a ...) performs (mvc #'-D2/ a[i] ...) for every row in a.

 ; VEQ:D2$/
 ;   [symbol]
 ;
 ; D2$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$/
 ;     broadcast for fx: -D2/
 ;     macroname: D2$/
 ;     ex: (D2$/ a ...) performs (mvc #'-D2/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$/!

```
broadcast for fx: -D2/
macroname: D2$/!
ex: (D2$/! a ...) performs (mvc #'-D2/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$/!
 ;   [symbol]
 ;
 ; D2$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$/!
 ;     broadcast for fx: -D2/
 ;     macroname: D2$/!
 ;     ex: (D2$/! a ...) performs (mvc #'-D2/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$ABS

```
broadcast for fx: -D2ABS
macroname: D2$ABS
ex: (D2$ABS a ...) performs (mvc #'-D2ABS a[i] ...) for every row in a.

 ; VEQ:D2$ABS
 ;   [symbol]
 ;
 ; D2$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ABS
 ;     broadcast for fx: -D2ABS
 ;     macroname: D2$ABS
 ;     ex: (D2$ABS a ...) performs (mvc #'-D2ABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$ABS!

```
broadcast for fx: -D2ABS
macroname: D2$ABS!
ex: (D2$ABS! a ...) performs (mvc #'-D2ABS a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$ABS!
 ;   [symbol]
 ;
 ; D2$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ABS!
 ;     broadcast for fx: -D2ABS
 ;     macroname: D2$ABS!
 ;     ex: (D2$ABS! a ...) performs (mvc #'-D2ABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$FROM

```
broadcast for fx: -D2FROM
macroname: D2$FROM
ex: (D2$FROM a ...) performs (mvc #'-D2FROM a[i] ...) for every row in a.

 ; VEQ:D2$FROM
 ;   [symbol]
 ;
 ; D2$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$FROM
 ;     broadcast for fx: -D2FROM
 ;     macroname: D2$FROM
 ;     ex: (D2$FROM a ...) performs (mvc #'-D2FROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$FROM!

```
broadcast for fx: -D2FROM
macroname: D2$FROM!
ex: (D2$FROM! a ...) performs (mvc #'-D2FROM a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$FROM!
 ;   [symbol]
 ;
 ; D2$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$FROM!
 ;     broadcast for fx: -D2FROM
 ;     macroname: D2$FROM!
 ;     ex: (D2$FROM! a ...) performs (mvc #'-D2FROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: D2$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

#### D2$I-

```
broadcast for fx: -D2I-
macroname: D2$I-
ex: (D2$I- a ...) performs (mvc #'-D2I- a[i] ...) for every row in a.

 ; VEQ:D2$I-
 ;   [symbol]
 ;
 ; D2$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$I-
 ;     broadcast for fx: -D2I-
 ;     macroname: D2$I-
 ;     ex: (D2$I- a ...) performs (mvc #'-D2I- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$I-!

```
broadcast for fx: -D2I-
macroname: D2$I-!
ex: (D2$I-! a ...) performs (mvc #'-D2I- a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$I-!
 ;   [symbol]
 ;
 ; D2$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$I-!
 ;     broadcast for fx: -D2I-
 ;     macroname: D2$I-!
 ;     ex: (D2$I-! a ...) performs (mvc #'-D2I- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$I/

```
broadcast for fx: -D2I/
macroname: D2$I/
ex: (D2$I/ a ...) performs (mvc #'-D2I/ a[i] ...) for every row in a.

 ; VEQ:D2$I/
 ;   [symbol]
 ;
 ; D2$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$I/
 ;     broadcast for fx: -D2I/
 ;     macroname: D2$I/
 ;     ex: (D2$I/ a ...) performs (mvc #'-D2I/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$I/!

```
broadcast for fx: -D2I/
macroname: D2$I/!
ex: (D2$I/! a ...) performs (mvc #'-D2I/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$I/!
 ;   [symbol]
 ;
 ; D2$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$I/!
 ;     broadcast for fx: -D2I/
 ;     macroname: D2$I/!
 ;     ex: (D2$I/! a ...) performs (mvc #'-D2I/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$ISCALE

```
broadcast for fx: -D2ISCALE
macroname: D2$ISCALE
ex: (D2$ISCALE a ...) performs (mvc #'-D2ISCALE a[i] ...) for every row in a.

 ; VEQ:D2$ISCALE
 ;   [symbol]
 ;
 ; D2$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ISCALE
 ;     broadcast for fx: -D2ISCALE
 ;     macroname: D2$ISCALE
 ;     ex: (D2$ISCALE a ...) performs (mvc #'-D2ISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$ISCALE!

```
broadcast for fx: -D2ISCALE
macroname: D2$ISCALE!
ex: (D2$ISCALE! a ...) performs (mvc #'-D2ISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$ISCALE!
 ;   [symbol]
 ;
 ; D2$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ISCALE!
 ;     broadcast for fx: -D2ISCALE
 ;     macroname: D2$ISCALE!
 ;     ex: (D2$ISCALE! a ...) performs (mvc #'-D2ISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$LAST

```
get last row of 2d array as (values ...)

 ; VEQ:D2$LAST
 ;   [symbol]
 ;
 ; D2$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get last row of 2d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### D2$LEN

```
broadcast for fx: -D2LEN
macroname: D2$LEN
ex: (D2$LEN a ...) performs (mvc #'-D2LEN a[i] ...) for every row in a.

 ; VEQ:D2$LEN
 ;   [symbol]
 ;
 ; D2$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$LEN
 ;     broadcast for fx: -D2LEN
 ;     macroname: D2$LEN
 ;     ex: (D2$LEN a ...) performs (mvc #'-D2LEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$LEN2

```
broadcast for fx: -D2LEN2
macroname: D2$LEN2
ex: (D2$LEN2 a ...) performs (mvc #'-D2LEN2 a[i] ...) for every row in a.

 ; VEQ:D2$LEN2
 ;   [symbol]
 ;
 ; D2$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$LEN2
 ;     broadcast for fx: -D2LEN2
 ;     macroname: D2$LEN2
 ;     ex: (D2$LEN2 a ...) performs (mvc #'-D2LEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
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
 ;   Source file: /data/x/veq/src/array-mima.lisp
```

#### D2$NEG

```
broadcast for fx: -D2NEG
macroname: D2$NEG
ex: (D2$NEG a ...) performs (mvc #'-D2NEG a[i] ...) for every row in a.

 ; VEQ:D2$NEG
 ;   [symbol]
 ;
 ; D2$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$NEG
 ;     broadcast for fx: -D2NEG
 ;     macroname: D2$NEG
 ;     ex: (D2$NEG a ...) performs (mvc #'-D2NEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$NEG!

```
broadcast for fx: -D2NEG
macroname: D2$NEG!
ex: (D2$NEG! a ...) performs (mvc #'-D2NEG a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$NEG!
 ;   [symbol]
 ;
 ; D2$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$NEG!
 ;     broadcast for fx: -D2NEG
 ;     macroname: D2$NEG!
 ;     ex: (D2$NEG! a ...) performs (mvc #'-D2NEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$NORM

```
broadcast for fx: -D2NORM
macroname: D2$NORM
ex: (D2$NORM a ...) performs (mvc #'-D2NORM a[i] ...) for every row in a.

 ; VEQ:D2$NORM
 ;   [symbol]
 ;
 ; D2$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$NORM
 ;     broadcast for fx: -D2NORM
 ;     macroname: D2$NORM
 ;     ex: (D2$NORM a ...) performs (mvc #'-D2NORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$NORM!

```
broadcast for fx: -D2NORM
macroname: D2$NORM!
ex: (D2$NORM! a ...) performs (mvc #'-D2NORM a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$NORM!
 ;   [symbol]
 ;
 ; D2$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$NORM!
 ;     broadcast for fx: -D2NORM
 ;     macroname: D2$NORM!
 ;     ex: (D2$NORM! a ...) performs (mvc #'-D2NORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### D2$ROT

```
broadcast for fx: -D2ROT
macroname: D2$ROT
ex: (D2$ROT a ...) performs (mvc #'-D2ROT a[i] ...) for every row in a.

 ; VEQ:D2$ROT
 ;   [symbol]
 ;
 ; D2$ROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ROT
 ;     broadcast for fx: -D2ROT
 ;     macroname: D2$ROT
 ;     ex: (D2$ROT a ...) performs (mvc #'-D2ROT a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$ROT!

```
broadcast for fx: -D2ROT
macroname: D2$ROT!
ex: (D2$ROT! a ...) performs (mvc #'-D2ROT a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$ROT!
 ;   [symbol]
 ;
 ; D2$ROT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ROT!
 ;     broadcast for fx: -D2ROT
 ;     macroname: D2$ROT!
 ;     ex: (D2$ROT! a ...) performs (mvc #'-D2ROT a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$ROTS

```
broadcast for fx: -D2ROTS
macroname: D2$ROTS
ex: (D2$ROTS a ...) performs (mvc #'-D2ROTS a[i] ...) for every row in a.

 ; VEQ:D2$ROTS
 ;   [symbol]
 ;
 ; D2$ROTS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ROTS
 ;     broadcast for fx: -D2ROTS
 ;     macroname: D2$ROTS
 ;     ex: (D2$ROTS a ...) performs (mvc #'-D2ROTS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$ROTS!

```
broadcast for fx: -D2ROTS
macroname: D2$ROTS!
ex: (D2$ROTS! a ...) performs (mvc #'-D2ROTS a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$ROTS!
 ;   [symbol]
 ;
 ; D2$ROTS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$ROTS!
 ;     broadcast for fx: -D2ROTS
 ;     macroname: D2$ROTS!
 ;     ex: (D2$ROTS! a ...) performs (mvc #'-D2ROTS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: D2$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D2$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 2
```

#### D2$SCALE

```
broadcast for fx: -D2SCALE
macroname: D2$SCALE
ex: (D2$SCALE a ...) performs (mvc #'-D2SCALE a[i] ...) for every row in a.

 ; VEQ:D2$SCALE
 ;   [symbol]
 ;
 ; D2$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$SCALE
 ;     broadcast for fx: -D2SCALE
 ;     macroname: D2$SCALE
 ;     ex: (D2$SCALE a ...) performs (mvc #'-D2SCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D2$SCALE!

```
broadcast for fx: -D2SCALE
macroname: D2$SCALE!
ex: (D2$SCALE! a ...) performs (mvc #'-D2SCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D2$SCALE!
 ;   [symbol]
 ;
 ; D2$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2$SCALE!
 ;     broadcast for fx: -D2SCALE
 ;     macroname: D2$SCALE!
 ;     ex: (D2$SCALE! a ...) performs (mvc #'-D2SCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### D2$TAKE

```
returns 2d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D2$WITH-ROWS

```
make 2d
```

#### D2$ZERO

```
make 2d array of zeros.
typed.

 ; VEQ:D2$ZERO
 ;   [symbol]
 ;
 ; D2$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D2\*

```
veq context op: D2*
fxname: -D2*
args: (AX AY BX BY)
body: (VALUES (* AX BX) (* AY BY))
```

#### :context: D2+

```
veq context op: D2+
fxname: -D2+
args: (AX AY BX BY)
body: (VALUES (+ AX BX) (+ AY BY))
```

#### :context: D2-

```
veq context op: D2-
fxname: -D2-
args: (AX AY BX BY)
body: (VALUES (- AX BX) (- AY BY))
```

#### :context: D2.

```
veq context op: D2.
fxname: -D2.
args: (AX AY BX BY)
body: (+ (* AX BX) (* AY BY))
```

#### :context: D2/

```
veq context op: D2/
fxname: -D2/
args: (AX AY BX BY)
body: (VALUES (/ AX BX) (/ AY BY))
```

#### :context: D2^

```
veq context op: D2^
fxname: -D2^
args: (A B S)
body: (VALUES (EXPT A S) (EXPT B S))
```

#### :context: D2ABS

```
veq context op: D2ABS
fxname: -D2ABS
args: (A B)
body: (VALUES (ABS A) (ABS B))
```

#### :context: D2ANGLE

```
veq context op: D2ANGLE
fxname: -D2ANGLE
args: (A B)
body: (MVC #'ATAN (-D2NORM B A))
```

#### :context: D2CROSS

```
veq context op: D2CROSS
fxname: -D2CROSS
args: (AX AY BX BY)
body: (- (* AX BY) (* AY BX))
```

#### :context: D2DST

```
veq context op: D2DST
fxname: -D2DST
args: (AX AY BX BY)
body: (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY)))))
```

#### :context: D2DST2

```
veq context op: D2DST2
fxname: -D2DST2
args: (AX AY BX BY)
body: (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY)))
```

#### :context: D2EXP

```
veq context op: D2EXP
fxname: -D2EXP
args: (A B)
body: (VALUES (EXP A) (EXP B))
```

#### :context: D2FLIP

```
veq context op: D2FLIP
fxname: -D2FLIP
args: (A B)
body: (VALUES B A)
```

#### :context: D2FROM

```
veq context op: D2FROM
fxname: -D2FROM
args: (AX AY BX BY S)
body: (-D2+ AX AY (* BX S) (* BY S))
```

#### :context: D2I-

```
veq context op: D2I-
fxname: -D2I-
args: (AX AY BX BY)
body: (VALUES (- BX AX) (- BY AY))
```

#### :context: D2I/

```
veq context op: D2I/
fxname: -D2I/
args: (AX AY BX BY)
body: (VALUES (/ BX AX) (/ BY AY))
```

#### :context: D2ISCALE

```
veq context op: D2ISCALE
fxname: -D2ISCALE
args: (A B S)
body: (VALUES (/ A S) (/ B S))
```

#### :context: D2LEN

```
veq context op: D2LEN
fxname: -D2LEN
args: (A B)
body: (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE A B)))))
```

#### :context: D2LEN2

```
veq context op: D2LEN2
fxname: -D2LEN2
args: (A B)
body: (THE POS-DF (MVC #'+ (-D2SQUARE A B)))
```

#### :context: D2LERP

```
veq context op: D2LERP
fxname: -D2LERP
args: (AX AY BX BY S)
body: (-D2+ AX AY (* (- BX AX) S) (* (- BY AY) S))
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
args: (A B)
body: (MAX A B)
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D2MID

```
veq context op: D2MID
fxname: -D2MID
args: (AX AY BX BY)
body: (VALUES (* 0.5d0 (+ AX BX)) (* 0.5d0 (+ AY BY)))
```

#### :context: D2MIN

```
veq context op: D2MIN
fxname: -D2MIN
args: (A B)
body: (MIN A B)
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D2MOD

```
veq context op: D2MOD
fxname: -D2MOD
args: (A B S)
body: (VALUES (MOD A S) (MOD B S))
```

#### D2MROT

```
docstring for %D2MROT
make 2d rotation matrix for rotating a rads

 ; VEQ:D2MROT
 ;   [symbol]
 ;
 ; D2MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2MROT
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### D2MROT\*

```
docstring for %D2MROT*
make 2d rotation matrix for rotating a rads

 ; VEQ:D2MROT*
 ;   [symbol]
 ;
 ; D2MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2MROT*
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### D2MSCALE

```
docstring for %D2MSCALE
make 2d matrix for scaling by x

 ; VEQ:D2MSCALE
 ;   [symbol]
 ;
 ; D2MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2MSCALE
 ;     make 2d matrix for scaling by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### D2MTRANS

```
docstring for %D2MTRANS
make 2d transpose matrix for moving by x

 ; VEQ:D2MTRANS
 ;   [symbol]
 ;
 ; D2MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D2MTRANS
 ;     make 2d transpose matrix for moving by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D2NEG

```
veq context op: D2NEG
fxname: -D2NEG
args: (A B)
body: (VALUES (- A) (- B))
```

#### :context: D2NORM

```
veq context op: D2NORM
fxname: -D2NORM
args: (A B)
body: (MVC #'-D2ISCALE A B (MVC #'-D2LEN A B))
```

#### :context: D2NSUM

```
make 2d
```

#### :context: D2ON-CIRC

```
veq context op: D2ON-CIRC
fxname: -D2ON-CIRC
args: (A RAD)
body: (MVC #'-D2SCALE (-DCOS-SIN (* A DPII)) RAD)
```

#### :context: D2ON-CIRC\*

```
veq context op: D2ON-CIRC*
fxname: -D2ON-CIRC*
args: (A RAD)
body: (MVC #'-D2SCALE (-DCOS-SIN A) RAD)
```

#### :context: D2PERP

```
veq context op: D2PERP
fxname: -D2PERP
args: (A B)
body: (VALUES B (- A))
```

#### :context: D2PERP\*

```
veq context op: D2PERP*
fxname: -D2PERP*
args: (A B)
body: (VALUES (- B) A)
```

#### :context: D2REP

```
repeat argument 2d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: D2REP\*

```
repeat the evaluated argument 2d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### :context: D2ROT

```
veq context op: D2ROT
fxname: -D2ROT
args: (X Y A)
body: (LET ((COSA (COS A)) (SINA (SIN A)))
        (DECLARE
         (DF
           COSA
           SINA))
        (VALUES (- (* X COSA) (* Y SINA)) (+ (* X SINA) (* Y COSA))))
```

#### :context: D2ROTS

```
veq context op: D2ROTS
fxname: -D2ROTS
args: (X Y A SX SY)
body: (MVC #'-D2+ (MVC #'-D2ROT (-D2- X Y SX SY) A) SX SY)
```

#### :context: D2SCALE

```
veq context op: D2SCALE
fxname: -D2SCALE
args: (A B S)
body: (VALUES (* A S) (* B S))
```

#### :context: D2SQRT

```
veq context op: D2SQRT
fxname: -D2SQRT
args: (A B)
body: (VALUES (THE POS-DF (SQRT (THE POS-DF A)))
              (THE POS-DF (SQRT (THE POS-DF B))))
```

#### :context: D2SQUARE

```
veq context op: D2SQUARE
fxname: -D2SQUARE
args: (A B)
body: (VALUES (* A A) (* B B))
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
coerce to type.
```

#### :context: D3

```
make 3d vector in veq context.
strict.
```

#### :context: D3$

```
returns values from 3d array.
supports multiple indices. default is 0.
ex: (D3$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### D3$\*

```
broadcast for fx: -D3*
macroname: D3$*
ex: (D3$* a ...) performs (mvc #'-D3* a[i] ...) for every row in a.

 ; VEQ:D3$*
 ;   [symbol]
 ;
 ; D3$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$*
 ;     broadcast for fx: -D3*
 ;     macroname: D3$*
 ;     ex: (D3$* a ...) performs (mvc #'-D3* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$\*!

```
broadcast for fx: -D3*
macroname: D3$*!
ex: (D3$*! a ...) performs (mvc #'-D3* a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$*!
 ;   [symbol]
 ;
 ; D3$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$*!
 ;     broadcast for fx: -D3*
 ;     macroname: D3$*!
 ;     ex: (D3$*! a ...) performs (mvc #'-D3* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$+

```
broadcast for fx: -D3+
macroname: D3$+
ex: (D3$+ a ...) performs (mvc #'-D3+ a[i] ...) for every row in a.

 ; VEQ:D3$+
 ;   [symbol]
 ;
 ; D3$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$+
 ;     broadcast for fx: -D3+
 ;     macroname: D3$+
 ;     ex: (D3$+ a ...) performs (mvc #'-D3+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$+!

```
broadcast for fx: -D3+
macroname: D3$+!
ex: (D3$+! a ...) performs (mvc #'-D3+ a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$+!
 ;   [symbol]
 ;
 ; D3$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$+!
 ;     broadcast for fx: -D3+
 ;     macroname: D3$+!
 ;     ex: (D3$+! a ...) performs (mvc #'-D3+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$-

```
broadcast for fx: -D3-
macroname: D3$-
ex: (D3$- a ...) performs (mvc #'-D3- a[i] ...) for every row in a.

 ; VEQ:D3$-
 ;   [symbol]
 ;
 ; D3$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$-
 ;     broadcast for fx: -D3-
 ;     macroname: D3$-
 ;     ex: (D3$- a ...) performs (mvc #'-D3- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$-!

```
broadcast for fx: -D3-
macroname: D3$-!
ex: (D3$-! a ...) performs (mvc #'-D3- a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$-!
 ;   [symbol]
 ;
 ; D3$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$-!
 ;     broadcast for fx: -D3-
 ;     macroname: D3$-!
 ;     ex: (D3$-! a ...) performs (mvc #'-D3- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$/

```
broadcast for fx: -D3/
macroname: D3$/
ex: (D3$/ a ...) performs (mvc #'-D3/ a[i] ...) for every row in a.

 ; VEQ:D3$/
 ;   [symbol]
 ;
 ; D3$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$/
 ;     broadcast for fx: -D3/
 ;     macroname: D3$/
 ;     ex: (D3$/ a ...) performs (mvc #'-D3/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$/!

```
broadcast for fx: -D3/
macroname: D3$/!
ex: (D3$/! a ...) performs (mvc #'-D3/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$/!
 ;   [symbol]
 ;
 ; D3$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$/!
 ;     broadcast for fx: -D3/
 ;     macroname: D3$/!
 ;     ex: (D3$/! a ...) performs (mvc #'-D3/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$ABS

```
broadcast for fx: -D3ABS
macroname: D3$ABS
ex: (D3$ABS a ...) performs (mvc #'-D3ABS a[i] ...) for every row in a.

 ; VEQ:D3$ABS
 ;   [symbol]
 ;
 ; D3$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ABS
 ;     broadcast for fx: -D3ABS
 ;     macroname: D3$ABS
 ;     ex: (D3$ABS a ...) performs (mvc #'-D3ABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$ABS!

```
broadcast for fx: -D3ABS
macroname: D3$ABS!
ex: (D3$ABS! a ...) performs (mvc #'-D3ABS a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$ABS!
 ;   [symbol]
 ;
 ; D3$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ABS!
 ;     broadcast for fx: -D3ABS
 ;     macroname: D3$ABS!
 ;     ex: (D3$ABS! a ...) performs (mvc #'-D3ABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$FROM

```
broadcast for fx: -D3FROM
macroname: D3$FROM
ex: (D3$FROM a ...) performs (mvc #'-D3FROM a[i] ...) for every row in a.

 ; VEQ:D3$FROM
 ;   [symbol]
 ;
 ; D3$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$FROM
 ;     broadcast for fx: -D3FROM
 ;     macroname: D3$FROM
 ;     ex: (D3$FROM a ...) performs (mvc #'-D3FROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$FROM!

```
broadcast for fx: -D3FROM
macroname: D3$FROM!
ex: (D3$FROM! a ...) performs (mvc #'-D3FROM a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$FROM!
 ;   [symbol]
 ;
 ; D3$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$FROM!
 ;     broadcast for fx: -D3FROM
 ;     macroname: D3$FROM!
 ;     ex: (D3$FROM! a ...) performs (mvc #'-D3FROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: D3$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

#### D3$I-

```
broadcast for fx: -D3I-
macroname: D3$I-
ex: (D3$I- a ...) performs (mvc #'-D3I- a[i] ...) for every row in a.

 ; VEQ:D3$I-
 ;   [symbol]
 ;
 ; D3$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$I-
 ;     broadcast for fx: -D3I-
 ;     macroname: D3$I-
 ;     ex: (D3$I- a ...) performs (mvc #'-D3I- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$I-!

```
broadcast for fx: -D3I-
macroname: D3$I-!
ex: (D3$I-! a ...) performs (mvc #'-D3I- a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$I-!
 ;   [symbol]
 ;
 ; D3$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$I-!
 ;     broadcast for fx: -D3I-
 ;     macroname: D3$I-!
 ;     ex: (D3$I-! a ...) performs (mvc #'-D3I- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$I/

```
broadcast for fx: -D3I/
macroname: D3$I/
ex: (D3$I/ a ...) performs (mvc #'-D3I/ a[i] ...) for every row in a.

 ; VEQ:D3$I/
 ;   [symbol]
 ;
 ; D3$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$I/
 ;     broadcast for fx: -D3I/
 ;     macroname: D3$I/
 ;     ex: (D3$I/ a ...) performs (mvc #'-D3I/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$I/!

```
broadcast for fx: -D3I/
macroname: D3$I/!
ex: (D3$I/! a ...) performs (mvc #'-D3I/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$I/!
 ;   [symbol]
 ;
 ; D3$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$I/!
 ;     broadcast for fx: -D3I/
 ;     macroname: D3$I/!
 ;     ex: (D3$I/! a ...) performs (mvc #'-D3I/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$ISCALE

```
broadcast for fx: -D3ISCALE
macroname: D3$ISCALE
ex: (D3$ISCALE a ...) performs (mvc #'-D3ISCALE a[i] ...) for every row in a.

 ; VEQ:D3$ISCALE
 ;   [symbol]
 ;
 ; D3$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ISCALE
 ;     broadcast for fx: -D3ISCALE
 ;     macroname: D3$ISCALE
 ;     ex: (D3$ISCALE a ...) performs (mvc #'-D3ISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$ISCALE!

```
broadcast for fx: -D3ISCALE
macroname: D3$ISCALE!
ex: (D3$ISCALE! a ...) performs (mvc #'-D3ISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$ISCALE!
 ;   [symbol]
 ;
 ; D3$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ISCALE!
 ;     broadcast for fx: -D3ISCALE
 ;     macroname: D3$ISCALE!
 ;     ex: (D3$ISCALE! a ...) performs (mvc #'-D3ISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$LAST

```
get last row of 3d array as (values ...)

 ; VEQ:D3$LAST
 ;   [symbol]
 ;
 ; D3$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get last row of 3d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### D3$LEN

```
broadcast for fx: -D3LEN
macroname: D3$LEN
ex: (D3$LEN a ...) performs (mvc #'-D3LEN a[i] ...) for every row in a.

 ; VEQ:D3$LEN
 ;   [symbol]
 ;
 ; D3$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$LEN
 ;     broadcast for fx: -D3LEN
 ;     macroname: D3$LEN
 ;     ex: (D3$LEN a ...) performs (mvc #'-D3LEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$LEN2

```
broadcast for fx: -D3LEN2
macroname: D3$LEN2
ex: (D3$LEN2 a ...) performs (mvc #'-D3LEN2 a[i] ...) for every row in a.

 ; VEQ:D3$LEN2
 ;   [symbol]
 ;
 ; D3$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$LEN2
 ;     broadcast for fx: -D3LEN2
 ;     macroname: D3$LEN2
 ;     ex: (D3$LEN2 a ...) performs (mvc #'-D3LEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
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
 ;   Source file: /data/x/veq/src/array-mima.lisp
```

#### D3$NEG

```
broadcast for fx: -D3NEG
macroname: D3$NEG
ex: (D3$NEG a ...) performs (mvc #'-D3NEG a[i] ...) for every row in a.

 ; VEQ:D3$NEG
 ;   [symbol]
 ;
 ; D3$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$NEG
 ;     broadcast for fx: -D3NEG
 ;     macroname: D3$NEG
 ;     ex: (D3$NEG a ...) performs (mvc #'-D3NEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$NEG!

```
broadcast for fx: -D3NEG
macroname: D3$NEG!
ex: (D3$NEG! a ...) performs (mvc #'-D3NEG a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$NEG!
 ;   [symbol]
 ;
 ; D3$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$NEG!
 ;     broadcast for fx: -D3NEG
 ;     macroname: D3$NEG!
 ;     ex: (D3$NEG! a ...) performs (mvc #'-D3NEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$NORM

```
broadcast for fx: -D3NORM
macroname: D3$NORM
ex: (D3$NORM a ...) performs (mvc #'-D3NORM a[i] ...) for every row in a.

 ; VEQ:D3$NORM
 ;   [symbol]
 ;
 ; D3$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$NORM
 ;     broadcast for fx: -D3NORM
 ;     macroname: D3$NORM
 ;     ex: (D3$NORM a ...) performs (mvc #'-D3NORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$NORM!

```
broadcast for fx: -D3NORM
macroname: D3$NORM!
ex: (D3$NORM! a ...) performs (mvc #'-D3NORM a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$NORM!
 ;   [symbol]
 ;
 ; D3$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$NORM!
 ;     broadcast for fx: -D3NORM
 ;     macroname: D3$NORM!
 ;     ex: (D3$NORM! a ...) performs (mvc #'-D3NORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### D3$ROT

```
broadcast for fx: -D3ROT
macroname: D3$ROT
ex: (D3$ROT a ...) performs (mvc #'-D3ROT a[i] ...) for every row in a.

 ; VEQ:D3$ROT
 ;   [symbol]
 ;
 ; D3$ROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ROT
 ;     broadcast for fx: -D3ROT
 ;     macroname: D3$ROT
 ;     ex: (D3$ROT a ...) performs (mvc #'-D3ROT a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$ROT!

```
broadcast for fx: -D3ROT
macroname: D3$ROT!
ex: (D3$ROT! a ...) performs (mvc #'-D3ROT a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$ROT!
 ;   [symbol]
 ;
 ; D3$ROT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ROT!
 ;     broadcast for fx: -D3ROT
 ;     macroname: D3$ROT!
 ;     ex: (D3$ROT! a ...) performs (mvc #'-D3ROT a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$ROTS

```
broadcast for fx: -D3ROTS
macroname: D3$ROTS
ex: (D3$ROTS a ...) performs (mvc #'-D3ROTS a[i] ...) for every row in a.

 ; VEQ:D3$ROTS
 ;   [symbol]
 ;
 ; D3$ROTS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ROTS
 ;     broadcast for fx: -D3ROTS
 ;     macroname: D3$ROTS
 ;     ex: (D3$ROTS a ...) performs (mvc #'-D3ROTS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$ROTS!

```
broadcast for fx: -D3ROTS
macroname: D3$ROTS!
ex: (D3$ROTS! a ...) performs (mvc #'-D3ROTS a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$ROTS!
 ;   [symbol]
 ;
 ; D3$ROTS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$ROTS!
 ;     broadcast for fx: -D3ROTS
 ;     macroname: D3$ROTS!
 ;     ex: (D3$ROTS! a ...) performs (mvc #'-D3ROTS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: D3$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D3$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 3
```

#### D3$SCALE

```
broadcast for fx: -D3SCALE
macroname: D3$SCALE
ex: (D3$SCALE a ...) performs (mvc #'-D3SCALE a[i] ...) for every row in a.

 ; VEQ:D3$SCALE
 ;   [symbol]
 ;
 ; D3$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$SCALE
 ;     broadcast for fx: -D3SCALE
 ;     macroname: D3$SCALE
 ;     ex: (D3$SCALE a ...) performs (mvc #'-D3SCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D3$SCALE!

```
broadcast for fx: -D3SCALE
macroname: D3$SCALE!
ex: (D3$SCALE! a ...) performs (mvc #'-D3SCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D3$SCALE!
 ;   [symbol]
 ;
 ; D3$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3$SCALE!
 ;     broadcast for fx: -D3SCALE
 ;     macroname: D3$SCALE!
 ;     ex: (D3$SCALE! a ...) performs (mvc #'-D3SCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### D3$TAKE

```
returns 3d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D3$WITH-ROWS

```
make 3d
```

#### D3$ZERO

```
make 3d array of zeros.
typed.

 ; VEQ:D3$ZERO
 ;   [symbol]
 ;
 ; D3$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D3\*

```
veq context op: D3*
fxname: -D3*
args: (AX AY AZ BX BY BZ)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ))
```

#### :context: D3+

```
veq context op: D3+
fxname: -D3+
args: (AX AY AZ BX BY BZ)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ))
```

#### :context: D3-

```
veq context op: D3-
fxname: -D3-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ))
```

#### :context: D3.

```
veq context op: D3.
fxname: -D3.
args: (AX AY AZ BX BY BZ)
body: (+ (* AX BX) (* AY BY) (* AZ BZ))
```

#### :context: D3/

```
veq context op: D3/
fxname: -D3/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ))
```

#### :context: D3^

```
veq context op: D3^
fxname: -D3^
args: (A B C S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S))
```

#### :context: D3ABS

```
veq context op: D3ABS
fxname: -D3ABS
args: (A B C)
body: (VALUES (ABS A) (ABS B) (ABS C))
```

#### :context: D3CROSS

```
veq context op: D3CROSS
fxname: -D3CROSS
args: (AX AY AZ BX BY BZ)
body: (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
              (- (* AX BY) (* AY BX)))
```

#### :context: D3DST

```
veq context op: D3DST
fxname: -D3DST
args: (AX AY AZ BX BY BZ)
body: (SQRT (THE POS-DF (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))))
```

#### :context: D3DST2

```
veq context op: D3DST2
fxname: -D3DST2
args: (AX AY AZ BX BY BZ)
body: (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))
```

#### :context: D3EXP

```
veq context op: D3EXP
fxname: -D3EXP
args: (A B C)
body: (VALUES (EXP A) (EXP B) (EXP C))
```

#### :context: D3FROM

```
veq context op: D3FROM
fxname: -D3FROM
args: (AX AY AZ BX BY BZ S)
body: (-D3+ AX AY AZ (* BX S) (* BY S) (* BZ S))
```

#### :context: D3I-

```
veq context op: D3I-
fxname: -D3I-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ))
```

#### :context: D3I/

```
veq context op: D3I/
fxname: -D3I/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ))
```

#### :context: D3ISCALE

```
veq context op: D3ISCALE
fxname: -D3ISCALE
args: (A B C S)
body: (VALUES (/ A S) (/ B S) (/ C S))
```

#### :context: D3LEN

```
veq context op: D3LEN
fxname: -D3LEN
args: (A B C)
body: (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D3SQUARE A B C)))))
```

#### :context: D3LEN2

```
veq context op: D3LEN2
fxname: -D3LEN2
args: (A B C)
body: (THE POS-DF (MVC #'+ (-D3SQUARE A B C)))
```

#### :context: D3LERP

```
veq context op: D3LERP
fxname: -D3LERP
args: (AX AY AZ BX BY BZ S)
body: (-D3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S))
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
args: (A B C)
body: (MAX A B C)
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D3MID

```
veq context op: D3MID
fxname: -D3MID
args: (AX AY AZ BX BY BZ)
body: (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0))
```

#### :context: D3MIN

```
veq context op: D3MIN
fxname: -D3MIN
args: (A B C)
body: (MIN A B C)
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D3MOD

```
veq context op: D3MOD
fxname: -D3MOD
args: (A B C S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S))
```

#### D3MROT

```
docstring for %D3MROT
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:D3MROT
 ;   [symbol]
 ;
 ; D3MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3MROT
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### D3MROT\*

```
docstring for %D3MROT*
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:D3MROT*
 ;   [symbol]
 ;
 ; D3MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3MROT*
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### D3MSCALE

```
docstring for %D3MSCALE
make 3d matrix for scaling by x

 ; VEQ:D3MSCALE
 ;   [symbol]
 ;
 ; D3MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3MSCALE
 ;     make 3d matrix for scaling by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### D3MTRANS

```
docstring for %D3MTRANS
make 3d transpose matrix for moving by x

 ; VEQ:D3MTRANS
 ;   [symbol]
 ;
 ; D3MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D3MTRANS
 ;     make 3d transpose matrix for moving by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D3NEG

```
veq context op: D3NEG
fxname: -D3NEG
args: (A B C)
body: (VALUES (- A) (- B) (- C))
```

#### :context: D3NORM

```
veq context op: D3NORM
fxname: -D3NORM
args: (A B C)
body: (MVC #'-D3ISCALE A B C (THE POS-DF (MVC #'-D3LEN A B C)))
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
repeat the evaluated argument 3d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### :context: D3ROT

```
veq context op: D3ROT
fxname: -D3ROT
args: (X Y Z NX NY NZ A)
body: (LET ((COSA (COS A)))
        (DECLARE
         (DF
           COSA))
        (MVC #'-D3FROM
             (MVC #'-D3FROM (-D3SCALE X Y Z COSA) (-D3CROSS NX NY NZ X Y Z)
                  (SIN A))
             NX NY NZ (* (-D3. NX NY NZ X Y Z) (- 1.0d0 COSA))))
```

#### :context: D3ROTS

```
veq context op: D3ROTS
fxname: -D3ROTS
args: (X Y Z NX NY NZ A SX SY SZ)
body: (MVC #'-D3+ (MVC #'-D3ROT (-D3- X Y Z SX SY SZ) NX NY NZ A) SX SY SZ)
```

#### :context: D3SCALE

```
veq context op: D3SCALE
fxname: -D3SCALE
args: (A B C S)
body: (VALUES (* A S) (* B S) (* C S))
```

#### :context: D3SQRT

```
veq context op: D3SQRT
fxname: -D3SQRT
args: (A B C)
body: (VALUES (THE POS-DF (SQRT (THE POS-DF A)))
              (THE POS-DF (SQRT (THE POS-DF B)))
              (THE POS-DF (SQRT (THE POS-DF C))))
```

#### :context: D3SQUARE

```
veq context op: D3SQUARE
fxname: -D3SQUARE
args: (A B C)
body: (VALUES (THE POS-DF (* A A)) (THE POS-DF (* B B)) (THE POS-DF (* C C)))
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
coerce to type.
```

#### :context: D4

```
make 4d vector in veq context.
strict.
```

#### :context: D4$

```
returns values from 4d array.
supports multiple indices. default is 0.
ex: (D4$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### D4$\*

```
broadcast for fx: -D4*
macroname: D4$*
ex: (D4$* a ...) performs (mvc #'-D4* a[i] ...) for every row in a.

 ; VEQ:D4$*
 ;   [symbol]
 ;
 ; D4$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$*
 ;     broadcast for fx: -D4*
 ;     macroname: D4$*
 ;     ex: (D4$* a ...) performs (mvc #'-D4* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$\*!

```
broadcast for fx: -D4*
macroname: D4$*!
ex: (D4$*! a ...) performs (mvc #'-D4* a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$*!
 ;   [symbol]
 ;
 ; D4$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$*!
 ;     broadcast for fx: -D4*
 ;     macroname: D4$*!
 ;     ex: (D4$*! a ...) performs (mvc #'-D4* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$+

```
broadcast for fx: -D4+
macroname: D4$+
ex: (D4$+ a ...) performs (mvc #'-D4+ a[i] ...) for every row in a.

 ; VEQ:D4$+
 ;   [symbol]
 ;
 ; D4$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$+
 ;     broadcast for fx: -D4+
 ;     macroname: D4$+
 ;     ex: (D4$+ a ...) performs (mvc #'-D4+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$+!

```
broadcast for fx: -D4+
macroname: D4$+!
ex: (D4$+! a ...) performs (mvc #'-D4+ a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$+!
 ;   [symbol]
 ;
 ; D4$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$+!
 ;     broadcast for fx: -D4+
 ;     macroname: D4$+!
 ;     ex: (D4$+! a ...) performs (mvc #'-D4+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$-

```
broadcast for fx: -D4-
macroname: D4$-
ex: (D4$- a ...) performs (mvc #'-D4- a[i] ...) for every row in a.

 ; VEQ:D4$-
 ;   [symbol]
 ;
 ; D4$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$-
 ;     broadcast for fx: -D4-
 ;     macroname: D4$-
 ;     ex: (D4$- a ...) performs (mvc #'-D4- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$-!

```
broadcast for fx: -D4-
macroname: D4$-!
ex: (D4$-! a ...) performs (mvc #'-D4- a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$-!
 ;   [symbol]
 ;
 ; D4$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$-!
 ;     broadcast for fx: -D4-
 ;     macroname: D4$-!
 ;     ex: (D4$-! a ...) performs (mvc #'-D4- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$/

```
broadcast for fx: -D4/
macroname: D4$/
ex: (D4$/ a ...) performs (mvc #'-D4/ a[i] ...) for every row in a.

 ; VEQ:D4$/
 ;   [symbol]
 ;
 ; D4$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$/
 ;     broadcast for fx: -D4/
 ;     macroname: D4$/
 ;     ex: (D4$/ a ...) performs (mvc #'-D4/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$/!

```
broadcast for fx: -D4/
macroname: D4$/!
ex: (D4$/! a ...) performs (mvc #'-D4/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$/!
 ;   [symbol]
 ;
 ; D4$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$/!
 ;     broadcast for fx: -D4/
 ;     macroname: D4$/!
 ;     ex: (D4$/! a ...) performs (mvc #'-D4/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$ABS

```
broadcast for fx: -D4ABS
macroname: D4$ABS
ex: (D4$ABS a ...) performs (mvc #'-D4ABS a[i] ...) for every row in a.

 ; VEQ:D4$ABS
 ;   [symbol]
 ;
 ; D4$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$ABS
 ;     broadcast for fx: -D4ABS
 ;     macroname: D4$ABS
 ;     ex: (D4$ABS a ...) performs (mvc #'-D4ABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$ABS!

```
broadcast for fx: -D4ABS
macroname: D4$ABS!
ex: (D4$ABS! a ...) performs (mvc #'-D4ABS a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$ABS!
 ;   [symbol]
 ;
 ; D4$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$ABS!
 ;     broadcast for fx: -D4ABS
 ;     macroname: D4$ABS!
 ;     ex: (D4$ABS! a ...) performs (mvc #'-D4ABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$FROM

```
broadcast for fx: -D4FROM
macroname: D4$FROM
ex: (D4$FROM a ...) performs (mvc #'-D4FROM a[i] ...) for every row in a.

 ; VEQ:D4$FROM
 ;   [symbol]
 ;
 ; D4$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$FROM
 ;     broadcast for fx: -D4FROM
 ;     macroname: D4$FROM
 ;     ex: (D4$FROM a ...) performs (mvc #'-D4FROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$FROM!

```
broadcast for fx: -D4FROM
macroname: D4$FROM!
ex: (D4$FROM! a ...) performs (mvc #'-D4FROM a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$FROM!
 ;   [symbol]
 ;
 ; D4$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$FROM!
 ;     broadcast for fx: -D4FROM
 ;     macroname: D4$FROM!
 ;     ex: (D4$FROM! a ...) performs (mvc #'-D4FROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: D4$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (D4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

#### D4$I-

```
broadcast for fx: -D4I-
macroname: D4$I-
ex: (D4$I- a ...) performs (mvc #'-D4I- a[i] ...) for every row in a.

 ; VEQ:D4$I-
 ;   [symbol]
 ;
 ; D4$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$I-
 ;     broadcast for fx: -D4I-
 ;     macroname: D4$I-
 ;     ex: (D4$I- a ...) performs (mvc #'-D4I- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$I-!

```
broadcast for fx: -D4I-
macroname: D4$I-!
ex: (D4$I-! a ...) performs (mvc #'-D4I- a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$I-!
 ;   [symbol]
 ;
 ; D4$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$I-!
 ;     broadcast for fx: -D4I-
 ;     macroname: D4$I-!
 ;     ex: (D4$I-! a ...) performs (mvc #'-D4I- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$I/

```
broadcast for fx: -D4I/
macroname: D4$I/
ex: (D4$I/ a ...) performs (mvc #'-D4I/ a[i] ...) for every row in a.

 ; VEQ:D4$I/
 ;   [symbol]
 ;
 ; D4$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$I/
 ;     broadcast for fx: -D4I/
 ;     macroname: D4$I/
 ;     ex: (D4$I/ a ...) performs (mvc #'-D4I/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$I/!

```
broadcast for fx: -D4I/
macroname: D4$I/!
ex: (D4$I/! a ...) performs (mvc #'-D4I/ a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$I/!
 ;   [symbol]
 ;
 ; D4$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$I/!
 ;     broadcast for fx: -D4I/
 ;     macroname: D4$I/!
 ;     ex: (D4$I/! a ...) performs (mvc #'-D4I/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$ISCALE

```
broadcast for fx: -D4ISCALE
macroname: D4$ISCALE
ex: (D4$ISCALE a ...) performs (mvc #'-D4ISCALE a[i] ...) for every row in a.

 ; VEQ:D4$ISCALE
 ;   [symbol]
 ;
 ; D4$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$ISCALE
 ;     broadcast for fx: -D4ISCALE
 ;     macroname: D4$ISCALE
 ;     ex: (D4$ISCALE a ...) performs (mvc #'-D4ISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$ISCALE!

```
broadcast for fx: -D4ISCALE
macroname: D4$ISCALE!
ex: (D4$ISCALE! a ...) performs (mvc #'-D4ISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$ISCALE!
 ;   [symbol]
 ;
 ; D4$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$ISCALE!
 ;     broadcast for fx: -D4ISCALE
 ;     macroname: D4$ISCALE!
 ;     ex: (D4$ISCALE! a ...) performs (mvc #'-D4ISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$LAST

```
get last row of 4d array as (values ...)

 ; VEQ:D4$LAST
 ;   [symbol]
 ;
 ; D4$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
 ;                  (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
 ;                          DOUBLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get last row of 4d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### D4$LEN

```
broadcast for fx: -D4LEN
macroname: D4$LEN
ex: (D4$LEN a ...) performs (mvc #'-D4LEN a[i] ...) for every row in a.

 ; VEQ:D4$LEN
 ;   [symbol]
 ;
 ; D4$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$LEN
 ;     broadcast for fx: -D4LEN
 ;     macroname: D4$LEN
 ;     ex: (D4$LEN a ...) performs (mvc #'-D4LEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$LEN2

```
broadcast for fx: -D4LEN2
macroname: D4$LEN2
ex: (D4$LEN2 a ...) performs (mvc #'-D4LEN2 a[i] ...) for every row in a.

 ; VEQ:D4$LEN2
 ;   [symbol]
 ;
 ; D4$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$LEN2
 ;     broadcast for fx: -D4LEN2
 ;     macroname: D4$LEN2
 ;     ex: (D4$LEN2 a ...) performs (mvc #'-D4LEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
```

#### D4$NEG

```
broadcast for fx: -D4NEG
macroname: D4$NEG
ex: (D4$NEG a ...) performs (mvc #'-D4NEG a[i] ...) for every row in a.

 ; VEQ:D4$NEG
 ;   [symbol]
 ;
 ; D4$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$NEG
 ;     broadcast for fx: -D4NEG
 ;     macroname: D4$NEG
 ;     ex: (D4$NEG a ...) performs (mvc #'-D4NEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$NEG!

```
broadcast for fx: -D4NEG
macroname: D4$NEG!
ex: (D4$NEG! a ...) performs (mvc #'-D4NEG a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$NEG!
 ;   [symbol]
 ;
 ; D4$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$NEG!
 ;     broadcast for fx: -D4NEG
 ;     macroname: D4$NEG!
 ;     ex: (D4$NEG! a ...) performs (mvc #'-D4NEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$NORM

```
broadcast for fx: -D4NORM
macroname: D4$NORM
ex: (D4$NORM a ...) performs (mvc #'-D4NORM a[i] ...) for every row in a.

 ; VEQ:D4$NORM
 ;   [symbol]
 ;
 ; D4$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$NORM
 ;     broadcast for fx: -D4NORM
 ;     macroname: D4$NORM
 ;     ex: (D4$NORM a ...) performs (mvc #'-D4NORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$NORM!

```
broadcast for fx: -D4NORM
macroname: D4$NORM!
ex: (D4$NORM! a ...) performs (mvc #'-D4NORM a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$NORM!
 ;   [symbol]
 ;
 ; D4$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$NORM!
 ;     broadcast for fx: -D4NORM
 ;     macroname: D4$NORM!
 ;     ex: (D4$NORM! a ...) performs (mvc #'-D4NORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### :context: D4$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (D4$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are DVEC of dim 4
```

#### D4$SCALE

```
broadcast for fx: -D4SCALE
macroname: D4$SCALE
ex: (D4$SCALE a ...) performs (mvc #'-D4SCALE a[i] ...) for every row in a.

 ; VEQ:D4$SCALE
 ;   [symbol]
 ;
 ; D4$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$SCALE
 ;     broadcast for fx: -D4SCALE
 ;     macroname: D4$SCALE
 ;     ex: (D4$SCALE a ...) performs (mvc #'-D4SCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### D4$SCALE!

```
broadcast for fx: -D4SCALE
macroname: D4$SCALE!
ex: (D4$SCALE! a ...) performs (mvc #'-D4SCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:D4$SCALE!
 ;   [symbol]
 ;
 ; D4$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %D4$SCALE!
 ;     broadcast for fx: -D4SCALE
 ;     macroname: D4$SCALE!
 ;     ex: (D4$SCALE! a ...) performs (mvc #'-D4SCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### D4$TAKE

```
returns 4d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D4$WITH-ROWS

```
make 4d
```

#### D4$ZERO

```
make 4d array of zeros.
typed.

 ; VEQ:D4$ZERO
 ;   [symbol]
 ;
 ; D4$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: D4\*

```
veq context op: D4*
fxname: -D4*
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```

#### :context: D4+

```
veq context op: D4+
fxname: -D4+
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW))
```

#### :context: D4-

```
veq context op: D4-
fxname: -D4-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW))
```

#### :context: D4.

```
veq context op: D4.
fxname: -D4.
args: (AX AY AZ AW BX BY BZ BW)
body: (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```

#### :context: D4/

```
veq context op: D4/
fxname: -D4/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW))
```

#### :context: D4^

```
veq context op: D4^
fxname: -D4^
args: (A B C D S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S) (EXPT D S))
```

#### :context: D4ABS

```
veq context op: D4ABS
fxname: -D4ABS
args: (A B C D)
body: (VALUES (ABS A) (ABS B) (ABS C) (ABS D))
```

#### :context: D4DST

```
veq context op: D4DST
fxname: -D4DST
args: (AX AY AZ AW BX BY BZ BW)
body: (SQRT
       (THE POS-DF
            (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))))
```

#### :context: D4DST2

```
veq context op: D4DST2
fxname: -D4DST2
args: (AX AY AZ AW BX BY BZ BW)
body: (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))
```

#### :context: D4EXP

```
veq context op: D4EXP
fxname: -D4EXP
args: (A B C D)
body: (VALUES (EXP A) (EXP B) (EXP C) (EXP D))
```

#### :context: D4FROM

```
veq context op: D4FROM
fxname: -D4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body: (-D4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S))
```

#### :context: D4I-

```
veq context op: D4I-
fxname: -D4I-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))
```

#### :context: D4I/

```
veq context op: D4I/
fxname: -D4I/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW))
```

#### :context: D4ISCALE

```
veq context op: D4ISCALE
fxname: -D4ISCALE
args: (A B C D S)
body: (VALUES (/ A S) (/ B S) (/ C S) (/ D S))
```

#### :context: D4LEN

```
veq context op: D4LEN
fxname: -D4LEN
args: (A B C D)
body: (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D4SQUARE A B C D)))))
```

#### :context: D4LEN2

```
veq context op: D4LEN2
fxname: -D4LEN2
args: (A B C D)
body: (THE POS-DF (MVC #'+ (-D4SQUARE A B C D)))
```

#### :context: D4LERP

```
veq context op: D4LERP
fxname: -D4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body: (-D4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
       (* (- BW AW) S))
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
args: (A B C D)
body: (MAX A B C D)
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D4MID

```
veq context op: D4MID
fxname: -D4MID
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)
              (* (+ BW AW) 0.5d0))
```

#### :context: D4MIN

```
veq context op: D4MIN
fxname: -D4MIN
args: (A B C D)
body: (MIN A B C D)
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D4MOD

```
veq context op: D4MOD
fxname: -D4MOD
args: (A B C D S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S) (MOD D S))
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: D4NEG

```
veq context op: D4NEG
fxname: -D4NEG
args: (A B C D)
body: (VALUES (- A) (- B) (- C) (- D))
```

#### :context: D4NORM

```
veq context op: D4NORM
fxname: -D4NORM
args: (A B C D)
body: (MVC #'-D4ISCALE A B C D (THE POS-DF (MVC #'-D4LEN A B C D)))
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
repeat the evaluated argument 4d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### :context: D4SCALE

```
veq context op: D4SCALE
fxname: -D4SCALE
args: (A B C D S)
body: (VALUES (* A S) (* B S) (* C S) (* D S))
```

#### :context: D4SQRT

```
veq context op: D4SQRT
fxname: -D4SQRT
args: (A B C D)
body: (VALUES (THE POS-DF (SQRT (THE POS-DF A)))
              (THE POS-DF (SQRT (THE POS-DF B)))
              (THE POS-DF (SQRT (THE POS-DF C)))
              (THE POS-DF (SQRT (THE POS-DF D))))
```

#### :context: D4SQUARE

```
veq context op: D4SQUARE
fxname: -D4SQUARE
args: (A B C D)
body: (VALUES (THE POS-DF (* A A)) (THE POS-DF (* B B)) (THE POS-DF (* C C))
              (THE POS-DF (* D D)))
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
coerce to type.
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### :context: D^

```
veq context op: D^
fxname: -D^
args: (A S)
body: (EXPT A S)
```

#### D_

```
create dvec from body: (d_ '(1d0 2d0 3d0))

 ; VEQ:D_
 ;   [symbol]
 ;
 ; D_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create dvec from body: (d_ '(1d0 2d0 3d0))
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: DABS

```
veq context op: DABS
fxname: -DABS
args: (A)
body: (ABS A)
```

#### :context: DCLAMP

```
veq context op: DCLAMP
fxname: -DCLAMP
args: (X)
body: (MIN 1.0d0 (MAX 0.0d0 X))
```

#### :context: DCLAMP\*

```
veq context op: DCLAMP*
fxname: -DCLAMP*
args: (X MI MA)
body: (MIN MA (MAX MI X))
```

#### :context: DCOS-SIN

```
veq context op: DCOS-SIN
fxname: -DCOS-SIN
args: (A)
body: (VALUES (COS A) (SIN A))
```

#### :context: DDEG->RAD

```
veq context op: DDEG->RAD
fxname: -DDEG->RAD
args: (DEG)
body: (* DPI (/ DEG 180.0d0))
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/macros.lisp
```

#### :context: DEXP

```
veq context op: DEXP
fxname: -DEXP
args: (A)
body: (VALUES (EXP A))
```

#### DF

```
:missing:todo:

 ; VEQ:DF
 ;   [symbol]
 ;
 ; DF names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### :context: DFROM

```
veq context op: DFROM
fxname: -DFROM
args: (AX BX S)
body: (+ AX (* BX S))
```

#### :context: DI-

```
veq context op: DI-
fxname: -DI-
args: (A B)
body: (- B A)
```

#### :context: DI/

```
veq context op: DI/
fxname: -DI/
args: (A B)
body: (/ B A)
```

#### :context: DISCALE

```
veq context op: DISCALE
fxname: -DISCALE
args: (A S)
body: (VALUES (/ A S))
```

#### :context: DLEN

```
veq context op: DLEN
fxname: -DLEN
args: (A)
body: (THE POS-DF A)
```

#### :context: DLEN2

```
veq context op: DLEN2
fxname: -DLEN2
args: (A)
body: (THE POS-DF (MVC #'+ (-DSQUARE A)))
```

#### :context: DLERP

```
veq context op: DLERP
fxname: -DLERP
args: (AX BX S)
body: (+ AX (* (- BX AX) S))
```

#### :context: DMID

```
veq context op: DMID
fxname: -DMID
args: (AX BX)
body: (* 0.5d0 (+ AX BX))
```

#### :context: DMOD

```
veq context op: DMOD
fxname: -DMOD
args: (A S)
body: (MOD A S)
```

#### :context: DNEG

```
veq context op: DNEG
fxname: -DNEG
args: (A)
body: (- A)
```

#### :context: DNORM

```
veq context op: DNORM
fxname: -DNORM
args: (A)
body: (MVC #'-DISCALE A (MVC #'-DLEN A))
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
repeat the evaluated argument 1d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### DSB

```
:missing:todo:

 ; VEQ:DSB
 ;   [symbol]
 ;
 ; DSB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### :context: DSCALE

```
veq context op: DSCALE
fxname: -DSCALE
args: (A S)
body: (VALUES (* A S))
```

#### :context: DSIN-COS

```
veq context op: DSIN-COS
fxname: -DSIN-COS
args: (A)
body: (VALUES (SIN A) (COS A))
```

#### :context: DSQRT

```
veq context op: DSQRT
fxname: -DSQRT
args: (A)
body: (THE POS-DF (SQRT (THE POS-DF A)))
```

#### :context: DSQUARE

```
veq context op: DSQUARE
fxname: -DSQUARE
args: (A)
body: (* A A)
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
start is the first index. then n-1 more.
inds is indices to iterate. replaces n/start
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
coerce to type.
```

#### EXT-SYMBOLS?

```
list all external symbols in veq. use :verbose to inlcude docstring.  use
  :pretty to print verbose output to stdout in a readable form.

 ; VEQ:EXT-SYMBOLS?
 ;   [symbol]
 ;
 ; EXT-SYMBOLS? names a macro:
 ;   Lambda-list: (&OPTIONAL MODE)
 ;   Documentation:
 ;     list all external symbols in veq. use :verbose to inlcude docstring.  use
 ;       :pretty to print verbose output to stdout in a readable form.
 ;   Source file: /data/x/veq/src/docs.lisp
```

#### :context: F

```
make 1d vector in veq context.
strict.
```

#### :context: F$

```
returns values from 1d array.
supports multiple indices. default is 0.
ex: (F$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### F$\*

```
broadcast for fx: -F*
macroname: F$*
ex: (F$* a ...) performs (mvc #'-F* a[i] ...) for every row in a.

 ; VEQ:F$*
 ;   [symbol]
 ;
 ; F$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$*
 ;     broadcast for fx: -F*
 ;     macroname: F$*
 ;     ex: (F$* a ...) performs (mvc #'-F* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$\*!

```
broadcast for fx: -F*
macroname: F$*!
ex: (F$*! a ...) performs (mvc #'-F* a[i] ...) for every row in a.
destructive.

 ; VEQ:F$*!
 ;   [symbol]
 ;
 ; F$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$*!
 ;     broadcast for fx: -F*
 ;     macroname: F$*!
 ;     ex: (F$*! a ...) performs (mvc #'-F* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$+

```
broadcast for fx: -F+
macroname: F$+
ex: (F$+ a ...) performs (mvc #'-F+ a[i] ...) for every row in a.

 ; VEQ:F$+
 ;   [symbol]
 ;
 ; F$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$+
 ;     broadcast for fx: -F+
 ;     macroname: F$+
 ;     ex: (F$+ a ...) performs (mvc #'-F+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$+!

```
broadcast for fx: -F+
macroname: F$+!
ex: (F$+! a ...) performs (mvc #'-F+ a[i] ...) for every row in a.
destructive.

 ; VEQ:F$+!
 ;   [symbol]
 ;
 ; F$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$+!
 ;     broadcast for fx: -F+
 ;     macroname: F$+!
 ;     ex: (F$+! a ...) performs (mvc #'-F+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$-

```
broadcast for fx: -F-
macroname: F$-
ex: (F$- a ...) performs (mvc #'-F- a[i] ...) for every row in a.

 ; VEQ:F$-
 ;   [symbol]
 ;
 ; F$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$-
 ;     broadcast for fx: -F-
 ;     macroname: F$-
 ;     ex: (F$- a ...) performs (mvc #'-F- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$-!

```
broadcast for fx: -F-
macroname: F$-!
ex: (F$-! a ...) performs (mvc #'-F- a[i] ...) for every row in a.
destructive.

 ; VEQ:F$-!
 ;   [symbol]
 ;
 ; F$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$-!
 ;     broadcast for fx: -F-
 ;     macroname: F$-!
 ;     ex: (F$-! a ...) performs (mvc #'-F- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$/

```
broadcast for fx: -F/
macroname: F$/
ex: (F$/ a ...) performs (mvc #'-F/ a[i] ...) for every row in a.

 ; VEQ:F$/
 ;   [symbol]
 ;
 ; F$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$/
 ;     broadcast for fx: -F/
 ;     macroname: F$/
 ;     ex: (F$/ a ...) performs (mvc #'-F/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$/!

```
broadcast for fx: -F/
macroname: F$/!
ex: (F$/! a ...) performs (mvc #'-F/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F$/!
 ;   [symbol]
 ;
 ; F$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$/!
 ;     broadcast for fx: -F/
 ;     macroname: F$/!
 ;     ex: (F$/! a ...) performs (mvc #'-F/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$_

```
create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
   or: ($_ '((1d0 2d0) (1d0 2d0)))

 ; VEQ:F$_
 ;   [symbol]
 ;
 ; F$_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
 ;        or: ($_ '((1d0 2d0) (1d0 2d0)))
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### F$ABS

```
broadcast for fx: -FABS
macroname: F$ABS
ex: (F$ABS a ...) performs (mvc #'-FABS a[i] ...) for every row in a.

 ; VEQ:F$ABS
 ;   [symbol]
 ;
 ; F$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$ABS
 ;     broadcast for fx: -FABS
 ;     macroname: F$ABS
 ;     ex: (F$ABS a ...) performs (mvc #'-FABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$ABS!

```
broadcast for fx: -FABS
macroname: F$ABS!
ex: (F$ABS! a ...) performs (mvc #'-FABS a[i] ...) for every row in a.
destructive.

 ; VEQ:F$ABS!
 ;   [symbol]
 ;
 ; F$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$ABS!
 ;     broadcast for fx: -FABS
 ;     macroname: F$ABS!
 ;     ex: (F$ABS! a ...) performs (mvc #'-FABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$COPY

```
copy array

 ; VEQ:F$COPY
 ;   [symbol]
 ;
 ; F$COPY names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     copy array
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### F$COS-SIN

```
broadcast for fx: -FCOS-SIN
macroname: F$COS-SIN
ex: (F$COS-SIN a ...) performs (mvc #'-FCOS-SIN a[i] ...) for every row in a.

 ; VEQ:F$COS-SIN
 ;   [symbol]
 ;
 ; F$COS-SIN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$COS-SIN
 ;     broadcast for fx: -FCOS-SIN
 ;     macroname: F$COS-SIN
 ;     ex: (F$COS-SIN a ...) performs (mvc #'-FCOS-SIN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$FROM

```
broadcast for fx: -FFROM
macroname: F$FROM
ex: (F$FROM a ...) performs (mvc #'-FFROM a[i] ...) for every row in a.

 ; VEQ:F$FROM
 ;   [symbol]
 ;
 ; F$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$FROM
 ;     broadcast for fx: -FFROM
 ;     macroname: F$FROM
 ;     ex: (F$FROM a ...) performs (mvc #'-FFROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$FROM!

```
broadcast for fx: -FFROM
macroname: F$FROM!
ex: (F$FROM! a ...) performs (mvc #'-FFROM a[i] ...) for every row in a.
destructive.

 ; VEQ:F$FROM!
 ;   [symbol]
 ;
 ; F$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$FROM!
 ;     broadcast for fx: -FFROM
 ;     macroname: F$FROM!
 ;     ex: (F$FROM! a ...) performs (mvc #'-FFROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: F$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 1d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F$FXLSPACE (n a b) (lambda (i (:va 1 a b)) (vpr i a b)))
```

#### F$I-

```
broadcast for fx: -FI-
macroname: F$I-
ex: (F$I- a ...) performs (mvc #'-FI- a[i] ...) for every row in a.

 ; VEQ:F$I-
 ;   [symbol]
 ;
 ; F$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$I-
 ;     broadcast for fx: -FI-
 ;     macroname: F$I-
 ;     ex: (F$I- a ...) performs (mvc #'-FI- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$I-!

```
broadcast for fx: -FI-
macroname: F$I-!
ex: (F$I-! a ...) performs (mvc #'-FI- a[i] ...) for every row in a.
destructive.

 ; VEQ:F$I-!
 ;   [symbol]
 ;
 ; F$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$I-!
 ;     broadcast for fx: -FI-
 ;     macroname: F$I-!
 ;     ex: (F$I-! a ...) performs (mvc #'-FI- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$I/

```
broadcast for fx: -FI/
macroname: F$I/
ex: (F$I/ a ...) performs (mvc #'-FI/ a[i] ...) for every row in a.

 ; VEQ:F$I/
 ;   [symbol]
 ;
 ; F$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$I/
 ;     broadcast for fx: -FI/
 ;     macroname: F$I/
 ;     ex: (F$I/ a ...) performs (mvc #'-FI/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$I/!

```
broadcast for fx: -FI/
macroname: F$I/!
ex: (F$I/! a ...) performs (mvc #'-FI/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F$I/!
 ;   [symbol]
 ;
 ; F$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$I/!
 ;     broadcast for fx: -FI/
 ;     macroname: F$I/!
 ;     ex: (F$I/! a ...) performs (mvc #'-FI/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$ISCALE

```
broadcast for fx: -FISCALE
macroname: F$ISCALE
ex: (F$ISCALE a ...) performs (mvc #'-FISCALE a[i] ...) for every row in a.

 ; VEQ:F$ISCALE
 ;   [symbol]
 ;
 ; F$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$ISCALE
 ;     broadcast for fx: -FISCALE
 ;     macroname: F$ISCALE
 ;     ex: (F$ISCALE a ...) performs (mvc #'-FISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$ISCALE!

```
broadcast for fx: -FISCALE
macroname: F$ISCALE!
ex: (F$ISCALE! a ...) performs (mvc #'-FISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F$ISCALE!
 ;   [symbol]
 ;
 ; F$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$ISCALE!
 ;     broadcast for fx: -FISCALE
 ;     macroname: F$ISCALE!
 ;     ex: (F$ISCALE! a ...) performs (mvc #'-FISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$LAST

```
get last row of 1d array as (values ...)

 ; VEQ:F$LAST
 ;   [symbol]
 ;
 ; F$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get last row of 1d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### F$LEN

```
broadcast for fx: -FLEN
macroname: F$LEN
ex: (F$LEN a ...) performs (mvc #'-FLEN a[i] ...) for every row in a.

 ; VEQ:F$LEN
 ;   [symbol]
 ;
 ; F$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$LEN
 ;     broadcast for fx: -FLEN
 ;     macroname: F$LEN
 ;     ex: (F$LEN a ...) performs (mvc #'-FLEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$LEN2

```
broadcast for fx: -FLEN2
macroname: F$LEN2
ex: (F$LEN2 a ...) performs (mvc #'-FLEN2 a[i] ...) for every row in a.

 ; VEQ:F$LEN2
 ;   [symbol]
 ;
 ; F$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$LEN2
 ;     broadcast for fx: -FLEN2
 ;     macroname: F$LEN2
 ;     ex: (F$LEN2 a ...) performs (mvc #'-FLEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
```

#### F$MAKE

```
 create array with size (n dim), and initial value v

 ; VEQ:F$MAKE
 ;   [symbol]
 ;
 ; F$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0))
 ;   Documentation:
 ;      create array with size (n dim), and initial value v
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-mima.lisp
```

#### F$NEG

```
broadcast for fx: -FNEG
macroname: F$NEG
ex: (F$NEG a ...) performs (mvc #'-FNEG a[i] ...) for every row in a.

 ; VEQ:F$NEG
 ;   [symbol]
 ;
 ; F$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$NEG
 ;     broadcast for fx: -FNEG
 ;     macroname: F$NEG
 ;     ex: (F$NEG a ...) performs (mvc #'-FNEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$NEG!

```
broadcast for fx: -FNEG
macroname: F$NEG!
ex: (F$NEG! a ...) performs (mvc #'-FNEG a[i] ...) for every row in a.
destructive.

 ; VEQ:F$NEG!
 ;   [symbol]
 ;
 ; F$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$NEG!
 ;     broadcast for fx: -FNEG
 ;     macroname: F$NEG!
 ;     ex: (F$NEG! a ...) performs (mvc #'-FNEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$NORM

```
broadcast for fx: -FNORM
macroname: F$NORM
ex: (F$NORM a ...) performs (mvc #'-FNORM a[i] ...) for every row in a.

 ; VEQ:F$NORM
 ;   [symbol]
 ;
 ; F$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$NORM
 ;     broadcast for fx: -FNORM
 ;     macroname: F$NORM
 ;     ex: (F$NORM a ...) performs (mvc #'-FNORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$NORM!

```
broadcast for fx: -FNORM
macroname: F$NORM!
ex: (F$NORM! a ...) performs (mvc #'-FNORM a[i] ...) for every row in a.
destructive.

 ; VEQ:F$NORM!
 ;   [symbol]
 ;
 ; F$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$NORM!
 ;     broadcast for fx: -FNORM
 ;     macroname: F$NORM!
 ;     ex: (F$NORM! a ...) performs (mvc #'-FNORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### :context: F$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 1
```

#### F$SCALE

```
broadcast for fx: -FSCALE
macroname: F$SCALE
ex: (F$SCALE a ...) performs (mvc #'-FSCALE a[i] ...) for every row in a.

 ; VEQ:F$SCALE
 ;   [symbol]
 ;
 ; F$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$SCALE
 ;     broadcast for fx: -FSCALE
 ;     macroname: F$SCALE
 ;     ex: (F$SCALE a ...) performs (mvc #'-FSCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F$SCALE!

```
broadcast for fx: -FSCALE
macroname: F$SCALE!
ex: (F$SCALE! a ...) performs (mvc #'-FSCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F$SCALE!
 ;   [symbol]
 ;
 ; F$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F$SCALE!
 ;     broadcast for fx: -FSCALE
 ;     macroname: F$SCALE!
 ;     ex: (F$SCALE! a ...) performs (mvc #'-FSCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### F$TAKE

```
returns 1d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F$WITH-ROWS

```
make 1d
```

#### F$ZERO

```
make 1d array of zeros.
typed.

 ; VEQ:F$ZERO
 ;   [symbol]
 ;
 ; F$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 1d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F\*

```
veq context op: F*
fxname: -F*
args: (A B)
body: (* A B)
```

#### :context: F+

```
veq context op: F+
fxname: -F+
args: (A B)
body: (+ A B)
```

#### :context: F-

```
veq context op: F-
fxname: -F-
args: (A B)
body: (- A B)
```

#### :context: F/

```
veq context op: F/
fxname: -F/
args: (A B)
body: (/ A B)
```

#### :context: F2

```
make 2d vector in veq context.
strict.
```

#### :context: F2$

```
returns values from 2d array.
supports multiple indices. default is 0.
ex: (F2$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### F2$\*

```
broadcast for fx: -F2*
macroname: F2$*
ex: (F2$* a ...) performs (mvc #'-F2* a[i] ...) for every row in a.

 ; VEQ:F2$*
 ;   [symbol]
 ;
 ; F2$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$*
 ;     broadcast for fx: -F2*
 ;     macroname: F2$*
 ;     ex: (F2$* a ...) performs (mvc #'-F2* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$\*!

```
broadcast for fx: -F2*
macroname: F2$*!
ex: (F2$*! a ...) performs (mvc #'-F2* a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$*!
 ;   [symbol]
 ;
 ; F2$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$*!
 ;     broadcast for fx: -F2*
 ;     macroname: F2$*!
 ;     ex: (F2$*! a ...) performs (mvc #'-F2* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$+

```
broadcast for fx: -F2+
macroname: F2$+
ex: (F2$+ a ...) performs (mvc #'-F2+ a[i] ...) for every row in a.

 ; VEQ:F2$+
 ;   [symbol]
 ;
 ; F2$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$+
 ;     broadcast for fx: -F2+
 ;     macroname: F2$+
 ;     ex: (F2$+ a ...) performs (mvc #'-F2+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$+!

```
broadcast for fx: -F2+
macroname: F2$+!
ex: (F2$+! a ...) performs (mvc #'-F2+ a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$+!
 ;   [symbol]
 ;
 ; F2$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$+!
 ;     broadcast for fx: -F2+
 ;     macroname: F2$+!
 ;     ex: (F2$+! a ...) performs (mvc #'-F2+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$-

```
broadcast for fx: -F2-
macroname: F2$-
ex: (F2$- a ...) performs (mvc #'-F2- a[i] ...) for every row in a.

 ; VEQ:F2$-
 ;   [symbol]
 ;
 ; F2$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$-
 ;     broadcast for fx: -F2-
 ;     macroname: F2$-
 ;     ex: (F2$- a ...) performs (mvc #'-F2- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$-!

```
broadcast for fx: -F2-
macroname: F2$-!
ex: (F2$-! a ...) performs (mvc #'-F2- a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$-!
 ;   [symbol]
 ;
 ; F2$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$-!
 ;     broadcast for fx: -F2-
 ;     macroname: F2$-!
 ;     ex: (F2$-! a ...) performs (mvc #'-F2- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$/

```
broadcast for fx: -F2/
macroname: F2$/
ex: (F2$/ a ...) performs (mvc #'-F2/ a[i] ...) for every row in a.

 ; VEQ:F2$/
 ;   [symbol]
 ;
 ; F2$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$/
 ;     broadcast for fx: -F2/
 ;     macroname: F2$/
 ;     ex: (F2$/ a ...) performs (mvc #'-F2/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$/!

```
broadcast for fx: -F2/
macroname: F2$/!
ex: (F2$/! a ...) performs (mvc #'-F2/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$/!
 ;   [symbol]
 ;
 ; F2$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$/!
 ;     broadcast for fx: -F2/
 ;     macroname: F2$/!
 ;     ex: (F2$/! a ...) performs (mvc #'-F2/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$ABS

```
broadcast for fx: -F2ABS
macroname: F2$ABS
ex: (F2$ABS a ...) performs (mvc #'-F2ABS a[i] ...) for every row in a.

 ; VEQ:F2$ABS
 ;   [symbol]
 ;
 ; F2$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ABS
 ;     broadcast for fx: -F2ABS
 ;     macroname: F2$ABS
 ;     ex: (F2$ABS a ...) performs (mvc #'-F2ABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$ABS!

```
broadcast for fx: -F2ABS
macroname: F2$ABS!
ex: (F2$ABS! a ...) performs (mvc #'-F2ABS a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$ABS!
 ;   [symbol]
 ;
 ; F2$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ABS!
 ;     broadcast for fx: -F2ABS
 ;     macroname: F2$ABS!
 ;     ex: (F2$ABS! a ...) performs (mvc #'-F2ABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$CENTER

```
docstring for %F2$CENTER
center 2d array according to n points in array. n is optional.

 ; VEQ:F2$CENTER
 ;   [symbol]
 ;
 ; F2$CENTER names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$CENTER
 ;     center 2d array according to n points in array. n is optional.
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### F2$CIRC

```
docstring for %F2$CIRC
return circle of size rad. (rs 0.5) is vertex density.

 ; VEQ:F2$CIRC
 ;   [symbol]
 ;
 ; F2$CIRC names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$CIRC
 ;     return circle of size rad. (rs 0.5) is vertex density.
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### F2$FROM

```
broadcast for fx: -F2FROM
macroname: F2$FROM
ex: (F2$FROM a ...) performs (mvc #'-F2FROM a[i] ...) for every row in a.

 ; VEQ:F2$FROM
 ;   [symbol]
 ;
 ; F2$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$FROM
 ;     broadcast for fx: -F2FROM
 ;     macroname: F2$FROM
 ;     ex: (F2$FROM a ...) performs (mvc #'-F2FROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$FROM!

```
broadcast for fx: -F2FROM
macroname: F2$FROM!
ex: (F2$FROM! a ...) performs (mvc #'-F2FROM a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$FROM!
 ;   [symbol]
 ;
 ; F2$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$FROM!
 ;     broadcast for fx: -F2FROM
 ;     macroname: F2$FROM!
 ;     ex: (F2$FROM! a ...) performs (mvc #'-F2FROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: F2$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 2d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F2$FXLSPACE (n a b) (lambda (i (:va 2 a b)) (vpr i a b)))
```

#### F2$I-

```
broadcast for fx: -F2I-
macroname: F2$I-
ex: (F2$I- a ...) performs (mvc #'-F2I- a[i] ...) for every row in a.

 ; VEQ:F2$I-
 ;   [symbol]
 ;
 ; F2$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$I-
 ;     broadcast for fx: -F2I-
 ;     macroname: F2$I-
 ;     ex: (F2$I- a ...) performs (mvc #'-F2I- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$I-!

```
broadcast for fx: -F2I-
macroname: F2$I-!
ex: (F2$I-! a ...) performs (mvc #'-F2I- a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$I-!
 ;   [symbol]
 ;
 ; F2$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$I-!
 ;     broadcast for fx: -F2I-
 ;     macroname: F2$I-!
 ;     ex: (F2$I-! a ...) performs (mvc #'-F2I- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$I/

```
broadcast for fx: -F2I/
macroname: F2$I/
ex: (F2$I/ a ...) performs (mvc #'-F2I/ a[i] ...) for every row in a.

 ; VEQ:F2$I/
 ;   [symbol]
 ;
 ; F2$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$I/
 ;     broadcast for fx: -F2I/
 ;     macroname: F2$I/
 ;     ex: (F2$I/ a ...) performs (mvc #'-F2I/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$I/!

```
broadcast for fx: -F2I/
macroname: F2$I/!
ex: (F2$I/! a ...) performs (mvc #'-F2I/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$I/!
 ;   [symbol]
 ;
 ; F2$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$I/!
 ;     broadcast for fx: -F2I/
 ;     macroname: F2$I/!
 ;     ex: (F2$I/! a ...) performs (mvc #'-F2I/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$ISCALE

```
broadcast for fx: -F2ISCALE
macroname: F2$ISCALE
ex: (F2$ISCALE a ...) performs (mvc #'-F2ISCALE a[i] ...) for every row in a.

 ; VEQ:F2$ISCALE
 ;   [symbol]
 ;
 ; F2$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ISCALE
 ;     broadcast for fx: -F2ISCALE
 ;     macroname: F2$ISCALE
 ;     ex: (F2$ISCALE a ...) performs (mvc #'-F2ISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$ISCALE!

```
broadcast for fx: -F2ISCALE
macroname: F2$ISCALE!
ex: (F2$ISCALE! a ...) performs (mvc #'-F2ISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$ISCALE!
 ;   [symbol]
 ;
 ; F2$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ISCALE!
 ;     broadcast for fx: -F2ISCALE
 ;     macroname: F2$ISCALE!
 ;     ex: (F2$ISCALE! a ...) performs (mvc #'-F2ISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$LAST

```
get last row of 2d array as (values ...)

 ; VEQ:F2$LAST
 ;   [symbol]
 ;
 ; F2$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get last row of 2d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### F2$LEN

```
broadcast for fx: -F2LEN
macroname: F2$LEN
ex: (F2$LEN a ...) performs (mvc #'-F2LEN a[i] ...) for every row in a.

 ; VEQ:F2$LEN
 ;   [symbol]
 ;
 ; F2$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$LEN
 ;     broadcast for fx: -F2LEN
 ;     macroname: F2$LEN
 ;     ex: (F2$LEN a ...) performs (mvc #'-F2LEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$LEN2

```
broadcast for fx: -F2LEN2
macroname: F2$LEN2
ex: (F2$LEN2 a ...) performs (mvc #'-F2LEN2 a[i] ...) for every row in a.

 ; VEQ:F2$LEN2
 ;   [symbol]
 ;
 ; F2$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$LEN2
 ;     broadcast for fx: -F2LEN2
 ;     macroname: F2$LEN2
 ;     ex: (F2$LEN2 a ...) performs (mvc #'-F2LEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
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
 ;   Source file: /data/x/veq/src/array-mima.lisp
```

#### F2$NEG

```
broadcast for fx: -F2NEG
macroname: F2$NEG
ex: (F2$NEG a ...) performs (mvc #'-F2NEG a[i] ...) for every row in a.

 ; VEQ:F2$NEG
 ;   [symbol]
 ;
 ; F2$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$NEG
 ;     broadcast for fx: -F2NEG
 ;     macroname: F2$NEG
 ;     ex: (F2$NEG a ...) performs (mvc #'-F2NEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$NEG!

```
broadcast for fx: -F2NEG
macroname: F2$NEG!
ex: (F2$NEG! a ...) performs (mvc #'-F2NEG a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$NEG!
 ;   [symbol]
 ;
 ; F2$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$NEG!
 ;     broadcast for fx: -F2NEG
 ;     macroname: F2$NEG!
 ;     ex: (F2$NEG! a ...) performs (mvc #'-F2NEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$NORM

```
broadcast for fx: -F2NORM
macroname: F2$NORM
ex: (F2$NORM a ...) performs (mvc #'-F2NORM a[i] ...) for every row in a.

 ; VEQ:F2$NORM
 ;   [symbol]
 ;
 ; F2$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$NORM
 ;     broadcast for fx: -F2NORM
 ;     macroname: F2$NORM
 ;     ex: (F2$NORM a ...) performs (mvc #'-F2NORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$NORM!

```
broadcast for fx: -F2NORM
macroname: F2$NORM!
ex: (F2$NORM! a ...) performs (mvc #'-F2NORM a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$NORM!
 ;   [symbol]
 ;
 ; F2$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$NORM!
 ;     broadcast for fx: -F2NORM
 ;     macroname: F2$NORM!
 ;     ex: (F2$NORM! a ...) performs (mvc #'-F2NORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### F2$POLYGON

```
docstring for %F2$POLYGON
return n-polygon of size rad. rotate by (rot 0)

 ; VEQ:F2$POLYGON
 ;   [symbol]
 ;
 ; F2$POLYGON names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$POLYGON
 ;     return n-polygon of size rad. rotate by (rot 0)
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### F2$ROT

```
broadcast for fx: -F2ROT
macroname: F2$ROT
ex: (F2$ROT a ...) performs (mvc #'-F2ROT a[i] ...) for every row in a.

 ; VEQ:F2$ROT
 ;   [symbol]
 ;
 ; F2$ROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ROT
 ;     broadcast for fx: -F2ROT
 ;     macroname: F2$ROT
 ;     ex: (F2$ROT a ...) performs (mvc #'-F2ROT a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$ROT!

```
broadcast for fx: -F2ROT
macroname: F2$ROT!
ex: (F2$ROT! a ...) performs (mvc #'-F2ROT a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$ROT!
 ;   [symbol]
 ;
 ; F2$ROT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ROT!
 ;     broadcast for fx: -F2ROT
 ;     macroname: F2$ROT!
 ;     ex: (F2$ROT! a ...) performs (mvc #'-F2ROT a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$ROTS

```
broadcast for fx: -F2ROTS
macroname: F2$ROTS
ex: (F2$ROTS a ...) performs (mvc #'-F2ROTS a[i] ...) for every row in a.

 ; VEQ:F2$ROTS
 ;   [symbol]
 ;
 ; F2$ROTS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ROTS
 ;     broadcast for fx: -F2ROTS
 ;     macroname: F2$ROTS
 ;     ex: (F2$ROTS a ...) performs (mvc #'-F2ROTS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$ROTS!

```
broadcast for fx: -F2ROTS
macroname: F2$ROTS!
ex: (F2$ROTS! a ...) performs (mvc #'-F2ROTS a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$ROTS!
 ;   [symbol]
 ;
 ; F2$ROTS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$ROTS!
 ;     broadcast for fx: -F2ROTS
 ;     macroname: F2$ROTS!
 ;     ex: (F2$ROTS! a ...) performs (mvc #'-F2ROTS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: F2$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F2$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 2
```

#### F2$SCALE

```
broadcast for fx: -F2SCALE
macroname: F2$SCALE
ex: (F2$SCALE a ...) performs (mvc #'-F2SCALE a[i] ...) for every row in a.

 ; VEQ:F2$SCALE
 ;   [symbol]
 ;
 ; F2$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$SCALE
 ;     broadcast for fx: -F2SCALE
 ;     macroname: F2$SCALE
 ;     ex: (F2$SCALE a ...) performs (mvc #'-F2SCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$SCALE!

```
broadcast for fx: -F2SCALE
macroname: F2$SCALE!
ex: (F2$SCALE! a ...) performs (mvc #'-F2SCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F2$SCALE!
 ;   [symbol]
 ;
 ; F2$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2$SCALE!
 ;     broadcast for fx: -F2SCALE
 ;     macroname: F2$SCALE!
 ;     ex: (F2$SCALE! a ...) performs (mvc #'-F2SCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F2$SQUARE

```
fx: %F2$SQUARE
macro wrapper: F2$SQUARE
defined via veq:def*

 ; VEQ:F2$SQUARE
 ;   [symbol]
 ;
 ; F2$SQUARE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     fx: %F2$SQUARE
 ;     macro wrapper: F2$SQUARE
 ;     defined via veq:def*
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### F2$TAKE

```
returns 2d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F2$WITH-ROWS

```
make 2d
```

#### F2$ZERO

```
make 2d array of zeros.
typed.

 ; VEQ:F2$ZERO
 ;   [symbol]
 ;
 ; F2$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 2d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F2\*

```
veq context op: F2*
fxname: -F2*
args: (AX AY BX BY)
body: (VALUES (* AX BX) (* AY BY))
```

#### :context: F2+

```
veq context op: F2+
fxname: -F2+
args: (AX AY BX BY)
body: (VALUES (+ AX BX) (+ AY BY))
```

#### :context: F2-

```
veq context op: F2-
fxname: -F2-
args: (AX AY BX BY)
body: (VALUES (- AX BX) (- AY BY))
```

#### :context: F2.

```
veq context op: F2.
fxname: -F2.
args: (AX AY BX BY)
body: (+ (* AX BX) (* AY BY))
```

#### :context: F2/

```
veq context op: F2/
fxname: -F2/
args: (AX AY BX BY)
body: (VALUES (/ AX BX) (/ AY BY))
```

#### :context: F2^

```
veq context op: F2^
fxname: -F2^
args: (A B S)
body: (VALUES (EXPT A S) (EXPT B S))
```

#### :context: F2ABS

```
veq context op: F2ABS
fxname: -F2ABS
args: (A B)
body: (VALUES (ABS A) (ABS B))
```

#### :context: F2ANGLE

```
veq context op: F2ANGLE
fxname: -F2ANGLE
args: (A B)
body: (MVC #'ATAN (-F2NORM B A))
```

#### :context: F2CROSS

```
veq context op: F2CROSS
fxname: -F2CROSS
args: (AX AY BX BY)
body: (- (* AX BY) (* AY BX))
```

#### :context: F2DST

```
veq context op: F2DST
fxname: -F2DST
args: (AX AY BX BY)
body: (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY)))))
```

#### :context: F2DST2

```
veq context op: F2DST2
fxname: -F2DST2
args: (AX AY BX BY)
body: (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY)))
```

#### :context: F2EXP

```
veq context op: F2EXP
fxname: -F2EXP
args: (A B)
body: (VALUES (EXP A) (EXP B))
```

#### :context: F2FLIP

```
veq context op: F2FLIP
fxname: -F2FLIP
args: (A B)
body: (VALUES B A)
```

#### :context: F2FROM

```
veq context op: F2FROM
fxname: -F2FROM
args: (AX AY BX BY S)
body: (-F2+ AX AY (* BX S) (* BY S))
```

#### :context: F2I-

```
veq context op: F2I-
fxname: -F2I-
args: (AX AY BX BY)
body: (VALUES (- BX AX) (- BY AY))
```

#### :context: F2I/

```
veq context op: F2I/
fxname: -F2I/
args: (AX AY BX BY)
body: (VALUES (/ BX AX) (/ BY AY))
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
 ;   Source file: /data/x/veq/src/checks.lisp
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
 ;   Source file: /data/x/veq/src/checks.lisp
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

#### :context: F2ISCALE

```
veq context op: F2ISCALE
fxname: -F2ISCALE
args: (A B S)
body: (VALUES (/ A S) (/ B S))
```

#### :context: F2LEN

```
veq context op: F2LEN
fxname: -F2LEN
args: (A B)
body: (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE A B)))))
```

#### :context: F2LEN2

```
veq context op: F2LEN2
fxname: -F2LEN2
args: (A B)
body: (THE POS-FF (MVC #'+ (-F2SQUARE A B)))
```

#### :context: F2LERP

```
veq context op: F2LERP
fxname: -F2LERP
args: (AX AY BX BY S)
body: (-F2+ AX AY (* (- BX AX) S) (* (- BY AY) S))
```

#### :context: F2LET

```
make 2d let.
ex: (f3let ((a (f3 1f0 3f0 4f0))) ...)
note that this behaves like native lisp let*.
```

#### F2LSEGX

```
docstring for %F2LSEGX
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
 ;     docstring for %F2LSEGX
 ;     lines = #( #(ax ay bx by) ... )
 ;
 ;        not entirely slow line-line intersection for all lines. this is faster than
 ;        comparing all lines when lines are short relative to the area that the lines
 ;        cover. it can be improved further by using binary search tree to store
 ;        current state.
 ;   Source file: /data/x/veq/src/checks.lisp
```

#### :context: F2MAX

```
veq context op: F2MAX
fxname: -F2MAX
args: (A B)
body: (MAX A B)
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F2MID

```
veq context op: F2MID
fxname: -F2MID
args: (AX AY BX BY)
body: (VALUES (* 0.5 (+ AX BX)) (* 0.5 (+ AY BY)))
```

#### :context: F2MIN

```
veq context op: F2MIN
fxname: -F2MIN
args: (A B)
body: (MIN A B)
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F2MOD

```
veq context op: F2MOD
fxname: -F2MOD
args: (A B S)
body: (VALUES (MOD A S) (MOD B S))
```

#### F2MROT

```
docstring for %F2MROT
make 2d rotation matrix for rotating a rads

 ; VEQ:F2MROT
 ;   [symbol]
 ;
 ; F2MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2MROT
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### F2MROT\*

```
docstring for %F2MROT*
make 2d rotation matrix for rotating a rads

 ; VEQ:F2MROT*
 ;   [symbol]
 ;
 ; F2MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2MROT*
 ;     make 2d rotation matrix for rotating a rads
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### F2MSCALE

```
docstring for %F2MSCALE
make 2d matrix for scaling by x

 ; VEQ:F2MSCALE
 ;   [symbol]
 ;
 ; F2MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2MSCALE
 ;     make 2d matrix for scaling by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### F2MTRANS

```
docstring for %F2MTRANS
make 2d transpose matrix for moving by x

 ; VEQ:F2MTRANS
 ;   [symbol]
 ;
 ; F2MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2MTRANS
 ;     make 2d transpose matrix for moving by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F2NEG

```
veq context op: F2NEG
fxname: -F2NEG
args: (A B)
body: (VALUES (- A) (- B))
```

#### :context: F2NORM

```
veq context op: F2NORM
fxname: -F2NORM
args: (A B)
body: (MVC #'-F2ISCALE A B (MVC #'-F2LEN A B))
```

#### :context: F2NSUM

```
make 2d
```

#### :context: F2ON-CIRC

```
veq context op: F2ON-CIRC
fxname: -F2ON-CIRC
args: (A RAD)
body: (MVC #'-F2SCALE (-FCOS-SIN (* A FPII)) RAD)
```

#### :context: F2ON-CIRC\*

```
veq context op: F2ON-CIRC*
fxname: -F2ON-CIRC*
args: (A RAD)
body: (MVC #'-F2SCALE (-FCOS-SIN A) RAD)
```

#### :context: F2PERP

```
veq context op: F2PERP
fxname: -F2PERP
args: (A B)
body: (VALUES B (- A))
```

#### :context: F2PERP\*

```
veq context op: F2PERP*
fxname: -F2PERP*
args: (A B)
body: (VALUES (- B) A)
```

#### :context: F2REP

```
repeat argument 2d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: F2REP\*

```
repeat the evaluated argument 2d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### :context: F2ROT

```
veq context op: F2ROT
fxname: -F2ROT
args: (X Y A)
body: (LET ((COSA (COS A)) (SINA (SIN A)))
        (DECLARE
         (FF
           COSA
           SINA))
        (VALUES (- (* X COSA) (* Y SINA)) (+ (* X SINA) (* Y COSA))))
```

#### :context: F2ROTS

```
veq context op: F2ROTS
fxname: -F2ROTS
args: (X Y A SX SY)
body: (MVC #'-F2+ (MVC #'-F2ROT (-F2- X Y SX SY) A) SX SY)
```

#### :context: F2SCALE

```
veq context op: F2SCALE
fxname: -F2SCALE
args: (A B S)
body: (VALUES (* A S) (* B S))
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

#### F2SEGX

```
docstring for %F2SEGX
find intersection between lines (a1 a2), (b1 b2).
   returns isect? p q where p and q is the distance along each line to the
   intersection point

 ; VEQ:F2SEGX
 ;   [symbol]
 ;
 ; F2SEGX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F2SEGX
 ;     find intersection between lines (a1 a2), (b1 b2).
 ;        returns isect? p q where p and q is the distance along each line to the
 ;        intersection point
 ;   Source file: /data/x/veq/src/checks.lisp
```

#### :context: F2SQRT

```
veq context op: F2SQRT
fxname: -F2SQRT
args: (A B)
body: (VALUES (THE POS-FF (SQRT (THE POS-FF A)))
              (THE POS-FF (SQRT (THE POS-FF B))))
```

#### :context: F2SQUARE

```
veq context op: F2SQUARE
fxname: -F2SQUARE
args: (A B)
body: (VALUES (* A A) (* B B))
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
coerce to type.
```

#### :context: F3

```
make 3d vector in veq context.
strict.
```

#### :context: F3$

```
returns values from 3d array.
supports multiple indices. default is 0.
ex: (F3$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### F3$\*

```
broadcast for fx: -F3*
macroname: F3$*
ex: (F3$* a ...) performs (mvc #'-F3* a[i] ...) for every row in a.

 ; VEQ:F3$*
 ;   [symbol]
 ;
 ; F3$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$*
 ;     broadcast for fx: -F3*
 ;     macroname: F3$*
 ;     ex: (F3$* a ...) performs (mvc #'-F3* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$\*!

```
broadcast for fx: -F3*
macroname: F3$*!
ex: (F3$*! a ...) performs (mvc #'-F3* a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$*!
 ;   [symbol]
 ;
 ; F3$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$*!
 ;     broadcast for fx: -F3*
 ;     macroname: F3$*!
 ;     ex: (F3$*! a ...) performs (mvc #'-F3* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$+

```
broadcast for fx: -F3+
macroname: F3$+
ex: (F3$+ a ...) performs (mvc #'-F3+ a[i] ...) for every row in a.

 ; VEQ:F3$+
 ;   [symbol]
 ;
 ; F3$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$+
 ;     broadcast for fx: -F3+
 ;     macroname: F3$+
 ;     ex: (F3$+ a ...) performs (mvc #'-F3+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$+!

```
broadcast for fx: -F3+
macroname: F3$+!
ex: (F3$+! a ...) performs (mvc #'-F3+ a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$+!
 ;   [symbol]
 ;
 ; F3$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$+!
 ;     broadcast for fx: -F3+
 ;     macroname: F3$+!
 ;     ex: (F3$+! a ...) performs (mvc #'-F3+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$-

```
broadcast for fx: -F3-
macroname: F3$-
ex: (F3$- a ...) performs (mvc #'-F3- a[i] ...) for every row in a.

 ; VEQ:F3$-
 ;   [symbol]
 ;
 ; F3$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$-
 ;     broadcast for fx: -F3-
 ;     macroname: F3$-
 ;     ex: (F3$- a ...) performs (mvc #'-F3- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$-!

```
broadcast for fx: -F3-
macroname: F3$-!
ex: (F3$-! a ...) performs (mvc #'-F3- a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$-!
 ;   [symbol]
 ;
 ; F3$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$-!
 ;     broadcast for fx: -F3-
 ;     macroname: F3$-!
 ;     ex: (F3$-! a ...) performs (mvc #'-F3- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$/

```
broadcast for fx: -F3/
macroname: F3$/
ex: (F3$/ a ...) performs (mvc #'-F3/ a[i] ...) for every row in a.

 ; VEQ:F3$/
 ;   [symbol]
 ;
 ; F3$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$/
 ;     broadcast for fx: -F3/
 ;     macroname: F3$/
 ;     ex: (F3$/ a ...) performs (mvc #'-F3/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$/!

```
broadcast for fx: -F3/
macroname: F3$/!
ex: (F3$/! a ...) performs (mvc #'-F3/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$/!
 ;   [symbol]
 ;
 ; F3$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$/!
 ;     broadcast for fx: -F3/
 ;     macroname: F3$/!
 ;     ex: (F3$/! a ...) performs (mvc #'-F3/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$ABS

```
broadcast for fx: -F3ABS
macroname: F3$ABS
ex: (F3$ABS a ...) performs (mvc #'-F3ABS a[i] ...) for every row in a.

 ; VEQ:F3$ABS
 ;   [symbol]
 ;
 ; F3$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ABS
 ;     broadcast for fx: -F3ABS
 ;     macroname: F3$ABS
 ;     ex: (F3$ABS a ...) performs (mvc #'-F3ABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$ABS!

```
broadcast for fx: -F3ABS
macroname: F3$ABS!
ex: (F3$ABS! a ...) performs (mvc #'-F3ABS a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$ABS!
 ;   [symbol]
 ;
 ; F3$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ABS!
 ;     broadcast for fx: -F3ABS
 ;     macroname: F3$ABS!
 ;     ex: (F3$ABS! a ...) performs (mvc #'-F3ABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$FROM

```
broadcast for fx: -F3FROM
macroname: F3$FROM
ex: (F3$FROM a ...) performs (mvc #'-F3FROM a[i] ...) for every row in a.

 ; VEQ:F3$FROM
 ;   [symbol]
 ;
 ; F3$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$FROM
 ;     broadcast for fx: -F3FROM
 ;     macroname: F3$FROM
 ;     ex: (F3$FROM a ...) performs (mvc #'-F3FROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$FROM!

```
broadcast for fx: -F3FROM
macroname: F3$FROM!
ex: (F3$FROM! a ...) performs (mvc #'-F3FROM a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$FROM!
 ;   [symbol]
 ;
 ; F3$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$FROM!
 ;     broadcast for fx: -F3FROM
 ;     macroname: F3$FROM!
 ;     ex: (F3$FROM! a ...) performs (mvc #'-F3FROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: F3$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 3d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F3$FXLSPACE (n a b) (lambda (i (:va 3 a b)) (vpr i a b)))
```

#### F3$I-

```
broadcast for fx: -F3I-
macroname: F3$I-
ex: (F3$I- a ...) performs (mvc #'-F3I- a[i] ...) for every row in a.

 ; VEQ:F3$I-
 ;   [symbol]
 ;
 ; F3$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$I-
 ;     broadcast for fx: -F3I-
 ;     macroname: F3$I-
 ;     ex: (F3$I- a ...) performs (mvc #'-F3I- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$I-!

```
broadcast for fx: -F3I-
macroname: F3$I-!
ex: (F3$I-! a ...) performs (mvc #'-F3I- a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$I-!
 ;   [symbol]
 ;
 ; F3$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$I-!
 ;     broadcast for fx: -F3I-
 ;     macroname: F3$I-!
 ;     ex: (F3$I-! a ...) performs (mvc #'-F3I- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$I/

```
broadcast for fx: -F3I/
macroname: F3$I/
ex: (F3$I/ a ...) performs (mvc #'-F3I/ a[i] ...) for every row in a.

 ; VEQ:F3$I/
 ;   [symbol]
 ;
 ; F3$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$I/
 ;     broadcast for fx: -F3I/
 ;     macroname: F3$I/
 ;     ex: (F3$I/ a ...) performs (mvc #'-F3I/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$I/!

```
broadcast for fx: -F3I/
macroname: F3$I/!
ex: (F3$I/! a ...) performs (mvc #'-F3I/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$I/!
 ;   [symbol]
 ;
 ; F3$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$I/!
 ;     broadcast for fx: -F3I/
 ;     macroname: F3$I/!
 ;     ex: (F3$I/! a ...) performs (mvc #'-F3I/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$ISCALE

```
broadcast for fx: -F3ISCALE
macroname: F3$ISCALE
ex: (F3$ISCALE a ...) performs (mvc #'-F3ISCALE a[i] ...) for every row in a.

 ; VEQ:F3$ISCALE
 ;   [symbol]
 ;
 ; F3$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ISCALE
 ;     broadcast for fx: -F3ISCALE
 ;     macroname: F3$ISCALE
 ;     ex: (F3$ISCALE a ...) performs (mvc #'-F3ISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$ISCALE!

```
broadcast for fx: -F3ISCALE
macroname: F3$ISCALE!
ex: (F3$ISCALE! a ...) performs (mvc #'-F3ISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$ISCALE!
 ;   [symbol]
 ;
 ; F3$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ISCALE!
 ;     broadcast for fx: -F3ISCALE
 ;     macroname: F3$ISCALE!
 ;     ex: (F3$ISCALE! a ...) performs (mvc #'-F3ISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$LAST

```
get last row of 3d array as (values ...)

 ; VEQ:F3$LAST
 ;   [symbol]
 ;
 ; F3$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get last row of 3d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### F3$LEN

```
broadcast for fx: -F3LEN
macroname: F3$LEN
ex: (F3$LEN a ...) performs (mvc #'-F3LEN a[i] ...) for every row in a.

 ; VEQ:F3$LEN
 ;   [symbol]
 ;
 ; F3$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$LEN
 ;     broadcast for fx: -F3LEN
 ;     macroname: F3$LEN
 ;     ex: (F3$LEN a ...) performs (mvc #'-F3LEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$LEN2

```
broadcast for fx: -F3LEN2
macroname: F3$LEN2
ex: (F3$LEN2 a ...) performs (mvc #'-F3LEN2 a[i] ...) for every row in a.

 ; VEQ:F3$LEN2
 ;   [symbol]
 ;
 ; F3$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$LEN2
 ;     broadcast for fx: -F3LEN2
 ;     macroname: F3$LEN2
 ;     ex: (F3$LEN2 a ...) performs (mvc #'-F3LEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
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
 ;   Source file: /data/x/veq/src/array-mima.lisp
```

#### F3$NEG

```
broadcast for fx: -F3NEG
macroname: F3$NEG
ex: (F3$NEG a ...) performs (mvc #'-F3NEG a[i] ...) for every row in a.

 ; VEQ:F3$NEG
 ;   [symbol]
 ;
 ; F3$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$NEG
 ;     broadcast for fx: -F3NEG
 ;     macroname: F3$NEG
 ;     ex: (F3$NEG a ...) performs (mvc #'-F3NEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$NEG!

```
broadcast for fx: -F3NEG
macroname: F3$NEG!
ex: (F3$NEG! a ...) performs (mvc #'-F3NEG a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$NEG!
 ;   [symbol]
 ;
 ; F3$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$NEG!
 ;     broadcast for fx: -F3NEG
 ;     macroname: F3$NEG!
 ;     ex: (F3$NEG! a ...) performs (mvc #'-F3NEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$NORM

```
broadcast for fx: -F3NORM
macroname: F3$NORM
ex: (F3$NORM a ...) performs (mvc #'-F3NORM a[i] ...) for every row in a.

 ; VEQ:F3$NORM
 ;   [symbol]
 ;
 ; F3$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$NORM
 ;     broadcast for fx: -F3NORM
 ;     macroname: F3$NORM
 ;     ex: (F3$NORM a ...) performs (mvc #'-F3NORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$NORM!

```
broadcast for fx: -F3NORM
macroname: F3$NORM!
ex: (F3$NORM! a ...) performs (mvc #'-F3NORM a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$NORM!
 ;   [symbol]
 ;
 ; F3$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$NORM!
 ;     broadcast for fx: -F3NORM
 ;     macroname: F3$NORM!
 ;     ex: (F3$NORM! a ...) performs (mvc #'-F3NORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### F3$ROT

```
broadcast for fx: -F3ROT
macroname: F3$ROT
ex: (F3$ROT a ...) performs (mvc #'-F3ROT a[i] ...) for every row in a.

 ; VEQ:F3$ROT
 ;   [symbol]
 ;
 ; F3$ROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ROT
 ;     broadcast for fx: -F3ROT
 ;     macroname: F3$ROT
 ;     ex: (F3$ROT a ...) performs (mvc #'-F3ROT a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$ROT!

```
broadcast for fx: -F3ROT
macroname: F3$ROT!
ex: (F3$ROT! a ...) performs (mvc #'-F3ROT a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$ROT!
 ;   [symbol]
 ;
 ; F3$ROT! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ROT!
 ;     broadcast for fx: -F3ROT
 ;     macroname: F3$ROT!
 ;     ex: (F3$ROT! a ...) performs (mvc #'-F3ROT a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$ROTS

```
broadcast for fx: -F3ROTS
macroname: F3$ROTS
ex: (F3$ROTS a ...) performs (mvc #'-F3ROTS a[i] ...) for every row in a.

 ; VEQ:F3$ROTS
 ;   [symbol]
 ;
 ; F3$ROTS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ROTS
 ;     broadcast for fx: -F3ROTS
 ;     macroname: F3$ROTS
 ;     ex: (F3$ROTS a ...) performs (mvc #'-F3ROTS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$ROTS!

```
broadcast for fx: -F3ROTS
macroname: F3$ROTS!
ex: (F3$ROTS! a ...) performs (mvc #'-F3ROTS a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$ROTS!
 ;   [symbol]
 ;
 ; F3$ROTS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$ROTS!
 ;     broadcast for fx: -F3ROTS
 ;     macroname: F3$ROTS!
 ;     ex: (F3$ROTS! a ...) performs (mvc #'-F3ROTS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: F3$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F3$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 3
```

#### F3$SCALE

```
broadcast for fx: -F3SCALE
macroname: F3$SCALE
ex: (F3$SCALE a ...) performs (mvc #'-F3SCALE a[i] ...) for every row in a.

 ; VEQ:F3$SCALE
 ;   [symbol]
 ;
 ; F3$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$SCALE
 ;     broadcast for fx: -F3SCALE
 ;     macroname: F3$SCALE
 ;     ex: (F3$SCALE a ...) performs (mvc #'-F3SCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F3$SCALE!

```
broadcast for fx: -F3SCALE
macroname: F3$SCALE!
ex: (F3$SCALE! a ...) performs (mvc #'-F3SCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F3$SCALE!
 ;   [symbol]
 ;
 ; F3$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3$SCALE!
 ;     broadcast for fx: -F3SCALE
 ;     macroname: F3$SCALE!
 ;     ex: (F3$SCALE! a ...) performs (mvc #'-F3SCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### F3$TAKE

```
returns 3d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F3$WITH-ROWS

```
make 3d
```

#### F3$ZERO

```
make 3d array of zeros.
typed.

 ; VEQ:F3$ZERO
 ;   [symbol]
 ;
 ; F3$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 3d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F3\*

```
veq context op: F3*
fxname: -F3*
args: (AX AY AZ BX BY BZ)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ))
```

#### :context: F3+

```
veq context op: F3+
fxname: -F3+
args: (AX AY AZ BX BY BZ)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ))
```

#### :context: F3-

```
veq context op: F3-
fxname: -F3-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ))
```

#### :context: F3.

```
veq context op: F3.
fxname: -F3.
args: (AX AY AZ BX BY BZ)
body: (+ (* AX BX) (* AY BY) (* AZ BZ))
```

#### :context: F3/

```
veq context op: F3/
fxname: -F3/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ))
```

#### :context: F3^

```
veq context op: F3^
fxname: -F3^
args: (A B C S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S))
```

#### :context: F3ABS

```
veq context op: F3ABS
fxname: -F3ABS
args: (A B C)
body: (VALUES (ABS A) (ABS B) (ABS C))
```

#### :context: F3CROSS

```
veq context op: F3CROSS
fxname: -F3CROSS
args: (AX AY AZ BX BY BZ)
body: (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ))
              (- (* AX BY) (* AY BX)))
```

#### :context: F3DST

```
veq context op: F3DST
fxname: -F3DST
args: (AX AY AZ BX BY BZ)
body: (SQRT (THE POS-FF (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))))
```

#### :context: F3DST2

```
veq context op: F3DST2
fxname: -F3DST2
args: (AX AY AZ BX BY BZ)
body: (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))
```

#### :context: F3EXP

```
veq context op: F3EXP
fxname: -F3EXP
args: (A B C)
body: (VALUES (EXP A) (EXP B) (EXP C))
```

#### :context: F3FROM

```
veq context op: F3FROM
fxname: -F3FROM
args: (AX AY AZ BX BY BZ S)
body: (-F3+ AX AY AZ (* BX S) (* BY S) (* BZ S))
```

#### :context: F3I-

```
veq context op: F3I-
fxname: -F3I-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ))
```

#### :context: F3I/

```
veq context op: F3I/
fxname: -F3I/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ))
```

#### :context: F3ISCALE

```
veq context op: F3ISCALE
fxname: -F3ISCALE
args: (A B C S)
body: (VALUES (/ A S) (/ B S) (/ C S))
```

#### :context: F3LEN

```
veq context op: F3LEN
fxname: -F3LEN
args: (A B C)
body: (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F3SQUARE A B C)))))
```

#### :context: F3LEN2

```
veq context op: F3LEN2
fxname: -F3LEN2
args: (A B C)
body: (THE POS-FF (MVC #'+ (-F3SQUARE A B C)))
```

#### :context: F3LERP

```
veq context op: F3LERP
fxname: -F3LERP
args: (AX AY AZ BX BY BZ S)
body: (-F3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S))
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
args: (A B C)
body: (MAX A B C)
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F3MID

```
veq context op: F3MID
fxname: -F3MID
args: (AX AY AZ BX BY BZ)
body: (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5))
```

#### :context: F3MIN

```
veq context op: F3MIN
fxname: -F3MIN
args: (A B C)
body: (MIN A B C)
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F3MOD

```
veq context op: F3MOD
fxname: -F3MOD
args: (A B C S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S))
```

#### F3MROT

```
docstring for %F3MROT
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:F3MROT
 ;   [symbol]
 ;
 ; F3MROT names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3MROT
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### F3MROT\*

```
docstring for %F3MROT*
make 3d rotation matrix for rotating a rad around unit vector (x y z)

 ; VEQ:F3MROT*
 ;   [symbol]
 ;
 ; F3MROT* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3MROT*
 ;     make 3d rotation matrix for rotating a rad around unit vector (x y z)
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### F3MSCALE

```
docstring for %F3MSCALE
make 3d matrix for scaling by x

 ; VEQ:F3MSCALE
 ;   [symbol]
 ;
 ; F3MSCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3MSCALE
 ;     make 3d matrix for scaling by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### F3MTRANS

```
docstring for %F3MTRANS
make 3d transpose matrix for moving by x

 ; VEQ:F3MTRANS
 ;   [symbol]
 ;
 ; F3MTRANS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3MTRANS
 ;     make 3d transpose matrix for moving by x
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F3NEG

```
veq context op: F3NEG
fxname: -F3NEG
args: (A B C)
body: (VALUES (- A) (- B) (- C))
```

#### :context: F3NORM

```
veq context op: F3NORM
fxname: -F3NORM
args: (A B C)
body: (MVC #'-F3ISCALE A B C (THE POS-FF (MVC #'-F3LEN A B C)))
```

#### :context: F3NSUM

```
make 3d
```

#### F3PLANEX

```
docstring for %F3PLANEX
intersection of plane (n:normal, p:point) and line (a b)

 ; VEQ:F3PLANEX
 ;   [symbol]
 ;
 ; F3PLANEX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F3PLANEX
 ;     intersection of plane (n:normal, p:point) and line (a b)
 ;   Source file: /data/x/veq/src/checks.lisp
```

#### :context: F3REP

```
repeat argument 3d times as values.
ex: (f3rep (fx)) corresponds to (values (fx) (fx) (fx)).
```

#### :context: F3REP\*

```
repeat the evaluated argument 3d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### :context: F3ROT

```
veq context op: F3ROT
fxname: -F3ROT
args: (X Y Z NX NY NZ A)
body: (LET ((COSA (COS A)))
        (DECLARE
         (FF
           COSA))
        (MVC #'-F3FROM
             (MVC #'-F3FROM (-F3SCALE X Y Z COSA) (-F3CROSS NX NY NZ X Y Z)
                  (SIN A))
             NX NY NZ (* (-F3. NX NY NZ X Y Z) (- 1.0 COSA))))
```

#### :context: F3ROTS

```
veq context op: F3ROTS
fxname: -F3ROTS
args: (X Y Z NX NY NZ A SX SY SZ)
body: (MVC #'-F3+ (MVC #'-F3ROT (-F3- X Y Z SX SY SZ) NX NY NZ A) SX SY SZ)
```

#### :context: F3SCALE

```
veq context op: F3SCALE
fxname: -F3SCALE
args: (A B C S)
body: (VALUES (* A S) (* B S) (* C S))
```

#### :context: F3SQRT

```
veq context op: F3SQRT
fxname: -F3SQRT
args: (A B C)
body: (VALUES (THE POS-FF (SQRT (THE POS-FF A)))
              (THE POS-FF (SQRT (THE POS-FF B)))
              (THE POS-FF (SQRT (THE POS-FF C))))
```

#### :context: F3SQUARE

```
veq context op: F3SQUARE
fxname: -F3SQUARE
args: (A B C)
body: (VALUES (THE POS-FF (* A A)) (THE POS-FF (* B B)) (THE POS-FF (* C C)))
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
coerce to type.
```

#### :context: F4

```
make 4d vector in veq context.
strict.
```

#### :context: F4$

```
returns values from 4d array.
supports multiple indices. default is 0.
ex: (F4$ a i j ...) returns (values a[i] a[j] ...).
note that the number of values depends on the dimension.
```

#### F4$\*

```
broadcast for fx: -F4*
macroname: F4$*
ex: (F4$* a ...) performs (mvc #'-F4* a[i] ...) for every row in a.

 ; VEQ:F4$*
 ;   [symbol]
 ;
 ; F4$* names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$*
 ;     broadcast for fx: -F4*
 ;     macroname: F4$*
 ;     ex: (F4$* a ...) performs (mvc #'-F4* a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$\*!

```
broadcast for fx: -F4*
macroname: F4$*!
ex: (F4$*! a ...) performs (mvc #'-F4* a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$*!
 ;   [symbol]
 ;
 ; F4$*! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$*!
 ;     broadcast for fx: -F4*
 ;     macroname: F4$*!
 ;     ex: (F4$*! a ...) performs (mvc #'-F4* a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$+

```
broadcast for fx: -F4+
macroname: F4$+
ex: (F4$+ a ...) performs (mvc #'-F4+ a[i] ...) for every row in a.

 ; VEQ:F4$+
 ;   [symbol]
 ;
 ; F4$+ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$+
 ;     broadcast for fx: -F4+
 ;     macroname: F4$+
 ;     ex: (F4$+ a ...) performs (mvc #'-F4+ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$+!

```
broadcast for fx: -F4+
macroname: F4$+!
ex: (F4$+! a ...) performs (mvc #'-F4+ a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$+!
 ;   [symbol]
 ;
 ; F4$+! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$+!
 ;     broadcast for fx: -F4+
 ;     macroname: F4$+!
 ;     ex: (F4$+! a ...) performs (mvc #'-F4+ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$-

```
broadcast for fx: -F4-
macroname: F4$-
ex: (F4$- a ...) performs (mvc #'-F4- a[i] ...) for every row in a.

 ; VEQ:F4$-
 ;   [symbol]
 ;
 ; F4$- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$-
 ;     broadcast for fx: -F4-
 ;     macroname: F4$-
 ;     ex: (F4$- a ...) performs (mvc #'-F4- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$-!

```
broadcast for fx: -F4-
macroname: F4$-!
ex: (F4$-! a ...) performs (mvc #'-F4- a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$-!
 ;   [symbol]
 ;
 ; F4$-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$-!
 ;     broadcast for fx: -F4-
 ;     macroname: F4$-!
 ;     ex: (F4$-! a ...) performs (mvc #'-F4- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$/

```
broadcast for fx: -F4/
macroname: F4$/
ex: (F4$/ a ...) performs (mvc #'-F4/ a[i] ...) for every row in a.

 ; VEQ:F4$/
 ;   [symbol]
 ;
 ; F4$/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$/
 ;     broadcast for fx: -F4/
 ;     macroname: F4$/
 ;     ex: (F4$/ a ...) performs (mvc #'-F4/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$/!

```
broadcast for fx: -F4/
macroname: F4$/!
ex: (F4$/! a ...) performs (mvc #'-F4/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$/!
 ;   [symbol]
 ;
 ; F4$/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$/!
 ;     broadcast for fx: -F4/
 ;     macroname: F4$/!
 ;     ex: (F4$/! a ...) performs (mvc #'-F4/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$ABS

```
broadcast for fx: -F4ABS
macroname: F4$ABS
ex: (F4$ABS a ...) performs (mvc #'-F4ABS a[i] ...) for every row in a.

 ; VEQ:F4$ABS
 ;   [symbol]
 ;
 ; F4$ABS names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$ABS
 ;     broadcast for fx: -F4ABS
 ;     macroname: F4$ABS
 ;     ex: (F4$ABS a ...) performs (mvc #'-F4ABS a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$ABS!

```
broadcast for fx: -F4ABS
macroname: F4$ABS!
ex: (F4$ABS! a ...) performs (mvc #'-F4ABS a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$ABS!
 ;   [symbol]
 ;
 ; F4$ABS! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$ABS!
 ;     broadcast for fx: -F4ABS
 ;     macroname: F4$ABS!
 ;     ex: (F4$ABS! a ...) performs (mvc #'-F4ABS a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$FROM

```
broadcast for fx: -F4FROM
macroname: F4$FROM
ex: (F4$FROM a ...) performs (mvc #'-F4FROM a[i] ...) for every row in a.

 ; VEQ:F4$FROM
 ;   [symbol]
 ;
 ; F4$FROM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$FROM
 ;     broadcast for fx: -F4FROM
 ;     macroname: F4$FROM
 ;     ex: (F4$FROM a ...) performs (mvc #'-F4FROM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$FROM!

```
broadcast for fx: -F4FROM
macroname: F4$FROM!
ex: (F4$FROM! a ...) performs (mvc #'-F4FROM a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$FROM!
 ;   [symbol]
 ;
 ; F4$FROM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$FROM!
 ;     broadcast for fx: -F4FROM
 ;     macroname: F4$FROM!
 ;     ex: (F4$FROM! a ...) performs (mvc #'-F4FROM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### :context: F4$FXLSPACE

```
args: ((n a b &key (end t)) &body fx)
for 4d points a, b.
assumes the last form in fx is a function and does
(mvc fx i x ...) for points between a, b.
ex: (F4$FXLSPACE (n a b) (lambda (i (:va 4 a b)) (vpr i a b)))
```

#### F4$I-

```
broadcast for fx: -F4I-
macroname: F4$I-
ex: (F4$I- a ...) performs (mvc #'-F4I- a[i] ...) for every row in a.

 ; VEQ:F4$I-
 ;   [symbol]
 ;
 ; F4$I- names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$I-
 ;     broadcast for fx: -F4I-
 ;     macroname: F4$I-
 ;     ex: (F4$I- a ...) performs (mvc #'-F4I- a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$I-!

```
broadcast for fx: -F4I-
macroname: F4$I-!
ex: (F4$I-! a ...) performs (mvc #'-F4I- a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$I-!
 ;   [symbol]
 ;
 ; F4$I-! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$I-!
 ;     broadcast for fx: -F4I-
 ;     macroname: F4$I-!
 ;     ex: (F4$I-! a ...) performs (mvc #'-F4I- a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$I/

```
broadcast for fx: -F4I/
macroname: F4$I/
ex: (F4$I/ a ...) performs (mvc #'-F4I/ a[i] ...) for every row in a.

 ; VEQ:F4$I/
 ;   [symbol]
 ;
 ; F4$I/ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$I/
 ;     broadcast for fx: -F4I/
 ;     macroname: F4$I/
 ;     ex: (F4$I/ a ...) performs (mvc #'-F4I/ a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$I/!

```
broadcast for fx: -F4I/
macroname: F4$I/!
ex: (F4$I/! a ...) performs (mvc #'-F4I/ a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$I/!
 ;   [symbol]
 ;
 ; F4$I/! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$I/!
 ;     broadcast for fx: -F4I/
 ;     macroname: F4$I/!
 ;     ex: (F4$I/! a ...) performs (mvc #'-F4I/ a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$ISCALE

```
broadcast for fx: -F4ISCALE
macroname: F4$ISCALE
ex: (F4$ISCALE a ...) performs (mvc #'-F4ISCALE a[i] ...) for every row in a.

 ; VEQ:F4$ISCALE
 ;   [symbol]
 ;
 ; F4$ISCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$ISCALE
 ;     broadcast for fx: -F4ISCALE
 ;     macroname: F4$ISCALE
 ;     ex: (F4$ISCALE a ...) performs (mvc #'-F4ISCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$ISCALE!

```
broadcast for fx: -F4ISCALE
macroname: F4$ISCALE!
ex: (F4$ISCALE! a ...) performs (mvc #'-F4ISCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$ISCALE!
 ;   [symbol]
 ;
 ; F4$ISCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$ISCALE!
 ;     broadcast for fx: -F4ISCALE
 ;     macroname: F4$ISCALE!
 ;     ex: (F4$ISCALE! a ...) performs (mvc #'-F4ISCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$LAST

```
get last row of 4d array as (values ...)

 ; VEQ:F4$LAST
 ;   [symbol]
 ;
 ; F4$LAST names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get last row of 4d array as (values ...)
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

#### F4$LEN

```
broadcast for fx: -F4LEN
macroname: F4$LEN
ex: (F4$LEN a ...) performs (mvc #'-F4LEN a[i] ...) for every row in a.

 ; VEQ:F4$LEN
 ;   [symbol]
 ;
 ; F4$LEN names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$LEN
 ;     broadcast for fx: -F4LEN
 ;     macroname: F4$LEN
 ;     ex: (F4$LEN a ...) performs (mvc #'-F4LEN a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$LEN2

```
broadcast for fx: -F4LEN2
macroname: F4$LEN2
ex: (F4$LEN2 a ...) performs (mvc #'-F4LEN2 a[i] ...) for every row in a.

 ; VEQ:F4$LEN2
 ;   [symbol]
 ;
 ; F4$LEN2 names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$LEN2
 ;     broadcast for fx: -F4LEN2
 ;     macroname: F4$LEN2
 ;     ex: (F4$LEN2 a ...) performs (mvc #'-F4LEN2 a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
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
 ;   Source file: /data/x/veq/src/lspace.lisp
```

#### F4$NEG

```
broadcast for fx: -F4NEG
macroname: F4$NEG
ex: (F4$NEG a ...) performs (mvc #'-F4NEG a[i] ...) for every row in a.

 ; VEQ:F4$NEG
 ;   [symbol]
 ;
 ; F4$NEG names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$NEG
 ;     broadcast for fx: -F4NEG
 ;     macroname: F4$NEG
 ;     ex: (F4$NEG a ...) performs (mvc #'-F4NEG a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$NEG!

```
broadcast for fx: -F4NEG
macroname: F4$NEG!
ex: (F4$NEG! a ...) performs (mvc #'-F4NEG a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$NEG!
 ;   [symbol]
 ;
 ; F4$NEG! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$NEG!
 ;     broadcast for fx: -F4NEG
 ;     macroname: F4$NEG!
 ;     ex: (F4$NEG! a ...) performs (mvc #'-F4NEG a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$NORM

```
broadcast for fx: -F4NORM
macroname: F4$NORM
ex: (F4$NORM a ...) performs (mvc #'-F4NORM a[i] ...) for every row in a.

 ; VEQ:F4$NORM
 ;   [symbol]
 ;
 ; F4$NORM names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$NORM
 ;     broadcast for fx: -F4NORM
 ;     macroname: F4$NORM
 ;     ex: (F4$NORM a ...) performs (mvc #'-F4NORM a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$NORM!

```
broadcast for fx: -F4NORM
macroname: F4$NORM!
ex: (F4$NORM! a ...) performs (mvc #'-F4NORM a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$NORM!
 ;   [symbol]
 ;
 ; F4$NORM! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$NORM!
 ;     broadcast for fx: -F4NORM
 ;     macroname: F4$NORM!
 ;     ex: (F4$NORM! a ...) performs (mvc #'-F4NORM a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

#### :context: F4$S

```
get vector array struct fields as (values ...)
use :keword as field names. other symbols, values pass through directly.
ex :  (F4$S structname- c :a :b val)
returns (values a ... adim b ... bdim val)
assuming c is a structname, and a,b are FVEC of dim 4
```

#### F4$SCALE

```
broadcast for fx: -F4SCALE
macroname: F4$SCALE
ex: (F4$SCALE a ...) performs (mvc #'-F4SCALE a[i] ...) for every row in a.

 ; VEQ:F4$SCALE
 ;   [symbol]
 ;
 ; F4$SCALE names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$SCALE
 ;     broadcast for fx: -F4SCALE
 ;     macroname: F4$SCALE
 ;     ex: (F4$SCALE a ...) performs (mvc #'-F4SCALE a[i] ...) for every row in a.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
```

#### F4$SCALE!

```
broadcast for fx: -F4SCALE
macroname: F4$SCALE!
ex: (F4$SCALE! a ...) performs (mvc #'-F4SCALE a[i] ...) for every row in a.
destructive.

 ; VEQ:F4$SCALE!
 ;   [symbol]
 ;
 ; F4$SCALE! names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %F4$SCALE!
 ;     broadcast for fx: -F4SCALE
 ;     macroname: F4$SCALE!
 ;     ex: (F4$SCALE! a ...) performs (mvc #'-F4SCALE a[i] ...) for every row in a.
 ;     destructive.
 ;
 ;   Source file: /data/x/veq/src/array-broadcast.lisp
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
 ;   Source file: /data/x/veq/src/array-reduce.lisp
```

#### F4$TAKE

```
returns 4d array with rows for inds.
use :res put result in existing array

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
 ;     use :res put result in existing array
 ;   Source file: /data/x/veq/src/array-take.lisp
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F4$WITH-ROWS

```
make 4d
```

#### F4$ZERO

```
make 4d array of zeros.
typed.

 ; VEQ:F4$ZERO
 ;   [symbol]
 ;
 ; F4$ZERO names a compiled function:
 ;   Lambda-list: (&OPTIONAL (N1 1))
 ;   Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     make 4d array of zeros.
 ;     typed.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: F4\*

```
veq context op: F4*
fxname: -F4*
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```

#### :context: F4+

```
veq context op: F4+
fxname: -F4+
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW))
```

#### :context: F4-

```
veq context op: F4-
fxname: -F4-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW))
```

#### :context: F4.

```
veq context op: F4.
fxname: -F4.
args: (AX AY AZ AW BX BY BZ BW)
body: (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```

#### :context: F4/

```
veq context op: F4/
fxname: -F4/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW))
```

#### :context: F4^

```
veq context op: F4^
fxname: -F4^
args: (A B C D S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S) (EXPT D S))
```

#### :context: F4ABS

```
veq context op: F4ABS
fxname: -F4ABS
args: (A B C D)
body: (VALUES (ABS A) (ABS B) (ABS C) (ABS D))
```

#### :context: F4DST

```
veq context op: F4DST
fxname: -F4DST
args: (AX AY AZ AW BX BY BZ BW)
body: (SQRT
       (THE POS-FF
            (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))))
```

#### :context: F4DST2

```
veq context op: F4DST2
fxname: -F4DST2
args: (AX AY AZ AW BX BY BZ BW)
body: (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))
```

#### :context: F4EXP

```
veq context op: F4EXP
fxname: -F4EXP
args: (A B C D)
body: (VALUES (EXP A) (EXP B) (EXP C) (EXP D))
```

#### :context: F4FROM

```
veq context op: F4FROM
fxname: -F4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body: (-F4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S))
```

#### :context: F4I-

```
veq context op: F4I-
fxname: -F4I-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))
```

#### :context: F4I/

```
veq context op: F4I/
fxname: -F4I/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW))
```

#### :context: F4ISCALE

```
veq context op: F4ISCALE
fxname: -F4ISCALE
args: (A B C D S)
body: (VALUES (/ A S) (/ B S) (/ C S) (/ D S))
```

#### :context: F4LEN

```
veq context op: F4LEN
fxname: -F4LEN
args: (A B C D)
body: (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F4SQUARE A B C D)))))
```

#### :context: F4LEN2

```
veq context op: F4LEN2
fxname: -F4LEN2
args: (A B C D)
body: (THE POS-FF (MVC #'+ (-F4SQUARE A B C D)))
```

#### :context: F4LERP

```
veq context op: F4LERP
fxname: -F4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body: (-F4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
       (* (- BW AW) S))
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
args: (A B C D)
body: (MAX A B C D)
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F4MID

```
veq context op: F4MID
fxname: -F4MID
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5)
              (* (+ BW AW) 0.5))
```

#### :context: F4MIN

```
veq context op: F4MIN
fxname: -F4MIN
args: (A B C D)
body: (MIN A B C D)
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F4MOD

```
veq context op: F4MOD
fxname: -F4MOD
args: (A B C D S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S) (MOD D S))
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

#### :context: F4NEG

```
veq context op: F4NEG
fxname: -F4NEG
args: (A B C D)
body: (VALUES (- A) (- B) (- C) (- D))
```

#### :context: F4NORM

```
veq context op: F4NORM
fxname: -F4NORM
args: (A B C D)
body: (MVC #'-F4ISCALE A B C D (THE POS-FF (MVC #'-F4LEN A B C D)))
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
repeat the evaluated argument 4d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### :context: F4SCALE

```
veq context op: F4SCALE
fxname: -F4SCALE
args: (A B C D S)
body: (VALUES (* A S) (* B S) (* C S) (* D S))
```

#### :context: F4SQRT

```
veq context op: F4SQRT
fxname: -F4SQRT
args: (A B C D)
body: (VALUES (THE POS-FF (SQRT (THE POS-FF A)))
              (THE POS-FF (SQRT (THE POS-FF B)))
              (THE POS-FF (SQRT (THE POS-FF C)))
              (THE POS-FF (SQRT (THE POS-FF D))))
```

#### :context: F4SQUARE

```
veq context op: F4SQUARE
fxname: -F4SQUARE
args: (A B C D)
body: (VALUES (THE POS-FF (* A A)) (THE POS-FF (* B B)) (THE POS-FF (* C C))
              (THE POS-FF (* D D)))
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
coerce to type.
```

#### :context: F^

```
veq context op: F^
fxname: -F^
args: (A S)
body: (EXPT A S)
```

#### F_

```
create fvec from body: (f_ '(1f0 2f0 3f0))

 ; VEQ:F_
 ;   [symbol]
 ;
 ; F_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create fvec from body: (f_ '(1f0 2f0 3f0))
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

#### :context: FABS

```
veq context op: FABS
fxname: -FABS
args: (A)
body: (ABS A)
```

#### :context: FCLAMP

```
veq context op: FCLAMP
fxname: -FCLAMP
args: (X)
body: (MIN 1.0 (MAX 0.0 X))
```

#### :context: FCLAMP\*

```
veq context op: FCLAMP*
fxname: -FCLAMP*
args: (X MI MA)
body: (MIN MA (MAX MI X))
```

#### :context: FCOS-SIN

```
veq context op: FCOS-SIN
fxname: -FCOS-SIN
args: (A)
body: (VALUES (COS A) (SIN A))
```

#### :context: FDEG->RAD

```
veq context op: FDEG->RAD
fxname: -FDEG->RAD
args: (DEG)
body: (* FPI (/ DEG 180.0))
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

#### :context: FEXP

```
veq context op: FEXP
fxname: -FEXP
args: (A)
body: (VALUES (EXP A))
```

#### FF

```
:missing:todo:

 ; VEQ:FF
 ;   [symbol]
 ;
 ; FF names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### :context: FFROM

```
veq context op: FFROM
fxname: -FFROM
args: (AX BX S)
body: (+ AX (* BX S))
```

#### :context: FI-

```
veq context op: FI-
fxname: -FI-
args: (A B)
body: (- B A)
```

#### :context: FI/

```
veq context op: FI/
fxname: -FI/
args: (A B)
body: (/ B A)
```

#### :context: FISCALE

```
veq context op: FISCALE
fxname: -FISCALE
args: (A S)
body: (VALUES (/ A S))
```

#### :context: FLEN

```
veq context op: FLEN
fxname: -FLEN
args: (A)
body: (THE POS-FF A)
```

#### :context: FLEN2

```
veq context op: FLEN2
fxname: -FLEN2
args: (A)
body: (THE POS-FF (MVC #'+ (-FSQUARE A)))
```

#### :context: FLERP

```
veq context op: FLERP
fxname: -FLERP
args: (AX BX S)
body: (+ AX (* (- BX AX) S))
```

#### FMAKE-ORTHO-PROJ-MATRIX

```
docstring for %FMAKE-ORTHO-PROJ-MATRIX
make orthogonal projection matrix

 ; VEQ:FMAKE-ORTHO-PROJ-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-ORTHO-PROJ-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %FMAKE-ORTHO-PROJ-MATRIX
 ;     make orthogonal projection matrix
 ;   Source file: /data/x/veq/src/mat-cam.lisp
```

#### FMAKE-PROJ-MATRIX

```
docstring for %FMAKE-PROJ-MATRIX
make projection matrix for width, height, near, far

 ; VEQ:FMAKE-PROJ-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-PROJ-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %FMAKE-PROJ-MATRIX
 ;     make projection matrix for width, height, near, far
 ;   Source file: /data/x/veq/src/mat-cam.lisp
```

#### FMAKE-VIEW-MATRIX

```
docstring for %FMAKE-VIEW-MATRIX
make view matrix for cam (w/up) looking at target

 ; VEQ:FMAKE-VIEW-MATRIX
 ;   [symbol]
 ;
 ; FMAKE-VIEW-MATRIX names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     docstring for %FMAKE-VIEW-MATRIX
 ;     make view matrix for cam (w/up) looking at target
 ;   Source file: /data/x/veq/src/mat-cam.lisp
```

#### :context: FMID

```
veq context op: FMID
fxname: -FMID
args: (AX BX)
body: (* 0.5 (+ AX BX))
```

#### :context: FMOD

```
veq context op: FMOD
fxname: -FMOD
args: (A S)
body: (MOD A S)
```

#### :context: FNEG

```
veq context op: FNEG
fxname: -FNEG
args: (A)
body: (- A)
```

#### :context: FNORM

```
veq context op: FNORM
fxname: -FNORM
args: (A)
body: (MVC #'-FISCALE A (MVC #'-FLEN A))
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
repeat the evaluated argument 1d times as values.
ex: (f3rep (fx)) corresponds to (values v v v) where v = (fx).
fx is evaluated exactly once.
```

#### FROM-LST

```
get values from list. equivalent to values-list.

 ; VEQ:FROM-LST
 ;   [symbol]
 ;
 ; FROM-LST names a macro:
 ;   Lambda-list: (L)
 ;   Documentation:
 ;     get values from list. equivalent to values-list.
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### :context: FSCALE

```
veq context op: FSCALE
fxname: -FSCALE
args: (A S)
body: (VALUES (* A S))
```

#### :context: FSIN-COS

```
veq context op: FSIN-COS
fxname: -FSIN-COS
args: (A)
body: (VALUES (SIN A) (COS A))
```

#### :context: FSQRT

```
veq context op: FSQRT
fxname: -FSQRT
args: (A)
body: (THE POS-FF (SQRT (THE POS-FF A)))
```

#### :context: FSQUARE

```
veq context op: FSQUARE
fxname: -FSQUARE
args: (A)
body: (* A A)
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
 ;   Source file: /data/x/veq/src/macros.lisp
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
 ;   Source file: /data/x/veq/src/macros.lisp
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
 ;         handles propagation and resolution of uses of (varg d var) and (vref var i).
 ;
 ;         works the same way as vprogn. but removes all macrolets that are not
 ;         directly used in body. this is faster, but may fail in some cases where
 ;         body is complex. in the event of errors try vprogn instead.
 ;   Source file: /data/x/veq/src/macros.lisp
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
start is the first index. then n-1 more.
inds is indices to iterate. replaces n/start
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
coerce to type.
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### IN

```
:missing:todo:

 ; VEQ:IN
 ;   [symbol]
 ;
 ; IN names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
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

#### LST

```
wrap (values ..) in (list ..).
almost like multuple-values-list, except it handles multuple arguments.

 ; VEQ:LST
 ;   [symbol]
 ;
 ; LST names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     wrap (values ..) in (list ..).
 ;     almost like multuple-values-list, except it handles multuple arguments.
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### MAC\*

```
expand macro all.

 ; VEQ:MAC*
 ;   [symbol]
 ;
 ; MAC* names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     expand macro all.
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### MVB

```
:missing:todo:

 ; VEQ:MVB
 ;   [symbol]
 ;
 ; MVB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### MVC

```
:missing:todo:

 ; VEQ:MVC
 ;   [symbol]
 ;
 ; MVC names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### MVCWRAP

```
wrap fx in a macro, m, so that fx will be called via mvc.

 ; VEQ:MVCWRAP
 ;   [symbol]
 ;
 ; MVCWRAP names a macro:
 ;   Lambda-list: (M FX)
 ;   Documentation:
 ;     wrap fx in a macro, m, so that fx will be called via mvc.
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### :context: VARG

```
use (veq:varg n a b ...) or (:vr n a b ...) to represent n dim vectors a,b
of dim n in vprogn, fvprog, fvdef*, vdef*, def*.
see replace-varg for implementation details.
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
 ;   Source file: /data/x/veq/src/macros.lisp
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
 ;   Source file: /data/x/veq/src/macros.lisp
```

#### VGRP-MVC

```
do (multiple-value-call fx g) where g is groups of size dim over (values ...) returned by body.

 ; VEQ:VGRP-MVC
 ;   [symbol]
 ;
 ; VGRP-MVC names a macro:
 ;   Lambda-list: ((DIM FX) &BODY BODY)
 ;   Documentation:
 ;     do (multiple-value-call fx g) where g is groups of size dim over (values ...) returned by body.
 ;   Source file: /data/x/veq/src/utils.lisp
```

#### VLABELS

```
wraps labels so that it can be used with implicit mvc. that is,
all labels are defined as if with def*, vdef* or fvdef*
use %labelname to call the function directly, not via mvc.

 ; VEQ:VLABELS
 ;   [symbol]
 ;
 ; VLABELS names a macro:
 ;   Lambda-list: ((&REST LABS) &BODY BODY)
 ;   Documentation:
 ;     wraps labels so that it can be used with implicit mvc. that is,
 ;     all labels are defined as if with def*, vdef* or fvdef*
 ;     use %labelname to call the function directly, not via mvc.
 ;   Source file: /data/x/veq/src/lets.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;         handles propagation and resolution of uses of (varg d var) and (vref var i).
 ;         fvprogn is faster, but has some limitations.
 ;   Source file: /data/x/veq/src/macros.lisp
```

#### :context: VREF

```
use (veq:vref s x) or (:vr s x) to get dim x of symbol s in vprogn, fvprogn,
fvdef*, vdef*, def*.
see replace-varg for implementation details.
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
 ;   Source file: /data/x/veq/src/utils.lisp
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

