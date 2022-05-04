# VEQ DOCUMENTATION

## Symbols
### $

```
:missing:

VEQ:$
  [symbol]

(SETF $) has a complex setf-expansion:
  Lambda-list: (A &OPTIONAL (I 0))
  (undocumented)
  Source file: /data/x/veq/src/vset.lisp
```
### $NUM

```
:missing:

VEQ:$NUM
  [symbol]

$NUM names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION (SIMPLE-ARRAY)
                 (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### $PRINT

```
pretty print a with dim columns
VEQ:$PRINT
  [symbol]

$PRINT names a compiled function:
  Lambda-list: (A &KEY N (DIM 1) &AUX
                (N
                 (IF N
                     N
                     (/ (LENGTH A) DIM))))
  Derived type: (FUNCTION
                 (SIMPLE-ARRAY &KEY (:N T) (:DIM (UNSIGNED-BYTE 31)))
                 (VALUES (SIMPLE-ARRAY * (*)) &OPTIONAL))
  Documentation:
    pretty print a with dim columns
  Source file: /data/x/veq/src/array-print.lisp
```
### $TO-LIST

```
return a as list of lists of length dim
VEQ:$TO-LIST
  [symbol]

$TO-LIST names a compiled function:
  Lambda-list: (A &KEY (DIM 1))
  Derived type: (FUNCTION (SIMPLE-ARRAY &KEY (:DIM (UNSIGNED-BYTE 31)))
                 (VALUES LIST &OPTIONAL))
  Documentation:
    return a as list of lists of length dim
  Source file: /data/x/veq/src/array-print.lisp
```
### $VSET

```
:missing:

VEQ:$VSET
  [symbol]
```
### *EPS*

```
:missing:

VEQ:*EPS*
  [symbol]

*EPS* names a special variable:
  Declared type: SINGLE-FLOAT
  Value: 1.7881396e-7
```
### 2$

```
:missing:

VEQ:2$
  [symbol]

(SETF 2$) has a complex setf-expansion:
  Lambda-list: (A &OPTIONAL (I 0))
  (undocumented)
  Source file: /data/x/veq/src/vset.lisp
```
### 2$NUM

```
:missing:

VEQ:2$NUM
  [symbol]

2$NUM names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION (SIMPLE-ARRAY)
                 (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### 2$PRINT

```
:missing:

VEQ:2$PRINT
  [symbol]

2$PRINT names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION (T &KEY (:N T)) *)
  Source file: /data/x/veq/src/array-print.lisp
```
### 2$TO-LIST

```
:missing:

VEQ:2$TO-LIST
  [symbol]

2$TO-LIST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION (T) *)
  Source file: /data/x/veq/src/array-print.lisp
```
### 2$VSET

```
:missing:

VEQ:2$VSET
  [symbol]
```
### 3$

```
:missing:

VEQ:3$
  [symbol]

(SETF 3$) has a complex setf-expansion:
  Lambda-list: (A &OPTIONAL (I 0))
  (undocumented)
  Source file: /data/x/veq/src/vset.lisp
```
### 3$NUM

```
:missing:

VEQ:3$NUM
  [symbol]

3$NUM names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION (SIMPLE-ARRAY)
                 (VALUES (UNSIGNED-BYTE 31) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### 3$PRINT

```
:missing:

VEQ:3$PRINT
  [symbol]

3$PRINT names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION (T &KEY (:N T)) *)
  Source file: /data/x/veq/src/array-print.lisp
```
### 3$TO-LIST

```
:missing:

VEQ:3$TO-LIST
  [symbol]

3$TO-LIST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION (T) *)
  Source file: /data/x/veq/src/array-print.lisp
```
### 3$VSET

```
:missing:

VEQ:3$VSET
  [symbol]
```
### 4$

```
:missing:

VEQ:4$
  [symbol]

(SETF 4$) has a complex setf-expansion:
  Lambda-list: (A &OPTIONAL (I 0))
  (undocumented)
  Source file: /data/x/veq/src/vset.lisp
```
### 4$PRINT

```
:missing:

VEQ:4$PRINT
  [symbol]

4$PRINT names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION (T &KEY (:N T)) *)
  Source file: /data/x/veq/src/array-print.lisp
```
### 4$VSET

```
:missing:

VEQ:4$VSET
  [symbol]
```
### 4TO-LIST

```
:missing:

VEQ:4TO-LIST
  [symbol]
```
### CONTEXT?

```
list all macrolets in veq context. that is ops available inside vprog,
  fvprogn, vdef, fvdef defined contexts/functions.
VEQ:CONTEXT?
  [symbol]

CONTEXT? names a macro:
  Lambda-list: ()
  Documentation:
    list all macrolets in veq context. that is ops available inside vprog,
      fvprogn, vdef, fvdef defined contexts/functions.
  Source file: /data/x/veq/src/veq.lisp
```
### D

```
:missing:

VEQ:D
  [symbol]
```
### D$

```
:missing:

VEQ:D$
  [symbol]
```
### D$*

```
broadcast for fx: -D*
macroname: D$*


VEQ:D$*
  [symbol]

D$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$* ...).
    see function: %D$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$*!

```
broadcast for fx: -D*
macroname: D$*!


VEQ:D$*!
  [symbol]

D$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$*! ...).
    see function: %D$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$+

```
broadcast for fx: -D+
macroname: D$+


VEQ:D$+
  [symbol]

D$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$+ ...).
    see function: %D$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$+!

```
broadcast for fx: -D+
macroname: D$+!


VEQ:D$+!
  [symbol]

D$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$+! ...).
    see function: %D$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$-

```
broadcast for fx: -D-
macroname: D$-


VEQ:D$-
  [symbol]

D$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$- ...).
    see function: %D$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$-!

```
broadcast for fx: -D-
macroname: D$-!


VEQ:D$-!
  [symbol]

D$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$-! ...).
    see function: %D$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$/

```
broadcast for fx: -D/
macroname: D$/


VEQ:D$/
  [symbol]

D$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$/ ...).
    see function: %D$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$/!

```
broadcast for fx: -D/
macroname: D$/!


VEQ:D$/!
  [symbol]

D$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$/! ...).
    see function: %D$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$_

```
create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
   or: ($_ '((1d0 2d0) (1d0 2d0)))
VEQ:D$_
  [symbol]

D$_ names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
       or: ($_ '((1d0 2d0) (1d0 2d0)))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D$ABS

```
broadcast for fx: -DABS
macroname: D$ABS


VEQ:D$ABS
  [symbol]

D$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$ABS ...).
    see function: %D$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$ABS!

```
broadcast for fx: -DABS
macroname: D$ABS!


VEQ:D$ABS!
  [symbol]

D$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$ABS! ...).
    see function: %D$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$COPY

```
:missing:

VEQ:D$COPY
  [symbol]

D$COPY names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D$COS-SIN

```
broadcast for fx: -DCOS-SIN
macroname: D$COS-SIN


VEQ:D$COS-SIN
  [symbol]

D$COS-SIN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$COS-SIN ...).
    see function: %D$COS-SIN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$FROM

```
broadcast for fx: -DFROM
macroname: D$FROM


VEQ:D$FROM
  [symbol]

D$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$FROM ...).
    see function: %D$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$FROM!

```
broadcast for fx: -DFROM
macroname: D$FROM!


VEQ:D$FROM!
  [symbol]

D$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$FROM! ...).
    see function: %D$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$FXLSPACE

```
:missing:

VEQ:D$FXLSPACE
  [symbol]
```
### D$I-

```
broadcast for fx: -DI-
macroname: D$I-


VEQ:D$I-
  [symbol]

D$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$I- ...).
    see function: %D$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$I-!

```
broadcast for fx: -DI-
macroname: D$I-!


VEQ:D$I-!
  [symbol]

D$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$I-! ...).
    see function: %D$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$I/

```
broadcast for fx: -DI/
macroname: D$I/


VEQ:D$I/
  [symbol]

D$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$I/ ...).
    see function: %D$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$I/!

```
broadcast for fx: -DI/
macroname: D$I/!


VEQ:D$I/!
  [symbol]

D$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$I/! ...).
    see function: %D$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$ISCALE

```
broadcast for fx: -DISCALE
macroname: D$ISCALE


VEQ:D$ISCALE
  [symbol]

D$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$ISCALE ...).
    see function: %D$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$ISCALE!

```
broadcast for fx: -DISCALE
macroname: D$ISCALE!


VEQ:D$ISCALE!
  [symbol]

D$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$ISCALE! ...).
    see function: %D$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$LAST

```
:missing:

VEQ:D$LAST
  [symbol]

D$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### D$LEN

```
broadcast for fx: -DLEN
macroname: D$LEN


VEQ:D$LEN
  [symbol]

D$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$LEN ...).
    see function: %D$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$LEN2

```
broadcast for fx: -DLEN2
macroname: D$LEN2


VEQ:D$LEN2
  [symbol]

D$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$LEN2 ...).
    see function: %D$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$LINE

```
macro wrapper: (mvc #'%D$LINE ...).
see function: %D$LINE
VEQ:D$LINE
  [symbol]

D$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$LINE ...).
    see function: %D$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### D$LSPACE

```
macro wrapper: (mvc #'%D$LSPACE ...) in veq context.
see function: %D$LSPACE
VEQ:D$LSPACE
  [symbol]

D$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$LSPACE ...) in veq context.
    see function: %D$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### D$MAKE

```
 create array with size (n dim), and initial value v
VEQ:D$MAKE
  [symbol]

D$MAKE names a macro:
  Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0d0))
  Documentation:
     create array with size (n dim), and initial value v
  Source file: /data/x/veq/src/array-utils.lisp
```
### D$MIMA

```
:missing:

VEQ:D$MIMA
  [symbol]

D$MIMA names a compiled function:
  Lambda-list: (A0 &KEY (N ($NUM A0)) INDS)
  Derived type: (FUNCTION
                 ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T) (:INDS T))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-mima.lisp
```
### D$NEG

```
broadcast for fx: -DNEG
macroname: D$NEG


VEQ:D$NEG
  [symbol]

D$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$NEG ...).
    see function: %D$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$NEG!

```
broadcast for fx: -DNEG
macroname: D$NEG!


VEQ:D$NEG!
  [symbol]

D$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$NEG! ...).
    see function: %D$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$NORM

```
broadcast for fx: -DNORM
macroname: D$NORM


VEQ:D$NORM
  [symbol]

D$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$NORM ...).
    see function: %D$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$NORM!

```
broadcast for fx: -DNORM
macroname: D$NORM!


VEQ:D$NORM!
  [symbol]

D$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$NORM! ...).
    see function: %D$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$ONE

```
:missing:

VEQ:D$ONE
  [symbol]

D$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D$POINT

```
macro wrapper: (mvc #'%D$POINT ...).
see function: %D$POINT
VEQ:D$POINT
  [symbol]

D$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$POINT ...).
    see function: %D$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### D$SCALE

```
broadcast for fx: -DSCALE
macroname: D$SCALE


VEQ:D$SCALE
  [symbol]

D$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$SCALE ...).
    see function: %D$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$SCALE!

```
broadcast for fx: -DSCALE
macroname: D$SCALE!


VEQ:D$SCALE!
  [symbol]

D$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D$SCALE! ...).
    see function: %D$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D$SUM

```
:missing:

VEQ:D$SUM
  [symbol]

D$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
                 (VALUES DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### D$TAKE

```
:missing:

VEQ:D$TAKE
  [symbol]

D$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### D$VAL

```
:missing:

VEQ:D$VAL
  [symbol]

D$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D$WITH-ROWS

```
:missing:

VEQ:D$WITH-ROWS
  [symbol]
```
### D$ZERO

```
:missing:

VEQ:D$ZERO
  [symbol]

D$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D*

```
veq context op: D*
fxname: -D*
args: (A B)
body: (* A B)
```
### D+

```
veq context op: D+
fxname: -D+
args: (A B)
body: (+ A B)
```
### D-

```
veq context op: D-
fxname: -D-
args: (A B)
body: (- A B)
```
### D/

```
veq context op: D/
fxname: -D/
args: (A B)
body: (/ A B)
```
### D2

```
:missing:

VEQ:D2
  [symbol]
```
### D2$

```
:missing:

VEQ:D2$
  [symbol]
```
### D2$*

```
broadcast for fx: -D2*
macroname: D2$*


VEQ:D2$*
  [symbol]

D2$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$* ...).
    see function: %D2$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$*!

```
broadcast for fx: -D2*
macroname: D2$*!


VEQ:D2$*!
  [symbol]

D2$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$*! ...).
    see function: %D2$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$+

```
broadcast for fx: -D2+
macroname: D2$+


VEQ:D2$+
  [symbol]

D2$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$+ ...).
    see function: %D2$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$+!

```
broadcast for fx: -D2+
macroname: D2$+!


VEQ:D2$+!
  [symbol]

D2$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$+! ...).
    see function: %D2$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$-

```
broadcast for fx: -D2-
macroname: D2$-


VEQ:D2$-
  [symbol]

D2$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$- ...).
    see function: %D2$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$-!

```
broadcast for fx: -D2-
macroname: D2$-!


VEQ:D2$-!
  [symbol]

D2$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$-! ...).
    see function: %D2$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$/

```
broadcast for fx: -D2/
macroname: D2$/


VEQ:D2$/
  [symbol]

D2$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$/ ...).
    see function: %D2$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$/!

```
broadcast for fx: -D2/
macroname: D2$/!


VEQ:D2$/!
  [symbol]

D2$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$/! ...).
    see function: %D2$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ABS

```
broadcast for fx: -D2ABS
macroname: D2$ABS


VEQ:D2$ABS
  [symbol]

D2$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ABS ...).
    see function: %D2$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ABS!

```
broadcast for fx: -D2ABS
macroname: D2$ABS!


VEQ:D2$ABS!
  [symbol]

D2$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ABS! ...).
    see function: %D2$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$CENTER

```
:missing:

VEQ:D2$CENTER
  [symbol]
```
### D2$CIRC

```
:missing:

VEQ:D2$CIRC
  [symbol]
```
### D2$COPY

```
:missing:

VEQ:D2$COPY
  [symbol]
```
### D2$FROM

```
broadcast for fx: -D2FROM
macroname: D2$FROM


VEQ:D2$FROM
  [symbol]

D2$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$FROM ...).
    see function: %D2$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$FROM!

```
broadcast for fx: -D2FROM
macroname: D2$FROM!


VEQ:D2$FROM!
  [symbol]

D2$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$FROM! ...).
    see function: %D2$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$FXLSPACE

```
:missing:

VEQ:D2$FXLSPACE
  [symbol]
```
### D2$I-

```
broadcast for fx: -D2I-
macroname: D2$I-


VEQ:D2$I-
  [symbol]

D2$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$I- ...).
    see function: %D2$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$I-!

```
broadcast for fx: -D2I-
macroname: D2$I-!


VEQ:D2$I-!
  [symbol]

D2$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$I-! ...).
    see function: %D2$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$I/

```
broadcast for fx: -D2I/
macroname: D2$I/


VEQ:D2$I/
  [symbol]

D2$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$I/ ...).
    see function: %D2$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$I/!

```
broadcast for fx: -D2I/
macroname: D2$I/!


VEQ:D2$I/!
  [symbol]

D2$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$I/! ...).
    see function: %D2$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ISCALE

```
broadcast for fx: -D2ISCALE
macroname: D2$ISCALE


VEQ:D2$ISCALE
  [symbol]

D2$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ISCALE ...).
    see function: %D2$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ISCALE!

```
broadcast for fx: -D2ISCALE
macroname: D2$ISCALE!


VEQ:D2$ISCALE!
  [symbol]

D2$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ISCALE! ...).
    see function: %D2$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$LAST

```
:missing:

VEQ:D2$LAST
  [symbol]

D2$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### D2$LEN

```
broadcast for fx: -D2LEN
macroname: D2$LEN


VEQ:D2$LEN
  [symbol]

D2$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$LEN ...).
    see function: %D2$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$LEN2

```
broadcast for fx: -D2LEN2
macroname: D2$LEN2


VEQ:D2$LEN2
  [symbol]

D2$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$LEN2 ...).
    see function: %D2$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$LINE

```
macro wrapper: (mvc #'%D2$LINE ...).
see function: %D2$LINE
VEQ:D2$LINE
  [symbol]

D2$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$LINE ...).
    see function: %D2$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### D2$LSPACE

```
macro wrapper: (mvc #'%D2$LSPACE ...) in veq context.
see function: %D2$LSPACE
VEQ:D2$LSPACE
  [symbol]

D2$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$LSPACE ...) in veq context.
    see function: %D2$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### D2$MAKE

```
:missing:

VEQ:D2$MAKE
  [symbol]
```
### D2$MIMA

```
:missing:

VEQ:D2$MIMA
  [symbol]

D2$MIMA names a compiled function:
  Lambda-list: (A0 &KEY (N (2$NUM A0)) INDS)
  Derived type: (FUNCTION
                 ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T) (:INDS T))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
                         DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-mima.lisp
```
### D2$NEG

```
broadcast for fx: -D2NEG
macroname: D2$NEG


VEQ:D2$NEG
  [symbol]

D2$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$NEG ...).
    see function: %D2$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$NEG!

```
broadcast for fx: -D2NEG
macroname: D2$NEG!


VEQ:D2$NEG!
  [symbol]

D2$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$NEG! ...).
    see function: %D2$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$NORM

```
broadcast for fx: -D2NORM
macroname: D2$NORM


VEQ:D2$NORM
  [symbol]

D2$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$NORM ...).
    see function: %D2$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$NORM!

```
broadcast for fx: -D2NORM
macroname: D2$NORM!


VEQ:D2$NORM!
  [symbol]

D2$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$NORM! ...).
    see function: %D2$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ONE

```
:missing:

VEQ:D2$ONE
  [symbol]

D2$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D2$POINT

```
macro wrapper: (mvc #'%D2$POINT ...).
see function: %D2$POINT
VEQ:D2$POINT
  [symbol]

D2$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$POINT ...).
    see function: %D2$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### D2$POLYGON

```
:missing:

VEQ:D2$POLYGON
  [symbol]
```
### D2$RECT

```
:missing:

VEQ:D2$RECT
  [symbol]
```
### D2$ROT

```
broadcast for fx: -D2ROT
macroname: D2$ROT


VEQ:D2$ROT
  [symbol]

D2$ROT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ROT ...).
    see function: %D2$ROT
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ROT!

```
broadcast for fx: -D2ROT
macroname: D2$ROT!


VEQ:D2$ROT!
  [symbol]

D2$ROT! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ROT! ...).
    see function: %D2$ROT!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ROTS

```
broadcast for fx: -D2ROTS
macroname: D2$ROTS


VEQ:D2$ROTS
  [symbol]

D2$ROTS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ROTS ...).
    see function: %D2$ROTS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$ROTS!

```
broadcast for fx: -D2ROTS
macroname: D2$ROTS!


VEQ:D2$ROTS!
  [symbol]

D2$ROTS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$ROTS! ...).
    see function: %D2$ROTS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$SCALE

```
broadcast for fx: -D2SCALE
macroname: D2$SCALE


VEQ:D2$SCALE
  [symbol]

D2$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$SCALE ...).
    see function: %D2$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$SCALE!

```
broadcast for fx: -D2SCALE
macroname: D2$SCALE!


VEQ:D2$SCALE!
  [symbol]

D2$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2$SCALE! ...).
    see function: %D2$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D2$SQUARE

```
:missing:

VEQ:D2$SQUARE
  [symbol]
```
### D2$SUM

```
:missing:

VEQ:D2$SUM
  [symbol]

D2$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### D2$TAKE

```
:missing:

VEQ:D2$TAKE
  [symbol]

D2$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### D2$VAL

```
:missing:

VEQ:D2$VAL
  [symbol]

D2$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D2$WITH-ROWS

```
:missing:

VEQ:D2$WITH-ROWS
  [symbol]
```
### D2$ZERO

```
:missing:

VEQ:D2$ZERO
  [symbol]

D2$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D2*

```
veq context op: D2*
fxname: -D2*
args: (AX AY BX BY)
body: (VALUES (* AX BX) (* AY BY))
```
### D2+

```
veq context op: D2+
fxname: -D2+
args: (AX AY BX BY)
body: (VALUES (+ AX BX) (+ AY BY))
```
### D2-

```
veq context op: D2-
fxname: -D2-
args: (AX AY BX BY)
body: (VALUES (- AX BX) (- AY BY))
```
### D2.

```
veq context op: D2.
fxname: -D2.
args: (AX AY BX BY)
body: (+ (* AX BX) (* AY BY))
```
### D2/

```
veq context op: D2/
fxname: -D2/
args: (AX AY BX BY)
body: (VALUES (/ AX BX) (/ AY BY))
```
### D2^

```
veq context op: D2^
fxname: -D2^
args: (A B S)
body: (VALUES (EXPT A S) (EXPT B S))
```
### D2ABS

```
veq context op: D2ABS
fxname: -D2ABS
args: (A B)
body: (VALUES (ABS A) (ABS B))
```
### D2ANGLE

```
veq context op: D2ANGLE
fxname: -D2ANGLE
args: (A B)
body: (MVC #'ATAN (-D2NORM B A))
```
### D2CROSS

```
veq context op: D2CROSS
fxname: -D2CROSS
args: (AX AY BX BY)
body: (- (* AX BY) (* AY BX))
```
### D2DST

```
veq context op: D2DST
fxname: -D2DST
args: (AX AY BX BY)
body: (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY)))))
```
### D2DST2

```
veq context op: D2DST2
fxname: -D2DST2
args: (AX AY BX BY)
body: (MVC #'+ (-D2SQUARE (- BX AX) (- BY AY)))
```
### D2EXP

```
veq context op: D2EXP
fxname: -D2EXP
args: (A B)
body: (VALUES (EXP A) (EXP B))
```
### D2FLIP

```
veq context op: D2FLIP
fxname: -D2FLIP
args: (A B)
body: (VALUES B A)
```
### D2FROM

```
veq context op: D2FROM
fxname: -D2FROM
args: (AX AY BX BY S)
body: (-D2+ AX AY (* BX S) (* BY S))
```
### D2I-

```
veq context op: D2I-
fxname: -D2I-
args: (AX AY BX BY)
body: (VALUES (- BX AX) (- BY AY))
```
### D2I/

```
veq context op: D2I/
fxname: -D2I/
args: (AX AY BX BY)
body: (VALUES (/ BX AX) (/ BY AY))
```
### D2ISCALE

```
veq context op: D2ISCALE
fxname: -D2ISCALE
args: (A B S)
body: (VALUES (/ A S) (/ B S))
```
### D2LEN

```
veq context op: D2LEN
fxname: -D2LEN
args: (A B)
body: (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D2SQUARE A B)))))
```
### D2LEN2

```
veq context op: D2LEN2
fxname: -D2LEN2
args: (A B)
body: (THE POS-DF (MVC #'+ (-D2SQUARE A B)))
```
### D2LERP

```
veq context op: D2LERP
fxname: -D2LERP
args: (AX AY BX BY S)
body: (-D2+ AX AY (* (- BX AX) S) (* (- BY AY) S))
```
### D2LET

```
:missing:

VEQ:D2LET
  [symbol]
```
### D2MAX

```
veq context op: D2MAX
fxname: -D2MAX
args: (A B)
body: (MAX A B)
```
### D2MEYE

```
return eye matrix for dimension
VEQ:D2MEYE
  [symbol]

D2MEYE names a compiled function:
  Lambda-list: (&OPTIONAL (V 1.0d0))
  Derived type: (FUNCTION (&OPTIONAL DOUBLE-FLOAT)
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (4)) &OPTIONAL))
  Documentation:
    return eye matrix for dimension
  Source file: /data/x/veq/src/mat.lisp
```
### D2MID

```
veq context op: D2MID
fxname: -D2MID
args: (AX AY BX BY)
body: (VALUES (* 0.5d0 (+ AX BX)) (* 0.5d0 (+ AY BY)))
```
### D2MIN

```
veq context op: D2MIN
fxname: -D2MIN
args: (A B)
body: (MIN A B)
```
### D2MINV

```
invert 2x2 matrix
VEQ:D2MINV
  [symbol]

D2MINV names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Documentation:
    invert 2x2 matrix
  Source file: /data/x/veq/src/mat-inv.lisp
```
### D2MM

```
multiply mat * mat
of type: DVEC
VEQ:D2MM
  [symbol]

D2MM names a macro:
  Lambda-list: (A*349 B*351)
  Documentation:
    multiply mat * mat
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D2MMT

```
multiply mat * (transpose mat)
of type: DVEC
VEQ:D2MMT
  [symbol]

D2MMT names a macro:
  Lambda-list: (A*407 B*409)
  Documentation:
    multiply mat * (transpose mat)
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D2MOD

```
veq context op: D2MOD
fxname: -D2MOD
args: (A B S)
body: (VALUES (MOD A S) (MOD B S))
```
### D2MROT

```
macro wrapper: (mvc #'%D2MROT ...).
see function: %D2MROT
VEQ:D2MROT
  [symbol]

D2MROT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2MROT ...).
    see function: %D2MROT
  Source file: /data/x/veq/src/mat.lisp
```
### D2MROT*

```
macro wrapper: (mvc #'%D2MROT* ...).
see function: %D2MROT*
VEQ:D2MROT*
  [symbol]

D2MROT* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2MROT* ...).
    see function: %D2MROT*
  Source file: /data/x/veq/src/mat.lisp
```
### D2MSCALE

```
macro wrapper: (mvc #'%D2MSCALE ...) in veq context.
see function: %D2MSCALE
VEQ:D2MSCALE
  [symbol]

D2MSCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2MSCALE ...) in veq context.
    see function: %D2MSCALE
  Source file: /data/x/veq/src/mat.lisp
```
### D2MT!

```
transpose matrix of type ~a in-place
VEQ:D2MT!
  [symbol]

D2MT! names a macro:
  Lambda-list: (A1)
  Documentation:
    transpose matrix of type ~a in-place
  Source file: /data/x/veq/src/mat.lisp
```
### D2MTM

```
multiply (transpose mat) * mat
of type: DVEC
VEQ:D2MTM
  [symbol]

D2MTM names a macro:
  Lambda-list: (A*436 B*438)
  Documentation:
    multiply (transpose mat) * mat
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D2MTMT

```
multiply (transpose mat) * (transpose mat)
of type: DVEC
VEQ:D2MTMT
  [symbol]

D2MTMT names a macro:
  Lambda-list: (A*378 B*380)
  Documentation:
    multiply (transpose mat) * (transpose mat)
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D2MTRANS

```
macro wrapper: (mvc #'%D2MTRANS ...) in veq context.
see function: %D2MTRANS
VEQ:D2MTRANS
  [symbol]

D2MTRANS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D2MTRANS ...) in veq context.
    see function: %D2MTRANS
  Source file: /data/x/veq/src/mat.lisp
```
### D2MTV

```
:missing:

VEQ:D2MTV
  [symbol]

D2MTV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### D2MV

```
:missing:

VEQ:D2MV
  [symbol]

D2MV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### D2MVB

```
:missing:

VEQ:D2MVB
  [symbol]
```
### D2NEG

```
veq context op: D2NEG
fxname: -D2NEG
args: (A B)
body: (VALUES (- A) (- B))
```
### D2NORM

```
veq context op: D2NORM
fxname: -D2NORM
args: (A B)
body: (MVC #'-D2ISCALE A B (MVC #'-D2LEN A B))
```
### D2NSUM

```
:missing:

VEQ:D2NSUM
  [symbol]
```
### D2ON-CIRC

```
veq context op: D2ON-CIRC
fxname: -D2ON-CIRC
args: (A RAD)
body: (MVC #'-D2SCALE (-DCOS-SIN (* A DPII)) RAD)
```
### D2ON-CIRC*

```
veq context op: D2ON-CIRC*
fxname: -D2ON-CIRC*
args: (A RAD)
body: (MVC #'-D2SCALE (-DCOS-SIN A) RAD)
```
### D2PERP

```
veq context op: D2PERP
fxname: -D2PERP
args: (A B)
body: (VALUES B (- A))
```
### D2PERP*

```
veq context op: D2PERP*
fxname: -D2PERP*
args: (A B)
body: (VALUES (- B) A)
```
### D2REP

```
:missing:

VEQ:D2REP
  [symbol]
```
### D2REP*

```
:missing:

VEQ:D2REP*
  [symbol]
```
### D2ROT

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
### D2ROTS

```
veq context op: D2ROTS
fxname: -D2ROTS
args: (X Y A SX SY)
body: (MVC #'-D2+ (MVC #'-D2ROT (-D2- X Y SX SY) A) SX SY)
```
### D2SCALE

```
veq context op: D2SCALE
fxname: -D2SCALE
args: (A B S)
body: (VALUES (* A S) (* B S))
```
### D2SQRT

```
veq context op: D2SQRT
fxname: -D2SQRT
args: (A B)
body: (VALUES (THE POS-DF (SQRT (THE POS-DF A))) (THE POS-DF (SQRT (THE POS-DF B))))
```
### D2SQUARE

```
veq context op: D2SQUARE
fxname: -D2SQUARE
args: (A B)
body: (VALUES (* A A) (* B B))
```
### D2VSET

```
:missing:

VEQ:D2VSET
  [symbol]
```
### D2~

```
:missing:

VEQ:D2~
  [symbol]
```
### D3

```
:missing:

VEQ:D3
  [symbol]
```
### D3$

```
:missing:

VEQ:D3$
  [symbol]
```
### D3$*

```
broadcast for fx: -D3*
macroname: D3$*


VEQ:D3$*
  [symbol]

D3$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$* ...).
    see function: %D3$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$*!

```
broadcast for fx: -D3*
macroname: D3$*!


VEQ:D3$*!
  [symbol]

D3$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$*! ...).
    see function: %D3$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$+

```
broadcast for fx: -D3+
macroname: D3$+


VEQ:D3$+
  [symbol]

D3$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$+ ...).
    see function: %D3$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$+!

```
broadcast for fx: -D3+
macroname: D3$+!


VEQ:D3$+!
  [symbol]

D3$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$+! ...).
    see function: %D3$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$-

```
broadcast for fx: -D3-
macroname: D3$-


VEQ:D3$-
  [symbol]

D3$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$- ...).
    see function: %D3$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$-!

```
broadcast for fx: -D3-
macroname: D3$-!


VEQ:D3$-!
  [symbol]

D3$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$-! ...).
    see function: %D3$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$/

```
broadcast for fx: -D3/
macroname: D3$/


VEQ:D3$/
  [symbol]

D3$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$/ ...).
    see function: %D3$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$/!

```
broadcast for fx: -D3/
macroname: D3$/!


VEQ:D3$/!
  [symbol]

D3$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$/! ...).
    see function: %D3$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$ABS

```
broadcast for fx: -D3ABS
macroname: D3$ABS


VEQ:D3$ABS
  [symbol]

D3$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$ABS ...).
    see function: %D3$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$ABS!

```
broadcast for fx: -D3ABS
macroname: D3$ABS!


VEQ:D3$ABS!
  [symbol]

D3$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$ABS! ...).
    see function: %D3$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$COPY

```
:missing:

VEQ:D3$COPY
  [symbol]
```
### D3$FROM

```
broadcast for fx: -D3FROM
macroname: D3$FROM


VEQ:D3$FROM
  [symbol]

D3$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$FROM ...).
    see function: %D3$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$FROM!

```
broadcast for fx: -D3FROM
macroname: D3$FROM!


VEQ:D3$FROM!
  [symbol]

D3$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$FROM! ...).
    see function: %D3$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$FXLSPACE

```
:missing:

VEQ:D3$FXLSPACE
  [symbol]
```
### D3$I-

```
broadcast for fx: -D3I-
macroname: D3$I-


VEQ:D3$I-
  [symbol]

D3$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$I- ...).
    see function: %D3$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$I-!

```
broadcast for fx: -D3I-
macroname: D3$I-!


VEQ:D3$I-!
  [symbol]

D3$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$I-! ...).
    see function: %D3$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$I/

```
broadcast for fx: -D3I/
macroname: D3$I/


VEQ:D3$I/
  [symbol]

D3$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$I/ ...).
    see function: %D3$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$I/!

```
broadcast for fx: -D3I/
macroname: D3$I/!


VEQ:D3$I/!
  [symbol]

D3$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$I/! ...).
    see function: %D3$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$ISCALE

```
broadcast for fx: -D3ISCALE
macroname: D3$ISCALE


VEQ:D3$ISCALE
  [symbol]

D3$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$ISCALE ...).
    see function: %D3$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$ISCALE!

```
broadcast for fx: -D3ISCALE
macroname: D3$ISCALE!


VEQ:D3$ISCALE!
  [symbol]

D3$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$ISCALE! ...).
    see function: %D3$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$LAST

```
:missing:

VEQ:D3$LAST
  [symbol]

D3$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
                         &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### D3$LEN

```
broadcast for fx: -D3LEN
macroname: D3$LEN


VEQ:D3$LEN
  [symbol]

D3$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$LEN ...).
    see function: %D3$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$LEN2

```
broadcast for fx: -D3LEN2
macroname: D3$LEN2


VEQ:D3$LEN2
  [symbol]

D3$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$LEN2 ...).
    see function: %D3$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$LINE

```
macro wrapper: (mvc #'%D3$LINE ...).
see function: %D3$LINE
VEQ:D3$LINE
  [symbol]

D3$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$LINE ...).
    see function: %D3$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### D3$LSPACE

```
macro wrapper: (mvc #'%D3$LSPACE ...) in veq context.
see function: %D3$LSPACE
VEQ:D3$LSPACE
  [symbol]

D3$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$LSPACE ...) in veq context.
    see function: %D3$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### D3$MAKE

```
:missing:

VEQ:D3$MAKE
  [symbol]
```
### D3$MIMA

```
:missing:

VEQ:D3$MIMA
  [symbol]

D3$MIMA names a compiled function:
  Lambda-list: (A0 &KEY (N (3$NUM A0)) INDS)
  Derived type: (FUNCTION
                 ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T) (:INDS T))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
                         DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
                         &OPTIONAL))
  Source file: /data/x/veq/src/array-mima.lisp
```
### D3$NEG

```
broadcast for fx: -D3NEG
macroname: D3$NEG


VEQ:D3$NEG
  [symbol]

D3$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$NEG ...).
    see function: %D3$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$NEG!

```
broadcast for fx: -D3NEG
macroname: D3$NEG!


VEQ:D3$NEG!
  [symbol]

D3$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$NEG! ...).
    see function: %D3$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$NORM

```
broadcast for fx: -D3NORM
macroname: D3$NORM


VEQ:D3$NORM
  [symbol]

D3$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$NORM ...).
    see function: %D3$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$NORM!

```
broadcast for fx: -D3NORM
macroname: D3$NORM!


VEQ:D3$NORM!
  [symbol]

D3$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$NORM! ...).
    see function: %D3$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$ONE

```
:missing:

VEQ:D3$ONE
  [symbol]

D3$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D3$POINT

```
macro wrapper: (mvc #'%D3$POINT ...).
see function: %D3$POINT
VEQ:D3$POINT
  [symbol]

D3$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$POINT ...).
    see function: %D3$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### D3$SCALE

```
broadcast for fx: -D3SCALE
macroname: D3$SCALE


VEQ:D3$SCALE
  [symbol]

D3$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$SCALE ...).
    see function: %D3$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$SCALE!

```
broadcast for fx: -D3SCALE
macroname: D3$SCALE!


VEQ:D3$SCALE!
  [symbol]

D3$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3$SCALE! ...).
    see function: %D3$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D3$SUM

```
:missing:

VEQ:D3$SUM
  [symbol]

D3$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
                         &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### D3$TAKE

```
:missing:

VEQ:D3$TAKE
  [symbol]

D3$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### D3$VAL

```
:missing:

VEQ:D3$VAL
  [symbol]

D3$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D3$WITH-ROWS

```
:missing:

VEQ:D3$WITH-ROWS
  [symbol]
```
### D3$ZERO

```
:missing:

VEQ:D3$ZERO
  [symbol]

D3$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D3*

```
veq context op: D3*
fxname: -D3*
args: (AX AY AZ BX BY BZ)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ))
```
### D3+

```
veq context op: D3+
fxname: -D3+
args: (AX AY AZ BX BY BZ)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ))
```
### D3-

```
veq context op: D3-
fxname: -D3-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ))
```
### D3.

```
veq context op: D3.
fxname: -D3.
args: (AX AY AZ BX BY BZ)
body: (+ (* AX BX) (* AY BY) (* AZ BZ))
```
### D3/

```
veq context op: D3/
fxname: -D3/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ))
```
### D3^

```
veq context op: D3^
fxname: -D3^
args: (A B C S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S))
```
### D3ABS

```
veq context op: D3ABS
fxname: -D3ABS
args: (A B C)
body: (VALUES (ABS A) (ABS B) (ABS C))
```
### D3CROSS

```
veq context op: D3CROSS
fxname: -D3CROSS
args: (AX AY AZ BX BY BZ)
body: (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ)) (- (* AX BY) (* AY BX)))
```
### D3DST

```
veq context op: D3DST
fxname: -D3DST
args: (AX AY AZ BX BY BZ)
body: (SQRT (THE POS-DF (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))))
```
### D3DST2

```
veq context op: D3DST2
fxname: -D3DST2
args: (AX AY AZ BX BY BZ)
body: (MVC #'+ (-D3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))
```
### D3EXP

```
veq context op: D3EXP
fxname: -D3EXP
args: (A B C)
body: (VALUES (EXP A) (EXP B) (EXP C))
```
### D3FROM

```
veq context op: D3FROM
fxname: -D3FROM
args: (AX AY AZ BX BY BZ S)
body: (-D3+ AX AY AZ (* BX S) (* BY S) (* BZ S))
```
### D3I-

```
veq context op: D3I-
fxname: -D3I-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ))
```
### D3I/

```
veq context op: D3I/
fxname: -D3I/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ))
```
### D3ISCALE

```
veq context op: D3ISCALE
fxname: -D3ISCALE
args: (A B C S)
body: (VALUES (/ A S) (/ B S) (/ C S))
```
### D3LEN

```
veq context op: D3LEN
fxname: -D3LEN
args: (A B C)
body: (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D3SQUARE A B C)))))
```
### D3LEN2

```
veq context op: D3LEN2
fxname: -D3LEN2
args: (A B C)
body: (THE POS-DF (MVC #'+ (-D3SQUARE A B C)))
```
### D3LERP

```
veq context op: D3LERP
fxname: -D3LERP
args: (AX AY AZ BX BY BZ S)
body: (-D3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S))
```
### D3LET

```
:missing:

VEQ:D3LET
  [symbol]
```
### D3MAX

```
veq context op: D3MAX
fxname: -D3MAX
args: (A B C)
body: (MAX A B C)
```
### D3MEYE

```
return eye matrix for dimension
VEQ:D3MEYE
  [symbol]

D3MEYE names a compiled function:
  Lambda-list: (&OPTIONAL (V 1.0d0))
  Derived type: (FUNCTION (&OPTIONAL DOUBLE-FLOAT)
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (9)) &OPTIONAL))
  Documentation:
    return eye matrix for dimension
  Source file: /data/x/veq/src/mat.lisp
```
### D3MID

```
veq context op: D3MID
fxname: -D3MID
args: (AX AY AZ BX BY BZ)
body: (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0))
```
### D3MIN

```
veq context op: D3MIN
fxname: -D3MIN
args: (A B C)
body: (MIN A B C)
```
### D3MINV

```
invert 3x3 matrix
VEQ:D3MINV
  [symbol]

D3MINV names a compiled function:
  Lambda-list: (A0)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Documentation:
    invert 3x3 matrix
  Source file: /data/x/veq/src/mat-inv.lisp
```
### D3MM

```
multiply mat * mat
of type: DVEC
VEQ:D3MM
  [symbol]

D3MM names a macro:
  Lambda-list: (A*465 B*467)
  Documentation:
    multiply mat * mat
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D3MMT

```
multiply mat * (transpose mat)
of type: DVEC
VEQ:D3MMT
  [symbol]

D3MMT names a macro:
  Lambda-list: (A*523 B*525)
  Documentation:
    multiply mat * (transpose mat)
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D3MOD

```
veq context op: D3MOD
fxname: -D3MOD
args: (A B C S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S))
```
### D3MROT

```
macro wrapper: (mvc #'%D3MROT ...).
see function: %D3MROT
VEQ:D3MROT
  [symbol]

D3MROT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3MROT ...).
    see function: %D3MROT
  Source file: /data/x/veq/src/mat.lisp
```
### D3MROT*

```
macro wrapper: (mvc #'%D3MROT* ...).
see function: %D3MROT*
VEQ:D3MROT*
  [symbol]

D3MROT* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3MROT* ...).
    see function: %D3MROT*
  Source file: /data/x/veq/src/mat.lisp
```
### D3MSCALE

```
macro wrapper: (mvc #'%D3MSCALE ...) in veq context.
see function: %D3MSCALE
VEQ:D3MSCALE
  [symbol]

D3MSCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3MSCALE ...) in veq context.
    see function: %D3MSCALE
  Source file: /data/x/veq/src/mat.lisp
```
### D3MT!

```
transpose matrix of type ~a in-place
VEQ:D3MT!
  [symbol]

D3MT! names a macro:
  Lambda-list: (A1)
  Documentation:
    transpose matrix of type ~a in-place
  Source file: /data/x/veq/src/mat.lisp
```
### D3MTM

```
multiply (transpose mat) * mat
of type: DVEC
VEQ:D3MTM
  [symbol]

D3MTM names a macro:
  Lambda-list: (A*552 B*554)
  Documentation:
    multiply (transpose mat) * mat
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D3MTMT

```
multiply (transpose mat) * (transpose mat)
of type: DVEC
VEQ:D3MTMT
  [symbol]

D3MTMT names a macro:
  Lambda-list: (A*494 B*496)
  Documentation:
    multiply (transpose mat) * (transpose mat)
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D3MTRANS

```
macro wrapper: (mvc #'%D3MTRANS ...) in veq context.
see function: %D3MTRANS
VEQ:D3MTRANS
  [symbol]

D3MTRANS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D3MTRANS ...) in veq context.
    see function: %D3MTRANS
  Source file: /data/x/veq/src/mat.lisp
```
### D3MTV

```
:missing:

VEQ:D3MTV
  [symbol]

D3MTV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### D3MV

```
:missing:

VEQ:D3MV
  [symbol]

D3MV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### D3MVB

```
:missing:

VEQ:D3MVB
  [symbol]
```
### D3NEG

```
veq context op: D3NEG
fxname: -D3NEG
args: (A B C)
body: (VALUES (- A) (- B) (- C))
```
### D3NORM

```
veq context op: D3NORM
fxname: -D3NORM
args: (A B C)
body: (MVC #'-D3ISCALE A B C (THE POS-DF (MVC #'-D3LEN A B C)))
```
### D3NSUM

```
:missing:

VEQ:D3NSUM
  [symbol]
```
### D3REP

```
:missing:

VEQ:D3REP
  [symbol]
```
### D3REP*

```
:missing:

VEQ:D3REP*
  [symbol]
```
### D3ROT

```
veq context op: D3ROT
fxname: -D3ROT
args: (X Y Z NX NY NZ A)
body: (LET ((COSA (COS A)))
  (DECLARE
   (DF
     COSA))
  (MVC #'-D3FROM
       (MVC #'-D3FROM (-D3SCALE X Y Z COSA) (-D3CROSS NX NY NZ X Y Z) (SIN A))
       NX NY NZ (* (-D3. NX NY NZ X Y Z) (- 1.0d0 COSA))))
```
### D3ROTS

```
veq context op: D3ROTS
fxname: -D3ROTS
args: (X Y Z NX NY NZ A SX SY SZ)
body: (MVC #'-D3+ (MVC #'-D3ROT (-D3- X Y Z SX SY SZ) NX NY NZ A) SX SY SZ)
```
### D3SCALE

```
veq context op: D3SCALE
fxname: -D3SCALE
args: (A B C S)
body: (VALUES (* A S) (* B S) (* C S))
```
### D3SQRT

```
veq context op: D3SQRT
fxname: -D3SQRT
args: (A B C)
body: (VALUES (THE POS-DF (SQRT (THE POS-DF A))) (THE POS-DF (SQRT (THE POS-DF B)))
        (THE POS-DF (SQRT (THE POS-DF C))))
```
### D3SQUARE

```
veq context op: D3SQUARE
fxname: -D3SQUARE
args: (A B C)
body: (VALUES (THE POS-DF (* A A)) (THE POS-DF (* B B)) (THE POS-DF (* C C)))
```
### D3VSET

```
:missing:

VEQ:D3VSET
  [symbol]
```
### D3~

```
:missing:

VEQ:D3~
  [symbol]
```
### D4

```
:missing:

VEQ:D4
  [symbol]
```
### D4$

```
:missing:

VEQ:D4$
  [symbol]
```
### D4$*

```
broadcast for fx: -D4*
macroname: D4$*


VEQ:D4$*
  [symbol]

D4$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$* ...).
    see function: %D4$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$*!

```
broadcast for fx: -D4*
macroname: D4$*!


VEQ:D4$*!
  [symbol]

D4$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$*! ...).
    see function: %D4$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$+

```
broadcast for fx: -D4+
macroname: D4$+


VEQ:D4$+
  [symbol]

D4$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$+ ...).
    see function: %D4$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$+!

```
broadcast for fx: -D4+
macroname: D4$+!


VEQ:D4$+!
  [symbol]

D4$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$+! ...).
    see function: %D4$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$-

```
broadcast for fx: -D4-
macroname: D4$-


VEQ:D4$-
  [symbol]

D4$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$- ...).
    see function: %D4$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$-!

```
broadcast for fx: -D4-
macroname: D4$-!


VEQ:D4$-!
  [symbol]

D4$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$-! ...).
    see function: %D4$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$/

```
broadcast for fx: -D4/
macroname: D4$/


VEQ:D4$/
  [symbol]

D4$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$/ ...).
    see function: %D4$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$/!

```
broadcast for fx: -D4/
macroname: D4$/!


VEQ:D4$/!
  [symbol]

D4$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$/! ...).
    see function: %D4$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$ABS

```
broadcast for fx: -D4ABS
macroname: D4$ABS


VEQ:D4$ABS
  [symbol]

D4$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$ABS ...).
    see function: %D4$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$ABS!

```
broadcast for fx: -D4ABS
macroname: D4$ABS!


VEQ:D4$ABS!
  [symbol]

D4$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$ABS! ...).
    see function: %D4$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$COPY

```
:missing:

VEQ:D4$COPY
  [symbol]
```
### D4$FROM

```
broadcast for fx: -D4FROM
macroname: D4$FROM


VEQ:D4$FROM
  [symbol]

D4$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$FROM ...).
    see function: %D4$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$FROM!

```
broadcast for fx: -D4FROM
macroname: D4$FROM!


VEQ:D4$FROM!
  [symbol]

D4$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$FROM! ...).
    see function: %D4$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$FXLSPACE

```
:missing:

VEQ:D4$FXLSPACE
  [symbol]
```
### D4$I-

```
broadcast for fx: -D4I-
macroname: D4$I-


VEQ:D4$I-
  [symbol]

D4$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$I- ...).
    see function: %D4$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$I-!

```
broadcast for fx: -D4I-
macroname: D4$I-!


VEQ:D4$I-!
  [symbol]

D4$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$I-! ...).
    see function: %D4$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$I/

```
broadcast for fx: -D4I/
macroname: D4$I/


VEQ:D4$I/
  [symbol]

D4$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$I/ ...).
    see function: %D4$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$I/!

```
broadcast for fx: -D4I/
macroname: D4$I/!


VEQ:D4$I/!
  [symbol]

D4$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$I/! ...).
    see function: %D4$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$ISCALE

```
broadcast for fx: -D4ISCALE
macroname: D4$ISCALE


VEQ:D4$ISCALE
  [symbol]

D4$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$ISCALE ...).
    see function: %D4$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$ISCALE!

```
broadcast for fx: -D4ISCALE
macroname: D4$ISCALE!


VEQ:D4$ISCALE!
  [symbol]

D4$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$ISCALE! ...).
    see function: %D4$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$LAST

```
:missing:

VEQ:D4$LAST
  [symbol]

D4$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
                         DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### D4$LEN

```
broadcast for fx: -D4LEN
macroname: D4$LEN


VEQ:D4$LEN
  [symbol]

D4$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$LEN ...).
    see function: %D4$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$LEN2

```
broadcast for fx: -D4LEN2
macroname: D4$LEN2


VEQ:D4$LEN2
  [symbol]

D4$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$LEN2 ...).
    see function: %D4$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$LINE

```
macro wrapper: (mvc #'%D4$LINE ...).
see function: %D4$LINE
VEQ:D4$LINE
  [symbol]

D4$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$LINE ...).
    see function: %D4$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### D4$LSPACE

```
macro wrapper: (mvc #'%D4$LSPACE ...) in veq context.
see function: %D4$LSPACE
VEQ:D4$LSPACE
  [symbol]

D4$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$LSPACE ...) in veq context.
    see function: %D4$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### D4$MAKE

```
:missing:

VEQ:D4$MAKE
  [symbol]
```
### D4$NEG

```
broadcast for fx: -D4NEG
macroname: D4$NEG


VEQ:D4$NEG
  [symbol]

D4$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$NEG ...).
    see function: %D4$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$NEG!

```
broadcast for fx: -D4NEG
macroname: D4$NEG!


VEQ:D4$NEG!
  [symbol]

D4$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$NEG! ...).
    see function: %D4$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$NORM

```
broadcast for fx: -D4NORM
macroname: D4$NORM


VEQ:D4$NORM
  [symbol]

D4$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$NORM ...).
    see function: %D4$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$NORM!

```
broadcast for fx: -D4NORM
macroname: D4$NORM!


VEQ:D4$NORM!
  [symbol]

D4$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$NORM! ...).
    see function: %D4$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$ONE

```
:missing:

VEQ:D4$ONE
  [symbol]

D4$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D4$POINT

```
macro wrapper: (mvc #'%D4$POINT ...).
see function: %D4$POINT
VEQ:D4$POINT
  [symbol]

D4$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$POINT ...).
    see function: %D4$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### D4$SCALE

```
broadcast for fx: -D4SCALE
macroname: D4$SCALE


VEQ:D4$SCALE
  [symbol]

D4$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$SCALE ...).
    see function: %D4$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$SCALE!

```
broadcast for fx: -D4SCALE
macroname: D4$SCALE!


VEQ:D4$SCALE!
  [symbol]

D4$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%D4$SCALE! ...).
    see function: %D4$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### D4$SUM

```
:missing:

VEQ:D4$SUM
  [symbol]

D4$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) &KEY (:N T))
                 (VALUES DOUBLE-FLOAT DOUBLE-FLOAT DOUBLE-FLOAT
                         DOUBLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### D4$TAKE

```
:missing:

VEQ:D4$TAKE
  [symbol]

D4$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### D4$VAL

```
:missing:

VEQ:D4$VAL
  [symbol]

D4$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D4$WITH-ROWS

```
:missing:

VEQ:D4$WITH-ROWS
  [symbol]
```
### D4$ZERO

```
:missing:

VEQ:D4$ZERO
  [symbol]

D4$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### D4*

```
veq context op: D4*
fxname: -D4*
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```
### D4+

```
veq context op: D4+
fxname: -D4+
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW))
```
### D4-

```
veq context op: D4-
fxname: -D4-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW))
```
### D4.

```
veq context op: D4.
fxname: -D4.
args: (AX AY AZ AW BX BY BZ BW)
body: (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```
### D4/

```
veq context op: D4/
fxname: -D4/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW))
```
### D4^

```
veq context op: D4^
fxname: -D4^
args: (A B C D S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S) (EXPT D S))
```
### D4ABS

```
veq context op: D4ABS
fxname: -D4ABS
args: (A B C D)
body: (VALUES (ABS A) (ABS B) (ABS C) (ABS D))
```
### D4DST

```
veq context op: D4DST
fxname: -D4DST
args: (AX AY AZ AW BX BY BZ BW)
body: (SQRT
 (THE POS-DF (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))))
```
### D4DST2

```
veq context op: D4DST2
fxname: -D4DST2
args: (AX AY AZ AW BX BY BZ BW)
body: (MVC #'+ (-D4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))
```
### D4EXP

```
veq context op: D4EXP
fxname: -D4EXP
args: (A B C D)
body: (VALUES (EXP A) (EXP B) (EXP C) (EXP D))
```
### D4FROM

```
veq context op: D4FROM
fxname: -D4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body: (-D4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S))
```
### D4I-

```
veq context op: D4I-
fxname: -D4I-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))
```
### D4I/

```
veq context op: D4I/
fxname: -D4I/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW))
```
### D4ISCALE

```
veq context op: D4ISCALE
fxname: -D4ISCALE
args: (A B C D S)
body: (VALUES (/ A S) (/ B S) (/ C S) (/ D S))
```
### D4LEN

```
veq context op: D4LEN
fxname: -D4LEN
args: (A B C D)
body: (THE POS-DF (SQRT (THE POS-DF (MVC #'+ (-D4SQUARE A B C D)))))
```
### D4LEN2

```
veq context op: D4LEN2
fxname: -D4LEN2
args: (A B C D)
body: (THE POS-DF (MVC #'+ (-D4SQUARE A B C D)))
```
### D4LERP

```
veq context op: D4LERP
fxname: -D4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body: (-D4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
 (* (- BW AW) S))
```
### D4LET

```
:missing:

VEQ:D4LET
  [symbol]
```
### D4MAX

```
veq context op: D4MAX
fxname: -D4MAX
args: (A B C D)
body: (MAX A B C D)
```
### D4MEYE

```
return eye matrix for dimension
VEQ:D4MEYE
  [symbol]

D4MEYE names a compiled function:
  Lambda-list: (&OPTIONAL (V 1.0d0))
  Derived type: (FUNCTION (&OPTIONAL DOUBLE-FLOAT)
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (16)) &OPTIONAL))
  Documentation:
    return eye matrix for dimension
  Source file: /data/x/veq/src/mat.lisp
```
### D4MID

```
veq context op: D4MID
fxname: -D4MID
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* (+ BX AX) 0.5d0) (* (+ BY AY) 0.5d0) (* (+ BZ AZ) 0.5d0)
        (* (+ BW AW) 0.5d0))
```
### D4MIN

```
veq context op: D4MIN
fxname: -D4MIN
args: (A B C D)
body: (MIN A B C D)
```
### D4MINV

```
invert 4x4 matrix
VEQ:D4MINV
  [symbol]

D4MINV names a compiled function:
  Lambda-list: (A0)
  Derived type: (FUNCTION ((SIMPLE-ARRAY DOUBLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
  Documentation:
    invert 4x4 matrix
  Source file: /data/x/veq/src/mat-inv.lisp
```
### D4MM

```
multiply mat * mat
of type: DVEC
VEQ:D4MM
  [symbol]

D4MM names a macro:
  Lambda-list: (A*581 B*583)
  Documentation:
    multiply mat * mat
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D4MMT

```
multiply mat * (transpose mat)
of type: DVEC
VEQ:D4MMT
  [symbol]

D4MMT names a macro:
  Lambda-list: (A*639 B*641)
  Documentation:
    multiply mat * (transpose mat)
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D4MOD

```
veq context op: D4MOD
fxname: -D4MOD
args: (A B C D S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S) (MOD D S))
```
### D4MT!

```
transpose matrix of type ~a in-place
VEQ:D4MT!
  [symbol]

D4MT! names a macro:
  Lambda-list: (A1)
  Documentation:
    transpose matrix of type ~a in-place
  Source file: /data/x/veq/src/mat.lisp
```
### D4MTM

```
multiply (transpose mat) * mat
of type: DVEC
VEQ:D4MTM
  [symbol]

D4MTM names a macro:
  Lambda-list: (A*668 B*670)
  Documentation:
    multiply (transpose mat) * mat
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D4MTMT

```
multiply (transpose mat) * (transpose mat)
of type: DVEC
VEQ:D4MTMT
  [symbol]

D4MTMT names a macro:
  Lambda-list: (A*610 B*612)
  Documentation:
    multiply (transpose mat) * (transpose mat)
    of type: DVEC
  Source file: /data/x/veq/src/mat.lisp
```
### D4MTV

```
:missing:

VEQ:D4MTV
  [symbol]

D4MTV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### D4MV

```
:missing:

VEQ:D4MV
  [symbol]

D4MV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### D4MVB

```
:missing:

VEQ:D4MVB
  [symbol]
```
### D4NEG

```
veq context op: D4NEG
fxname: -D4NEG
args: (A B C D)
body: (VALUES (- A) (- B) (- C) (- D))
```
### D4NORM

```
veq context op: D4NORM
fxname: -D4NORM
args: (A B C D)
body: (MVC #'-D4ISCALE A B C D (THE POS-DF (MVC #'-D4LEN A B C D)))
```
### D4NSUM

```
:missing:

VEQ:D4NSUM
  [symbol]
```
### D4REP

```
:missing:

VEQ:D4REP
  [symbol]
```
### D4REP*

```
:missing:

VEQ:D4REP*
  [symbol]
```
### D4SCALE

```
veq context op: D4SCALE
fxname: -D4SCALE
args: (A B C D S)
body: (VALUES (* A S) (* B S) (* C S) (* D S))
```
### D4SQRT

```
veq context op: D4SQRT
fxname: -D4SQRT
args: (A B C D)
body: (VALUES (THE POS-DF (SQRT (THE POS-DF A))) (THE POS-DF (SQRT (THE POS-DF B)))
        (THE POS-DF (SQRT (THE POS-DF C))) (THE POS-DF (SQRT (THE POS-DF D))))
```
### D4SQUARE

```
veq context op: D4SQUARE
fxname: -D4SQUARE
args: (A B C D)
body: (VALUES (THE POS-DF (* A A)) (THE POS-DF (* B B)) (THE POS-DF (* C C))
        (THE POS-DF (* D D)))
```
### D4VSET

```
:missing:

VEQ:D4VSET
  [symbol]
```
### D4~

```
:missing:

VEQ:D4~
  [symbol]
```
### D?

```
describe argument
VEQ:D?
  [symbol]

D? names a compiled function:
  Lambda-list: (F)
  Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
  Documentation:
    describe argument
  Source file: /data/x/veq/src/utils.lisp
```
### D^

```
veq context op: D^
fxname: -D^
args: (A S)
body: (EXPT A S)
```
### D_

```
create dvec from body: (d_ '(1d0 2d0 3d0))
VEQ:D_
  [symbol]

D_ names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    create dvec from body: (d_ '(1d0 2d0 3d0))
  Source file: /data/x/veq/src/array-utils.lisp
```
### DABS

```
veq context op: DABS
fxname: -DABS
args: (A)
body: (ABS A)
```
### DCLAMP

```
veq context op: DCLAMP
fxname: -DCLAMP
args: (X)
body: (MIN 1.0d0 (MAX 0.0d0 X))
```
### DCLAMP*

```
veq context op: DCLAMP*
fxname: -DCLAMP*
args: (X MI MA)
body: (MIN MA (MAX MI X))
```
### DCOS-SIN

```
veq context op: DCOS-SIN
fxname: -DCOS-SIN
args: (A)
body: (VALUES (COS A) (SIN A))
```
### DDEG->RAD

```
veq context op: DDEG->RAD
fxname: -DDEG->RAD
args: (DEG)
body: (* DPI (/ DEG 180.0d0))
```
### DEASE-IN-BACK

```
ease in:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0d0 S) X) S))
VEQ:DEASE-IN-BACK
  [symbol]

DEASE-IN-BACK names a compiled function:
  Lambda-list: (X &OPTIONAL (S 1.7015800476074219d0))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
                         &OPTIONAL))
  Documentation:
    ease in:
    arg: (X &OPTIONAL (S 1.70158))
    body: (* X X (- (* (+ 1.0d0 S) X) S))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-CIRC

```
ease in:
arg: (X)
body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
VEQ:DEASE-IN-CIRC
  [symbol]

DEASE-IN-CIRC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (COMPLEX DOUBLE-FLOAT)
                      (DOUBLE-FLOAT -0.0d0 1.0d0))
                  &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-CUBIC

```
ease in:
arg: (X)
body: (* X X X)
VEQ:DEASE-IN-CUBIC
  [symbol]

DEASE-IN-CUBIC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (* X X X)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-ELASTIC

```
ease in:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
        (-
         (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
            (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
VEQ:DEASE-IN-ELASTIC
  [symbol]

DEASE-IN-ELASTIC names a compiled function:
  Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
  Derived type: (FUNCTION (T &OPTIONAL T T)
                 (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
                         &OPTIONAL))
  Documentation:
    ease in:
    arg: (X &OPTIONAL (P 0.3) (S NIL))
    body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
            (-
             (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
                (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-EXP

```
ease in:
arg: (X)
body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
VEQ:DEASE-IN-EXP
  [symbol]

DEASE-IN-EXP names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES (DOUBLE-FLOAT 0.0d0) &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-LINEAR

```
ease in:
arg: (X)
body: X
VEQ:DEASE-IN-LINEAR
  [symbol]

DEASE-IN-LINEAR names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: X
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-BACK

```
ease in-out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0d0 S) X) S))
VEQ:DEASE-IN-OUT-BACK
  [symbol]

DEASE-IN-OUT-BACK names a compiled function:
  Lambda-list: (X &OPTIONAL (S 1.7015800476074219d0))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR (COMPLEX DOUBLE-FLOAT) DOUBLE-FLOAT)
                         &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X &OPTIONAL (S 1.70158))
    body: (* X X (- (* (+ 1.0d0 S) X) S))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-CIRC

```
ease in-out:
arg: (X)
body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
VEQ:DEASE-IN-OUT-CIRC
  [symbol]

DEASE-IN-OUT-CIRC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (DOUBLE-FLOAT -0.0d0 1.0d0)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-CUBIC

```
ease in-out:
arg: (X)
body: (* X X X)
VEQ:DEASE-IN-OUT-CUBIC
  [symbol]

DEASE-IN-OUT-CUBIC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (* X X X)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-ELASTIC

```
ease in-out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
        (-
         (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
            (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
VEQ:DEASE-IN-OUT-ELASTIC
  [symbol]

DEASE-IN-OUT-ELASTIC names a compiled function:
  Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
  Derived type: (FUNCTION (T &OPTIONAL T T)
                 (VALUES (OR (COMPLEX DOUBLE-FLOAT) DOUBLE-FLOAT)
                         &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X &OPTIONAL (P 0.3) (S NIL))
    body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
            (-
             (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
                (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-EXP

```
ease in-out:
arg: (X)
body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
VEQ:DEASE-IN-OUT-EXP
  [symbol]

DEASE-IN-OUT-EXP names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-LINEAR

```
ease in-out:
arg: (X)
body: X
VEQ:DEASE-IN-OUT-LINEAR
  [symbol]

DEASE-IN-OUT-LINEAR names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: X
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-QUART

```
ease in-out:
arg: (X)
body: (EXPT X 4.0d0)
VEQ:DEASE-IN-OUT-QUART
  [symbol]

DEASE-IN-OUT-QUART names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (EXPT X 4.0d0)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-QUINT

```
ease in-out:
arg: (X)
body: (EXPT X 5.0d0)
VEQ:DEASE-IN-OUT-QUINT
  [symbol]

DEASE-IN-OUT-QUINT names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (EXPT X 5.0d0)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-OUT-SIN

```
ease in-out:
arg: (X)
body: (- 1.0d0 (COS (* X DPI5)))
VEQ:DEASE-IN-OUT-SIN
  [symbol]

DEASE-IN-OUT-SIN names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES (DOUBLE-FLOAT 0.0d0 1.0d0) &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (- 1.0d0 (COS (* X DPI5)))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-QUART

```
ease in:
arg: (X)
body: (EXPT X 4.0d0)
VEQ:DEASE-IN-QUART
  [symbol]

DEASE-IN-QUART names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (EXPT X 4.0d0)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-QUINT

```
ease in:
arg: (X)
body: (EXPT X 5.0d0)
VEQ:DEASE-IN-QUINT
  [symbol]

DEASE-IN-QUINT names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (EXPT X 5.0d0)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-IN-SIN

```
ease in:
arg: (X)
body: (- 1.0d0 (COS (* X DPI5)))
VEQ:DEASE-IN-SIN
  [symbol]

DEASE-IN-SIN names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES (DOUBLE-FLOAT 0.0d0 2.0d0) &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (- 1.0d0 (COS (* X DPI5)))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-BACK

```
ease out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0d0 S) X) S))
VEQ:DEASE-OUT-BACK
  [symbol]

DEASE-OUT-BACK names a compiled function:
  Lambda-list: (X &OPTIONAL (S 1.7015800476074219d0))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
                         &OPTIONAL))
  Documentation:
    ease out:
    arg: (X &OPTIONAL (S 1.70158))
    body: (* X X (- (* (+ 1.0d0 S) X) S))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-CIRC

```
ease out:
arg: (X)
body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
VEQ:DEASE-OUT-CIRC
  [symbol]

DEASE-OUT-CIRC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (COMPLEX DOUBLE-FLOAT)
                      (DOUBLE-FLOAT 0.0d0 1.0d0))
                  &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (- (- (SQRT (- 1.0d0 (* X X))) 1.0d0))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-CUBIC

```
ease out:
arg: (X)
body: (* X X X)
VEQ:DEASE-OUT-CUBIC
  [symbol]

DEASE-OUT-CUBIC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (* X X X)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-ELASTIC

```
ease out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
        (-
         (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
            (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
VEQ:DEASE-OUT-ELASTIC
  [symbol]

DEASE-OUT-ELASTIC names a compiled function:
  Lambda-list: (X &OPTIONAL (P 0.30000001192092896d0) (S NIL))
  Derived type: (FUNCTION (T &OPTIONAL T T)
                 (VALUES (OR DOUBLE-FLOAT (COMPLEX DOUBLE-FLOAT))
                         &OPTIONAL))
  Documentation:
    ease out:
    arg: (X &OPTIONAL (P 0.3) (S NIL))
    body: (LET ((S (OR S (* (ASIN 1.0d0) (/ P DPII)))))
            (-
             (* (EXPT 2.0d0 (* 10.0d0 (- X 1)))
                (SIN (/ (* (- (- X 1.0d0) S) DPII) P)))))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-EXP

```
ease out:
arg: (X)
body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
VEQ:DEASE-OUT-EXP
  [symbol]

DEASE-OUT-EXP names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES (DOUBLE-FLOAT * 1.0d0) &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (EXPT 2.0d0 (* 10.0d0 (- X 1)))
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-LINEAR

```
ease out:
arg: (X)
body: X
VEQ:DEASE-OUT-LINEAR
  [symbol]

DEASE-OUT-LINEAR names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: X
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-QUART

```
ease out:
arg: (X)
body: (EXPT X 4.0d0)
VEQ:DEASE-OUT-QUART
  [symbol]

DEASE-OUT-QUART names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (EXPT X 4.0d0)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-QUINT

```
ease out:
arg: (X)
body: (EXPT X 5.0d0)
VEQ:DEASE-OUT-QUINT
  [symbol]

DEASE-OUT-QUINT names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (EXPT X 5.0d0)
  Source file: /data/x/veq/src/easing.lisp
```
### DEASE-OUT-SIN

```
ease out:
arg: (X)
body: (- 1.0d0 (COS (* X DPI5)))
VEQ:DEASE-OUT-SIN
  [symbol]

DEASE-OUT-SIN names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES (DOUBLE-FLOAT -1.0d0 1.0d0) &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (- 1.0d0 (COS (* X DPI5)))
  Source file: /data/x/veq/src/easing.lisp
```
### DEF*

```
define function, and corresponding macro, without veq enabled.
VEQ:DEF*
  [symbol]

DEF* names a macro:
  Lambda-list: (MNAME &BODY BODY)
  Documentation:
    define function, and corresponding macro, without veq enabled.
  Source file: /data/x/veq/src/macros.lisp
```
### DEXP

```
veq context op: DEXP
fxname: -DEXP
args: (A)
body: (VALUES (EXP A))
```
### DF

```
:missing:

VEQ:DF
  [symbol]

DF names a macro:
  Lambda-list: (&BODY BODY)
  Source file: /data/x/veq/src/utils.lisp

DF names a type-specifier:
  Lambda-list: ()
  Expansion: DOUBLE-FLOAT
```
### DF*

```
:missing:

VEQ:DF*
  [symbol]

DF* names a macro:
  Lambda-list: (&BODY BODY)
  Source file: /data/x/veq/src/utils.lisp
```
### DFL

```
return (values (df a) (df b ..) from (list a b ..)
VEQ:DFL
  [symbol]

DFL names a compiled function:
  Lambda-list: (L)
  Derived type: (FUNCTION (LIST) *)
  Documentation:
    return (values (df a) (df b ..) from (list a b ..)
  Source file: /data/x/veq/src/utils.lisp
```
### DFROM

```
veq context op: DFROM
fxname: -DFROM
args: (AX BX S)
body: (+ AX (* BX S))
```
### DI-

```
veq context op: DI-
fxname: -DI-
args: (A B)
body: (- B A)
```
### DI/

```
veq context op: DI/
fxname: -DI/
args: (A B)
body: (/ B A)
```
### DISCALE

```
veq context op: DISCALE
fxname: -DISCALE
args: (A S)
body: (VALUES (/ A S))
```
### DLEN

```
veq context op: DLEN
fxname: -DLEN
args: (A)
body: (THE POS-DF A)
```
### DLEN2

```
veq context op: DLEN2
fxname: -DLEN2
args: (A)
body: (THE POS-DF (MVC #'+ (-DSQUARE A)))
```
### DLERP

```
veq context op: DLERP
fxname: -DLERP
args: (AX BX S)
body: (+ AX (* (- BX AX) S))
```
### DMID

```
veq context op: DMID
fxname: -DMID
args: (AX BX)
body: (* 0.5d0 (+ AX BX))
```
### DMOD

```
veq context op: DMOD
fxname: -DMOD
args: (A S)
body: (MOD A S)
```
### DNEG

```
veq context op: DNEG
fxname: -DNEG
args: (A)
body: (- A)
```
### DNORM

```
veq context op: DNORM
fxname: -DNORM
args: (A)
body: (MVC #'-DISCALE A (MVC #'-DLEN A))
```
### DNSUM

```
:missing:

VEQ:DNSUM
  [symbol]
```
### DPI

```
:missing:

VEQ:DPI
  [symbol]

DPI names a constant variable:
  Declared type: DOUBLE-FLOAT
  Value: 3.141592653589793d0
```
### DPI5

```
:missing:

VEQ:DPI5
  [symbol]

DPI5 names a constant variable:
  Declared type: DOUBLE-FLOAT
  Value: 1.5707963267948966d0
```
### DPII

```
:missing:

VEQ:DPII
  [symbol]

DPII names a constant variable:
  Declared type: DOUBLE-FLOAT
  Value: 6.283185307179586d0
```
### DREP

```
:missing:

VEQ:DREP
  [symbol]
```
### DREP*

```
:missing:

VEQ:DREP*
  [symbol]
```
### DSB

```
:missing:

VEQ:DSB
  [symbol]

DSB names a macro:
  Lambda-list: (&REST ARGS)
  Source file: /data/x/veq/src/utils.lisp
```
### DSCALE

```
veq context op: DSCALE
fxname: -DSCALE
args: (A S)
body: (VALUES (* A S))
```
### DSIN-COS

```
veq context op: DSIN-COS
fxname: -DSIN-COS
args: (A)
body: (VALUES (SIN A) (COS A))
```
### DSQRT

```
veq context op: DSQRT
fxname: -DSQRT
args: (A)
body: (THE POS-DF (SQRT (THE POS-DF A)))
```
### DSQUARE

```
veq context op: DSQUARE
fxname: -DSQUARE
args: (A)
body: (* A A)
```
### DVEC

```
:missing:

VEQ:DVEC
  [symbol]

DVEC names a type-specifier:
  Lambda-list: ()
  Expansion: (SIMPLE-ARRAY VEQ:DF)
```
### DVLET

```
:missing:

VEQ:DVLET
  [symbol]
```
### DVSET

```
:missing:

VEQ:DVSET
  [symbol]
```
### DW

```
macro. reorder arguments (X Y Z W) as (W), (IGNORE Z Y X).
```
### DWITH-ARRAYS

```
:missing:

VEQ:DWITH-ARRAYS
  [symbol]
```
### DWW

```
macro. reorder arguments (X Y Z W) as (W W), (IGNORE Z Y X).
```
### DWWW

```
macro. reorder arguments (X Y Z W) as (W W W), (IGNORE Z Y X).
```
### DWWWW

```
macro. reorder arguments (X Y Z W) as (W W W W), (IGNORE Z Y X).
```
### DWWWX

```
macro. reorder arguments (X Y Z W) as (W W W X), (IGNORE Z Y).
```
### DWWWY

```
macro. reorder arguments (X Y Z W) as (W W W Y), (IGNORE Z X).
```
### DWWWZ

```
macro. reorder arguments (X Y Z W) as (W W W Z), (IGNORE Y X).
```
### DWWX

```
macro. reorder arguments (X Y Z W) as (W W X), (IGNORE Z Y).
```
### DWWXW

```
macro. reorder arguments (X Y Z W) as (W W X W), (IGNORE Z Y).
```
### DWWXX

```
macro. reorder arguments (X Y Z W) as (W W X X), (IGNORE Z Y).
```
### DWWXY

```
macro. reorder arguments (X Y Z W) as (W W X Y), (IGNORE Z).
```
### DWWXZ

```
macro. reorder arguments (X Y Z W) as (W W X Z), (IGNORE Y).
```
### DWWY

```
macro. reorder arguments (X Y Z W) as (W W Y), (IGNORE Z X).
```
### DWWYW

```
macro. reorder arguments (X Y Z W) as (W W Y W), (IGNORE Z X).
```
### DWWYX

```
macro. reorder arguments (X Y Z W) as (W W Y X), (IGNORE Z).
```
### DWWYY

```
macro. reorder arguments (X Y Z W) as (W W Y Y), (IGNORE Z X).
```
### DWWYZ

```
macro. reorder arguments (X Y Z W) as (W W Y Z), (IGNORE X).
```
### DWWZ

```
macro. reorder arguments (X Y Z W) as (W W Z), (IGNORE Y X).
```
### DWWZW

```
macro. reorder arguments (X Y Z W) as (W W Z W), (IGNORE Y X).
```
### DWWZX

```
macro. reorder arguments (X Y Z W) as (W W Z X), (IGNORE Y).
```
### DWWZY

```
macro. reorder arguments (X Y Z W) as (W W Z Y), (IGNORE X).
```
### DWWZZ

```
macro. reorder arguments (X Y Z W) as (W W Z Z), (IGNORE Y X).
```
### DWX

```
macro. reorder arguments (X Y Z W) as (W X), (IGNORE Z Y).
```
### DWXW

```
macro. reorder arguments (X Y Z W) as (W X W), (IGNORE Z Y).
```
### DWXWW

```
macro. reorder arguments (X Y Z W) as (W X W W), (IGNORE Z Y).
```
### DWXWX

```
macro. reorder arguments (X Y Z W) as (W X W X), (IGNORE Z Y).
```
### DWXWY

```
macro. reorder arguments (X Y Z W) as (W X W Y), (IGNORE Z).
```
### DWXWZ

```
macro. reorder arguments (X Y Z W) as (W X W Z), (IGNORE Y).
```
### DWXX

```
macro. reorder arguments (X Y Z W) as (W X X), (IGNORE Z Y).
```
### DWXXW

```
macro. reorder arguments (X Y Z W) as (W X X W), (IGNORE Z Y).
```
### DWXXX

```
macro. reorder arguments (X Y Z W) as (W X X X), (IGNORE Z Y).
```
### DWXXY

```
macro. reorder arguments (X Y Z W) as (W X X Y), (IGNORE Z).
```
### DWXXZ

```
macro. reorder arguments (X Y Z W) as (W X X Z), (IGNORE Y).
```
### DWXY

```
macro. reorder arguments (X Y Z W) as (W X Y), (IGNORE Z).
```
### DWXYW

```
macro. reorder arguments (X Y Z W) as (W X Y W), (IGNORE Z).
```
### DWXYX

```
macro. reorder arguments (X Y Z W) as (W X Y X), (IGNORE Z).
```
### DWXYY

```
macro. reorder arguments (X Y Z W) as (W X Y Y), (IGNORE Z).
```
### DWXYZ

```
macro. reorder arguments (X Y Z W) as (W X Y Z), (IGNORE).
```
### DWXZ

```
macro. reorder arguments (X Y Z W) as (W X Z), (IGNORE Y).
```
### DWXZW

```
macro. reorder arguments (X Y Z W) as (W X Z W), (IGNORE Y).
```
### DWXZX

```
macro. reorder arguments (X Y Z W) as (W X Z X), (IGNORE Y).
```
### DWXZY

```
macro. reorder arguments (X Y Z W) as (W X Z Y), (IGNORE).
```
### DWXZZ

```
macro. reorder arguments (X Y Z W) as (W X Z Z), (IGNORE Y).
```
### DWY

```
macro. reorder arguments (X Y Z W) as (W Y), (IGNORE Z X).
```
### DWYW

```
macro. reorder arguments (X Y Z W) as (W Y W), (IGNORE Z X).
```
### DWYWW

```
macro. reorder arguments (X Y Z W) as (W Y W W), (IGNORE Z X).
```
### DWYWX

```
macro. reorder arguments (X Y Z W) as (W Y W X), (IGNORE Z).
```
### DWYWY

```
macro. reorder arguments (X Y Z W) as (W Y W Y), (IGNORE Z X).
```
### DWYWZ

```
macro. reorder arguments (X Y Z W) as (W Y W Z), (IGNORE X).
```
### DWYX

```
macro. reorder arguments (X Y Z W) as (W Y X), (IGNORE Z).
```
### DWYXW

```
macro. reorder arguments (X Y Z W) as (W Y X W), (IGNORE Z).
```
### DWYXX

```
macro. reorder arguments (X Y Z W) as (W Y X X), (IGNORE Z).
```
### DWYXY

```
macro. reorder arguments (X Y Z W) as (W Y X Y), (IGNORE Z).
```
### DWYXZ

```
macro. reorder arguments (X Y Z W) as (W Y X Z), (IGNORE).
```
### DWYY

```
macro. reorder arguments (X Y Z W) as (W Y Y), (IGNORE Z X).
```
### DWYYW

```
macro. reorder arguments (X Y Z W) as (W Y Y W), (IGNORE Z X).
```
### DWYYX

```
macro. reorder arguments (X Y Z W) as (W Y Y X), (IGNORE Z).
```
### DWYYY

```
macro. reorder arguments (X Y Z W) as (W Y Y Y), (IGNORE Z X).
```
### DWYYZ

```
macro. reorder arguments (X Y Z W) as (W Y Y Z), (IGNORE X).
```
### DWYZ

```
macro. reorder arguments (X Y Z W) as (W Y Z), (IGNORE X).
```
### DWYZW

```
macro. reorder arguments (X Y Z W) as (W Y Z W), (IGNORE X).
```
### DWYZX

```
macro. reorder arguments (X Y Z W) as (W Y Z X), (IGNORE).
```
### DWYZY

```
macro. reorder arguments (X Y Z W) as (W Y Z Y), (IGNORE X).
```
### DWYZZ

```
macro. reorder arguments (X Y Z W) as (W Y Z Z), (IGNORE X).
```
### DWZ

```
macro. reorder arguments (X Y Z W) as (W Z), (IGNORE Y X).
```
### DWZW

```
macro. reorder arguments (X Y Z W) as (W Z W), (IGNORE Y X).
```
### DWZWW

```
macro. reorder arguments (X Y Z W) as (W Z W W), (IGNORE Y X).
```
### DWZWX

```
macro. reorder arguments (X Y Z W) as (W Z W X), (IGNORE Y).
```
### DWZWY

```
macro. reorder arguments (X Y Z W) as (W Z W Y), (IGNORE X).
```
### DWZWZ

```
macro. reorder arguments (X Y Z W) as (W Z W Z), (IGNORE Y X).
```
### DWZX

```
macro. reorder arguments (X Y Z W) as (W Z X), (IGNORE Y).
```
### DWZXW

```
macro. reorder arguments (X Y Z W) as (W Z X W), (IGNORE Y).
```
### DWZXX

```
macro. reorder arguments (X Y Z W) as (W Z X X), (IGNORE Y).
```
### DWZXY

```
macro. reorder arguments (X Y Z W) as (W Z X Y), (IGNORE).
```
### DWZXZ

```
macro. reorder arguments (X Y Z W) as (W Z X Z), (IGNORE Y).
```
### DWZY

```
macro. reorder arguments (X Y Z W) as (W Z Y), (IGNORE X).
```
### DWZYW

```
macro. reorder arguments (X Y Z W) as (W Z Y W), (IGNORE X).
```
### DWZYX

```
macro. reorder arguments (X Y Z W) as (W Z Y X), (IGNORE).
```
### DWZYY

```
macro. reorder arguments (X Y Z W) as (W Z Y Y), (IGNORE X).
```
### DWZYZ

```
macro. reorder arguments (X Y Z W) as (W Z Y Z), (IGNORE X).
```
### DWZZ

```
macro. reorder arguments (X Y Z W) as (W Z Z), (IGNORE Y X).
```
### DWZZW

```
macro. reorder arguments (X Y Z W) as (W Z Z W), (IGNORE Y X).
```
### DWZZX

```
macro. reorder arguments (X Y Z W) as (W Z Z X), (IGNORE Y).
```
### DWZZY

```
macro. reorder arguments (X Y Z W) as (W Z Z Y), (IGNORE X).
```
### DWZZZ

```
macro. reorder arguments (X Y Z W) as (W Z Z Z), (IGNORE Y X).
```
### DX

```
macro. reorder arguments (X Y Z W) as (X), (IGNORE W Z Y).
```
### DXW

```
macro. reorder arguments (X Y Z W) as (X W), (IGNORE Z Y).
```
### DXWW

```
macro. reorder arguments (X Y Z W) as (X W W), (IGNORE Z Y).
```
### DXWWW

```
macro. reorder arguments (X Y Z W) as (X W W W), (IGNORE Z Y).
```
### DXWWX

```
macro. reorder arguments (X Y Z W) as (X W W X), (IGNORE Z Y).
```
### DXWWY

```
macro. reorder arguments (X Y Z W) as (X W W Y), (IGNORE Z).
```
### DXWWZ

```
macro. reorder arguments (X Y Z W) as (X W W Z), (IGNORE Y).
```
### DXWX

```
macro. reorder arguments (X Y Z W) as (X W X), (IGNORE Z Y).
```
### DXWXW

```
macro. reorder arguments (X Y Z W) as (X W X W), (IGNORE Z Y).
```
### DXWXX

```
macro. reorder arguments (X Y Z W) as (X W X X), (IGNORE Z Y).
```
### DXWXY

```
macro. reorder arguments (X Y Z W) as (X W X Y), (IGNORE Z).
```
### DXWXZ

```
macro. reorder arguments (X Y Z W) as (X W X Z), (IGNORE Y).
```
### DXWY

```
macro. reorder arguments (X Y Z W) as (X W Y), (IGNORE Z).
```
### DXWYW

```
macro. reorder arguments (X Y Z W) as (X W Y W), (IGNORE Z).
```
### DXWYX

```
macro. reorder arguments (X Y Z W) as (X W Y X), (IGNORE Z).
```
### DXWYY

```
macro. reorder arguments (X Y Z W) as (X W Y Y), (IGNORE Z).
```
### DXWYZ

```
macro. reorder arguments (X Y Z W) as (X W Y Z), (IGNORE).
```
### DXWZ

```
macro. reorder arguments (X Y Z W) as (X W Z), (IGNORE Y).
```
### DXWZW

```
macro. reorder arguments (X Y Z W) as (X W Z W), (IGNORE Y).
```
### DXWZX

```
macro. reorder arguments (X Y Z W) as (X W Z X), (IGNORE Y).
```
### DXWZY

```
macro. reorder arguments (X Y Z W) as (X W Z Y), (IGNORE).
```
### DXWZZ

```
macro. reorder arguments (X Y Z W) as (X W Z Z), (IGNORE Y).
```
### DXX

```
macro. reorder arguments (X Y Z W) as (X X), (IGNORE W Z Y).
```
### DXXW

```
macro. reorder arguments (X Y Z W) as (X X W), (IGNORE Z Y).
```
### DXXWW

```
macro. reorder arguments (X Y Z W) as (X X W W), (IGNORE Z Y).
```
### DXXWX

```
macro. reorder arguments (X Y Z W) as (X X W X), (IGNORE Z Y).
```
### DXXWY

```
macro. reorder arguments (X Y Z W) as (X X W Y), (IGNORE Z).
```
### DXXWZ

```
macro. reorder arguments (X Y Z W) as (X X W Z), (IGNORE Y).
```
### DXXX

```
macro. reorder arguments (X Y Z W) as (X X X), (IGNORE W Z Y).
```
### DXXXW

```
macro. reorder arguments (X Y Z W) as (X X X W), (IGNORE Z Y).
```
### DXXXX

```
macro. reorder arguments (X Y Z W) as (X X X X), (IGNORE W Z Y).
```
### DXXXY

```
macro. reorder arguments (X Y Z W) as (X X X Y), (IGNORE W Z).
```
### DXXXZ

```
macro. reorder arguments (X Y Z W) as (X X X Z), (IGNORE W Y).
```
### DXXY

```
macro. reorder arguments (X Y Z W) as (X X Y), (IGNORE W Z).
```
### DXXYW

```
macro. reorder arguments (X Y Z W) as (X X Y W), (IGNORE Z).
```
### DXXYX

```
macro. reorder arguments (X Y Z W) as (X X Y X), (IGNORE W Z).
```
### DXXYY

```
macro. reorder arguments (X Y Z W) as (X X Y Y), (IGNORE W Z).
```
### DXXYZ

```
macro. reorder arguments (X Y Z W) as (X X Y Z), (IGNORE W).
```
### DXXZ

```
macro. reorder arguments (X Y Z W) as (X X Z), (IGNORE W Y).
```
### DXXZW

```
macro. reorder arguments (X Y Z W) as (X X Z W), (IGNORE Y).
```
### DXXZX

```
macro. reorder arguments (X Y Z W) as (X X Z X), (IGNORE W Y).
```
### DXXZY

```
macro. reorder arguments (X Y Z W) as (X X Z Y), (IGNORE W).
```
### DXXZZ

```
macro. reorder arguments (X Y Z W) as (X X Z Z), (IGNORE W Y).
```
### DXY

```
macro. reorder arguments (X Y Z W) as (X Y), (IGNORE W Z).
```
### DXYW

```
macro. reorder arguments (X Y Z W) as (X Y W), (IGNORE Z).
```
### DXYWW

```
macro. reorder arguments (X Y Z W) as (X Y W W), (IGNORE Z).
```
### DXYWX

```
macro. reorder arguments (X Y Z W) as (X Y W X), (IGNORE Z).
```
### DXYWY

```
macro. reorder arguments (X Y Z W) as (X Y W Y), (IGNORE Z).
```
### DXYWZ

```
macro. reorder arguments (X Y Z W) as (X Y W Z), (IGNORE).
```
### DXYX

```
macro. reorder arguments (X Y Z W) as (X Y X), (IGNORE W Z).
```
### DXYXW

```
macro. reorder arguments (X Y Z W) as (X Y X W), (IGNORE Z).
```
### DXYXX

```
macro. reorder arguments (X Y Z W) as (X Y X X), (IGNORE W Z).
```
### DXYXY

```
macro. reorder arguments (X Y Z W) as (X Y X Y), (IGNORE W Z).
```
### DXYXZ

```
macro. reorder arguments (X Y Z W) as (X Y X Z), (IGNORE W).
```
### DXYY

```
macro. reorder arguments (X Y Z W) as (X Y Y), (IGNORE W Z).
```
### DXYYW

```
macro. reorder arguments (X Y Z W) as (X Y Y W), (IGNORE Z).
```
### DXYYX

```
macro. reorder arguments (X Y Z W) as (X Y Y X), (IGNORE W Z).
```
### DXYYY

```
macro. reorder arguments (X Y Z W) as (X Y Y Y), (IGNORE W Z).
```
### DXYYZ

```
macro. reorder arguments (X Y Z W) as (X Y Y Z), (IGNORE W).
```
### DXYZ

```
macro. reorder arguments (X Y Z W) as (X Y Z), (IGNORE W).
```
### DXYZW

```
macro. reorder arguments (X Y Z W) as (X Y Z W), (IGNORE).
```
### DXYZX

```
macro. reorder arguments (X Y Z W) as (X Y Z X), (IGNORE W).
```
### DXYZY

```
macro. reorder arguments (X Y Z W) as (X Y Z Y), (IGNORE W).
```
### DXYZZ

```
macro. reorder arguments (X Y Z W) as (X Y Z Z), (IGNORE W).
```
### DXZ

```
macro. reorder arguments (X Y Z W) as (X Z), (IGNORE W Y).
```
### DXZW

```
macro. reorder arguments (X Y Z W) as (X Z W), (IGNORE Y).
```
### DXZWW

```
macro. reorder arguments (X Y Z W) as (X Z W W), (IGNORE Y).
```
### DXZWX

```
macro. reorder arguments (X Y Z W) as (X Z W X), (IGNORE Y).
```
### DXZWY

```
macro. reorder arguments (X Y Z W) as (X Z W Y), (IGNORE).
```
### DXZWZ

```
macro. reorder arguments (X Y Z W) as (X Z W Z), (IGNORE Y).
```
### DXZX

```
macro. reorder arguments (X Y Z W) as (X Z X), (IGNORE W Y).
```
### DXZXW

```
macro. reorder arguments (X Y Z W) as (X Z X W), (IGNORE Y).
```
### DXZXX

```
macro. reorder arguments (X Y Z W) as (X Z X X), (IGNORE W Y).
```
### DXZXY

```
macro. reorder arguments (X Y Z W) as (X Z X Y), (IGNORE W).
```
### DXZXZ

```
macro. reorder arguments (X Y Z W) as (X Z X Z), (IGNORE W Y).
```
### DXZY

```
macro. reorder arguments (X Y Z W) as (X Z Y), (IGNORE W).
```
### DXZYW

```
macro. reorder arguments (X Y Z W) as (X Z Y W), (IGNORE).
```
### DXZYX

```
macro. reorder arguments (X Y Z W) as (X Z Y X), (IGNORE W).
```
### DXZYY

```
macro. reorder arguments (X Y Z W) as (X Z Y Y), (IGNORE W).
```
### DXZYZ

```
macro. reorder arguments (X Y Z W) as (X Z Y Z), (IGNORE W).
```
### DXZZ

```
macro. reorder arguments (X Y Z W) as (X Z Z), (IGNORE W Y).
```
### DXZZW

```
macro. reorder arguments (X Y Z W) as (X Z Z W), (IGNORE Y).
```
### DXZZX

```
macro. reorder arguments (X Y Z W) as (X Z Z X), (IGNORE W Y).
```
### DXZZY

```
macro. reorder arguments (X Y Z W) as (X Z Z Y), (IGNORE W).
```
### DXZZZ

```
macro. reorder arguments (X Y Z W) as (X Z Z Z), (IGNORE W Y).
```
### DY

```
macro. reorder arguments (X Y Z W) as (Y), (IGNORE W Z X).
```
### DYW

```
macro. reorder arguments (X Y Z W) as (Y W), (IGNORE Z X).
```
### DYWW

```
macro. reorder arguments (X Y Z W) as (Y W W), (IGNORE Z X).
```
### DYWWW

```
macro. reorder arguments (X Y Z W) as (Y W W W), (IGNORE Z X).
```
### DYWWX

```
macro. reorder arguments (X Y Z W) as (Y W W X), (IGNORE Z).
```
### DYWWY

```
macro. reorder arguments (X Y Z W) as (Y W W Y), (IGNORE Z X).
```
### DYWWZ

```
macro. reorder arguments (X Y Z W) as (Y W W Z), (IGNORE X).
```
### DYWX

```
macro. reorder arguments (X Y Z W) as (Y W X), (IGNORE Z).
```
### DYWXW

```
macro. reorder arguments (X Y Z W) as (Y W X W), (IGNORE Z).
```
### DYWXX

```
macro. reorder arguments (X Y Z W) as (Y W X X), (IGNORE Z).
```
### DYWXY

```
macro. reorder arguments (X Y Z W) as (Y W X Y), (IGNORE Z).
```
### DYWXZ

```
macro. reorder arguments (X Y Z W) as (Y W X Z), (IGNORE).
```
### DYWY

```
macro. reorder arguments (X Y Z W) as (Y W Y), (IGNORE Z X).
```
### DYWYW

```
macro. reorder arguments (X Y Z W) as (Y W Y W), (IGNORE Z X).
```
### DYWYX

```
macro. reorder arguments (X Y Z W) as (Y W Y X), (IGNORE Z).
```
### DYWYY

```
macro. reorder arguments (X Y Z W) as (Y W Y Y), (IGNORE Z X).
```
### DYWYZ

```
macro. reorder arguments (X Y Z W) as (Y W Y Z), (IGNORE X).
```
### DYWZ

```
macro. reorder arguments (X Y Z W) as (Y W Z), (IGNORE X).
```
### DYWZW

```
macro. reorder arguments (X Y Z W) as (Y W Z W), (IGNORE X).
```
### DYWZX

```
macro. reorder arguments (X Y Z W) as (Y W Z X), (IGNORE).
```
### DYWZY

```
macro. reorder arguments (X Y Z W) as (Y W Z Y), (IGNORE X).
```
### DYWZZ

```
macro. reorder arguments (X Y Z W) as (Y W Z Z), (IGNORE X).
```
### DYX

```
macro. reorder arguments (X Y Z W) as (Y X), (IGNORE W Z).
```
### DYXW

```
macro. reorder arguments (X Y Z W) as (Y X W), (IGNORE Z).
```
### DYXWW

```
macro. reorder arguments (X Y Z W) as (Y X W W), (IGNORE Z).
```
### DYXWX

```
macro. reorder arguments (X Y Z W) as (Y X W X), (IGNORE Z).
```
### DYXWY

```
macro. reorder arguments (X Y Z W) as (Y X W Y), (IGNORE Z).
```
### DYXWZ

```
macro. reorder arguments (X Y Z W) as (Y X W Z), (IGNORE).
```
### DYXX

```
macro. reorder arguments (X Y Z W) as (Y X X), (IGNORE W Z).
```
### DYXXW

```
macro. reorder arguments (X Y Z W) as (Y X X W), (IGNORE Z).
```
### DYXXX

```
macro. reorder arguments (X Y Z W) as (Y X X X), (IGNORE W Z).
```
### DYXXY

```
macro. reorder arguments (X Y Z W) as (Y X X Y), (IGNORE W Z).
```
### DYXXZ

```
macro. reorder arguments (X Y Z W) as (Y X X Z), (IGNORE W).
```
### DYXY

```
macro. reorder arguments (X Y Z W) as (Y X Y), (IGNORE W Z).
```
### DYXYW

```
macro. reorder arguments (X Y Z W) as (Y X Y W), (IGNORE Z).
```
### DYXYX

```
macro. reorder arguments (X Y Z W) as (Y X Y X), (IGNORE W Z).
```
### DYXYY

```
macro. reorder arguments (X Y Z W) as (Y X Y Y), (IGNORE W Z).
```
### DYXYZ

```
macro. reorder arguments (X Y Z W) as (Y X Y Z), (IGNORE W).
```
### DYXZ

```
macro. reorder arguments (X Y Z W) as (Y X Z), (IGNORE W).
```
### DYXZW

```
macro. reorder arguments (X Y Z W) as (Y X Z W), (IGNORE).
```
### DYXZX

```
macro. reorder arguments (X Y Z W) as (Y X Z X), (IGNORE W).
```
### DYXZY

```
macro. reorder arguments (X Y Z W) as (Y X Z Y), (IGNORE W).
```
### DYXZZ

```
macro. reorder arguments (X Y Z W) as (Y X Z Z), (IGNORE W).
```
### DYY

```
macro. reorder arguments (X Y Z W) as (Y Y), (IGNORE W Z X).
```
### DYYW

```
macro. reorder arguments (X Y Z W) as (Y Y W), (IGNORE Z X).
```
### DYYWW

```
macro. reorder arguments (X Y Z W) as (Y Y W W), (IGNORE Z X).
```
### DYYWX

```
macro. reorder arguments (X Y Z W) as (Y Y W X), (IGNORE Z).
```
### DYYWY

```
macro. reorder arguments (X Y Z W) as (Y Y W Y), (IGNORE Z X).
```
### DYYWZ

```
macro. reorder arguments (X Y Z W) as (Y Y W Z), (IGNORE X).
```
### DYYX

```
macro. reorder arguments (X Y Z W) as (Y Y X), (IGNORE W Z).
```
### DYYXW

```
macro. reorder arguments (X Y Z W) as (Y Y X W), (IGNORE Z).
```
### DYYXX

```
macro. reorder arguments (X Y Z W) as (Y Y X X), (IGNORE W Z).
```
### DYYXY

```
macro. reorder arguments (X Y Z W) as (Y Y X Y), (IGNORE W Z).
```
### DYYXZ

```
macro. reorder arguments (X Y Z W) as (Y Y X Z), (IGNORE W).
```
### DYYY

```
macro. reorder arguments (X Y Z W) as (Y Y Y), (IGNORE W Z X).
```
### DYYYW

```
macro. reorder arguments (X Y Z W) as (Y Y Y W), (IGNORE Z X).
```
### DYYYX

```
macro. reorder arguments (X Y Z W) as (Y Y Y X), (IGNORE W Z).
```
### DYYYY

```
macro. reorder arguments (X Y Z W) as (Y Y Y Y), (IGNORE W Z X).
```
### DYYYZ

```
macro. reorder arguments (X Y Z W) as (Y Y Y Z), (IGNORE W X).
```
### DYYZ

```
macro. reorder arguments (X Y Z W) as (Y Y Z), (IGNORE W X).
```
### DYYZW

```
macro. reorder arguments (X Y Z W) as (Y Y Z W), (IGNORE X).
```
### DYYZX

```
macro. reorder arguments (X Y Z W) as (Y Y Z X), (IGNORE W).
```
### DYYZY

```
macro. reorder arguments (X Y Z W) as (Y Y Z Y), (IGNORE W X).
```
### DYYZZ

```
macro. reorder arguments (X Y Z W) as (Y Y Z Z), (IGNORE W X).
```
### DYZ

```
macro. reorder arguments (X Y Z W) as (Y Z), (IGNORE W X).
```
### DYZW

```
macro. reorder arguments (X Y Z W) as (Y Z W), (IGNORE X).
```
### DYZWW

```
macro. reorder arguments (X Y Z W) as (Y Z W W), (IGNORE X).
```
### DYZWX

```
macro. reorder arguments (X Y Z W) as (Y Z W X), (IGNORE).
```
### DYZWY

```
macro. reorder arguments (X Y Z W) as (Y Z W Y), (IGNORE X).
```
### DYZWZ

```
macro. reorder arguments (X Y Z W) as (Y Z W Z), (IGNORE X).
```
### DYZX

```
macro. reorder arguments (X Y Z W) as (Y Z X), (IGNORE W).
```
### DYZXW

```
macro. reorder arguments (X Y Z W) as (Y Z X W), (IGNORE).
```
### DYZXX

```
macro. reorder arguments (X Y Z W) as (Y Z X X), (IGNORE W).
```
### DYZXY

```
macro. reorder arguments (X Y Z W) as (Y Z X Y), (IGNORE W).
```
### DYZXZ

```
macro. reorder arguments (X Y Z W) as (Y Z X Z), (IGNORE W).
```
### DYZY

```
macro. reorder arguments (X Y Z W) as (Y Z Y), (IGNORE W X).
```
### DYZYW

```
macro. reorder arguments (X Y Z W) as (Y Z Y W), (IGNORE X).
```
### DYZYX

```
macro. reorder arguments (X Y Z W) as (Y Z Y X), (IGNORE W).
```
### DYZYY

```
macro. reorder arguments (X Y Z W) as (Y Z Y Y), (IGNORE W X).
```
### DYZYZ

```
macro. reorder arguments (X Y Z W) as (Y Z Y Z), (IGNORE W X).
```
### DYZZ

```
macro. reorder arguments (X Y Z W) as (Y Z Z), (IGNORE W X).
```
### DYZZW

```
macro. reorder arguments (X Y Z W) as (Y Z Z W), (IGNORE X).
```
### DYZZX

```
macro. reorder arguments (X Y Z W) as (Y Z Z X), (IGNORE W).
```
### DYZZY

```
macro. reorder arguments (X Y Z W) as (Y Z Z Y), (IGNORE W X).
```
### DYZZZ

```
macro. reorder arguments (X Y Z W) as (Y Z Z Z), (IGNORE W X).
```
### DZ

```
macro. reorder arguments (X Y Z W) as (Z), (IGNORE W Y X).
```
### DZW

```
macro. reorder arguments (X Y Z W) as (Z W), (IGNORE Y X).
```
### DZWW

```
macro. reorder arguments (X Y Z W) as (Z W W), (IGNORE Y X).
```
### DZWWW

```
macro. reorder arguments (X Y Z W) as (Z W W W), (IGNORE Y X).
```
### DZWWX

```
macro. reorder arguments (X Y Z W) as (Z W W X), (IGNORE Y).
```
### DZWWY

```
macro. reorder arguments (X Y Z W) as (Z W W Y), (IGNORE X).
```
### DZWWZ

```
macro. reorder arguments (X Y Z W) as (Z W W Z), (IGNORE Y X).
```
### DZWX

```
macro. reorder arguments (X Y Z W) as (Z W X), (IGNORE Y).
```
### DZWXW

```
macro. reorder arguments (X Y Z W) as (Z W X W), (IGNORE Y).
```
### DZWXX

```
macro. reorder arguments (X Y Z W) as (Z W X X), (IGNORE Y).
```
### DZWXY

```
macro. reorder arguments (X Y Z W) as (Z W X Y), (IGNORE).
```
### DZWXZ

```
macro. reorder arguments (X Y Z W) as (Z W X Z), (IGNORE Y).
```
### DZWY

```
macro. reorder arguments (X Y Z W) as (Z W Y), (IGNORE X).
```
### DZWYW

```
macro. reorder arguments (X Y Z W) as (Z W Y W), (IGNORE X).
```
### DZWYX

```
macro. reorder arguments (X Y Z W) as (Z W Y X), (IGNORE).
```
### DZWYY

```
macro. reorder arguments (X Y Z W) as (Z W Y Y), (IGNORE X).
```
### DZWYZ

```
macro. reorder arguments (X Y Z W) as (Z W Y Z), (IGNORE X).
```
### DZWZ

```
macro. reorder arguments (X Y Z W) as (Z W Z), (IGNORE Y X).
```
### DZWZW

```
macro. reorder arguments (X Y Z W) as (Z W Z W), (IGNORE Y X).
```
### DZWZX

```
macro. reorder arguments (X Y Z W) as (Z W Z X), (IGNORE Y).
```
### DZWZY

```
macro. reorder arguments (X Y Z W) as (Z W Z Y), (IGNORE X).
```
### DZWZZ

```
macro. reorder arguments (X Y Z W) as (Z W Z Z), (IGNORE Y X).
```
### DZX

```
macro. reorder arguments (X Y Z W) as (Z X), (IGNORE W Y).
```
### DZXW

```
macro. reorder arguments (X Y Z W) as (Z X W), (IGNORE Y).
```
### DZXWW

```
macro. reorder arguments (X Y Z W) as (Z X W W), (IGNORE Y).
```
### DZXWX

```
macro. reorder arguments (X Y Z W) as (Z X W X), (IGNORE Y).
```
### DZXWY

```
macro. reorder arguments (X Y Z W) as (Z X W Y), (IGNORE).
```
### DZXWZ

```
macro. reorder arguments (X Y Z W) as (Z X W Z), (IGNORE Y).
```
### DZXX

```
macro. reorder arguments (X Y Z W) as (Z X X), (IGNORE W Y).
```
### DZXXW

```
macro. reorder arguments (X Y Z W) as (Z X X W), (IGNORE Y).
```
### DZXXX

```
macro. reorder arguments (X Y Z W) as (Z X X X), (IGNORE W Y).
```
### DZXXY

```
macro. reorder arguments (X Y Z W) as (Z X X Y), (IGNORE W).
```
### DZXXZ

```
macro. reorder arguments (X Y Z W) as (Z X X Z), (IGNORE W Y).
```
### DZXY

```
macro. reorder arguments (X Y Z W) as (Z X Y), (IGNORE W).
```
### DZXYW

```
macro. reorder arguments (X Y Z W) as (Z X Y W), (IGNORE).
```
### DZXYX

```
macro. reorder arguments (X Y Z W) as (Z X Y X), (IGNORE W).
```
### DZXYY

```
macro. reorder arguments (X Y Z W) as (Z X Y Y), (IGNORE W).
```
### DZXYZ

```
macro. reorder arguments (X Y Z W) as (Z X Y Z), (IGNORE W).
```
### DZXZ

```
macro. reorder arguments (X Y Z W) as (Z X Z), (IGNORE W Y).
```
### DZXZW

```
macro. reorder arguments (X Y Z W) as (Z X Z W), (IGNORE Y).
```
### DZXZX

```
macro. reorder arguments (X Y Z W) as (Z X Z X), (IGNORE W Y).
```
### DZXZY

```
macro. reorder arguments (X Y Z W) as (Z X Z Y), (IGNORE W).
```
### DZXZZ

```
macro. reorder arguments (X Y Z W) as (Z X Z Z), (IGNORE W Y).
```
### DZY

```
macro. reorder arguments (X Y Z W) as (Z Y), (IGNORE W X).
```
### DZYW

```
macro. reorder arguments (X Y Z W) as (Z Y W), (IGNORE X).
```
### DZYWW

```
macro. reorder arguments (X Y Z W) as (Z Y W W), (IGNORE X).
```
### DZYWX

```
macro. reorder arguments (X Y Z W) as (Z Y W X), (IGNORE).
```
### DZYWY

```
macro. reorder arguments (X Y Z W) as (Z Y W Y), (IGNORE X).
```
### DZYWZ

```
macro. reorder arguments (X Y Z W) as (Z Y W Z), (IGNORE X).
```
### DZYX

```
macro. reorder arguments (X Y Z W) as (Z Y X), (IGNORE W).
```
### DZYXW

```
macro. reorder arguments (X Y Z W) as (Z Y X W), (IGNORE).
```
### DZYXX

```
macro. reorder arguments (X Y Z W) as (Z Y X X), (IGNORE W).
```
### DZYXY

```
macro. reorder arguments (X Y Z W) as (Z Y X Y), (IGNORE W).
```
### DZYXZ

```
macro. reorder arguments (X Y Z W) as (Z Y X Z), (IGNORE W).
```
### DZYY

```
macro. reorder arguments (X Y Z W) as (Z Y Y), (IGNORE W X).
```
### DZYYW

```
macro. reorder arguments (X Y Z W) as (Z Y Y W), (IGNORE X).
```
### DZYYX

```
macro. reorder arguments (X Y Z W) as (Z Y Y X), (IGNORE W).
```
### DZYYY

```
macro. reorder arguments (X Y Z W) as (Z Y Y Y), (IGNORE W X).
```
### DZYYZ

```
macro. reorder arguments (X Y Z W) as (Z Y Y Z), (IGNORE W X).
```
### DZYZ

```
macro. reorder arguments (X Y Z W) as (Z Y Z), (IGNORE W X).
```
### DZYZW

```
macro. reorder arguments (X Y Z W) as (Z Y Z W), (IGNORE X).
```
### DZYZX

```
macro. reorder arguments (X Y Z W) as (Z Y Z X), (IGNORE W).
```
### DZYZY

```
macro. reorder arguments (X Y Z W) as (Z Y Z Y), (IGNORE W X).
```
### DZYZZ

```
macro. reorder arguments (X Y Z W) as (Z Y Z Z), (IGNORE W X).
```
### DZZ

```
macro. reorder arguments (X Y Z W) as (Z Z), (IGNORE W Y X).
```
### DZZW

```
macro. reorder arguments (X Y Z W) as (Z Z W), (IGNORE Y X).
```
### DZZWW

```
macro. reorder arguments (X Y Z W) as (Z Z W W), (IGNORE Y X).
```
### DZZWX

```
macro. reorder arguments (X Y Z W) as (Z Z W X), (IGNORE Y).
```
### DZZWY

```
macro. reorder arguments (X Y Z W) as (Z Z W Y), (IGNORE X).
```
### DZZWZ

```
macro. reorder arguments (X Y Z W) as (Z Z W Z), (IGNORE Y X).
```
### DZZX

```
macro. reorder arguments (X Y Z W) as (Z Z X), (IGNORE W Y).
```
### DZZXW

```
macro. reorder arguments (X Y Z W) as (Z Z X W), (IGNORE Y).
```
### DZZXX

```
macro. reorder arguments (X Y Z W) as (Z Z X X), (IGNORE W Y).
```
### DZZXY

```
macro. reorder arguments (X Y Z W) as (Z Z X Y), (IGNORE W).
```
### DZZXZ

```
macro. reorder arguments (X Y Z W) as (Z Z X Z), (IGNORE W Y).
```
### DZZY

```
macro. reorder arguments (X Y Z W) as (Z Z Y), (IGNORE W X).
```
### DZZYW

```
macro. reorder arguments (X Y Z W) as (Z Z Y W), (IGNORE X).
```
### DZZYX

```
macro. reorder arguments (X Y Z W) as (Z Z Y X), (IGNORE W).
```
### DZZYY

```
macro. reorder arguments (X Y Z W) as (Z Z Y Y), (IGNORE W X).
```
### DZZYZ

```
macro. reorder arguments (X Y Z W) as (Z Z Y Z), (IGNORE W X).
```
### DZZZ

```
macro. reorder arguments (X Y Z W) as (Z Z Z), (IGNORE W Y X).
```
### DZZZW

```
macro. reorder arguments (X Y Z W) as (Z Z Z W), (IGNORE Y X).
```
### DZZZX

```
macro. reorder arguments (X Y Z W) as (Z Z Z X), (IGNORE W Y).
```
### DZZZY

```
macro. reorder arguments (X Y Z W) as (Z Z Z Y), (IGNORE W X).
```
### DZZZZ

```
macro. reorder arguments (X Y Z W) as (Z Z Z Z), (IGNORE W Y X).
```
### D~

```
:missing:

VEQ:D~
  [symbol]
```
### EXT-SYMBOLS?

```
list all external symbols in veq. use :verbose to inlcude docstring.  use
  :pretty to print verbose output to stdout in a readable form.
VEQ:EXT-SYMBOLS?
  [symbol]

EXT-SYMBOLS? names a macro:
  Lambda-list: (&OPTIONAL MODE)
  Documentation:
    list all external symbols in veq. use :verbose to inlcude docstring.  use
      :pretty to print verbose output to stdout in a readable form.
  Source file: /data/x/veq/src/veq.lisp
```
### F

```
:missing:

VEQ:F
  [symbol]
```
### F$

```
:missing:

VEQ:F$
  [symbol]
```
### F$*

```
broadcast for fx: -F*
macroname: F$*


VEQ:F$*
  [symbol]

F$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$* ...).
    see function: %F$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$*!

```
broadcast for fx: -F*
macroname: F$*!


VEQ:F$*!
  [symbol]

F$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$*! ...).
    see function: %F$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$+

```
broadcast for fx: -F+
macroname: F$+


VEQ:F$+
  [symbol]

F$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$+ ...).
    see function: %F$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$+!

```
broadcast for fx: -F+
macroname: F$+!


VEQ:F$+!
  [symbol]

F$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$+! ...).
    see function: %F$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$-

```
broadcast for fx: -F-
macroname: F$-


VEQ:F$-
  [symbol]

F$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$- ...).
    see function: %F$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$-!

```
broadcast for fx: -F-
macroname: F$-!


VEQ:F$-!
  [symbol]

F$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$-! ...).
    see function: %F$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$/

```
broadcast for fx: -F/
macroname: F$/


VEQ:F$/
  [symbol]

F$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$/ ...).
    see function: %F$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$/!

```
broadcast for fx: -F/
macroname: F$/!


VEQ:F$/!
  [symbol]

F$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$/! ...).
    see function: %F$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$_

```
create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
   or: ($_ '((1d0 2d0) (1d0 2d0)))
VEQ:F$_
  [symbol]

F$_ names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    create array from body. use either: ($_ (loop repeat 2 collect `(1d0 2d0)))
       or: ($_ '((1d0 2d0) (1d0 2d0)))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F$ABS

```
broadcast for fx: -FABS
macroname: F$ABS


VEQ:F$ABS
  [symbol]

F$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$ABS ...).
    see function: %F$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$ABS!

```
broadcast for fx: -FABS
macroname: F$ABS!


VEQ:F$ABS!
  [symbol]

F$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$ABS! ...).
    see function: %F$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$COPY

```
:missing:

VEQ:F$COPY
  [symbol]

F$COPY names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F$COS-SIN

```
broadcast for fx: -FCOS-SIN
macroname: F$COS-SIN


VEQ:F$COS-SIN
  [symbol]

F$COS-SIN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$COS-SIN ...).
    see function: %F$COS-SIN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$FROM

```
broadcast for fx: -FFROM
macroname: F$FROM


VEQ:F$FROM
  [symbol]

F$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$FROM ...).
    see function: %F$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$FROM!

```
broadcast for fx: -FFROM
macroname: F$FROM!


VEQ:F$FROM!
  [symbol]

F$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$FROM! ...).
    see function: %F$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$FXLSPACE

```
:missing:

VEQ:F$FXLSPACE
  [symbol]
```
### F$I-

```
broadcast for fx: -FI-
macroname: F$I-


VEQ:F$I-
  [symbol]

F$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$I- ...).
    see function: %F$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$I-!

```
broadcast for fx: -FI-
macroname: F$I-!


VEQ:F$I-!
  [symbol]

F$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$I-! ...).
    see function: %F$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$I/

```
broadcast for fx: -FI/
macroname: F$I/


VEQ:F$I/
  [symbol]

F$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$I/ ...).
    see function: %F$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$I/!

```
broadcast for fx: -FI/
macroname: F$I/!


VEQ:F$I/!
  [symbol]

F$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$I/! ...).
    see function: %F$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$ISCALE

```
broadcast for fx: -FISCALE
macroname: F$ISCALE


VEQ:F$ISCALE
  [symbol]

F$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$ISCALE ...).
    see function: %F$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$ISCALE!

```
broadcast for fx: -FISCALE
macroname: F$ISCALE!


VEQ:F$ISCALE!
  [symbol]

F$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$ISCALE! ...).
    see function: %F$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$LAST

```
:missing:

VEQ:F$LAST
  [symbol]

F$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### F$LEN

```
broadcast for fx: -FLEN
macroname: F$LEN


VEQ:F$LEN
  [symbol]

F$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$LEN ...).
    see function: %F$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$LEN2

```
broadcast for fx: -FLEN2
macroname: F$LEN2


VEQ:F$LEN2
  [symbol]

F$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$LEN2 ...).
    see function: %F$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$LINE

```
macro wrapper: (mvc #'%F$LINE ...).
see function: %F$LINE
VEQ:F$LINE
  [symbol]

F$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$LINE ...).
    see function: %F$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### F$LSPACE

```
macro wrapper: (mvc #'%F$LSPACE ...) in veq context.
see function: %F$LSPACE
VEQ:F$LSPACE
  [symbol]

F$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$LSPACE ...) in veq context.
    see function: %F$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### F$MAKE

```
 create array with size (n dim), and initial value v
VEQ:F$MAKE
  [symbol]

F$MAKE names a macro:
  Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0))
  Documentation:
     create array with size (n dim), and initial value v
  Source file: /data/x/veq/src/array-utils.lisp
```
### F$MIMA

```
:missing:

VEQ:F$MIMA
  [symbol]

F$MIMA names a compiled function:
  Lambda-list: (A0 &KEY (N ($NUM A0)) INDS)
  Derived type: (FUNCTION
                 ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T) (:INDS T))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-mima.lisp
```
### F$NEG

```
broadcast for fx: -FNEG
macroname: F$NEG


VEQ:F$NEG
  [symbol]

F$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$NEG ...).
    see function: %F$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$NEG!

```
broadcast for fx: -FNEG
macroname: F$NEG!


VEQ:F$NEG!
  [symbol]

F$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$NEG! ...).
    see function: %F$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$NORM

```
broadcast for fx: -FNORM
macroname: F$NORM


VEQ:F$NORM
  [symbol]

F$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$NORM ...).
    see function: %F$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$NORM!

```
broadcast for fx: -FNORM
macroname: F$NORM!


VEQ:F$NORM!
  [symbol]

F$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$NORM! ...).
    see function: %F$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$ONE

```
:missing:

VEQ:F$ONE
  [symbol]

F$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F$POINT

```
macro wrapper: (mvc #'%F$POINT ...).
see function: %F$POINT
VEQ:F$POINT
  [symbol]

F$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$POINT ...).
    see function: %F$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### F$SCALE

```
broadcast for fx: -FSCALE
macroname: F$SCALE


VEQ:F$SCALE
  [symbol]

F$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$SCALE ...).
    see function: %F$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$SCALE!

```
broadcast for fx: -FSCALE
macroname: F$SCALE!


VEQ:F$SCALE!
  [symbol]

F$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F$SCALE! ...).
    see function: %F$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F$SUM

```
:missing:

VEQ:F$SUM
  [symbol]

F$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
                 (VALUES SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### F$TAKE

```
:missing:

VEQ:F$TAKE
  [symbol]

F$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### F$VAL

```
:missing:

VEQ:F$VAL
  [symbol]

F$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F$WITH-ROWS

```
:missing:

VEQ:F$WITH-ROWS
  [symbol]
```
### F$ZERO

```
:missing:

VEQ:F$ZERO
  [symbol]

F$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F*

```
veq context op: F*
fxname: -F*
args: (A B)
body: (* A B)
```
### F+

```
veq context op: F+
fxname: -F+
args: (A B)
body: (+ A B)
```
### F-

```
veq context op: F-
fxname: -F-
args: (A B)
body: (- A B)
```
### F/

```
veq context op: F/
fxname: -F/
args: (A B)
body: (/ A B)
```
### F2

```
:missing:

VEQ:F2
  [symbol]
```
### F2$

```
:missing:

VEQ:F2$
  [symbol]
```
### F2$*

```
broadcast for fx: -F2*
macroname: F2$*


VEQ:F2$*
  [symbol]

F2$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$* ...).
    see function: %F2$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$*!

```
broadcast for fx: -F2*
macroname: F2$*!


VEQ:F2$*!
  [symbol]

F2$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$*! ...).
    see function: %F2$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$+

```
broadcast for fx: -F2+
macroname: F2$+


VEQ:F2$+
  [symbol]

F2$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$+ ...).
    see function: %F2$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$+!

```
broadcast for fx: -F2+
macroname: F2$+!


VEQ:F2$+!
  [symbol]

F2$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$+! ...).
    see function: %F2$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$-

```
broadcast for fx: -F2-
macroname: F2$-


VEQ:F2$-
  [symbol]

F2$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$- ...).
    see function: %F2$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$-!

```
broadcast for fx: -F2-
macroname: F2$-!


VEQ:F2$-!
  [symbol]

F2$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$-! ...).
    see function: %F2$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$/

```
broadcast for fx: -F2/
macroname: F2$/


VEQ:F2$/
  [symbol]

F2$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$/ ...).
    see function: %F2$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$/!

```
broadcast for fx: -F2/
macroname: F2$/!


VEQ:F2$/!
  [symbol]

F2$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$/! ...).
    see function: %F2$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ABS

```
broadcast for fx: -F2ABS
macroname: F2$ABS


VEQ:F2$ABS
  [symbol]

F2$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ABS ...).
    see function: %F2$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ABS!

```
broadcast for fx: -F2ABS
macroname: F2$ABS!


VEQ:F2$ABS!
  [symbol]

F2$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ABS! ...).
    see function: %F2$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$CENTER

```
macro wrapper: (mvc #'%F2$CENTER ...).
see function: %F2$CENTER
VEQ:F2$CENTER
  [symbol]

F2$CENTER names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$CENTER ...).
    see function: %F2$CENTER
  Source file: /data/x/veq/src/shapes.lisp
```
### F2$CIRC

```
macro wrapper: (mvc #'%F2$CIRC ...).
see function: %F2$CIRC
VEQ:F2$CIRC
  [symbol]

F2$CIRC names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$CIRC ...).
    see function: %F2$CIRC
  Source file: /data/x/veq/src/shapes.lisp
```
### F2$COPY

```
:missing:

VEQ:F2$COPY
  [symbol]
```
### F2$FROM

```
broadcast for fx: -F2FROM
macroname: F2$FROM


VEQ:F2$FROM
  [symbol]

F2$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$FROM ...).
    see function: %F2$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$FROM!

```
broadcast for fx: -F2FROM
macroname: F2$FROM!


VEQ:F2$FROM!
  [symbol]

F2$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$FROM! ...).
    see function: %F2$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$FXLSPACE

```
:missing:

VEQ:F2$FXLSPACE
  [symbol]
```
### F2$I-

```
broadcast for fx: -F2I-
macroname: F2$I-


VEQ:F2$I-
  [symbol]

F2$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$I- ...).
    see function: %F2$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$I-!

```
broadcast for fx: -F2I-
macroname: F2$I-!


VEQ:F2$I-!
  [symbol]

F2$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$I-! ...).
    see function: %F2$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$I/

```
broadcast for fx: -F2I/
macroname: F2$I/


VEQ:F2$I/
  [symbol]

F2$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$I/ ...).
    see function: %F2$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$I/!

```
broadcast for fx: -F2I/
macroname: F2$I/!


VEQ:F2$I/!
  [symbol]

F2$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$I/! ...).
    see function: %F2$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ISCALE

```
broadcast for fx: -F2ISCALE
macroname: F2$ISCALE


VEQ:F2$ISCALE
  [symbol]

F2$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ISCALE ...).
    see function: %F2$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ISCALE!

```
broadcast for fx: -F2ISCALE
macroname: F2$ISCALE!


VEQ:F2$ISCALE!
  [symbol]

F2$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ISCALE! ...).
    see function: %F2$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$LAST

```
:missing:

VEQ:F2$LAST
  [symbol]

F2$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### F2$LEN

```
broadcast for fx: -F2LEN
macroname: F2$LEN


VEQ:F2$LEN
  [symbol]

F2$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$LEN ...).
    see function: %F2$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$LEN2

```
broadcast for fx: -F2LEN2
macroname: F2$LEN2


VEQ:F2$LEN2
  [symbol]

F2$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$LEN2 ...).
    see function: %F2$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$LINE

```
macro wrapper: (mvc #'%F2$LINE ...).
see function: %F2$LINE
VEQ:F2$LINE
  [symbol]

F2$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$LINE ...).
    see function: %F2$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### F2$LSPACE

```
macro wrapper: (mvc #'%F2$LSPACE ...) in veq context.
see function: %F2$LSPACE
VEQ:F2$LSPACE
  [symbol]

F2$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$LSPACE ...) in veq context.
    see function: %F2$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### F2$MAKE

```
:missing:

VEQ:F2$MAKE
  [symbol]
```
### F2$MIMA

```
:missing:

VEQ:F2$MIMA
  [symbol]

F2$MIMA names a compiled function:
  Lambda-list: (A0 &KEY (N (2$NUM A0)) INDS)
  Derived type: (FUNCTION
                 ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T) (:INDS T))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
                         SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-mima.lisp
```
### F2$NEG

```
broadcast for fx: -F2NEG
macroname: F2$NEG


VEQ:F2$NEG
  [symbol]

F2$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$NEG ...).
    see function: %F2$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$NEG!

```
broadcast for fx: -F2NEG
macroname: F2$NEG!


VEQ:F2$NEG!
  [symbol]

F2$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$NEG! ...).
    see function: %F2$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$NORM

```
broadcast for fx: -F2NORM
macroname: F2$NORM


VEQ:F2$NORM
  [symbol]

F2$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$NORM ...).
    see function: %F2$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$NORM!

```
broadcast for fx: -F2NORM
macroname: F2$NORM!


VEQ:F2$NORM!
  [symbol]

F2$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$NORM! ...).
    see function: %F2$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ONE

```
:missing:

VEQ:F2$ONE
  [symbol]

F2$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F2$POINT

```
macro wrapper: (mvc #'%F2$POINT ...).
see function: %F2$POINT
VEQ:F2$POINT
  [symbol]

F2$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$POINT ...).
    see function: %F2$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### F2$POLYGON

```
macro wrapper: (mvc #'%F2$POLYGON ...).
see function: %F2$POLYGON
VEQ:F2$POLYGON
  [symbol]

F2$POLYGON names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$POLYGON ...).
    see function: %F2$POLYGON
  Source file: /data/x/veq/src/shapes.lisp
```
### F2$RECT

```
macro wrapper: (mvc #'%F2$RECT ...).
see function: %F2$RECT
VEQ:F2$RECT
  [symbol]

F2$RECT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$RECT ...).
    see function: %F2$RECT
  Source file: /data/x/veq/src/shapes.lisp
```
### F2$ROT

```
broadcast for fx: -F2ROT
macroname: F2$ROT


VEQ:F2$ROT
  [symbol]

F2$ROT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ROT ...).
    see function: %F2$ROT
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ROT!

```
broadcast for fx: -F2ROT
macroname: F2$ROT!


VEQ:F2$ROT!
  [symbol]

F2$ROT! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ROT! ...).
    see function: %F2$ROT!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ROTS

```
broadcast for fx: -F2ROTS
macroname: F2$ROTS


VEQ:F2$ROTS
  [symbol]

F2$ROTS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ROTS ...).
    see function: %F2$ROTS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$ROTS!

```
broadcast for fx: -F2ROTS
macroname: F2$ROTS!


VEQ:F2$ROTS!
  [symbol]

F2$ROTS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$ROTS! ...).
    see function: %F2$ROTS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$SCALE

```
broadcast for fx: -F2SCALE
macroname: F2$SCALE


VEQ:F2$SCALE
  [symbol]

F2$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$SCALE ...).
    see function: %F2$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$SCALE!

```
broadcast for fx: -F2SCALE
macroname: F2$SCALE!


VEQ:F2$SCALE!
  [symbol]

F2$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$SCALE! ...).
    see function: %F2$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F2$SQUARE

```
macro wrapper: (mvc #'%F2$SQUARE ...).
see function: %F2$SQUARE
VEQ:F2$SQUARE
  [symbol]

F2$SQUARE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2$SQUARE ...).
    see function: %F2$SQUARE
  Source file: /data/x/veq/src/shapes.lisp
```
### F2$SUM

```
:missing:

VEQ:F2$SUM
  [symbol]

F2$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### F2$TAKE

```
:missing:

VEQ:F2$TAKE
  [symbol]

F2$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### F2$VAL

```
:missing:

VEQ:F2$VAL
  [symbol]

F2$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F2$WITH-ROWS

```
:missing:

VEQ:F2$WITH-ROWS
  [symbol]
```
### F2$ZERO

```
:missing:

VEQ:F2$ZERO
  [symbol]

F2$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F2*

```
veq context op: F2*
fxname: -F2*
args: (AX AY BX BY)
body: (VALUES (* AX BX) (* AY BY))
```
### F2+

```
veq context op: F2+
fxname: -F2+
args: (AX AY BX BY)
body: (VALUES (+ AX BX) (+ AY BY))
```
### F2-

```
veq context op: F2-
fxname: -F2-
args: (AX AY BX BY)
body: (VALUES (- AX BX) (- AY BY))
```
### F2.

```
veq context op: F2.
fxname: -F2.
args: (AX AY BX BY)
body: (+ (* AX BX) (* AY BY))
```
### F2/

```
veq context op: F2/
fxname: -F2/
args: (AX AY BX BY)
body: (VALUES (/ AX BX) (/ AY BY))
```
### F2^

```
veq context op: F2^
fxname: -F2^
args: (A B S)
body: (VALUES (EXPT A S) (EXPT B S))
```
### F2ABS

```
veq context op: F2ABS
fxname: -F2ABS
args: (A B)
body: (VALUES (ABS A) (ABS B))
```
### F2ANGLE

```
veq context op: F2ANGLE
fxname: -F2ANGLE
args: (A B)
body: (MVC #'ATAN (-F2NORM B A))
```
### F2CROSS

```
veq context op: F2CROSS
fxname: -F2CROSS
args: (AX AY BX BY)
body: (- (* AX BY) (* AY BX))
```
### F2DST

```
veq context op: F2DST
fxname: -F2DST
args: (AX AY BX BY)
body: (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY)))))
```
### F2DST2

```
veq context op: F2DST2
fxname: -F2DST2
args: (AX AY BX BY)
body: (MVC #'+ (-F2SQUARE (- BX AX) (- BY AY)))
```
### F2EXP

```
veq context op: F2EXP
fxname: -F2EXP
args: (A B)
body: (VALUES (EXP A) (EXP B))
```
### F2FLIP

```
veq context op: F2FLIP
fxname: -F2FLIP
args: (A B)
body: (VALUES B A)
```
### F2FROM

```
veq context op: F2FROM
fxname: -F2FROM
args: (AX AY BX BY S)
body: (-F2+ AX AY (* BX S) (* BY S))
```
### F2I-

```
veq context op: F2I-
fxname: -F2I-
args: (AX AY BX BY)
body: (VALUES (- BX AX) (- BY AY))
```
### F2I/

```
veq context op: F2I/
fxname: -F2I/
args: (AX AY BX BY)
body: (VALUES (/ BX AX) (/ BY AY))
```
### F2IN-BBOX

```
macro wrapper: (mvc #'%F2IN-BBOX ...) in veq context.
see function: %F2IN-BBOX
VEQ:F2IN-BBOX
  [symbol]

F2IN-BBOX names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2IN-BBOX ...) in veq context.
    see function: %F2IN-BBOX
  Source file: /data/x/veq/src/checks.lisp
```
### F2IN-CONCAVE

```
macro wrapper: (mvc #'%F2IN-CONCAVE ...) in veq context.
see function: %F2IN-CONCAVE
VEQ:F2IN-CONCAVE
  [symbol]

F2IN-CONCAVE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2IN-CONCAVE ...) in veq context.
    see function: %F2IN-CONCAVE
  Source file: /data/x/veq/src/checks.lisp
```
### F2IN-TRIANGLE

```
macro wrapper: (mvc #'%F2IN-TRIANGLE ...) in veq context.
see function: %F2IN-TRIANGLE
VEQ:F2IN-TRIANGLE
  [symbol]

F2IN-TRIANGLE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2IN-TRIANGLE ...) in veq context.
    see function: %F2IN-TRIANGLE
  Source file: /data/x/veq/src/checks.lisp
```
### F2ISCALE

```
veq context op: F2ISCALE
fxname: -F2ISCALE
args: (A B S)
body: (VALUES (/ A S) (/ B S))
```
### F2LEN

```
veq context op: F2LEN
fxname: -F2LEN
args: (A B)
body: (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F2SQUARE A B)))))
```
### F2LEN2

```
veq context op: F2LEN2
fxname: -F2LEN2
args: (A B)
body: (THE POS-FF (MVC #'+ (-F2SQUARE A B)))
```
### F2LERP

```
veq context op: F2LERP
fxname: -F2LERP
args: (AX AY BX BY S)
body: (-F2+ AX AY (* (- BX AX) S) (* (- BY AY) S))
```
### F2LET

```
:missing:

VEQ:F2LET
  [symbol]
```
### F2LSEGX

```
macro wrapper: (mvc #'%F2LSEGX ...) in veq context.
see function: %F2LSEGX
VEQ:F2LSEGX
  [symbol]

F2LSEGX names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2LSEGX ...) in veq context.
    see function: %F2LSEGX
  Source file: /data/x/veq/src/checks.lisp
```
### F2MAX

```
veq context op: F2MAX
fxname: -F2MAX
args: (A B)
body: (MAX A B)
```
### F2MEYE

```
return eye matrix for dimension
VEQ:F2MEYE
  [symbol]

F2MEYE names a compiled function:
  Lambda-list: (&OPTIONAL (V 1.0))
  Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (4)) &OPTIONAL))
  Documentation:
    return eye matrix for dimension
  Source file: /data/x/veq/src/mat.lisp
```
### F2MID

```
veq context op: F2MID
fxname: -F2MID
args: (AX AY BX BY)
body: (VALUES (* 0.5 (+ AX BX)) (* 0.5 (+ AY BY)))
```
### F2MIN

```
veq context op: F2MIN
fxname: -F2MIN
args: (A B)
body: (MIN A B)
```
### F2MINV

```
invert 2x2 matrix
VEQ:F2MINV
  [symbol]

F2MINV names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Documentation:
    invert 2x2 matrix
  Source file: /data/x/veq/src/mat-inv.lisp
```
### F2MM

```
multiply mat * mat
of type: FVEC
VEQ:F2MM
  [symbol]

F2MM names a macro:
  Lambda-list: (A*1 B*3)
  Documentation:
    multiply mat * mat
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F2MMT

```
multiply mat * (transpose mat)
of type: FVEC
VEQ:F2MMT
  [symbol]

F2MMT names a macro:
  Lambda-list: (A*59 B*61)
  Documentation:
    multiply mat * (transpose mat)
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F2MOD

```
veq context op: F2MOD
fxname: -F2MOD
args: (A B S)
body: (VALUES (MOD A S) (MOD B S))
```
### F2MROT

```
macro wrapper: (mvc #'%F2MROT ...).
see function: %F2MROT
VEQ:F2MROT
  [symbol]

F2MROT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2MROT ...).
    see function: %F2MROT
  Source file: /data/x/veq/src/mat.lisp
```
### F2MROT*

```
macro wrapper: (mvc #'%F2MROT* ...).
see function: %F2MROT*
VEQ:F2MROT*
  [symbol]

F2MROT* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2MROT* ...).
    see function: %F2MROT*
  Source file: /data/x/veq/src/mat.lisp
```
### F2MSCALE

```
macro wrapper: (mvc #'%F2MSCALE ...) in veq context.
see function: %F2MSCALE
VEQ:F2MSCALE
  [symbol]

F2MSCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2MSCALE ...) in veq context.
    see function: %F2MSCALE
  Source file: /data/x/veq/src/mat.lisp
```
### F2MT!

```
transpose matrix of type ~a in-place
VEQ:F2MT!
  [symbol]

F2MT! names a macro:
  Lambda-list: (A1)
  Documentation:
    transpose matrix of type ~a in-place
  Source file: /data/x/veq/src/mat.lisp
```
### F2MTM

```
multiply (transpose mat) * mat
of type: FVEC
VEQ:F2MTM
  [symbol]

F2MTM names a macro:
  Lambda-list: (A*88 B*90)
  Documentation:
    multiply (transpose mat) * mat
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F2MTMT

```
multiply (transpose mat) * (transpose mat)
of type: FVEC
VEQ:F2MTMT
  [symbol]

F2MTMT names a macro:
  Lambda-list: (A*30 B*32)
  Documentation:
    multiply (transpose mat) * (transpose mat)
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F2MTRANS

```
macro wrapper: (mvc #'%F2MTRANS ...) in veq context.
see function: %F2MTRANS
VEQ:F2MTRANS
  [symbol]

F2MTRANS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2MTRANS ...) in veq context.
    see function: %F2MTRANS
  Source file: /data/x/veq/src/mat.lisp
```
### F2MTV

```
:missing:

VEQ:F2MTV
  [symbol]

F2MTV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### F2MV

```
:missing:

VEQ:F2MV
  [symbol]

F2MV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### F2MVB

```
:missing:

VEQ:F2MVB
  [symbol]
```
### F2NEG

```
veq context op: F2NEG
fxname: -F2NEG
args: (A B)
body: (VALUES (- A) (- B))
```
### F2NORM

```
veq context op: F2NORM
fxname: -F2NORM
args: (A B)
body: (MVC #'-F2ISCALE A B (MVC #'-F2LEN A B))
```
### F2NSUM

```
:missing:

VEQ:F2NSUM
  [symbol]
```
### F2ON-CIRC

```
veq context op: F2ON-CIRC
fxname: -F2ON-CIRC
args: (A RAD)
body: (MVC #'-F2SCALE (-FCOS-SIN (* A FPII)) RAD)
```
### F2ON-CIRC*

```
veq context op: F2ON-CIRC*
fxname: -F2ON-CIRC*
args: (A RAD)
body: (MVC #'-F2SCALE (-FCOS-SIN A) RAD)
```
### F2PERP

```
veq context op: F2PERP
fxname: -F2PERP
args: (A B)
body: (VALUES B (- A))
```
### F2PERP*

```
veq context op: F2PERP*
fxname: -F2PERP*
args: (A B)
body: (VALUES (- B) A)
```
### F2REP

```
:missing:

VEQ:F2REP
  [symbol]
```
### F2REP*

```
:missing:

VEQ:F2REP*
  [symbol]
```
### F2ROT

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
### F2ROTS

```
veq context op: F2ROTS
fxname: -F2ROTS
args: (X Y A SX SY)
body: (MVC #'-F2+ (MVC #'-F2ROT (-F2- X Y SX SY) A) SX SY)
```
### F2SCALE

```
veq context op: F2SCALE
fxname: -F2SCALE
args: (A B S)
body: (VALUES (* A S) (* B S))
```
### F2SEGDST

```
macro wrapper: (mvc #'%F2SEGDST ...) in veq context.
see function: %F2SEGDST
VEQ:F2SEGDST
  [symbol]

F2SEGDST names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2SEGDST ...) in veq context.
    see function: %F2SEGDST
  Source file: /data/x/veq/src/checks.lisp
```
### F2SEGX

```
macro wrapper: (mvc #'%F2SEGX ...) in veq context.
see function: %F2SEGX
VEQ:F2SEGX
  [symbol]

F2SEGX names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F2SEGX ...) in veq context.
    see function: %F2SEGX
  Source file: /data/x/veq/src/checks.lisp
```
### F2SQRT

```
veq context op: F2SQRT
fxname: -F2SQRT
args: (A B)
body: (VALUES (THE POS-FF (SQRT (THE POS-FF A))) (THE POS-FF (SQRT (THE POS-FF B))))
```
### F2SQUARE

```
veq context op: F2SQUARE
fxname: -F2SQUARE
args: (A B)
body: (VALUES (* A A) (* B B))
```
### F2VSET

```
:missing:

VEQ:F2VSET
  [symbol]
```
### F2~

```
:missing:

VEQ:F2~
  [symbol]
```
### F3

```
:missing:

VEQ:F3
  [symbol]
```
### F3$

```
:missing:

VEQ:F3$
  [symbol]
```
### F3$*

```
broadcast for fx: -F3*
macroname: F3$*


VEQ:F3$*
  [symbol]

F3$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$* ...).
    see function: %F3$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$*!

```
broadcast for fx: -F3*
macroname: F3$*!


VEQ:F3$*!
  [symbol]

F3$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$*! ...).
    see function: %F3$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$+

```
broadcast for fx: -F3+
macroname: F3$+


VEQ:F3$+
  [symbol]

F3$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$+ ...).
    see function: %F3$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$+!

```
broadcast for fx: -F3+
macroname: F3$+!


VEQ:F3$+!
  [symbol]

F3$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$+! ...).
    see function: %F3$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$-

```
broadcast for fx: -F3-
macroname: F3$-


VEQ:F3$-
  [symbol]

F3$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$- ...).
    see function: %F3$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$-!

```
broadcast for fx: -F3-
macroname: F3$-!


VEQ:F3$-!
  [symbol]

F3$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$-! ...).
    see function: %F3$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$/

```
broadcast for fx: -F3/
macroname: F3$/


VEQ:F3$/
  [symbol]

F3$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$/ ...).
    see function: %F3$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$/!

```
broadcast for fx: -F3/
macroname: F3$/!


VEQ:F3$/!
  [symbol]

F3$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$/! ...).
    see function: %F3$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$ABS

```
broadcast for fx: -F3ABS
macroname: F3$ABS


VEQ:F3$ABS
  [symbol]

F3$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$ABS ...).
    see function: %F3$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$ABS!

```
broadcast for fx: -F3ABS
macroname: F3$ABS!


VEQ:F3$ABS!
  [symbol]

F3$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$ABS! ...).
    see function: %F3$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$COPY

```
:missing:

VEQ:F3$COPY
  [symbol]
```
### F3$FROM

```
broadcast for fx: -F3FROM
macroname: F3$FROM


VEQ:F3$FROM
  [symbol]

F3$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$FROM ...).
    see function: %F3$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$FROM!

```
broadcast for fx: -F3FROM
macroname: F3$FROM!


VEQ:F3$FROM!
  [symbol]

F3$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$FROM! ...).
    see function: %F3$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$FXLSPACE

```
:missing:

VEQ:F3$FXLSPACE
  [symbol]
```
### F3$I-

```
broadcast for fx: -F3I-
macroname: F3$I-


VEQ:F3$I-
  [symbol]

F3$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$I- ...).
    see function: %F3$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$I-!

```
broadcast for fx: -F3I-
macroname: F3$I-!


VEQ:F3$I-!
  [symbol]

F3$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$I-! ...).
    see function: %F3$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$I/

```
broadcast for fx: -F3I/
macroname: F3$I/


VEQ:F3$I/
  [symbol]

F3$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$I/ ...).
    see function: %F3$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$I/!

```
broadcast for fx: -F3I/
macroname: F3$I/!


VEQ:F3$I/!
  [symbol]

F3$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$I/! ...).
    see function: %F3$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$ISCALE

```
broadcast for fx: -F3ISCALE
macroname: F3$ISCALE


VEQ:F3$ISCALE
  [symbol]

F3$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$ISCALE ...).
    see function: %F3$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$ISCALE!

```
broadcast for fx: -F3ISCALE
macroname: F3$ISCALE!


VEQ:F3$ISCALE!
  [symbol]

F3$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$ISCALE! ...).
    see function: %F3$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$LAST

```
:missing:

VEQ:F3$LAST
  [symbol]

F3$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
                         &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### F3$LEN

```
broadcast for fx: -F3LEN
macroname: F3$LEN


VEQ:F3$LEN
  [symbol]

F3$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$LEN ...).
    see function: %F3$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$LEN2

```
broadcast for fx: -F3LEN2
macroname: F3$LEN2


VEQ:F3$LEN2
  [symbol]

F3$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$LEN2 ...).
    see function: %F3$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$LINE

```
macro wrapper: (mvc #'%F3$LINE ...).
see function: %F3$LINE
VEQ:F3$LINE
  [symbol]

F3$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$LINE ...).
    see function: %F3$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### F3$LSPACE

```
macro wrapper: (mvc #'%F3$LSPACE ...) in veq context.
see function: %F3$LSPACE
VEQ:F3$LSPACE
  [symbol]

F3$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$LSPACE ...) in veq context.
    see function: %F3$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### F3$MAKE

```
:missing:

VEQ:F3$MAKE
  [symbol]
```
### F3$MIMA

```
:missing:

VEQ:F3$MIMA
  [symbol]

F3$MIMA names a compiled function:
  Lambda-list: (A0 &KEY (N (3$NUM A0)) INDS)
  Derived type: (FUNCTION
                 ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T) (:INDS T))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
                         SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
                         &OPTIONAL))
  Source file: /data/x/veq/src/array-mima.lisp
```
### F3$NEG

```
broadcast for fx: -F3NEG
macroname: F3$NEG


VEQ:F3$NEG
  [symbol]

F3$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$NEG ...).
    see function: %F3$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$NEG!

```
broadcast for fx: -F3NEG
macroname: F3$NEG!


VEQ:F3$NEG!
  [symbol]

F3$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$NEG! ...).
    see function: %F3$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$NORM

```
broadcast for fx: -F3NORM
macroname: F3$NORM


VEQ:F3$NORM
  [symbol]

F3$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$NORM ...).
    see function: %F3$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$NORM!

```
broadcast for fx: -F3NORM
macroname: F3$NORM!


VEQ:F3$NORM!
  [symbol]

F3$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$NORM! ...).
    see function: %F3$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$ONE

```
:missing:

VEQ:F3$ONE
  [symbol]

F3$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F3$POINT

```
macro wrapper: (mvc #'%F3$POINT ...).
see function: %F3$POINT
VEQ:F3$POINT
  [symbol]

F3$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$POINT ...).
    see function: %F3$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### F3$SCALE

```
broadcast for fx: -F3SCALE
macroname: F3$SCALE


VEQ:F3$SCALE
  [symbol]

F3$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$SCALE ...).
    see function: %F3$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$SCALE!

```
broadcast for fx: -F3SCALE
macroname: F3$SCALE!


VEQ:F3$SCALE!
  [symbol]

F3$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3$SCALE! ...).
    see function: %F3$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F3$SUM

```
:missing:

VEQ:F3$SUM
  [symbol]

F3$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
                         &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### F3$TAKE

```
:missing:

VEQ:F3$TAKE
  [symbol]

F3$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### F3$VAL

```
:missing:

VEQ:F3$VAL
  [symbol]

F3$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F3$WITH-ROWS

```
:missing:

VEQ:F3$WITH-ROWS
  [symbol]
```
### F3$ZERO

```
:missing:

VEQ:F3$ZERO
  [symbol]

F3$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F3*

```
veq context op: F3*
fxname: -F3*
args: (AX AY AZ BX BY BZ)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ))
```
### F3+

```
veq context op: F3+
fxname: -F3+
args: (AX AY AZ BX BY BZ)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ))
```
### F3-

```
veq context op: F3-
fxname: -F3-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ))
```
### F3.

```
veq context op: F3.
fxname: -F3.
args: (AX AY AZ BX BY BZ)
body: (+ (* AX BX) (* AY BY) (* AZ BZ))
```
### F3/

```
veq context op: F3/
fxname: -F3/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ))
```
### F3^

```
veq context op: F3^
fxname: -F3^
args: (A B C S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S))
```
### F3ABS

```
veq context op: F3ABS
fxname: -F3ABS
args: (A B C)
body: (VALUES (ABS A) (ABS B) (ABS C))
```
### F3CROSS

```
veq context op: F3CROSS
fxname: -F3CROSS
args: (AX AY AZ BX BY BZ)
body: (VALUES (- (* AY BZ) (* AZ BY)) (- (* AZ BX) (* AX BZ)) (- (* AX BY) (* AY BX)))
```
### F3DST

```
veq context op: F3DST
fxname: -F3DST
args: (AX AY AZ BX BY BZ)
body: (SQRT (THE POS-FF (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))))
```
### F3DST2

```
veq context op: F3DST2
fxname: -F3DST2
args: (AX AY AZ BX BY BZ)
body: (MVC #'+ (-F3SQUARE (- BX AX) (- BY AY) (- BZ AZ)))
```
### F3EXP

```
veq context op: F3EXP
fxname: -F3EXP
args: (A B C)
body: (VALUES (EXP A) (EXP B) (EXP C))
```
### F3FROM

```
veq context op: F3FROM
fxname: -F3FROM
args: (AX AY AZ BX BY BZ S)
body: (-F3+ AX AY AZ (* BX S) (* BY S) (* BZ S))
```
### F3I-

```
veq context op: F3I-
fxname: -F3I-
args: (AX AY AZ BX BY BZ)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ))
```
### F3I/

```
veq context op: F3I/
fxname: -F3I/
args: (AX AY AZ BX BY BZ)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ))
```
### F3ISCALE

```
veq context op: F3ISCALE
fxname: -F3ISCALE
args: (A B C S)
body: (VALUES (/ A S) (/ B S) (/ C S))
```
### F3LEN

```
veq context op: F3LEN
fxname: -F3LEN
args: (A B C)
body: (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F3SQUARE A B C)))))
```
### F3LEN2

```
veq context op: F3LEN2
fxname: -F3LEN2
args: (A B C)
body: (THE POS-FF (MVC #'+ (-F3SQUARE A B C)))
```
### F3LERP

```
veq context op: F3LERP
fxname: -F3LERP
args: (AX AY AZ BX BY BZ S)
body: (-F3+ AX AY AZ (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S))
```
### F3LET

```
:missing:

VEQ:F3LET
  [symbol]
```
### F3MAX

```
veq context op: F3MAX
fxname: -F3MAX
args: (A B C)
body: (MAX A B C)
```
### F3MEYE

```
return eye matrix for dimension
VEQ:F3MEYE
  [symbol]

F3MEYE names a compiled function:
  Lambda-list: (&OPTIONAL (V 1.0))
  Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (9)) &OPTIONAL))
  Documentation:
    return eye matrix for dimension
  Source file: /data/x/veq/src/mat.lisp
```
### F3MID

```
veq context op: F3MID
fxname: -F3MID
args: (AX AY AZ BX BY BZ)
body: (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5))
```
### F3MIN

```
veq context op: F3MIN
fxname: -F3MIN
args: (A B C)
body: (MIN A B C)
```
### F3MINV

```
invert 3x3 matrix
VEQ:F3MINV
  [symbol]

F3MINV names a compiled function:
  Lambda-list: (A0)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Documentation:
    invert 3x3 matrix
  Source file: /data/x/veq/src/mat-inv.lisp
```
### F3MM

```
multiply mat * mat
of type: FVEC
VEQ:F3MM
  [symbol]

F3MM names a macro:
  Lambda-list: (A*117 B*119)
  Documentation:
    multiply mat * mat
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F3MMT

```
multiply mat * (transpose mat)
of type: FVEC
VEQ:F3MMT
  [symbol]

F3MMT names a macro:
  Lambda-list: (A*175 B*177)
  Documentation:
    multiply mat * (transpose mat)
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F3MOD

```
veq context op: F3MOD
fxname: -F3MOD
args: (A B C S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S))
```
### F3MROT

```
macro wrapper: (mvc #'%F3MROT ...).
see function: %F3MROT
VEQ:F3MROT
  [symbol]

F3MROT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3MROT ...).
    see function: %F3MROT
  Source file: /data/x/veq/src/mat.lisp
```
### F3MROT*

```
macro wrapper: (mvc #'%F3MROT* ...).
see function: %F3MROT*
VEQ:F3MROT*
  [symbol]

F3MROT* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3MROT* ...).
    see function: %F3MROT*
  Source file: /data/x/veq/src/mat.lisp
```
### F3MSCALE

```
macro wrapper: (mvc #'%F3MSCALE ...) in veq context.
see function: %F3MSCALE
VEQ:F3MSCALE
  [symbol]

F3MSCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3MSCALE ...) in veq context.
    see function: %F3MSCALE
  Source file: /data/x/veq/src/mat.lisp
```
### F3MT!

```
transpose matrix of type ~a in-place
VEQ:F3MT!
  [symbol]

F3MT! names a macro:
  Lambda-list: (A1)
  Documentation:
    transpose matrix of type ~a in-place
  Source file: /data/x/veq/src/mat.lisp
```
### F3MTM

```
multiply (transpose mat) * mat
of type: FVEC
VEQ:F3MTM
  [symbol]

F3MTM names a macro:
  Lambda-list: (A*204 B*206)
  Documentation:
    multiply (transpose mat) * mat
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F3MTMT

```
multiply (transpose mat) * (transpose mat)
of type: FVEC
VEQ:F3MTMT
  [symbol]

F3MTMT names a macro:
  Lambda-list: (A*146 B*148)
  Documentation:
    multiply (transpose mat) * (transpose mat)
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F3MTRANS

```
macro wrapper: (mvc #'%F3MTRANS ...) in veq context.
see function: %F3MTRANS
VEQ:F3MTRANS
  [symbol]

F3MTRANS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3MTRANS ...) in veq context.
    see function: %F3MTRANS
  Source file: /data/x/veq/src/mat.lisp
```
### F3MTV

```
:missing:

VEQ:F3MTV
  [symbol]

F3MTV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### F3MV

```
:missing:

VEQ:F3MV
  [symbol]

F3MV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### F3MVB

```
:missing:

VEQ:F3MVB
  [symbol]
```
### F3NEG

```
veq context op: F3NEG
fxname: -F3NEG
args: (A B C)
body: (VALUES (- A) (- B) (- C))
```
### F3NORM

```
veq context op: F3NORM
fxname: -F3NORM
args: (A B C)
body: (MVC #'-F3ISCALE A B C (THE POS-FF (MVC #'-F3LEN A B C)))
```
### F3NSUM

```
:missing:

VEQ:F3NSUM
  [symbol]
```
### F3PLANEX

```
macro wrapper: (mvc #'%F3PLANEX ...) in veq context.
see function: %F3PLANEX
VEQ:F3PLANEX
  [symbol]

F3PLANEX names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F3PLANEX ...) in veq context.
    see function: %F3PLANEX
  Source file: /data/x/veq/src/checks.lisp
```
### F3REP

```
:missing:

VEQ:F3REP
  [symbol]
```
### F3REP*

```
:missing:

VEQ:F3REP*
  [symbol]
```
### F3ROT

```
veq context op: F3ROT
fxname: -F3ROT
args: (X Y Z NX NY NZ A)
body: (LET ((COSA (COS A)))
  (DECLARE
   (FF
     COSA))
  (MVC #'-F3FROM
       (MVC #'-F3FROM (-F3SCALE X Y Z COSA) (-F3CROSS NX NY NZ X Y Z) (SIN A))
       NX NY NZ (* (-F3. NX NY NZ X Y Z) (- 1.0 COSA))))
```
### F3ROTS

```
veq context op: F3ROTS
fxname: -F3ROTS
args: (X Y Z NX NY NZ A SX SY SZ)
body: (MVC #'-F3+ (MVC #'-F3ROT (-F3- X Y Z SX SY SZ) NX NY NZ A) SX SY SZ)
```
### F3SCALE

```
veq context op: F3SCALE
fxname: -F3SCALE
args: (A B C S)
body: (VALUES (* A S) (* B S) (* C S))
```
### F3SQRT

```
veq context op: F3SQRT
fxname: -F3SQRT
args: (A B C)
body: (VALUES (THE POS-FF (SQRT (THE POS-FF A))) (THE POS-FF (SQRT (THE POS-FF B)))
        (THE POS-FF (SQRT (THE POS-FF C))))
```
### F3SQUARE

```
veq context op: F3SQUARE
fxname: -F3SQUARE
args: (A B C)
body: (VALUES (THE POS-FF (* A A)) (THE POS-FF (* B B)) (THE POS-FF (* C C)))
```
### F3VSET

```
:missing:

VEQ:F3VSET
  [symbol]
```
### F3~

```
:missing:

VEQ:F3~
  [symbol]
```
### F4

```
:missing:

VEQ:F4
  [symbol]
```
### F4$

```
:missing:

VEQ:F4$
  [symbol]
```
### F4$*

```
broadcast for fx: -F4*
macroname: F4$*


VEQ:F4$*
  [symbol]

F4$* names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$* ...).
    see function: %F4$*
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$*!

```
broadcast for fx: -F4*
macroname: F4$*!


VEQ:F4$*!
  [symbol]

F4$*! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$*! ...).
    see function: %F4$*!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$+

```
broadcast for fx: -F4+
macroname: F4$+


VEQ:F4$+
  [symbol]

F4$+ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$+ ...).
    see function: %F4$+
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$+!

```
broadcast for fx: -F4+
macroname: F4$+!


VEQ:F4$+!
  [symbol]

F4$+! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$+! ...).
    see function: %F4$+!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$-

```
broadcast for fx: -F4-
macroname: F4$-


VEQ:F4$-
  [symbol]

F4$- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$- ...).
    see function: %F4$-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$-!

```
broadcast for fx: -F4-
macroname: F4$-!


VEQ:F4$-!
  [symbol]

F4$-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$-! ...).
    see function: %F4$-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$/

```
broadcast for fx: -F4/
macroname: F4$/


VEQ:F4$/
  [symbol]

F4$/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$/ ...).
    see function: %F4$/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$/!

```
broadcast for fx: -F4/
macroname: F4$/!


VEQ:F4$/!
  [symbol]

F4$/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$/! ...).
    see function: %F4$/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$ABS

```
broadcast for fx: -F4ABS
macroname: F4$ABS


VEQ:F4$ABS
  [symbol]

F4$ABS names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$ABS ...).
    see function: %F4$ABS
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$ABS!

```
broadcast for fx: -F4ABS
macroname: F4$ABS!


VEQ:F4$ABS!
  [symbol]

F4$ABS! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$ABS! ...).
    see function: %F4$ABS!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$COPY

```
:missing:

VEQ:F4$COPY
  [symbol]
```
### F4$FROM

```
broadcast for fx: -F4FROM
macroname: F4$FROM


VEQ:F4$FROM
  [symbol]

F4$FROM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$FROM ...).
    see function: %F4$FROM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$FROM!

```
broadcast for fx: -F4FROM
macroname: F4$FROM!


VEQ:F4$FROM!
  [symbol]

F4$FROM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$FROM! ...).
    see function: %F4$FROM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$FXLSPACE

```
:missing:

VEQ:F4$FXLSPACE
  [symbol]
```
### F4$I-

```
broadcast for fx: -F4I-
macroname: F4$I-


VEQ:F4$I-
  [symbol]

F4$I- names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$I- ...).
    see function: %F4$I-
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$I-!

```
broadcast for fx: -F4I-
macroname: F4$I-!


VEQ:F4$I-!
  [symbol]

F4$I-! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$I-! ...).
    see function: %F4$I-!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$I/

```
broadcast for fx: -F4I/
macroname: F4$I/


VEQ:F4$I/
  [symbol]

F4$I/ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$I/ ...).
    see function: %F4$I/
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$I/!

```
broadcast for fx: -F4I/
macroname: F4$I/!


VEQ:F4$I/!
  [symbol]

F4$I/! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$I/! ...).
    see function: %F4$I/!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$ISCALE

```
broadcast for fx: -F4ISCALE
macroname: F4$ISCALE


VEQ:F4$ISCALE
  [symbol]

F4$ISCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$ISCALE ...).
    see function: %F4$ISCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$ISCALE!

```
broadcast for fx: -F4ISCALE
macroname: F4$ISCALE!


VEQ:F4$ISCALE!
  [symbol]

F4$ISCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$ISCALE! ...).
    see function: %F4$ISCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$LAST

```
:missing:

VEQ:F4$LAST
  [symbol]

F4$LAST names a compiled function:
  Lambda-list: (A)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
                         SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-rows.lisp
```
### F4$LEN

```
broadcast for fx: -F4LEN
macroname: F4$LEN


VEQ:F4$LEN
  [symbol]

F4$LEN names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$LEN ...).
    see function: %F4$LEN
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$LEN2

```
broadcast for fx: -F4LEN2
macroname: F4$LEN2


VEQ:F4$LEN2
  [symbol]

F4$LEN2 names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$LEN2 ...).
    see function: %F4$LEN2
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$LINE

```
macro wrapper: (mvc #'%F4$LINE ...).
see function: %F4$LINE
VEQ:F4$LINE
  [symbol]

F4$LINE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$LINE ...).
    see function: %F4$LINE
  Source file: /data/x/veq/src/shapes.lisp
```
### F4$LSPACE

```
macro wrapper: (mvc #'%F4$LSPACE ...) in veq context.
see function: %F4$LSPACE
VEQ:F4$LSPACE
  [symbol]

F4$LSPACE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$LSPACE ...) in veq context.
    see function: %F4$LSPACE
  Source file: /data/x/veq/src/lspace.lisp
```
### F4$MAKE

```
:missing:

VEQ:F4$MAKE
  [symbol]
```
### F4$NEG

```
broadcast for fx: -F4NEG
macroname: F4$NEG


VEQ:F4$NEG
  [symbol]

F4$NEG names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$NEG ...).
    see function: %F4$NEG
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$NEG!

```
broadcast for fx: -F4NEG
macroname: F4$NEG!


VEQ:F4$NEG!
  [symbol]

F4$NEG! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$NEG! ...).
    see function: %F4$NEG!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$NORM

```
broadcast for fx: -F4NORM
macroname: F4$NORM


VEQ:F4$NORM
  [symbol]

F4$NORM names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$NORM ...).
    see function: %F4$NORM
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$NORM!

```
broadcast for fx: -F4NORM
macroname: F4$NORM!


VEQ:F4$NORM!
  [symbol]

F4$NORM! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$NORM! ...).
    see function: %F4$NORM!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$ONE

```
:missing:

VEQ:F4$ONE
  [symbol]

F4$ONE names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F4$POINT

```
macro wrapper: (mvc #'%F4$POINT ...).
see function: %F4$POINT
VEQ:F4$POINT
  [symbol]

F4$POINT names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$POINT ...).
    see function: %F4$POINT
  Source file: /data/x/veq/src/shapes.lisp
```
### F4$SCALE

```
broadcast for fx: -F4SCALE
macroname: F4$SCALE


VEQ:F4$SCALE
  [symbol]

F4$SCALE names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$SCALE ...).
    see function: %F4$SCALE
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$SCALE!

```
broadcast for fx: -F4SCALE
macroname: F4$SCALE!


VEQ:F4$SCALE!
  [symbol]

F4$SCALE! names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%F4$SCALE! ...).
    see function: %F4$SCALE!
  Source file: /data/x/veq/src/array-broadcast.lisp
```
### F4$SUM

```
:missing:

VEQ:F4$SUM
  [symbol]

F4$SUM names a compiled function:
  Lambda-list: (A &KEY N)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) &KEY (:N T))
                 (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
                         SINGLE-FLOAT &OPTIONAL))
  Source file: /data/x/veq/src/array-reduce.lisp
```
### F4$TAKE

```
:missing:

VEQ:F4$TAKE
  [symbol]

F4$TAKE names a compiled function:
  Lambda-list: (A INDS &KEY RES)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT) T &KEY (:RES T))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT) &OPTIONAL))
  Source file: /data/x/veq/src/array-take.lisp
```
### F4$VAL

```
:missing:

VEQ:F4$VAL
  [symbol]

F4$VAL names a compiled function:
  Lambda-list: (V &OPTIONAL (N 1))
  Derived type: (FUNCTION (T &OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F4$WITH-ROWS

```
:missing:

VEQ:F4$WITH-ROWS
  [symbol]
```
### F4$ZERO

```
:missing:

VEQ:F4$ZERO
  [symbol]

F4$ZERO names a compiled function:
  Lambda-list: (&OPTIONAL (N 1))
  Derived type: (FUNCTION (&OPTIONAL (UNSIGNED-BYTE 31))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Source file: /data/x/veq/src/array-utils.lisp
```
### F4*

```
veq context op: F4*
fxname: -F4*
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```
### F4+

```
veq context op: F4+
fxname: -F4+
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (+ AX BX) (+ AY BY) (+ AZ BZ) (+ AW BW))
```
### F4-

```
veq context op: F4-
fxname: -F4-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- AX BX) (- AY BY) (- AZ BZ) (- AW BW))
```
### F4.

```
veq context op: F4.
fxname: -F4.
args: (AX AY AZ AW BX BY BZ BW)
body: (+ (* AX BX) (* AY BY) (* AZ BZ) (* AW BW))
```
### F4/

```
veq context op: F4/
fxname: -F4/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ AX BX) (/ AY BY) (/ AZ BZ) (/ AW BW))
```
### F4^

```
veq context op: F4^
fxname: -F4^
args: (A B C D S)
body: (VALUES (EXPT A S) (EXPT B S) (EXPT C S) (EXPT D S))
```
### F4ABS

```
veq context op: F4ABS
fxname: -F4ABS
args: (A B C D)
body: (VALUES (ABS A) (ABS B) (ABS C) (ABS D))
```
### F4DST

```
veq context op: F4DST
fxname: -F4DST
args: (AX AY AZ AW BX BY BZ BW)
body: (SQRT
 (THE POS-FF (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))))
```
### F4DST2

```
veq context op: F4DST2
fxname: -F4DST2
args: (AX AY AZ AW BX BY BZ BW)
body: (MVC #'+ (-F4SQUARE (- BX AX) (- BY AY) (- BZ AZ) (- BW AW)))
```
### F4EXP

```
veq context op: F4EXP
fxname: -F4EXP
args: (A B C D)
body: (VALUES (EXP A) (EXP B) (EXP C) (EXP D))
```
### F4FROM

```
veq context op: F4FROM
fxname: -F4FROM
args: (AX AY AZ AW BX BY BZ BW S)
body: (-F4+ AX AY AZ AW (* BX S) (* BY S) (* BZ S) (* BW S))
```
### F4I-

```
veq context op: F4I-
fxname: -F4I-
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (- BX AX) (- BY AY) (- BZ AZ) (- BW AW))
```
### F4I/

```
veq context op: F4I/
fxname: -F4I/
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (/ BX AX) (/ BY AY) (/ BZ AZ) (/ BW AW))
```
### F4ISCALE

```
veq context op: F4ISCALE
fxname: -F4ISCALE
args: (A B C D S)
body: (VALUES (/ A S) (/ B S) (/ C S) (/ D S))
```
### F4LEN

```
veq context op: F4LEN
fxname: -F4LEN
args: (A B C D)
body: (THE POS-FF (SQRT (THE POS-FF (MVC #'+ (-F4SQUARE A B C D)))))
```
### F4LEN2

```
veq context op: F4LEN2
fxname: -F4LEN2
args: (A B C D)
body: (THE POS-FF (MVC #'+ (-F4SQUARE A B C D)))
```
### F4LERP

```
veq context op: F4LERP
fxname: -F4LERP
args: (AX AY AZ AW BX BY BZ BW S)
body: (-F4+ AX AY AZ AW (* (- BX AX) S) (* (- BY AY) S) (* (- BZ AZ) S)
 (* (- BW AW) S))
```
### F4LET

```
:missing:

VEQ:F4LET
  [symbol]
```
### F4MAX

```
veq context op: F4MAX
fxname: -F4MAX
args: (A B C D)
body: (MAX A B C D)
```
### F4MEYE

```
return eye matrix for dimension
VEQ:F4MEYE
  [symbol]

F4MEYE names a compiled function:
  Lambda-list: (&OPTIONAL (V 1.0))
  Derived type: (FUNCTION (&OPTIONAL SINGLE-FLOAT)
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (16)) &OPTIONAL))
  Documentation:
    return eye matrix for dimension
  Source file: /data/x/veq/src/mat.lisp
```
### F4MID

```
veq context op: F4MID
fxname: -F4MID
args: (AX AY AZ AW BX BY BZ BW)
body: (VALUES (* (+ BX AX) 0.5) (* (+ BY AY) 0.5) (* (+ BZ AZ) 0.5) (* (+ BW AW) 0.5))
```
### F4MIN

```
veq context op: F4MIN
fxname: -F4MIN
args: (A B C D)
body: (MIN A B C D)
```
### F4MINV

```
invert 4x4 matrix
VEQ:F4MINV
  [symbol]

F4MINV names a compiled function:
  Lambda-list: (A0)
  Derived type: (FUNCTION ((SIMPLE-ARRAY SINGLE-FLOAT))
                 (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
  Documentation:
    invert 4x4 matrix
  Source file: /data/x/veq/src/mat-inv.lisp
```
### F4MM

```
multiply mat * mat
of type: FVEC
VEQ:F4MM
  [symbol]

F4MM names a macro:
  Lambda-list: (A*233 B*235)
  Documentation:
    multiply mat * mat
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F4MMT

```
multiply mat * (transpose mat)
of type: FVEC
VEQ:F4MMT
  [symbol]

F4MMT names a macro:
  Lambda-list: (A*291 B*293)
  Documentation:
    multiply mat * (transpose mat)
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F4MOD

```
veq context op: F4MOD
fxname: -F4MOD
args: (A B C D S)
body: (VALUES (MOD A S) (MOD B S) (MOD C S) (MOD D S))
```
### F4MT!

```
transpose matrix of type ~a in-place
VEQ:F4MT!
  [symbol]

F4MT! names a macro:
  Lambda-list: (A1)
  Documentation:
    transpose matrix of type ~a in-place
  Source file: /data/x/veq/src/mat.lisp
```
### F4MTM

```
multiply (transpose mat) * mat
of type: FVEC
VEQ:F4MTM
  [symbol]

F4MTM names a macro:
  Lambda-list: (A*320 B*322)
  Documentation:
    multiply (transpose mat) * mat
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F4MTMT

```
multiply (transpose mat) * (transpose mat)
of type: FVEC
VEQ:F4MTMT
  [symbol]

F4MTMT names a macro:
  Lambda-list: (A*262 B*264)
  Documentation:
    multiply (transpose mat) * (transpose mat)
    of type: FVEC
  Source file: /data/x/veq/src/mat.lisp
```
### F4MTV

```
:missing:

VEQ:F4MTV
  [symbol]

F4MTV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### F4MV

```
:missing:

VEQ:F4MV
  [symbol]

F4MV names a macro:
  Lambda-list: (M1 &REST V2)
  Source file: /data/x/veq/src/mat.lisp
```
### F4MVB

```
:missing:

VEQ:F4MVB
  [symbol]
```
### F4NEG

```
veq context op: F4NEG
fxname: -F4NEG
args: (A B C D)
body: (VALUES (- A) (- B) (- C) (- D))
```
### F4NORM

```
veq context op: F4NORM
fxname: -F4NORM
args: (A B C D)
body: (MVC #'-F4ISCALE A B C D (THE POS-FF (MVC #'-F4LEN A B C D)))
```
### F4NSUM

```
:missing:

VEQ:F4NSUM
  [symbol]
```
### F4REP

```
:missing:

VEQ:F4REP
  [symbol]
```
### F4REP*

```
:missing:

VEQ:F4REP*
  [symbol]
```
### F4SCALE

```
veq context op: F4SCALE
fxname: -F4SCALE
args: (A B C D S)
body: (VALUES (* A S) (* B S) (* C S) (* D S))
```
### F4SQRT

```
veq context op: F4SQRT
fxname: -F4SQRT
args: (A B C D)
body: (VALUES (THE POS-FF (SQRT (THE POS-FF A))) (THE POS-FF (SQRT (THE POS-FF B)))
        (THE POS-FF (SQRT (THE POS-FF C))) (THE POS-FF (SQRT (THE POS-FF D))))
```
### F4SQUARE

```
veq context op: F4SQUARE
fxname: -F4SQUARE
args: (A B C D)
body: (VALUES (THE POS-FF (* A A)) (THE POS-FF (* B B)) (THE POS-FF (* C C))
        (THE POS-FF (* D D)))
```
### F4VSET

```
:missing:

VEQ:F4VSET
  [symbol]
```
### F4~

```
:missing:

VEQ:F4~
  [symbol]
```
### F^

```
veq context op: F^
fxname: -F^
args: (A S)
body: (EXPT A S)
```
### F_

```
create fvec from body: (f_ '(1f0 2f0 3f0))
VEQ:F_
  [symbol]

F_ names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    create fvec from body: (f_ '(1f0 2f0 3f0))
  Source file: /data/x/veq/src/array-utils.lisp
```
### FABS

```
veq context op: FABS
fxname: -FABS
args: (A)
body: (ABS A)
```
### FCLAMP

```
veq context op: FCLAMP
fxname: -FCLAMP
args: (X)
body: (MIN 1.0 (MAX 0.0 X))
```
### FCLAMP*

```
veq context op: FCLAMP*
fxname: -FCLAMP*
args: (X MI MA)
body: (MIN MA (MAX MI X))
```
### FCOS-SIN

```
veq context op: FCOS-SIN
fxname: -FCOS-SIN
args: (A)
body: (VALUES (COS A) (SIN A))
```
### FDEG->RAD

```
veq context op: FDEG->RAD
fxname: -FDEG->RAD
args: (DEG)
body: (* FPI (/ DEG 180.0))
```
### FEASE-IN-BACK

```
ease in:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0 S) X) S))
VEQ:FEASE-IN-BACK
  [symbol]

FEASE-IN-BACK names a compiled function:
  Lambda-list: (X &OPTIONAL (S 1.70158))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES
                  (OR FLOAT (COMPLEX DOUBLE-FLOAT)
                      (COMPLEX SINGLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease in:
    arg: (X &OPTIONAL (S 1.70158))
    body: (* X X (- (* (+ 1.0 S) X) S))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-CIRC

```
ease in:
arg: (X)
body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
VEQ:FEASE-IN-CIRC
  [symbol]

FEASE-IN-CIRC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (COMPLEX SINGLE-FLOAT) (SINGLE-FLOAT -0.0 1.0))
                  &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-CUBIC

```
ease in:
arg: (X)
body: (* X X X)
VEQ:FEASE-IN-CUBIC
  [symbol]

FEASE-IN-CUBIC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (* X X X)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-ELASTIC

```
ease in:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
        (-
         (* (EXPT 2.0 (* 10.0 (- X 1.0)))
            (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
VEQ:FEASE-IN-ELASTIC
  [symbol]

FEASE-IN-ELASTIC names a compiled function:
  Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
  Derived type: (FUNCTION (T &OPTIONAL T T)
                 (VALUES
                  (OR FLOAT (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease in:
    arg: (X &OPTIONAL (P 0.3) (S NIL))
    body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
            (-
             (* (EXPT 2.0 (* 10.0 (- X 1.0)))
                (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-EXP

```
ease in:
arg: (X)
body: (EXPT 2.0 (* 10.0 (- X 1.0)))
VEQ:FEASE-IN-EXP
  [symbol]

FEASE-IN-EXP names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0) &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (EXPT 2.0 (* 10.0 (- X 1.0)))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-LINEAR

```
ease in:
arg: (X)
body: X
VEQ:FEASE-IN-LINEAR
  [symbol]

FEASE-IN-LINEAR names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: X
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-BACK

```
ease in-out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0 S) X) S))
VEQ:FEASE-IN-OUT-BACK
  [symbol]

FEASE-IN-OUT-BACK names a compiled function:
  Lambda-list: (X &OPTIONAL (S 1.70158))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES
                  (OR FLOAT (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X &OPTIONAL (S 1.70158))
    body: (* X X (- (* (+ 1.0 S) X) S))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-CIRC

```
ease in-out:
arg: (X)
body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
VEQ:FEASE-IN-OUT-CIRC
  [symbol]

FEASE-IN-OUT-CIRC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (SINGLE-FLOAT -0.0 1.0) (COMPLEX SINGLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-CUBIC

```
ease in-out:
arg: (X)
body: (* X X X)
VEQ:FEASE-IN-OUT-CUBIC
  [symbol]

FEASE-IN-OUT-CUBIC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (* X X X)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-ELASTIC

```
ease in-out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
        (-
         (* (EXPT 2.0 (* 10.0 (- X 1.0)))
            (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
VEQ:FEASE-IN-OUT-ELASTIC
  [symbol]

FEASE-IN-OUT-ELASTIC names a compiled function:
  Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
  Derived type: (FUNCTION (T &OPTIONAL T T)
                 (VALUES
                  (OR FLOAT (COMPLEX DOUBLE-FLOAT)
                      (COMPLEX SINGLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X &OPTIONAL (P 0.3) (S NIL))
    body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
            (-
             (* (EXPT 2.0 (* 10.0 (- X 1.0)))
                (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-EXP

```
ease in-out:
arg: (X)
body: (EXPT 2.0 (* 10.0 (- X 1.0)))
VEQ:FEASE-IN-OUT-EXP
  [symbol]

FEASE-IN-OUT-EXP names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (EXPT 2.0 (* 10.0 (- X 1.0)))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-LINEAR

```
ease in-out:
arg: (X)
body: X
VEQ:FEASE-IN-OUT-LINEAR
  [symbol]

FEASE-IN-OUT-LINEAR names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: X
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-QUART

```
ease in-out:
arg: (X)
body: (EXPT X 4.0)
VEQ:FEASE-IN-OUT-QUART
  [symbol]

FEASE-IN-OUT-QUART names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (EXPT X 4.0)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-QUINT

```
ease in-out:
arg: (X)
body: (EXPT X 5.0)
VEQ:FEASE-IN-OUT-QUINT
  [symbol]

FEASE-IN-OUT-QUINT names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (EXPT X 5.0)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-OUT-SIN

```
ease in-out:
arg: (X)
body: (- 1.0 (COS (* X FPI5)))
VEQ:FEASE-IN-OUT-SIN
  [symbol]

FEASE-IN-OUT-SIN names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 1.0) &OPTIONAL))
  Documentation:
    ease in-out:
    arg: (X)
    body: (- 1.0 (COS (* X FPI5)))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-QUART

```
ease in:
arg: (X)
body: (EXPT X 4.0)
VEQ:FEASE-IN-QUART
  [symbol]

FEASE-IN-QUART names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (EXPT X 4.0)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-QUINT

```
ease in:
arg: (X)
body: (EXPT X 5.0)
VEQ:FEASE-IN-QUINT
  [symbol]

FEASE-IN-QUINT names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (EXPT X 5.0)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-IN-SIN

```
ease in:
arg: (X)
body: (- 1.0 (COS (* X FPI5)))
VEQ:FEASE-IN-SIN
  [symbol]

FEASE-IN-SIN names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT 0.0 2.0) &OPTIONAL))
  Documentation:
    ease in:
    arg: (X)
    body: (- 1.0 (COS (* X FPI5)))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-BACK

```
ease out:
arg: (X &OPTIONAL (S 1.70158))
body: (* X X (- (* (+ 1.0 S) X) S))
VEQ:FEASE-OUT-BACK
  [symbol]

FEASE-OUT-BACK names a compiled function:
  Lambda-list: (X &OPTIONAL (S 1.70158))
  Derived type: (FUNCTION (T &OPTIONAL T)
                 (VALUES
                  (OR FLOAT (COMPLEX DOUBLE-FLOAT)
                      (COMPLEX SINGLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease out:
    arg: (X &OPTIONAL (S 1.70158))
    body: (* X X (- (* (+ 1.0 S) X) S))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-CIRC

```
ease out:
arg: (X)
body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
VEQ:FEASE-OUT-CIRC
  [symbol]

FEASE-OUT-CIRC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES
                  (OR (COMPLEX SINGLE-FLOAT) (SINGLE-FLOAT 0.0 1.0))
                  &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (- (- (SQRT (- 1.0 (* X X))) 1.0))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-CUBIC

```
ease out:
arg: (X)
body: (* X X X)
VEQ:FEASE-OUT-CUBIC
  [symbol]

FEASE-OUT-CUBIC names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (* X X X)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-ELASTIC

```
ease out:
arg: (X &OPTIONAL (P 0.3) (S NIL))
body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
        (-
         (* (EXPT 2.0 (* 10.0 (- X 1.0)))
            (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
VEQ:FEASE-OUT-ELASTIC
  [symbol]

FEASE-OUT-ELASTIC names a compiled function:
  Lambda-list: (X &OPTIONAL (P 0.3) (S NIL))
  Derived type: (FUNCTION (T &OPTIONAL T T)
                 (VALUES
                  (OR FLOAT (COMPLEX SINGLE-FLOAT)
                      (COMPLEX DOUBLE-FLOAT))
                  &OPTIONAL))
  Documentation:
    ease out:
    arg: (X &OPTIONAL (P 0.3) (S NIL))
    body: (LET ((S (OR S (* (ASIN 1.0) (/ P FPII)))))
            (-
             (* (EXPT 2.0 (* 10.0 (- X 1.0)))
                (SIN (/ (* (- (- X 1.0) S) FPII) P)))))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-EXP

```
ease out:
arg: (X)
body: (EXPT 2.0 (* 10.0 (- X 1.0)))
VEQ:FEASE-OUT-EXP
  [symbol]

FEASE-OUT-EXP names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES (SINGLE-FLOAT * 1.0) &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (EXPT 2.0 (* 10.0 (- X 1.0)))
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-LINEAR

```
ease out:
arg: (X)
body: X
VEQ:FEASE-OUT-LINEAR
  [symbol]

FEASE-OUT-LINEAR names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: X
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-QUART

```
ease out:
arg: (X)
body: (EXPT X 4.0)
VEQ:FEASE-OUT-QUART
  [symbol]

FEASE-OUT-QUART names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (EXPT X 4.0)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-QUINT

```
ease out:
arg: (X)
body: (EXPT X 5.0)
VEQ:FEASE-OUT-QUINT
  [symbol]

FEASE-OUT-QUINT names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (EXPT X 5.0)
  Source file: /data/x/veq/src/easing.lisp
```
### FEASE-OUT-SIN

```
ease out:
arg: (X)
body: (- 1.0 (COS (* X FPI5)))
VEQ:FEASE-OUT-SIN
  [symbol]

FEASE-OUT-SIN names a compiled function:
  Lambda-list: (X)
  Derived type: (FUNCTION (T)
                 (VALUES (SINGLE-FLOAT -1.0 1.0) &OPTIONAL))
  Documentation:
    ease out:
    arg: (X)
    body: (- 1.0 (COS (* X FPI5)))
  Source file: /data/x/veq/src/easing.lisp
```
### FEXP

```
veq context op: FEXP
fxname: -FEXP
args: (A)
body: (VALUES (EXP A))
```
### FF

```
:missing:

VEQ:FF
  [symbol]

FF names a macro:
  Lambda-list: (&BODY BODY)
  Source file: /data/x/veq/src/utils.lisp

FF names a type-specifier:
  Lambda-list: ()
  Expansion: SINGLE-FLOAT
```
### FF*

```
:missing:

VEQ:FF*
  [symbol]

FF* names a macro:
  Lambda-list: (&BODY BODY)
  Source file: /data/x/veq/src/utils.lisp
```
### FFL

```
return (values (ff a) (ff b) ..) from (list a b ..)
VEQ:FFL
  [symbol]

FFL names a compiled function:
  Lambda-list: (L)
  Derived type: (FUNCTION (LIST) *)
  Documentation:
    return (values (ff a) (ff b) ..) from (list a b ..)
  Source file: /data/x/veq/src/utils.lisp
```
### FFROM

```
veq context op: FFROM
fxname: -FFROM
args: (AX BX S)
body: (+ AX (* BX S))
```
### FI-

```
veq context op: FI-
fxname: -FI-
args: (A B)
body: (- B A)
```
### FI/

```
veq context op: FI/
fxname: -FI/
args: (A B)
body: (/ B A)
```
### FISCALE

```
veq context op: FISCALE
fxname: -FISCALE
args: (A S)
body: (VALUES (/ A S))
```
### FLEN

```
veq context op: FLEN
fxname: -FLEN
args: (A)
body: (THE POS-FF A)
```
### FLEN2

```
veq context op: FLEN2
fxname: -FLEN2
args: (A)
body: (THE POS-FF (MVC #'+ (-FSQUARE A)))
```
### FLERP

```
veq context op: FLERP
fxname: -FLERP
args: (AX BX S)
body: (+ AX (* (- BX AX) S))
```
### FMAKE-ORTHO-PROJ-MATRIX

```
macro wrapper: (mvc #'%FMAKE-ORTHO-PROJ-MATRIX ...) in veq context.
see function: %FMAKE-ORTHO-PROJ-MATRIX
VEQ:FMAKE-ORTHO-PROJ-MATRIX
  [symbol]

FMAKE-ORTHO-PROJ-MATRIX names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%FMAKE-ORTHO-PROJ-MATRIX ...) in veq context.
    see function: %FMAKE-ORTHO-PROJ-MATRIX
  Source file: /data/x/veq/src/mat-cam.lisp
```
### FMAKE-PROJ-MATRIX

```
macro wrapper: (mvc #'%FMAKE-PROJ-MATRIX ...) in veq context.
see function: %FMAKE-PROJ-MATRIX
VEQ:FMAKE-PROJ-MATRIX
  [symbol]

FMAKE-PROJ-MATRIX names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%FMAKE-PROJ-MATRIX ...) in veq context.
    see function: %FMAKE-PROJ-MATRIX
  Source file: /data/x/veq/src/mat-cam.lisp
```
### FMAKE-VIEW-MATRIX

```
macro wrapper: (mvc #'%FMAKE-VIEW-MATRIX ...) in veq context.
see function: %FMAKE-VIEW-MATRIX
VEQ:FMAKE-VIEW-MATRIX
  [symbol]

FMAKE-VIEW-MATRIX names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    macro wrapper: (mvc #'%FMAKE-VIEW-MATRIX ...) in veq context.
    see function: %FMAKE-VIEW-MATRIX
  Source file: /data/x/veq/src/mat-cam.lisp
```
### FMID

```
veq context op: FMID
fxname: -FMID
args: (AX BX)
body: (* 0.5 (+ AX BX))
```
### FMOD

```
veq context op: FMOD
fxname: -FMOD
args: (A S)
body: (MOD A S)
```
### FNEG

```
veq context op: FNEG
fxname: -FNEG
args: (A)
body: (- A)
```
### FNORM

```
veq context op: FNORM
fxname: -FNORM
args: (A)
body: (MVC #'-FISCALE A (MVC #'-FLEN A))
```
### FNSUM

```
:missing:

VEQ:FNSUM
  [symbol]
```
### FPI

```
:missing:

VEQ:FPI
  [symbol]

FPI names a constant variable:
  Declared type: SINGLE-FLOAT
  Value: 3.1415927
```
### FPI5

```
:missing:

VEQ:FPI5
  [symbol]

FPI5 names a constant variable:
  Declared type: SINGLE-FLOAT
  Value: 1.5707964
```
### FPII

```
:missing:

VEQ:FPII
  [symbol]

FPII names a constant variable:
  Declared type: SINGLE-FLOAT
  Value: 6.2831855
```
### FREP

```
:missing:

VEQ:FREP
  [symbol]
```
### FREP*

```
:missing:

VEQ:FREP*
  [symbol]
```
### FROM-LST

```
get values from list
VEQ:FROM-LST
  [symbol]

FROM-LST names a macro:
  Lambda-list: (L)
  Documentation:
    get values from list
  Source file: /data/x/veq/src/macros.lisp
```
### FSCALE

```
veq context op: FSCALE
fxname: -FSCALE
args: (A S)
body: (VALUES (* A S))
```
### FSIN-COS

```
veq context op: FSIN-COS
fxname: -FSIN-COS
args: (A)
body: (VALUES (SIN A) (COS A))
```
### FSQRT

```
veq context op: FSQRT
fxname: -FSQRT
args: (A)
body: (THE POS-FF (SQRT (THE POS-FF A)))
```
### FSQUARE

```
veq context op: FSQUARE
fxname: -FSQUARE
args: (A)
body: (* A A)
```
### FVDEF

```
define function with veq enabled. see fvprogn.
VEQ:FVDEF
  [symbol]

FVDEF names a macro:
  Lambda-list: (FNAME &BODY BODY)
  Documentation:
    define function with veq enabled. see fvprogn.
  Source file: /data/x/veq/src/macros.lisp
```
### FVDEF*

```
define function, and corresponding macro, with veq enabled. see fvprogn.
     use %mname to call function outside mvc.
VEQ:FVDEF*
  [symbol]

FVDEF* names a macro:
  Lambda-list: (MNAME &BODY BODY)
  Documentation:
    define function, and corresponding macro, with veq enabled. see fvprogn.
         use %mname to call function outside mvc.
  Source file: /data/x/veq/src/macros.lisp
```
### FVEC

```
:missing:

VEQ:FVEC
  [symbol]

FVEC names a type-specifier:
  Lambda-list: ()
  Expansion: (SIMPLE-ARRAY VEQ:FF)
```
### FVLET

```
:missing:

VEQ:FVLET
  [symbol]
```
### FVPROGN

```
enable veq inside this progn.  removes all macrolets that are not directly
    used in body. this is faster, but will fail if macros in body introduce
    macrolets from veq
VEQ:FVPROGN
  [symbol]

FVPROGN names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    enable veq inside this progn.  removes all macrolets that are not directly
        used in body. this is faster, but will fail if macros in body introduce
        macrolets from veq
  Source file: /data/x/veq/src/macros.lisp
```
### FVSET

```
:missing:

VEQ:FVSET
  [symbol]
```
### FW

```
macro. reorder arguments (X Y Z W) as (W), (IGNORE Z Y X).
```
### FWITH-ARRAYS

```
:missing:

VEQ:FWITH-ARRAYS
  [symbol]
```
### FWW

```
macro. reorder arguments (X Y Z W) as (W W), (IGNORE Z Y X).
```
### FWWW

```
macro. reorder arguments (X Y Z W) as (W W W), (IGNORE Z Y X).
```
### FWWWW

```
macro. reorder arguments (X Y Z W) as (W W W W), (IGNORE Z Y X).
```
### FWWWX

```
macro. reorder arguments (X Y Z W) as (W W W X), (IGNORE Z Y).
```
### FWWWY

```
macro. reorder arguments (X Y Z W) as (W W W Y), (IGNORE Z X).
```
### FWWWZ

```
macro. reorder arguments (X Y Z W) as (W W W Z), (IGNORE Y X).
```
### FWWX

```
macro. reorder arguments (X Y Z W) as (W W X), (IGNORE Z Y).
```
### FWWXW

```
macro. reorder arguments (X Y Z W) as (W W X W), (IGNORE Z Y).
```
### FWWXX

```
macro. reorder arguments (X Y Z W) as (W W X X), (IGNORE Z Y).
```
### FWWXY

```
macro. reorder arguments (X Y Z W) as (W W X Y), (IGNORE Z).
```
### FWWXZ

```
macro. reorder arguments (X Y Z W) as (W W X Z), (IGNORE Y).
```
### FWWY

```
macro. reorder arguments (X Y Z W) as (W W Y), (IGNORE Z X).
```
### FWWYW

```
macro. reorder arguments (X Y Z W) as (W W Y W), (IGNORE Z X).
```
### FWWYX

```
macro. reorder arguments (X Y Z W) as (W W Y X), (IGNORE Z).
```
### FWWYY

```
macro. reorder arguments (X Y Z W) as (W W Y Y), (IGNORE Z X).
```
### FWWYZ

```
macro. reorder arguments (X Y Z W) as (W W Y Z), (IGNORE X).
```
### FWWZ

```
macro. reorder arguments (X Y Z W) as (W W Z), (IGNORE Y X).
```
### FWWZW

```
macro. reorder arguments (X Y Z W) as (W W Z W), (IGNORE Y X).
```
### FWWZX

```
macro. reorder arguments (X Y Z W) as (W W Z X), (IGNORE Y).
```
### FWWZY

```
macro. reorder arguments (X Y Z W) as (W W Z Y), (IGNORE X).
```
### FWWZZ

```
macro. reorder arguments (X Y Z W) as (W W Z Z), (IGNORE Y X).
```
### FWX

```
macro. reorder arguments (X Y Z W) as (W X), (IGNORE Z Y).
```
### FWXW

```
macro. reorder arguments (X Y Z W) as (W X W), (IGNORE Z Y).
```
### FWXWW

```
macro. reorder arguments (X Y Z W) as (W X W W), (IGNORE Z Y).
```
### FWXWX

```
macro. reorder arguments (X Y Z W) as (W X W X), (IGNORE Z Y).
```
### FWXWY

```
macro. reorder arguments (X Y Z W) as (W X W Y), (IGNORE Z).
```
### FWXWZ

```
macro. reorder arguments (X Y Z W) as (W X W Z), (IGNORE Y).
```
### FWXX

```
macro. reorder arguments (X Y Z W) as (W X X), (IGNORE Z Y).
```
### FWXXW

```
macro. reorder arguments (X Y Z W) as (W X X W), (IGNORE Z Y).
```
### FWXXX

```
macro. reorder arguments (X Y Z W) as (W X X X), (IGNORE Z Y).
```
### FWXXY

```
macro. reorder arguments (X Y Z W) as (W X X Y), (IGNORE Z).
```
### FWXXZ

```
macro. reorder arguments (X Y Z W) as (W X X Z), (IGNORE Y).
```
### FWXY

```
macro. reorder arguments (X Y Z W) as (W X Y), (IGNORE Z).
```
### FWXYW

```
macro. reorder arguments (X Y Z W) as (W X Y W), (IGNORE Z).
```
### FWXYX

```
macro. reorder arguments (X Y Z W) as (W X Y X), (IGNORE Z).
```
### FWXYY

```
macro. reorder arguments (X Y Z W) as (W X Y Y), (IGNORE Z).
```
### FWXYZ

```
macro. reorder arguments (X Y Z W) as (W X Y Z), (IGNORE).
```
### FWXZ

```
macro. reorder arguments (X Y Z W) as (W X Z), (IGNORE Y).
```
### FWXZW

```
macro. reorder arguments (X Y Z W) as (W X Z W), (IGNORE Y).
```
### FWXZX

```
macro. reorder arguments (X Y Z W) as (W X Z X), (IGNORE Y).
```
### FWXZY

```
macro. reorder arguments (X Y Z W) as (W X Z Y), (IGNORE).
```
### FWXZZ

```
macro. reorder arguments (X Y Z W) as (W X Z Z), (IGNORE Y).
```
### FWY

```
macro. reorder arguments (X Y Z W) as (W Y), (IGNORE Z X).
```
### FWYW

```
macro. reorder arguments (X Y Z W) as (W Y W), (IGNORE Z X).
```
### FWYWW

```
macro. reorder arguments (X Y Z W) as (W Y W W), (IGNORE Z X).
```
### FWYWX

```
macro. reorder arguments (X Y Z W) as (W Y W X), (IGNORE Z).
```
### FWYWY

```
macro. reorder arguments (X Y Z W) as (W Y W Y), (IGNORE Z X).
```
### FWYWZ

```
macro. reorder arguments (X Y Z W) as (W Y W Z), (IGNORE X).
```
### FWYX

```
macro. reorder arguments (X Y Z W) as (W Y X), (IGNORE Z).
```
### FWYXW

```
macro. reorder arguments (X Y Z W) as (W Y X W), (IGNORE Z).
```
### FWYXX

```
macro. reorder arguments (X Y Z W) as (W Y X X), (IGNORE Z).
```
### FWYXY

```
macro. reorder arguments (X Y Z W) as (W Y X Y), (IGNORE Z).
```
### FWYXZ

```
macro. reorder arguments (X Y Z W) as (W Y X Z), (IGNORE).
```
### FWYY

```
macro. reorder arguments (X Y Z W) as (W Y Y), (IGNORE Z X).
```
### FWYYW

```
macro. reorder arguments (X Y Z W) as (W Y Y W), (IGNORE Z X).
```
### FWYYX

```
macro. reorder arguments (X Y Z W) as (W Y Y X), (IGNORE Z).
```
### FWYYY

```
macro. reorder arguments (X Y Z W) as (W Y Y Y), (IGNORE Z X).
```
### FWYYZ

```
macro. reorder arguments (X Y Z W) as (W Y Y Z), (IGNORE X).
```
### FWYZ

```
macro. reorder arguments (X Y Z W) as (W Y Z), (IGNORE X).
```
### FWYZW

```
macro. reorder arguments (X Y Z W) as (W Y Z W), (IGNORE X).
```
### FWYZX

```
macro. reorder arguments (X Y Z W) as (W Y Z X), (IGNORE).
```
### FWYZY

```
macro. reorder arguments (X Y Z W) as (W Y Z Y), (IGNORE X).
```
### FWYZZ

```
macro. reorder arguments (X Y Z W) as (W Y Z Z), (IGNORE X).
```
### FWZ

```
macro. reorder arguments (X Y Z W) as (W Z), (IGNORE Y X).
```
### FWZW

```
macro. reorder arguments (X Y Z W) as (W Z W), (IGNORE Y X).
```
### FWZWW

```
macro. reorder arguments (X Y Z W) as (W Z W W), (IGNORE Y X).
```
### FWZWX

```
macro. reorder arguments (X Y Z W) as (W Z W X), (IGNORE Y).
```
### FWZWY

```
macro. reorder arguments (X Y Z W) as (W Z W Y), (IGNORE X).
```
### FWZWZ

```
macro. reorder arguments (X Y Z W) as (W Z W Z), (IGNORE Y X).
```
### FWZX

```
macro. reorder arguments (X Y Z W) as (W Z X), (IGNORE Y).
```
### FWZXW

```
macro. reorder arguments (X Y Z W) as (W Z X W), (IGNORE Y).
```
### FWZXX

```
macro. reorder arguments (X Y Z W) as (W Z X X), (IGNORE Y).
```
### FWZXY

```
macro. reorder arguments (X Y Z W) as (W Z X Y), (IGNORE).
```
### FWZXZ

```
macro. reorder arguments (X Y Z W) as (W Z X Z), (IGNORE Y).
```
### FWZY

```
macro. reorder arguments (X Y Z W) as (W Z Y), (IGNORE X).
```
### FWZYW

```
macro. reorder arguments (X Y Z W) as (W Z Y W), (IGNORE X).
```
### FWZYX

```
macro. reorder arguments (X Y Z W) as (W Z Y X), (IGNORE).
```
### FWZYY

```
macro. reorder arguments (X Y Z W) as (W Z Y Y), (IGNORE X).
```
### FWZYZ

```
macro. reorder arguments (X Y Z W) as (W Z Y Z), (IGNORE X).
```
### FWZZ

```
macro. reorder arguments (X Y Z W) as (W Z Z), (IGNORE Y X).
```
### FWZZW

```
macro. reorder arguments (X Y Z W) as (W Z Z W), (IGNORE Y X).
```
### FWZZX

```
macro. reorder arguments (X Y Z W) as (W Z Z X), (IGNORE Y).
```
### FWZZY

```
macro. reorder arguments (X Y Z W) as (W Z Z Y), (IGNORE X).
```
### FWZZZ

```
macro. reorder arguments (X Y Z W) as (W Z Z Z), (IGNORE Y X).
```
### FX

```
macro. reorder arguments (X Y Z W) as (X), (IGNORE W Z Y).
```
### FXW

```
macro. reorder arguments (X Y Z W) as (X W), (IGNORE Z Y).
```
### FXWW

```
macro. reorder arguments (X Y Z W) as (X W W), (IGNORE Z Y).
```
### FXWWW

```
macro. reorder arguments (X Y Z W) as (X W W W), (IGNORE Z Y).
```
### FXWWX

```
macro. reorder arguments (X Y Z W) as (X W W X), (IGNORE Z Y).
```
### FXWWY

```
macro. reorder arguments (X Y Z W) as (X W W Y), (IGNORE Z).
```
### FXWWZ

```
macro. reorder arguments (X Y Z W) as (X W W Z), (IGNORE Y).
```
### FXWX

```
macro. reorder arguments (X Y Z W) as (X W X), (IGNORE Z Y).
```
### FXWXW

```
macro. reorder arguments (X Y Z W) as (X W X W), (IGNORE Z Y).
```
### FXWXX

```
macro. reorder arguments (X Y Z W) as (X W X X), (IGNORE Z Y).
```
### FXWXY

```
macro. reorder arguments (X Y Z W) as (X W X Y), (IGNORE Z).
```
### FXWXZ

```
macro. reorder arguments (X Y Z W) as (X W X Z), (IGNORE Y).
```
### FXWY

```
macro. reorder arguments (X Y Z W) as (X W Y), (IGNORE Z).
```
### FXWYW

```
macro. reorder arguments (X Y Z W) as (X W Y W), (IGNORE Z).
```
### FXWYX

```
macro. reorder arguments (X Y Z W) as (X W Y X), (IGNORE Z).
```
### FXWYY

```
macro. reorder arguments (X Y Z W) as (X W Y Y), (IGNORE Z).
```
### FXWYZ

```
macro. reorder arguments (X Y Z W) as (X W Y Z), (IGNORE).
```
### FXWZ

```
macro. reorder arguments (X Y Z W) as (X W Z), (IGNORE Y).
```
### FXWZW

```
macro. reorder arguments (X Y Z W) as (X W Z W), (IGNORE Y).
```
### FXWZX

```
macro. reorder arguments (X Y Z W) as (X W Z X), (IGNORE Y).
```
### FXWZY

```
macro. reorder arguments (X Y Z W) as (X W Z Y), (IGNORE).
```
### FXWZZ

```
macro. reorder arguments (X Y Z W) as (X W Z Z), (IGNORE Y).
```
### FXX

```
macro. reorder arguments (X Y Z W) as (X X), (IGNORE W Z Y).
```
### FXXW

```
macro. reorder arguments (X Y Z W) as (X X W), (IGNORE Z Y).
```
### FXXWW

```
macro. reorder arguments (X Y Z W) as (X X W W), (IGNORE Z Y).
```
### FXXWX

```
macro. reorder arguments (X Y Z W) as (X X W X), (IGNORE Z Y).
```
### FXXWY

```
macro. reorder arguments (X Y Z W) as (X X W Y), (IGNORE Z).
```
### FXXWZ

```
macro. reorder arguments (X Y Z W) as (X X W Z), (IGNORE Y).
```
### FXXX

```
macro. reorder arguments (X Y Z W) as (X X X), (IGNORE W Z Y).
```
### FXXXW

```
macro. reorder arguments (X Y Z W) as (X X X W), (IGNORE Z Y).
```
### FXXXX

```
macro. reorder arguments (X Y Z W) as (X X X X), (IGNORE W Z Y).
```
### FXXXY

```
macro. reorder arguments (X Y Z W) as (X X X Y), (IGNORE W Z).
```
### FXXXZ

```
macro. reorder arguments (X Y Z W) as (X X X Z), (IGNORE W Y).
```
### FXXY

```
macro. reorder arguments (X Y Z W) as (X X Y), (IGNORE W Z).
```
### FXXYW

```
macro. reorder arguments (X Y Z W) as (X X Y W), (IGNORE Z).
```
### FXXYX

```
macro. reorder arguments (X Y Z W) as (X X Y X), (IGNORE W Z).
```
### FXXYY

```
macro. reorder arguments (X Y Z W) as (X X Y Y), (IGNORE W Z).
```
### FXXYZ

```
macro. reorder arguments (X Y Z W) as (X X Y Z), (IGNORE W).
```
### FXXZ

```
macro. reorder arguments (X Y Z W) as (X X Z), (IGNORE W Y).
```
### FXXZW

```
macro. reorder arguments (X Y Z W) as (X X Z W), (IGNORE Y).
```
### FXXZX

```
macro. reorder arguments (X Y Z W) as (X X Z X), (IGNORE W Y).
```
### FXXZY

```
macro. reorder arguments (X Y Z W) as (X X Z Y), (IGNORE W).
```
### FXXZZ

```
macro. reorder arguments (X Y Z W) as (X X Z Z), (IGNORE W Y).
```
### FXY

```
macro. reorder arguments (X Y Z W) as (X Y), (IGNORE W Z).
```
### FXYW

```
macro. reorder arguments (X Y Z W) as (X Y W), (IGNORE Z).
```
### FXYWW

```
macro. reorder arguments (X Y Z W) as (X Y W W), (IGNORE Z).
```
### FXYWX

```
macro. reorder arguments (X Y Z W) as (X Y W X), (IGNORE Z).
```
### FXYWY

```
macro. reorder arguments (X Y Z W) as (X Y W Y), (IGNORE Z).
```
### FXYWZ

```
macro. reorder arguments (X Y Z W) as (X Y W Z), (IGNORE).
```
### FXYX

```
macro. reorder arguments (X Y Z W) as (X Y X), (IGNORE W Z).
```
### FXYXW

```
macro. reorder arguments (X Y Z W) as (X Y X W), (IGNORE Z).
```
### FXYXX

```
macro. reorder arguments (X Y Z W) as (X Y X X), (IGNORE W Z).
```
### FXYXY

```
macro. reorder arguments (X Y Z W) as (X Y X Y), (IGNORE W Z).
```
### FXYXZ

```
macro. reorder arguments (X Y Z W) as (X Y X Z), (IGNORE W).
```
### FXYY

```
macro. reorder arguments (X Y Z W) as (X Y Y), (IGNORE W Z).
```
### FXYYW

```
macro. reorder arguments (X Y Z W) as (X Y Y W), (IGNORE Z).
```
### FXYYX

```
macro. reorder arguments (X Y Z W) as (X Y Y X), (IGNORE W Z).
```
### FXYYY

```
macro. reorder arguments (X Y Z W) as (X Y Y Y), (IGNORE W Z).
```
### FXYYZ

```
macro. reorder arguments (X Y Z W) as (X Y Y Z), (IGNORE W).
```
### FXYZ

```
macro. reorder arguments (X Y Z W) as (X Y Z), (IGNORE W).
```
### FXYZW

```
macro. reorder arguments (X Y Z W) as (X Y Z W), (IGNORE).
```
### FXYZX

```
macro. reorder arguments (X Y Z W) as (X Y Z X), (IGNORE W).
```
### FXYZY

```
macro. reorder arguments (X Y Z W) as (X Y Z Y), (IGNORE W).
```
### FXYZZ

```
macro. reorder arguments (X Y Z W) as (X Y Z Z), (IGNORE W).
```
### FXZ

```
macro. reorder arguments (X Y Z W) as (X Z), (IGNORE W Y).
```
### FXZW

```
macro. reorder arguments (X Y Z W) as (X Z W), (IGNORE Y).
```
### FXZWW

```
macro. reorder arguments (X Y Z W) as (X Z W W), (IGNORE Y).
```
### FXZWX

```
macro. reorder arguments (X Y Z W) as (X Z W X), (IGNORE Y).
```
### FXZWY

```
macro. reorder arguments (X Y Z W) as (X Z W Y), (IGNORE).
```
### FXZWZ

```
macro. reorder arguments (X Y Z W) as (X Z W Z), (IGNORE Y).
```
### FXZX

```
macro. reorder arguments (X Y Z W) as (X Z X), (IGNORE W Y).
```
### FXZXW

```
macro. reorder arguments (X Y Z W) as (X Z X W), (IGNORE Y).
```
### FXZXX

```
macro. reorder arguments (X Y Z W) as (X Z X X), (IGNORE W Y).
```
### FXZXY

```
macro. reorder arguments (X Y Z W) as (X Z X Y), (IGNORE W).
```
### FXZXZ

```
macro. reorder arguments (X Y Z W) as (X Z X Z), (IGNORE W Y).
```
### FXZY

```
macro. reorder arguments (X Y Z W) as (X Z Y), (IGNORE W).
```
### FXZYW

```
macro. reorder arguments (X Y Z W) as (X Z Y W), (IGNORE).
```
### FXZYX

```
macro. reorder arguments (X Y Z W) as (X Z Y X), (IGNORE W).
```
### FXZYY

```
macro. reorder arguments (X Y Z W) as (X Z Y Y), (IGNORE W).
```
### FXZYZ

```
macro. reorder arguments (X Y Z W) as (X Z Y Z), (IGNORE W).
```
### FXZZ

```
macro. reorder arguments (X Y Z W) as (X Z Z), (IGNORE W Y).
```
### FXZZW

```
macro. reorder arguments (X Y Z W) as (X Z Z W), (IGNORE Y).
```
### FXZZX

```
macro. reorder arguments (X Y Z W) as (X Z Z X), (IGNORE W Y).
```
### FXZZY

```
macro. reorder arguments (X Y Z W) as (X Z Z Y), (IGNORE W).
```
### FXZZZ

```
macro. reorder arguments (X Y Z W) as (X Z Z Z), (IGNORE W Y).
```
### FY

```
macro. reorder arguments (X Y Z W) as (Y), (IGNORE W Z X).
```
### FYW

```
macro. reorder arguments (X Y Z W) as (Y W), (IGNORE Z X).
```
### FYWW

```
macro. reorder arguments (X Y Z W) as (Y W W), (IGNORE Z X).
```
### FYWWW

```
macro. reorder arguments (X Y Z W) as (Y W W W), (IGNORE Z X).
```
### FYWWX

```
macro. reorder arguments (X Y Z W) as (Y W W X), (IGNORE Z).
```
### FYWWY

```
macro. reorder arguments (X Y Z W) as (Y W W Y), (IGNORE Z X).
```
### FYWWZ

```
macro. reorder arguments (X Y Z W) as (Y W W Z), (IGNORE X).
```
### FYWX

```
macro. reorder arguments (X Y Z W) as (Y W X), (IGNORE Z).
```
### FYWXW

```
macro. reorder arguments (X Y Z W) as (Y W X W), (IGNORE Z).
```
### FYWXX

```
macro. reorder arguments (X Y Z W) as (Y W X X), (IGNORE Z).
```
### FYWXY

```
macro. reorder arguments (X Y Z W) as (Y W X Y), (IGNORE Z).
```
### FYWXZ

```
macro. reorder arguments (X Y Z W) as (Y W X Z), (IGNORE).
```
### FYWY

```
macro. reorder arguments (X Y Z W) as (Y W Y), (IGNORE Z X).
```
### FYWYW

```
macro. reorder arguments (X Y Z W) as (Y W Y W), (IGNORE Z X).
```
### FYWYX

```
macro. reorder arguments (X Y Z W) as (Y W Y X), (IGNORE Z).
```
### FYWYY

```
macro. reorder arguments (X Y Z W) as (Y W Y Y), (IGNORE Z X).
```
### FYWYZ

```
macro. reorder arguments (X Y Z W) as (Y W Y Z), (IGNORE X).
```
### FYWZ

```
macro. reorder arguments (X Y Z W) as (Y W Z), (IGNORE X).
```
### FYWZW

```
macro. reorder arguments (X Y Z W) as (Y W Z W), (IGNORE X).
```
### FYWZX

```
macro. reorder arguments (X Y Z W) as (Y W Z X), (IGNORE).
```
### FYWZY

```
macro. reorder arguments (X Y Z W) as (Y W Z Y), (IGNORE X).
```
### FYWZZ

```
macro. reorder arguments (X Y Z W) as (Y W Z Z), (IGNORE X).
```
### FYX

```
macro. reorder arguments (X Y Z W) as (Y X), (IGNORE W Z).
```
### FYXW

```
macro. reorder arguments (X Y Z W) as (Y X W), (IGNORE Z).
```
### FYXWW

```
macro. reorder arguments (X Y Z W) as (Y X W W), (IGNORE Z).
```
### FYXWX

```
macro. reorder arguments (X Y Z W) as (Y X W X), (IGNORE Z).
```
### FYXWY

```
macro. reorder arguments (X Y Z W) as (Y X W Y), (IGNORE Z).
```
### FYXWZ

```
macro. reorder arguments (X Y Z W) as (Y X W Z), (IGNORE).
```
### FYXX

```
macro. reorder arguments (X Y Z W) as (Y X X), (IGNORE W Z).
```
### FYXXW

```
macro. reorder arguments (X Y Z W) as (Y X X W), (IGNORE Z).
```
### FYXXX

```
macro. reorder arguments (X Y Z W) as (Y X X X), (IGNORE W Z).
```
### FYXXY

```
macro. reorder arguments (X Y Z W) as (Y X X Y), (IGNORE W Z).
```
### FYXXZ

```
macro. reorder arguments (X Y Z W) as (Y X X Z), (IGNORE W).
```
### FYXY

```
macro. reorder arguments (X Y Z W) as (Y X Y), (IGNORE W Z).
```
### FYXYW

```
macro. reorder arguments (X Y Z W) as (Y X Y W), (IGNORE Z).
```
### FYXYX

```
macro. reorder arguments (X Y Z W) as (Y X Y X), (IGNORE W Z).
```
### FYXYY

```
macro. reorder arguments (X Y Z W) as (Y X Y Y), (IGNORE W Z).
```
### FYXYZ

```
macro. reorder arguments (X Y Z W) as (Y X Y Z), (IGNORE W).
```
### FYXZ

```
macro. reorder arguments (X Y Z W) as (Y X Z), (IGNORE W).
```
### FYXZW

```
macro. reorder arguments (X Y Z W) as (Y X Z W), (IGNORE).
```
### FYXZX

```
macro. reorder arguments (X Y Z W) as (Y X Z X), (IGNORE W).
```
### FYXZY

```
macro. reorder arguments (X Y Z W) as (Y X Z Y), (IGNORE W).
```
### FYXZZ

```
macro. reorder arguments (X Y Z W) as (Y X Z Z), (IGNORE W).
```
### FYY

```
macro. reorder arguments (X Y Z W) as (Y Y), (IGNORE W Z X).
```
### FYYW

```
macro. reorder arguments (X Y Z W) as (Y Y W), (IGNORE Z X).
```
### FYYWW

```
macro. reorder arguments (X Y Z W) as (Y Y W W), (IGNORE Z X).
```
### FYYWX

```
macro. reorder arguments (X Y Z W) as (Y Y W X), (IGNORE Z).
```
### FYYWY

```
macro. reorder arguments (X Y Z W) as (Y Y W Y), (IGNORE Z X).
```
### FYYWZ

```
macro. reorder arguments (X Y Z W) as (Y Y W Z), (IGNORE X).
```
### FYYX

```
macro. reorder arguments (X Y Z W) as (Y Y X), (IGNORE W Z).
```
### FYYXW

```
macro. reorder arguments (X Y Z W) as (Y Y X W), (IGNORE Z).
```
### FYYXX

```
macro. reorder arguments (X Y Z W) as (Y Y X X), (IGNORE W Z).
```
### FYYXY

```
macro. reorder arguments (X Y Z W) as (Y Y X Y), (IGNORE W Z).
```
### FYYXZ

```
macro. reorder arguments (X Y Z W) as (Y Y X Z), (IGNORE W).
```
### FYYY

```
macro. reorder arguments (X Y Z W) as (Y Y Y), (IGNORE W Z X).
```
### FYYYW

```
macro. reorder arguments (X Y Z W) as (Y Y Y W), (IGNORE Z X).
```
### FYYYX

```
macro. reorder arguments (X Y Z W) as (Y Y Y X), (IGNORE W Z).
```
### FYYYY

```
macro. reorder arguments (X Y Z W) as (Y Y Y Y), (IGNORE W Z X).
```
### FYYYZ

```
macro. reorder arguments (X Y Z W) as (Y Y Y Z), (IGNORE W X).
```
### FYYZ

```
macro. reorder arguments (X Y Z W) as (Y Y Z), (IGNORE W X).
```
### FYYZW

```
macro. reorder arguments (X Y Z W) as (Y Y Z W), (IGNORE X).
```
### FYYZX

```
macro. reorder arguments (X Y Z W) as (Y Y Z X), (IGNORE W).
```
### FYYZY

```
macro. reorder arguments (X Y Z W) as (Y Y Z Y), (IGNORE W X).
```
### FYYZZ

```
macro. reorder arguments (X Y Z W) as (Y Y Z Z), (IGNORE W X).
```
### FYZ

```
macro. reorder arguments (X Y Z W) as (Y Z), (IGNORE W X).
```
### FYZW

```
macro. reorder arguments (X Y Z W) as (Y Z W), (IGNORE X).
```
### FYZWW

```
macro. reorder arguments (X Y Z W) as (Y Z W W), (IGNORE X).
```
### FYZWX

```
macro. reorder arguments (X Y Z W) as (Y Z W X), (IGNORE).
```
### FYZWY

```
macro. reorder arguments (X Y Z W) as (Y Z W Y), (IGNORE X).
```
### FYZWZ

```
macro. reorder arguments (X Y Z W) as (Y Z W Z), (IGNORE X).
```
### FYZX

```
macro. reorder arguments (X Y Z W) as (Y Z X), (IGNORE W).
```
### FYZXW

```
macro. reorder arguments (X Y Z W) as (Y Z X W), (IGNORE).
```
### FYZXX

```
macro. reorder arguments (X Y Z W) as (Y Z X X), (IGNORE W).
```
### FYZXY

```
macro. reorder arguments (X Y Z W) as (Y Z X Y), (IGNORE W).
```
### FYZXZ

```
macro. reorder arguments (X Y Z W) as (Y Z X Z), (IGNORE W).
```
### FYZY

```
macro. reorder arguments (X Y Z W) as (Y Z Y), (IGNORE W X).
```
### FYZYW

```
macro. reorder arguments (X Y Z W) as (Y Z Y W), (IGNORE X).
```
### FYZYX

```
macro. reorder arguments (X Y Z W) as (Y Z Y X), (IGNORE W).
```
### FYZYY

```
macro. reorder arguments (X Y Z W) as (Y Z Y Y), (IGNORE W X).
```
### FYZYZ

```
macro. reorder arguments (X Y Z W) as (Y Z Y Z), (IGNORE W X).
```
### FYZZ

```
macro. reorder arguments (X Y Z W) as (Y Z Z), (IGNORE W X).
```
### FYZZW

```
macro. reorder arguments (X Y Z W) as (Y Z Z W), (IGNORE X).
```
### FYZZX

```
macro. reorder arguments (X Y Z W) as (Y Z Z X), (IGNORE W).
```
### FYZZY

```
macro. reorder arguments (X Y Z W) as (Y Z Z Y), (IGNORE W X).
```
### FYZZZ

```
macro. reorder arguments (X Y Z W) as (Y Z Z Z), (IGNORE W X).
```
### FZ

```
macro. reorder arguments (X Y Z W) as (Z), (IGNORE W Y X).
```
### FZW

```
macro. reorder arguments (X Y Z W) as (Z W), (IGNORE Y X).
```
### FZWW

```
macro. reorder arguments (X Y Z W) as (Z W W), (IGNORE Y X).
```
### FZWWW

```
macro. reorder arguments (X Y Z W) as (Z W W W), (IGNORE Y X).
```
### FZWWX

```
macro. reorder arguments (X Y Z W) as (Z W W X), (IGNORE Y).
```
### FZWWY

```
macro. reorder arguments (X Y Z W) as (Z W W Y), (IGNORE X).
```
### FZWWZ

```
macro. reorder arguments (X Y Z W) as (Z W W Z), (IGNORE Y X).
```
### FZWX

```
macro. reorder arguments (X Y Z W) as (Z W X), (IGNORE Y).
```
### FZWXW

```
macro. reorder arguments (X Y Z W) as (Z W X W), (IGNORE Y).
```
### FZWXX

```
macro. reorder arguments (X Y Z W) as (Z W X X), (IGNORE Y).
```
### FZWXY

```
macro. reorder arguments (X Y Z W) as (Z W X Y), (IGNORE).
```
### FZWXZ

```
macro. reorder arguments (X Y Z W) as (Z W X Z), (IGNORE Y).
```
### FZWY

```
macro. reorder arguments (X Y Z W) as (Z W Y), (IGNORE X).
```
### FZWYW

```
macro. reorder arguments (X Y Z W) as (Z W Y W), (IGNORE X).
```
### FZWYX

```
macro. reorder arguments (X Y Z W) as (Z W Y X), (IGNORE).
```
### FZWYY

```
macro. reorder arguments (X Y Z W) as (Z W Y Y), (IGNORE X).
```
### FZWYZ

```
macro. reorder arguments (X Y Z W) as (Z W Y Z), (IGNORE X).
```
### FZWZ

```
macro. reorder arguments (X Y Z W) as (Z W Z), (IGNORE Y X).
```
### FZWZW

```
macro. reorder arguments (X Y Z W) as (Z W Z W), (IGNORE Y X).
```
### FZWZX

```
macro. reorder arguments (X Y Z W) as (Z W Z X), (IGNORE Y).
```
### FZWZY

```
macro. reorder arguments (X Y Z W) as (Z W Z Y), (IGNORE X).
```
### FZWZZ

```
macro. reorder arguments (X Y Z W) as (Z W Z Z), (IGNORE Y X).
```
### FZX

```
macro. reorder arguments (X Y Z W) as (Z X), (IGNORE W Y).
```
### FZXW

```
macro. reorder arguments (X Y Z W) as (Z X W), (IGNORE Y).
```
### FZXWW

```
macro. reorder arguments (X Y Z W) as (Z X W W), (IGNORE Y).
```
### FZXWX

```
macro. reorder arguments (X Y Z W) as (Z X W X), (IGNORE Y).
```
### FZXWY

```
macro. reorder arguments (X Y Z W) as (Z X W Y), (IGNORE).
```
### FZXWZ

```
macro. reorder arguments (X Y Z W) as (Z X W Z), (IGNORE Y).
```
### FZXX

```
macro. reorder arguments (X Y Z W) as (Z X X), (IGNORE W Y).
```
### FZXXW

```
macro. reorder arguments (X Y Z W) as (Z X X W), (IGNORE Y).
```
### FZXXX

```
macro. reorder arguments (X Y Z W) as (Z X X X), (IGNORE W Y).
```
### FZXXY

```
macro. reorder arguments (X Y Z W) as (Z X X Y), (IGNORE W).
```
### FZXXZ

```
macro. reorder arguments (X Y Z W) as (Z X X Z), (IGNORE W Y).
```
### FZXY

```
macro. reorder arguments (X Y Z W) as (Z X Y), (IGNORE W).
```
### FZXYW

```
macro. reorder arguments (X Y Z W) as (Z X Y W), (IGNORE).
```
### FZXYX

```
macro. reorder arguments (X Y Z W) as (Z X Y X), (IGNORE W).
```
### FZXYY

```
macro. reorder arguments (X Y Z W) as (Z X Y Y), (IGNORE W).
```
### FZXYZ

```
macro. reorder arguments (X Y Z W) as (Z X Y Z), (IGNORE W).
```
### FZXZ

```
macro. reorder arguments (X Y Z W) as (Z X Z), (IGNORE W Y).
```
### FZXZW

```
macro. reorder arguments (X Y Z W) as (Z X Z W), (IGNORE Y).
```
### FZXZX

```
macro. reorder arguments (X Y Z W) as (Z X Z X), (IGNORE W Y).
```
### FZXZY

```
macro. reorder arguments (X Y Z W) as (Z X Z Y), (IGNORE W).
```
### FZXZZ

```
macro. reorder arguments (X Y Z W) as (Z X Z Z), (IGNORE W Y).
```
### FZY

```
macro. reorder arguments (X Y Z W) as (Z Y), (IGNORE W X).
```
### FZYW

```
macro. reorder arguments (X Y Z W) as (Z Y W), (IGNORE X).
```
### FZYWW

```
macro. reorder arguments (X Y Z W) as (Z Y W W), (IGNORE X).
```
### FZYWX

```
macro. reorder arguments (X Y Z W) as (Z Y W X), (IGNORE).
```
### FZYWY

```
macro. reorder arguments (X Y Z W) as (Z Y W Y), (IGNORE X).
```
### FZYWZ

```
macro. reorder arguments (X Y Z W) as (Z Y W Z), (IGNORE X).
```
### FZYX

```
macro. reorder arguments (X Y Z W) as (Z Y X), (IGNORE W).
```
### FZYXW

```
macro. reorder arguments (X Y Z W) as (Z Y X W), (IGNORE).
```
### FZYXX

```
macro. reorder arguments (X Y Z W) as (Z Y X X), (IGNORE W).
```
### FZYXY

```
macro. reorder arguments (X Y Z W) as (Z Y X Y), (IGNORE W).
```
### FZYXZ

```
macro. reorder arguments (X Y Z W) as (Z Y X Z), (IGNORE W).
```
### FZYY

```
macro. reorder arguments (X Y Z W) as (Z Y Y), (IGNORE W X).
```
### FZYYW

```
macro. reorder arguments (X Y Z W) as (Z Y Y W), (IGNORE X).
```
### FZYYX

```
macro. reorder arguments (X Y Z W) as (Z Y Y X), (IGNORE W).
```
### FZYYY

```
macro. reorder arguments (X Y Z W) as (Z Y Y Y), (IGNORE W X).
```
### FZYYZ

```
macro. reorder arguments (X Y Z W) as (Z Y Y Z), (IGNORE W X).
```
### FZYZ

```
macro. reorder arguments (X Y Z W) as (Z Y Z), (IGNORE W X).
```
### FZYZW

```
macro. reorder arguments (X Y Z W) as (Z Y Z W), (IGNORE X).
```
### FZYZX

```
macro. reorder arguments (X Y Z W) as (Z Y Z X), (IGNORE W).
```
### FZYZY

```
macro. reorder arguments (X Y Z W) as (Z Y Z Y), (IGNORE W X).
```
### FZYZZ

```
macro. reorder arguments (X Y Z W) as (Z Y Z Z), (IGNORE W X).
```
### FZZ

```
macro. reorder arguments (X Y Z W) as (Z Z), (IGNORE W Y X).
```
### FZZW

```
macro. reorder arguments (X Y Z W) as (Z Z W), (IGNORE Y X).
```
### FZZWW

```
macro. reorder arguments (X Y Z W) as (Z Z W W), (IGNORE Y X).
```
### FZZWX

```
macro. reorder arguments (X Y Z W) as (Z Z W X), (IGNORE Y).
```
### FZZWY

```
macro. reorder arguments (X Y Z W) as (Z Z W Y), (IGNORE X).
```
### FZZWZ

```
macro. reorder arguments (X Y Z W) as (Z Z W Z), (IGNORE Y X).
```
### FZZX

```
macro. reorder arguments (X Y Z W) as (Z Z X), (IGNORE W Y).
```
### FZZXW

```
macro. reorder arguments (X Y Z W) as (Z Z X W), (IGNORE Y).
```
### FZZXX

```
macro. reorder arguments (X Y Z W) as (Z Z X X), (IGNORE W Y).
```
### FZZXY

```
macro. reorder arguments (X Y Z W) as (Z Z X Y), (IGNORE W).
```
### FZZXZ

```
macro. reorder arguments (X Y Z W) as (Z Z X Z), (IGNORE W Y).
```
### FZZY

```
macro. reorder arguments (X Y Z W) as (Z Z Y), (IGNORE W X).
```
### FZZYW

```
macro. reorder arguments (X Y Z W) as (Z Z Y W), (IGNORE X).
```
### FZZYX

```
macro. reorder arguments (X Y Z W) as (Z Z Y X), (IGNORE W).
```
### FZZYY

```
macro. reorder arguments (X Y Z W) as (Z Z Y Y), (IGNORE W X).
```
### FZZYZ

```
macro. reorder arguments (X Y Z W) as (Z Z Y Z), (IGNORE W X).
```
### FZZZ

```
macro. reorder arguments (X Y Z W) as (Z Z Z), (IGNORE W Y X).
```
### FZZZW

```
macro. reorder arguments (X Y Z W) as (Z Z Z W), (IGNORE Y X).
```
### FZZZX

```
macro. reorder arguments (X Y Z W) as (Z Z Z X), (IGNORE W Y).
```
### FZZZY

```
macro. reorder arguments (X Y Z W) as (Z Z Z Y), (IGNORE W X).
```
### FZZZZ

```
macro. reorder arguments (X Y Z W) as (Z Z Z Z), (IGNORE W Y X).
```
### F~

```
:missing:

VEQ:F~
  [symbol]
```
### I?

```
inspect argument
VEQ:I?
  [symbol]

I? names a compiled function:
  Lambda-list: (F)
  Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
  Documentation:
    inspect argument
  Source file: /data/x/veq/src/utils.lisp
```
### IN

```
:missing:

VEQ:IN
  [symbol]

IN names a macro:
  Lambda-list: (&BODY BODY)
  Source file: /data/x/veq/src/utils.lisp

IN names a type-specifier:
  Lambda-list: ()
  Expansion: FIXNUM
```
### IN*

```
:missing:

VEQ:IN*
  [symbol]

IN* names a macro:
  Lambda-list: (&BODY BODY)
  Source file: /data/x/veq/src/utils.lisp
```
### IVEC

```
:missing:

VEQ:IVEC
  [symbol]

IVEC names a type-specifier:
  Lambda-list: ()
  Expansion: (SIMPLE-ARRAY VEQ:IN)
```
### LST

```
wrap (values ..) in (list ..)
VEQ:LST
  [symbol]

LST names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    wrap (values ..) in (list ..)
  Source file: /data/x/veq/src/macros.lisp
```
### MAC

```
:missing:

VEQ:MAC
  [symbol]

MAC names a macro:
  Lambda-list: (EXPR)
  Source file: /data/x/veq/src/utils.lisp
```
### MAC*

```
:missing:

VEQ:MAC*
  [symbol]

MAC* names a macro:
  Lambda-list: (EXPR)
  Source file: /data/x/veq/src/utils.lisp
```
### MVB

```
:missing:

VEQ:MVB
  [symbol]

MVB names a macro:
  Lambda-list: (&REST ARGS)
  Source file: /data/x/veq/src/utils.lisp
```
### MVC

```
:missing:

VEQ:MVC
  [symbol]

MVC names a macro:
  Lambda-list: (&REST ARGS)
  Source file: /data/x/veq/src/utils.lisp
```
### MVCWRAP

```
wrap fx in a macro, m, so that fx will be called via mvc
VEQ:MVCWRAP
  [symbol]

MVCWRAP names a macro:
  Lambda-list: (M FX)
  Documentation:
    wrap fx in a macro, m, so that fx will be called via mvc
  Source file: /data/x/veq/src/extra.lisp
```
### V?

```
get version. use silent to surpress stdout
VEQ:V?
  [symbol]

V? names a compiled function:
  Lambda-list: (&OPTIONAL (SILENT T))
  Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
  Documentation:
    get version. use silent to surpress stdout
  Source file: /data/x/veq/src/utils.lisp
```
### VARG

```
use (veq:varg n a b ...) or (:vr n a b ...) to represent vectors a,b 
of dim n in fvdef*, vdef*, def*. see replace-varg for details```
### VDEF

```
define function with veq enabled. see vprogn.
VEQ:VDEF
  [symbol]

VDEF names a macro:
  Lambda-list: (FNAME &BODY BODY)
  Documentation:
    define function with veq enabled. see vprogn.
  Source file: /data/x/veq/src/macros.lisp
```
### VDEF*

```
define function, and corresponding macro, with veq enabled.
     use %mname to call function outside mvc.
VEQ:VDEF*
  [symbol]

VDEF* names a macro:
  Lambda-list: (MNAME &BODY BODY)
  Documentation:
    define function, and corresponding macro, with veq enabled.
         use %mname to call function outside mvc.
  Source file: /data/x/veq/src/macros.lisp
```
### VGRP-MVC

```
do (multiple-value-call fx g) where g is groups
   of size dim over the (values ...) returned by body
   
VEQ:VGRP-MVC
  [symbol]

VGRP-MVC names a macro:
  Lambda-list: ((DIM FX) &BODY BODY)
  Documentation:
    do (multiple-value-call fx g) where g is groups
       of size dim over the (values ...) returned by body

  Source file: /data/x/veq/src/utils.lisp
```
### VLABELS

```
wraps labels so that it can be used with implicit multiple value call (mvc).
   that is, all labels are defined as if with vdef* or fvdef*
   use %labelname to call the function directly, not via mvc.
VEQ:VLABELS
  [symbol]

VLABELS names a macro:
  Lambda-list: ((&REST LABS) &BODY BODY)
  Documentation:
    wraps labels so that it can be used with implicit multiple value call (mvc).
       that is, all labels are defined as if with vdef* or fvdef*
       use %labelname to call the function directly, not via mvc.
  Source file: /data/x/veq/src/macros.lisp
```
### VPR

```
print (mvc #'list rest) and return (mvc #'values rest)
VEQ:VPR
  [symbol]

VPR names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    print (mvc #'list rest) and return (mvc #'values rest)
  Source file: /data/x/veq/src/extra.lisp
```
### VPROD

```
:missing:

VEQ:VPROD
  [symbol]

VPROD names a macro:
  Lambda-list: (&REST ARGS)
  Source file: /data/x/veq/src/extra.lisp
```
### VPROGN

```
enable veq inside this progn
VEQ:VPROGN
  [symbol]

VPROGN names a macro:
  Lambda-list: (&BODY BODY)
  Documentation:
    enable veq inside this progn
  Source file: /data/x/veq/src/macros.lisp
```
### VREF

```
use (veq:vref s x) or (:vr s x) to get dim x of symbol s
in fvdef*, vdef*, def*. see replace-varg for details```
### VSUM

```
:missing:

VEQ:VSUM
  [symbol]

VSUM names a macro:
  Lambda-list: (&REST ARGS)
  Source file: /data/x/veq/src/extra.lisp
```
### ~

```
wraps arguments in (mvc #'values ...)
VEQ:~
  [symbol]

~ names a macro:
  Lambda-list: (&REST REST)
  Documentation:
    wraps arguments in (mvc #'values ...)
  Source file: /data/x/veq/src/macros.lisp
```
