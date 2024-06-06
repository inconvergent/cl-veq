## `$`
```
 ; VEQ:$
 ;   [symbol]
 ; 
 ; $ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: /data/x/veq/src/array-utils.lisp
 ; 
 ; (SETF $) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: ($ a i) yields (values ...)
 ;     set: (setf ($ a i) (values ...))
 ;   Source file: /data/x/veq/src/vset.lisp
```

## `$coerce`
```
 ; VEQ:$COERCE
 ;   [symbol]
 ; 
 ; $COERCE names a macro:
 ;   Lambda-list: (TYPE A)
 ;   Documentation:
 ;     coerce sequence a to vec of type (eg: veq:ff, veq:df)
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `$copy`
```
:missing:

 ; VEQ:$COPY
 ;   [symbol]
`
```

## `$make`
```
 ; VEQ:$MAKE
 ;   [symbol]
 ; 
 ; $MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) V (TYPE T))
 ;   Documentation:
 ;     create vector array with size (n dim), and initial value v.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `$nvset`
```
 ; VEQ:$NVSET
 ;   [symbol]
 ; 
 ; $NVSET names a macro:
 ;   Lambda-list: ((A N &OPTIONAL (I 0)) &BODY BODY)
 ;   Documentation:
 ;     set n indices in a, from a[i] with n values. body must yield n values
 ;   Source file: /data/x/veq/src/vset.lisp
```

## `$print`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `$rowset`
```
 ; VEQ:$ROWSET
 ;   [symbol]
 ; 
 ; $ROWSET names a macro:
 ;   Lambda-list: ((A N &OPTIONAL (I 0)) &BODY BODY)
 ;   Documentation:
 ;     performs (setf (aref a i) row0 (aref a (1+ i) r1 ...))
 ;     n must be less than or equal to (length row)
 ;   Source file: /data/x/veq/src/vset.lisp
```

## `$to-list`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `*eps*`
```
:missing:

 ; VEQ:*EPS*
 ;   [symbol]
 ; 
 ; *EPS* names a special variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 5.960465e-8
`
```

## `2$`
```
 ; VEQ:2$
 ;   [symbol]
 ; 
 ; 2$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: /data/x/veq/src/array-utils.lisp
 ; 
 ; (SETF 2$) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: (2$ a i) yields (values ...)
 ;     set: (setf (2$ a i) (values ...))
 ;   Source file: /data/x/veq/src/vset.lisp
```

## `2$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `2$print`
```
 ; VEQ:2$PRINT
 ;   [symbol]
 ; 
 ; 2$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY (N 16) (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 2d array. returns array.
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `2$to-list`
```
 ; VEQ:2$TO-LIST
 ;   [symbol]
 ; 
 ; 2$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of lists of length 2.
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `3$`
```
 ; VEQ:3$
 ;   [symbol]
 ; 
 ; 3$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: /data/x/veq/src/array-utils.lisp
 ; 
 ; (SETF 3$) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: (3$ a i) yields (values ...)
 ;     set: (setf (3$ a i) (values ...))
 ;   Source file: /data/x/veq/src/vset.lisp
```

## `3$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `3$print`
```
 ; VEQ:3$PRINT
 ;   [symbol]
 ; 
 ; 3$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY (N 16) (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 3d array. returns array.
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `3$to-list`
```
 ; VEQ:3$TO-LIST
 ;   [symbol]
 ; 
 ; 3$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of lists of length 3.
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `4$`
```
 ; VEQ:4$
 ;   [symbol]
 ; 
 ; 4$ names a macro:
 ;   Lambda-list: (A &REST REST)
 ;   Source file: /data/x/veq/src/array-utils.lisp
 ; 
 ; (SETF 4$) has a complex setf-expansion:
 ;   Lambda-list: (A0 &OPTIONAL (I1 0))
 ;   Documentation:
 ;     get: (4$ a i) yields (values ...)
 ;     set: (setf (4$ a i) (values ...))
 ;   Source file: /data/x/veq/src/vset.lisp
```

## `4$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `4$print`
```
 ; VEQ:4$PRINT
 ;   [symbol]
 ; 
 ; 4$PRINT names a compiled function:
 ;   Lambda-list: (A &KEY (N 16) (S T))
 ;   Derived type: (FUNCTION (T &KEY (:N T) (:S T)) *)
 ;   Documentation:
 ;     pretty print 4d array. returns array.
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `4$to-list`
```
 ; VEQ:4$TO-LIST
 ;   [symbol]
 ; 
 ; 4$TO-LIST names a compiled function:
 ;   Lambda-list: (A)
 ;   Derived type: (FUNCTION (T) *)
 ;   Documentation:
 ;     return array as a list of lists of length 4.
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `arrtype`
```
 ; VEQ:ARRTYPE
 ;   [symbol]
 ; 
 ; ARRTYPE names a compiled function:
 ;   Lambda-list: (TY &OPTIONAL (MISSING NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     select array type from type hint. eg: :ff :df 'f 'i
 ;   Source file: /data/x/veq/src/types.lisp
```

## `context?`
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
 ;   Source file: /data/x/veq/src/docs.lisp
```

## `d$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$_`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$coerce`
```
 ; VEQ:D$COERCE
 ;   [symbol]
 ; 
 ; D$COERCE names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SEQUENCE)
 ;                  (VALUES (SIMPLE-ARRAY DOUBLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     coerce sequence to DVEC.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$copy`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `d$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `d$make`
```
 ; VEQ:D$MAKE
 ;   [symbol]
 ; 
 ; D$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0d0))
 ;   Documentation:
 ;     create DVEC vector array with size n * dim, and initial value v.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$mima`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `d$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d$~`
```
 ; VEQ:D$~
 ;   [symbol]
 ; 
 ; D$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create DVEC vector array from n values in body.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d2$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d2$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `d2$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d2$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `d2$mima`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `d2$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d2$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d2$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d2$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d2$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d2meye`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2minv`
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
```

## `d2mm`
```
 ; VEQ:D2MM
 ;   [symbol]
 ; 
 ; D2MM names a macro:
 ;   Lambda-list: (A*385 B*387)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mmt`
```
 ; VEQ:D2MMT
 ;   [symbol]
 ; 
 ; D2MMT names a macro:
 ;   Lambda-list: (A*449 B*451)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mrot`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mrot*`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mscale`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mt!`
```
 ; VEQ:D2MT!
 ;   [symbol]
 ; 
 ; D2MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 2d matrix in-place.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mtm`
```
 ; VEQ:D2MTM
 ;   [symbol]
 ; 
 ; D2MTM names a macro:
 ;   Lambda-list: (A*481 B*483)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mtmt`
```
 ; VEQ:D2MTMT
 ;   [symbol]
 ; 
 ; D2MTMT names a macro:
 ;   Lambda-list: (A*417 B*419)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mtrans`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mtv`
```
 ; VEQ:D2MTV
 ;   [symbol]
 ; 
 ; D2MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 2d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d2mv`
```
 ; VEQ:D2MV
 ;   [symbol]
 ; 
 ; D2MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 2d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d3$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `d3$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d3$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `d3$mima`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `d3$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d3$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d3$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d3$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d3$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d3meye`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3minv`
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
```

## `d3mm`
```
 ; VEQ:D3MM
 ;   [symbol]
 ; 
 ; D3MM names a macro:
 ;   Lambda-list: (A*513 B*515)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mmt`
```
 ; VEQ:D3MMT
 ;   [symbol]
 ; 
 ; D3MMT names a macro:
 ;   Lambda-list: (A*577 B*579)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mrot`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mrot*`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mscale`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mt!`
```
 ; VEQ:D3MT!
 ;   [symbol]
 ; 
 ; D3MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 3d matrix in-place.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mtm`
```
 ; VEQ:D3MTM
 ;   [symbol]
 ; 
 ; D3MTM names a macro:
 ;   Lambda-list: (A*609 B*611)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mtmt`
```
 ; VEQ:D3MTMT
 ;   [symbol]
 ; 
 ; D3MTMT names a macro:
 ;   Lambda-list: (A*545 B*547)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mtrans`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mtv`
```
 ; VEQ:D3MTV
 ;   [symbol]
 ; 
 ; D3MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 3d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d3mv`
```
 ; VEQ:D3MV
 ;   [symbol]
 ; 
 ; D3MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 3d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d4$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `d4$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d4$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `d4$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d4$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d4$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `d4$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d4$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `d4meye`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4minv`
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
```

## `d4mm`
```
 ; VEQ:D4MM
 ;   [symbol]
 ; 
 ; D4MM names a macro:
 ;   Lambda-list: (A*641 B*643)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4mmt`
```
 ; VEQ:D4MMT
 ;   [symbol]
 ; 
 ; D4MMT names a macro:
 ;   Lambda-list: (A*705 B*707)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4mt!`
```
 ; VEQ:D4MT!
 ;   [symbol]
 ; 
 ; D4MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 4d matrix in-place.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4mtm`
```
 ; VEQ:D4MTM
 ;   [symbol]
 ; 
 ; D4MTM names a macro:
 ;   Lambda-list: (A*737 B*739)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4mtmt`
```
 ; VEQ:D4MTMT
 ;   [symbol]
 ; 
 ; D4MTMT names a macro:
 ;   Lambda-list: (A*673 B*675)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: DVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4mtv`
```
 ; VEQ:D4MTV
 ;   [symbol]
 ; 
 ; D4MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 4d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d4mv`
```
 ; VEQ:D4MV
 ;   [symbol]
 ; 
 ; D4MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 4d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `d?`
```
 ; VEQ:D?
 ;   [symbol]
 ; 
 ; D? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     describe argument
 ;   Source file: /data/x/veq/src/config.lisp
```

## `d_`
```
 ; VEQ:D_
 ;   [symbol]
 ; 
 ; D_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create DVEC vector array from body: (D_ '(a b c ...)).
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `dalpha`
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
 ;   Source file: /data/x/veq/src/extra.lisp
```

## `dease-in-back`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-circ`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-cubic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-elastic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-exp`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-linear`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-back`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-circ`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-cubic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-elastic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-exp`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-linear`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-quart`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-quint`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-out-sin`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-quart`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-quint`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-in-sin`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-back`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-circ`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-cubic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-elastic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-exp`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-linear`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-quart`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-quint`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `dease-out-sin`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `def*`
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
 ;   Source file: /data/x/veq/src/macrolets.lisp
```

## `deps=`
```
:missing:

 ; VEQ:DEPS=
 ;   [symbol]
`
```

## `df`
```
:missing:

 ; VEQ:DF
 ;   [symbol]
 ; 
 ; DF names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES DOUBLE-FLOAT &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
 ; 
 ; DF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: DOUBLE-FLOAT
`
```

## `df*`
```
 ; VEQ:DF*
 ;   [symbol]
 ; 
 ; DF* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     coerce these values to df.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `dfl`
```
 ; VEQ:DFL
 ;   [symbol]
 ; 
 ; DFL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (df a) (df b) ...) from list.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
```

## `dpi`
```
:missing:

 ; VEQ:DPI
 ;   [symbol]
 ; 
 ; DPI names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 3.141592653589793d0
`
```

## `dpi25`
```
:missing:

 ; VEQ:DPI25
 ;   [symbol]
 ; 
 ; DPI25 names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 0.7853981633974483d0
`
```

## `dpi5`
```
:missing:

 ; VEQ:DPI5
 ;   [symbol]
 ; 
 ; DPI5 names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 1.5707963267948966d0
`
```

## `dpii`
```
:missing:

 ; VEQ:DPII
 ;   [symbol]
 ; 
 ; DPII names a constant variable:
 ;   Declared type: DOUBLE-FLOAT
 ;   Value: 6.283185307179586d0
`
```

## `dsb`
```
:missing:

 ; VEQ:DSB
 ;   [symbol]
 ; 
 ; DSB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/veq/src/generic-utils.lisp
`
```

## `dsel`
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
 ;   Source file: /data/x/veq/src/select-dim.lisp
```

## `dvec`
```
:missing:

 ; VEQ:DVEC
 ;   [symbol]
 ; 
 ; DVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY DF *)
`
```

## `dvlet`
```
:missing:

 ; VEQ:DVLET
 ;   [symbol]
`
```

## `ext-symbols?`
```
 ; VEQ:EXT-SYMBOLS?
 ;   [symbol]
 ; 
 ; EXT-SYMBOLS? names a macro:
 ;   Lambda-list: (PKG &OPTIONAL MODE (FLTFX (QUOTE AND)))
 ;   Documentation:
 ;     list all external symbols in pkg. use :verbose to inlcude docstring.
 ;     use :pretty to print verbose output to stdout in a readable form.
 ;   Source file: /data/x/veq/src/docs.lisp
```

## `f$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$_`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$coerce`
```
 ; VEQ:F$COERCE
 ;   [symbol]
 ; 
 ; F$COERCE names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SEQUENCE)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (*)) &OPTIONAL))
 ;   Documentation:
 ;     coerce sequence to FVEC.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$copy`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `f$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `f$make`
```
 ; VEQ:F$MAKE
 ;   [symbol]
 ; 
 ; F$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0.0))
 ;   Documentation:
 ;     create FVEC vector array with size n * dim, and initial value v.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$mima`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `f$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f$~`
```
 ; VEQ:F$~
 ;   [symbol]
 ; 
 ; F$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create FVEC vector array from n values in body.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f2$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f2$center`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f2$circ`
```
 ; VEQ:F2$CIRC
 ;   [symbol]
 ; 
 ; F2$CIRC names a compiled function:
 ;   Lambda-list: (RAD &OPTIONAL (RS 0.5))
 ;   Derived type: (FUNCTION (SINGLE-FLOAT &OPTIONAL (SINGLE-FLOAT 0.0)) *)
 ;   Documentation:
 ;     return circle of size rad. (rs 0.5) is vertex density.
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f2$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `f2$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f2$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `f2$mima`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `f2$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f2$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f2$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f2$polygon`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f2$rect`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f2$square*`
```
:missing:

 ; VEQ:F2$SQUARE*
 ;   [symbol]
 ; 
 ; F2$SQUARE* names a compiled function:
 ;   Lambda-list: (S)
 ;   Derived type: (FUNCTION (SINGLE-FLOAT)
 ;                  (VALUES (SIMPLE-ARRAY SINGLE-FLOAT (8)) &OPTIONAL))
 ;   Source file: /data/x/veq/src/shapes.lisp
`
```

## `f2$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f2$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f2in-bbox`
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

## `f2in-concave`
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

## `f2in-triangle`
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

## `f2lsegx`
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
 ;   Source file: /data/x/veq/src/checks-sweep.lisp
```

## `f2meye`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2minv`
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
```

## `f2mm`
```
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

## `f2mmt`
```
 ; VEQ:F2MMT
 ;   [symbol]
 ; 
 ; F2MMT names a macro:
 ;   Lambda-list: (A*65 B*67)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mrot`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mrot*`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mscale`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mt!`
```
 ; VEQ:F2MT!
 ;   [symbol]
 ; 
 ; F2MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 2d matrix in-place.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mtm`
```
 ; VEQ:F2MTM
 ;   [symbol]
 ; 
 ; F2MTM names a macro:
 ;   Lambda-list: (A*97 B*99)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mtmt`
```
 ; VEQ:F2MTMT
 ;   [symbol]
 ; 
 ; F2MTMT names a macro:
 ;   Lambda-list: (A*33 B*35)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mtrans`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mtv`
```
 ; VEQ:F2MTV
 ;   [symbol]
 ; 
 ; F2MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 2d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2mv`
```
 ; VEQ:F2MV
 ;   [symbol]
 ; 
 ; F2MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 2d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f2segdst`
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

## `f2segx`
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

## `f2ssegx`
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
 ;   Source file: /data/x/veq/src/checks-sweep.lisp
```

## `f3$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f3$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `f3$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f3$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `f3$mima`
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
 ;   Source file: /data/x/veq/src/array-extra.lisp
```

## `f3$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f3$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f3$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f3$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f3$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f3meye`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3minv`
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
```

## `f3mm`
```
 ; VEQ:F3MM
 ;   [symbol]
 ; 
 ; F3MM names a macro:
 ;   Lambda-list: (A*129 B*131)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mmt`
```
 ; VEQ:F3MMT
 ;   [symbol]
 ; 
 ; F3MMT names a macro:
 ;   Lambda-list: (A*193 B*195)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mrot`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mrot*`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mscale`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mt!`
```
 ; VEQ:F3MT!
 ;   [symbol]
 ; 
 ; F3MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 3d matrix in-place.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mtm`
```
 ; VEQ:F3MTM
 ;   [symbol]
 ; 
 ; F3MTM names a macro:
 ;   Lambda-list: (A*225 B*227)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mtmt`
```
 ; VEQ:F3MTMT
 ;   [symbol]
 ; 
 ; F3MTMT names a macro:
 ;   Lambda-list: (A*161 B*163)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mtrans`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mtv`
```
 ; VEQ:F3MTV
 ;   [symbol]
 ; 
 ; F3MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 3d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3mv`
```
 ; VEQ:F3MV
 ;   [symbol]
 ; 
 ; F3MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 3d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f3planex`
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
 ;   Source file: /data/x/veq/src/checks.lisp
```

## `f4$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f4$last`
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
 ;   Source file: /data/x/veq/src/array-rows.lisp
```

## `f4$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f4$lspace`
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
 ;   Source file: /data/x/veq/src/fxlspace.lisp
```

## `f4$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f4$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f4$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `f4$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f4$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `f4meye`
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
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f4minv`
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
 ;   Source file: /data/x/veq/src/mat-inv.lisp
```

## `f4mm`
```
 ; VEQ:F4MM
 ;   [symbol]
 ; 
 ; F4MM names a macro:
 ;   Lambda-list: (A*257 B*259)
 ;   Documentation:
 ;     multiply mat * mat
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f4mmt`
```
 ; VEQ:F4MMT
 ;   [symbol]
 ; 
 ; F4MMT names a macro:
 ;   Lambda-list: (A*321 B*323)
 ;   Documentation:
 ;     multiply mat * (transpose mat)
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f4mt!`
```
 ; VEQ:F4MT!
 ;   [symbol]
 ; 
 ; F4MT! names a macro:
 ;   Lambda-list: (A1)
 ;   Documentation:
 ;     transpose 4d matrix in-place.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f4mtm`
```
 ; VEQ:F4MTM
 ;   [symbol]
 ; 
 ; F4MTM names a macro:
 ;   Lambda-list: (A*353 B*355)
 ;   Documentation:
 ;     multiply (transpose mat) * mat
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f4mtmt`
```
 ; VEQ:F4MTMT
 ;   [symbol]
 ; 
 ; F4MTMT names a macro:
 ;   Lambda-list: (A*289 B*291)
 ;   Documentation:
 ;     multiply (transpose mat) * (transpose mat)
 ;     of type: FVEC
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f4mtv`
```
 ; VEQ:F4MTV
 ;   [symbol]
 ; 
 ; F4MTV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     transpose(mat) * v. for 4d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f4mv`
```
 ; VEQ:F4MV
 ;   [symbol]
 ; 
 ; F4MV names a macro:
 ;   Lambda-list: (M1 &REST V2)
 ;   Documentation:
 ;     mat * v. for 4d matrix and vector.
 ;   Source file: /data/x/veq/src/mat.lisp
```

## `f_`
```
 ; VEQ:F_
 ;   [symbol]
 ; 
 ; F_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create FVEC vector array from body: (F_ '(a b c ...)).
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `falpha`
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
 ;   Source file: /data/x/veq/src/extra.lisp
```

## `fease-in-back`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-circ`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-cubic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-elastic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-exp`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-linear`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-back`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-circ`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-cubic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-elastic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-exp`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-linear`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-quart`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-quint`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-out-sin`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-quart`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-quint`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-in-sin`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-back`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-circ`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-cubic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-elastic`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-exp`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-linear`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-quart`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-quint`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `fease-out-sin`
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
 ;   Source file: /data/x/veq/src/easing.lisp
```

## `feps=`
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
 ;   Source file: /data/x/veq/src/extra.lisp
```

## `ff`
```
:missing:

 ; VEQ:FF
 ;   [symbol]
 ; 
 ; FF names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES SINGLE-FLOAT &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
 ; 
 ; FF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: SINGLE-FLOAT
`
```

## `ff*`
```
 ; VEQ:FF*
 ;   [symbol]
 ; 
 ; FF* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     coerce these values to ff.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `ffl`
```
 ; VEQ:FFL
 ;   [symbol]
 ; 
 ; FFL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (ff a) (ff b) ...) from list.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
```

## `fmake-ortho-proj-matrix`
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
 ;   Source file: /data/x/veq/src/mat-cam.lisp
```

## `fmake-proj-matrix`
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
 ;   Source file: /data/x/veq/src/mat-cam.lisp
```

## `fmake-view-matrix`
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
 ;   Source file: /data/x/veq/src/mat-cam.lisp
```

## `fpi`
```
:missing:

 ; VEQ:FPI
 ;   [symbol]
 ; 
 ; FPI names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 3.1415927
`
```

## `fpi25`
```
:missing:

 ; VEQ:FPI25
 ;   [symbol]
 ; 
 ; FPI25 names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 0.7853982
`
```

## `fpi5`
```
:missing:

 ; VEQ:FPI5
 ;   [symbol]
 ; 
 ; FPI5 names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 1.5707964
`
```

## `fpii`
```
:missing:

 ; VEQ:FPII
 ;   [symbol]
 ; 
 ; FPII names a constant variable:
 ;   Declared type: SINGLE-FLOAT
 ;   Value: 6.2831855
`
```

## `from-lst`
```
 ; VEQ:FROM-LST
 ;   [symbol]
 ; 
 ; FROM-LST names a macro:
 ;   Lambda-list: (L)
 ;   Documentation:
 ;     return list as values. equivalent to (values-list ...).
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `fsel`
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
 ;   Source file: /data/x/veq/src/select-dim.lisp
```

## `fvdef`
```
 ; VEQ:FVDEF
 ;   [symbol]
 ; 
 ; FVDEF names a macro:
 ;   Lambda-list: (FNAME &BODY BODY)
 ;   Documentation:
 ;     define function with veq context enabled. see fvprogn.
 ;   Source file: /data/x/veq/src/macrolets.lisp
```

## `fvdef*`
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
 ;   Source file: /data/x/veq/src/macrolets.lisp
```

## `fvec`
```
:missing:

 ; VEQ:FVEC
 ;   [symbol]
 ; 
 ; FVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY FF *)
`
```

## `fvlet`
```
:missing:

 ; VEQ:FVLET
 ;   [symbol]
`
```

## `fvprogn`
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
 ;   Source file: /data/x/veq/src/macrolets.lisp
```

## `get-arg-key`
```
 ; VEQ:GET-ARG-KEY
 ;   [symbol]
 ; 
 ; GET-ARG-KEY names a compiled function:
 ;   Lambda-list: (LL K &OPTIONAL D)
 ;   Derived type: (FUNCTION (T T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get the value of keyword k in ll where ll is a list of kw function args.
 ;   Source file: /data/x/veq/src/generic-utils.lisp
```

## `group`
```
 ; VEQ:GROUP
 ;   [symbol]
 ; 
 ; GROUP names a compiled function:
 ;   Lambda-list: (L N)
 ;   Derived type: (FUNCTION (LIST FIXNUM) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     group l into lists of n elements. see ungroup.
 ;   Source file: /data/x/veq/src/generic-utils.lisp
```

## `i$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$_`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$coerce`
```
 ; VEQ:I$COERCE
 ;   [symbol]
 ; 
 ; I$COERCE names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SEQUENCE)
 ;                  (VALUES (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)) &OPTIONAL))
 ;   Documentation:
 ;     coerce sequence to IVEC.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$copy`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i$make`
```
 ; VEQ:I$MAKE
 ;   [symbol]
 ; 
 ; I$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0))
 ;   Documentation:
 ;     create IVEC vector array with size n * dim, and initial value v.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i$~`
```
 ; VEQ:I$~
 ;   [symbol]
 ; 
 ; I$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create IVEC vector array from n values in body.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i2$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i2$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i2$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i2$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i2$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i2$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i2$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i3$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i3$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i3$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i3$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i3$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i3$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i3$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i4$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i4$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i4$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i4$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i4$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `i4$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i4$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `i?`
```
 ; VEQ:I?
 ;   [symbol]
 ; 
 ; I? names a compiled function:
 ;   Lambda-list: (F)
 ;   Derived type: (FUNCTION (T) (VALUES &OPTIONAL))
 ;   Documentation:
 ;     inspect argument
 ;   Source file: /data/x/veq/src/config.lisp
```

## `i_`
```
 ; VEQ:I_
 ;   [symbol]
 ; 
 ; I_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create IVEC vector array from body: (I_ '(a b c ...)).
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `in`
```
:missing:

 ; VEQ:IN
 ;   [symbol]
 ; 
 ; IN names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES (SIGNED-BYTE 32) &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
 ; 
 ; IN names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 32))
 ;   Expansion: (SIGNED-BYTE 32)
`
```

## `in*`
```
 ; VEQ:IN*
 ;   [symbol]
 ; 
 ; IN* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     coerce these values to in.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `inl`
```
 ; VEQ:INL
 ;   [symbol]
 ; 
 ; INL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (in a) (in b) ...) from list.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
```

## `isel`
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
 ;   Source file: /data/x/veq/src/select-dim.lisp
```

## `ivec`
```
:missing:

 ; VEQ:IVEC
 ;   [symbol]
 ; 
 ; IVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY IN *)
`
```

## `ivlet`
```
:missing:

 ; VEQ:IVLET
 ;   [symbol]
`
```

## `kv`
```
:missing:

 ; VEQ:KV
 ;   [symbol]
 ; 
 ; KV names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES SYMBOL &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
 ; 
 ; KV names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: KEYWORD
`
```

## `kv*`
```
 ; VEQ:KV*
 ;   [symbol]
 ; 
 ; KV* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     coerce these values to kv.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `kvec`
```
:missing:

 ; VEQ:KVEC
 ;   [symbol]
 ; 
 ; KVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY KV *)
`
```

## `kvl`
```
 ; VEQ:KVL
 ;   [symbol]
 ; 
 ; KVL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (kv a) (kv b) ...) from list.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
```

## `ll`
```
:missing:

 ; VEQ:LL
 ;   [symbol]
 ; 
 ; LL names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES LIST &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
 ; 
 ; LL names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: LIST
`
```

## `ll*`
```
 ; VEQ:LL*
 ;   [symbol]
 ; 
 ; LL* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     coerce these values to ll.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `lll`
```
 ; VEQ:LLL
 ;   [symbol]
 ; 
 ; LLL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (ll a) (ll b) ...) from list.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
```

## `lpos`
```
 ; VEQ:LPOS
 ;   [symbol]
 ; 
 ; LPOS names a macro:
 ;   Lambda-list: (L &OPTIONAL (I 0) J)
 ;   Documentation:
 ;     get list of index i or subseq i j from list of lists.
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `lst`
```
 ; VEQ:LST
 ;   [symbol]
 ; 
 ; LST names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     get all (values ... ) in body as a list.
 ;     almost like multiple-value-list, except it handles multiple arguments.
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `lvec`
```
:missing:

 ; VEQ:LVEC
 ;   [symbol]
 ; 
 ; LVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY LL *)
`
```

## `mac`
```
 ; VEQ:MAC
 ;   [symbol]
 ; 
 ; MAC names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     expand macro.
 ;   Source file: /data/x/veq/src/generic-utils.lisp
```

## `mac*`
```
 ; VEQ:MAC*
 ;   [symbol]
 ; 
 ; MAC* names a macro:
 ;   Lambda-list: (EXPR)
 ;   Documentation:
 ;     expand macro all. only in SBCL.
 ;   Source file: /data/x/veq/src/generic-utils.lisp
```

## `mutate!`
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `mvb`
```
:missing:

 ; VEQ:MVB
 ;   [symbol]
 ; 
 ; MVB names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/veq/src/generic-utils.lisp
`
```

## `mvc`
```
:missing:

 ; VEQ:MVC
 ;   [symbol]
 ; 
 ; MVC names a macro:
 ;   Lambda-list: (&REST ARGS)
 ;   Source file: /data/x/veq/src/generic-utils.lisp
`
```

## `mvcgrp`
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `mvcmap`
```
 ; VEQ:MVCMAP
 ;   [symbol]
 ; 
 ; MVCMAP names a macro:
 ;   Lambda-list: ((DIM FX) &BODY BODY)
 ;   Documentation:
 ;     returns (values (fx i) (fx j) ...) for dim values from body.
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `mvcwrap`
```
:missing:

 ; VEQ:MVCWRAP
 ;   [symbol]
`
```

## `new-stride`
```
 ; VEQ:NEW-STRIDE
 ;   [symbol]
 ; 
 ; NEW-STRIDE names a macro:
 ;   Lambda-list: ((FROM TO TYPE &OPTIONAL (V 0)) ARR)
 ;   Documentation:
 ;     shift arr from stride to stride.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$_`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$coerce`
```
 ; VEQ:P$COERCE
 ;   [symbol]
 ; 
 ; P$COERCE names a compiled function:
 ;   Lambda-list: (A0)
 ;   Derived type: (FUNCTION (SEQUENCE)
 ;                  (VALUES (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*))
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     coerce sequence to PVEC.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$copy`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p$make`
```
 ; VEQ:P$MAKE
 ;   [symbol]
 ; 
 ; P$MAKE names a macro:
 ;   Lambda-list: (&KEY (DIM 1) (N 1) (V 0))
 ;   Documentation:
 ;     create PVEC vector array with size n * dim, and initial value v.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p$~`
```
 ; VEQ:P$~
 ;   [symbol]
 ; 
 ; P$~ names a macro:
 ;   Lambda-list: ((&OPTIONAL (N 1)) &BODY BODY)
 ;   Documentation:
 ;     create PVEC vector array from n values in body.
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p2$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p2$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p2$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p2$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p2$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p2$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p2$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p3$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p3$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p3$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p3$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p3$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p3$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p3$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p4$`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p4$line`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p4$num`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p4$one`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p4$point`
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
 ;   Source file: /data/x/veq/src/shapes.lisp
```

## `p4$val`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p4$zero`
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
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `p_`
```
 ; VEQ:P_
 ;   [symbol]
 ; 
 ; P_ names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     create PVEC vector array from body: (P_ '(a b c ...)).
 ;   Source file: /data/x/veq/src/array-utils.lisp
```

## `pn`
```
:missing:

 ; VEQ:PN
 ;   [symbol]
 ; 
 ; PN names a compiled function:
 ;   Lambda-list: (V)
 ;   Derived type: (FUNCTION (T) (VALUES (UNSIGNED-BYTE 32) &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
 ; 
 ; PN names a type-specifier:
 ;   Lambda-list: (&OPTIONAL (BITS 32))
 ;   Expansion: (UNSIGNED-BYTE 32)
`
```

## `pn*`
```
 ; VEQ:PN*
 ;   [symbol]
 ; 
 ; PN* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     coerce these values to pn.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `pnl`
```
 ; VEQ:PNL
 ;   [symbol]
 ; 
 ; PNL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (pn a) (pn b) ...) from list.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
```

## `pos-df`
```
:missing:

 ; VEQ:POS-DF
 ;   [symbol]
 ; 
 ; POS-DF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (DOUBLE-FLOAT 0.0d0 *)
`
```

## `pos-ff`
```
:missing:

 ; VEQ:POS-FF
 ;   [symbol]
 ; 
 ; POS-FF names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: (SINGLE-FLOAT 0.0 *)
`
```

## `proc-vv`
```
:missing:

 ; VEQ:PROC-VV
 ;   [symbol]
 ; 
 ; PROC-VV names a compiled function:
 ;   Lambda-list: (BODY)
 ;   Derived type: (FUNCTION (T) (VALUES T &OPTIONAL))
 ;   Source file: /data/x/veq/src/ops-vv.lisp
`
```

## `psel`
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
 ;   Source file: /data/x/veq/src/select-dim.lisp
```

## `pvec`
```
:missing:

 ; VEQ:PVEC
 ;   [symbol]
 ; 
 ; PVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY PN *)
`
```

## `pvlet`
```
:missing:

 ; VEQ:PVLET
 ;   [symbol]
`
```

## `replace-varg`
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
 ;   Source file: /data/x/veq/src/macros-helpers.lisp
```

## `strip-arg-keys`
```
 ; VEQ:STRIP-ARG-KEYS
 ;   [symbol]
 ; 
 ; STRIP-ARG-KEYS names a compiled function:
 ;   Lambda-list: (LL KK &AUX (LL (GROUP LL 2)))
 ;   Derived type: (FUNCTION (T T) *)
 ;   Documentation:
 ;     strip keywords in kk from ll where ll is a list of kw function args.
 ;   Source file: /data/x/veq/src/generic-utils.lisp
```

## `svec`
```
:missing:

 ; VEQ:SVEC
 ;   [symbol]
 ; 
 ; SVEC names a type-specifier:
 ;   Lambda-list: (&OPTIONAL N)
 ;   Expansion: (SIMPLE-ARRAY SY *)
`
```

## `sy`
```
:missing:

 ; VEQ:SY
 ;   [symbol]
 ; 
 ; SY names a compiled function:
 ;   Lambda-list: (V &OPTIONAL (PKG (QUOTE CL-USER)))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES SYMBOL &OPTIONAL))
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
 ; 
 ; SY names a type-specifier:
 ;   Lambda-list: ()
 ;   Expansion: SYMBOL
`
```

## `sy*`
```
 ; VEQ:SY*
 ;   [symbol]
 ; 
 ; SY* names a macro:
 ;   Lambda-list: (&BODY BODY)
 ;   Documentation:
 ;     coerce these values to sy.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `syl`
```
 ; VEQ:SYL
 ;   [symbol]
 ; 
 ; SYL names a compiled function:
 ;   Lambda-list: (L)
 ;   Derived type: (FUNCTION (LIST) *)
 ;   Documentation:
 ;     return (values (sy a) (sy b) ...) from list.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /data/x/veq/src/types.lisp
```

## `type-default`
```
 ; VEQ:TYPE-DEFAULT
 ;   [symbol]
 ; 
 ; TYPE-DEFAULT names a compiled function:
 ;   Lambda-list: (TY &OPTIONAL (MISSING NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     default value for array with elements of type (hint) ty. eg: 0 0f0 0d0 nil :val
 ;   Source file: /data/x/veq/src/types.lisp
```

## `type-from-short`
```
 ; VEQ:TYPE-FROM-SHORT
 ;   [symbol]
 ; 
 ; TYPE-FROM-SHORT names a compiled function:
 ;   Lambda-list: (TY &OPTIONAL (MISSING NIL))
 ;   Derived type: (FUNCTION (T &OPTIONAL T) (VALUES T &OPTIONAL T))
 ;   Documentation:
 ;     select type fom type hint.
 ;   Source file: /data/x/veq/src/types.lisp
```

## `ungroup`
```
 ; VEQ:UNGROUP
 ;   [symbol]
 ; 
 ; UNGROUP names a compiled function:
 ;   Lambda-list: (L &AUX (RES (LIST)))
 ;   Derived type: (FUNCTION (LIST) (VALUES LIST &OPTIONAL))
 ;   Documentation:
 ;     inverse of group.
 ;   Source file: /data/x/veq/src/generic-utils.lisp
```

## `unpack-vvsym`
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
 ;   Source file: /data/x/veq/src/types.lisp
```

## `v?`
```
 ; VEQ:V?
 ;   [symbol]
 ; 
 ; V? names a compiled function:
 ;   Lambda-list: (&OPTIONAL (SILENT T))
 ;   Derived type: (FUNCTION (&OPTIONAL T) (VALUES T &OPTIONAL))
 ;   Documentation:
 ;     get version. use silent to surpress stdout
 ;   Source file: /data/x/veq/src/config.lisp
```

## `vchain`
```
 ; VEQ:VCHAIN
 ;   [symbol]
 ; 
 ; VCHAIN names a macro:
 ;   Lambda-list: (FXS &REST REST)
 ;   Documentation:
 ;     chain functions, on all values.
 ;     eg: (vchain #'a #'b (values 1 2)) equals: (mvc #'a (mvc #'b (values 1 2)))
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `vdef`
```
 ; VEQ:VDEF
 ;   [symbol]
 ; 
 ; VDEF names a macro:
 ;   Lambda-list: (FNAME &BODY BODY)
 ;   Documentation:
 ;     define function with veq context enabled. see vprogn.
 ;   Source file: /data/x/veq/src/macrolets.lisp
```

## `vdef*`
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
 ;   Source file: /data/x/veq/src/macrolets.lisp
```

## `vector-rearrange`
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `vlabels`
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
 ;   Source file: /data/x/veq/src/lets.lisp
```

## `vnrep`
```
 ; VEQ:VNREP
 ;   [symbol]
 ; 
 ; VNREP names a macro:
 ;   Lambda-list: (N &REST REST)
 ;   Documentation:
 ;     corresponds to (values [rest n times]). see vnval.
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `vnval`
```
 ; VEQ:VNVAL
 ;   [symbol]
 ; 
 ; VNVAL names a macro:
 ;   Lambda-list: (N &REST REST)
 ;   Documentation:
 ;     returns (values v ...), where v is (progn ,@rest) evaluated once. see vnrep.
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `vp`
```
 ; VEQ:VP
 ;   [symbol]
 ; 
 ; VP names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     print values and return values, return values.
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `vpr`
```
 ; VEQ:VPR
 ;   [symbol]
 ; 
 ; VPR names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     print input code with resulting values, return values.
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `vprogn`
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
 ;   Source file: /data/x/veq/src/macrolets.lisp
```

## `vsel`
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
 ;   Source file: /data/x/veq/src/select-dim.lisp
```

## `vvdb`
```
:missing:

 ; VEQ:VVDB
 ;   [symbol]
 ; 
 ; VVDB names a macro:
 ;   Lambda-list: ((&KEY (EXEC T) (S T)) &BODY BODY)
 ;   Source file: /data/x/veq/src/ops-vv.lisp
`
```

## `vvsym`
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
 ;   Source file: /data/x/veq/src/types.lisp
```

## `with-symbs`
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
 ;   Source file: /data/x/veq/src/utils.lisp
```

## `xlet`
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

## `~`
```
 ; VEQ:~
 ;   [symbol]
 ; 
 ; ~ names a macro:
 ;   Lambda-list: (&REST REST)
 ;   Documentation:
 ;     wraps rest in (mvc #'values ...).
 ;   Source file: /data/x/veq/src/utils.lisp
```

