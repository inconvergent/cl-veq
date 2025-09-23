## `2in-circ`
```
 ; SRND:2IN-CIRC
 ;   [symbol]
 ; 
 ; 2IN-CIRC names a compiled function:
 ;   Lambda-list: (RS R)
 ;   Derived type: (FUNCTION (SRND:SRND SINGLE-FLOAT)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     random point in circle.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `2in-circ+`
```
:missing:

 ; SRND:2IN-CIRC+
 ;   [symbol]
 ; 
 ; 2IN-CIRC+ names a macro:
 ;   Lambda-list: (RS S &REST REST)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `2on-circ`
```
 ; SRND:2ON-CIRC
 ;   [symbol]
 ; 
 ; 2ON-CIRC names a compiled function:
 ;   Lambda-list: (RS R)
 ;   Derived type: (FUNCTION (SRND:SRND SINGLE-FLOAT)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT &OPTIONAL))
 ;   Documentation:
 ;     get random point on circle.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `2on-circ+`
```
:missing:

 ; SRND:2ON-CIRC+
 ;   [symbol]
 ; 
 ; 2ON-CIRC+ names a macro:
 ;   Lambda-list: (RS S &REST REST)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `3diffuse`
```
 ; SRND:3DIFFUSE
 ;   [symbol]
 ; 
 ; 3DIFFUSE names a compiled function:
 ;   Lambda-list: (RS NOISE HN/X-7 HN/Y-8 HN/Z-9)
 ;   Derived type: (FUNCTION
 ;                  (SRND:SRND SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                             SINGLE-FLOAT)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     offset hn with bias.
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `3in-hemi`
```
 ; SRND:3IN-HEMI
 ;   [symbol]
 ; 
 ; 3IN-HEMI names a compiled function:
 ;   Lambda-list: (RS DIR/X-7 DIR/Y-8 DIR/Z-9)
 ;   Derived type: (FUNCTION
 ;                  (SRND:SRND SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT)
 ;                  (VALUES (SINGLE-FLOAT -3.0 3.0)
 ;                          (SINGLE-FLOAT -3.0 3.0)
 ;                          (SINGLE-FLOAT -3.0 3.0) &OPTIONAL))
 ;   Documentation:
 ;     get random point in hemisphere where (dot dir res) is positive.
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `3in-sphere`
```
 ; SRND:3IN-SPHERE
 ;   [symbol]
 ; 
 ; 3IN-SPHERE names a compiled function:
 ;   Lambda-list: (RS R)
 ;   Derived type: (FUNCTION (SRND:SRND SINGLE-FLOAT)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get random point in sphere with rad r.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `3in-sphere+`
```
:missing:

 ; SRND:3IN-SPHERE+
 ;   [symbol]
 ; 
 ; 3IN-SPHERE+ names a macro:
 ;   Lambda-list: (RS S &REST REST)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `3on-hemi`
```
 ; SRND:3ON-HEMI
 ;   [symbol]
 ; 
 ; 3ON-HEMI names a compiled function:
 ;   Lambda-list: (RS DIR/X-7 DIR/Y-8 DIR/Z-9)
 ;   Derived type: (FUNCTION
 ;                  (SRND:SRND SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT)
 ;                  (VALUES (SINGLE-FLOAT -1.0 1.0)
 ;                          (SINGLE-FLOAT -1.0 1.0)
 ;                          (SINGLE-FLOAT -1.0 1.0) &OPTIONAL))
 ;   Documentation:
 ;     get random point on hemisphere where (dot dir res) is positive.
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `3on-sphere`
```
 ; SRND:3ON-SPHERE
 ;   [symbol]
 ; 
 ; 3ON-SPHERE names a compiled function:
 ;   Lambda-list: (RS R)
 ;   Derived type: (FUNCTION (SRND:SRND SINGLE-FLOAT)
 ;                  (VALUES SINGLE-FLOAT SINGLE-FLOAT SINGLE-FLOAT
 ;                          &OPTIONAL))
 ;   Documentation:
 ;     get random point on sphere.
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `3on-sphere+`
```
:missing:

 ; SRND:3ON-SPHERE+
 ;   [symbol]
 ; 
 ; 3ON-SPHERE+ names a macro:
 ;   Lambda-list: (RS S &REST REST)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `make`
```
 ; SRND:MAKE
 ;   [symbol]
 ; 
 ; MAKE names a compiled function:
 ;   Lambda-list: (RS)
 ;   Derived type: (FUNCTION (FIXNUM) (VALUES SRND:SRND &OPTIONAL))
 ;   Documentation:
 ;     make stateful rnd generator
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `prob`
```
 ; SRND:PROB
 ;   [symbol]
 ; 
 ; PROB names a macro:
 ;   Lambda-list: (RS P A &OPTIONAL B)
 ;   Documentation:
 ;     evaluate first form in body with probability p. second form (optional) is
 ;     executed with probability 1-p. ex: (prob 0.1 (print :a) (print :b))
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `rcond`
```
 ; SRND:RCOND
 ;   [symbol]
 ; 
 ; RCOND names a macro:
 ;   Lambda-list: (RS &REST CLAUSES)
 ;   Documentation:
 ;     executes the forms in clauses according to the probability of the weighted sum
 ;     ex: (rcond (0.1 (print :a)) (0.3 (print :b)) ...)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `rnd`
```
 ; SRND:RND
 ;   [symbol]
 ; 
 ; RND names a macro:
 ;   Lambda-list: (RS &OPTIONAL R)
 ;   Documentation:
 ;     get a random float [0.0 1.0] from state rs (scaled by r)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `rnd*`
```
 ; SRND:RND*
 ;   [symbol]
 ; 
 ; RND* names a macro:
 ;   Lambda-list: (RS &OPTIONAL R)
 ;   Documentation:
 ;     get a random float in range [-r r] from state rs. r defaults to 1.0
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `rndi`
```
 ; SRND:RNDI
 ;   [symbol]
 ; 
 ; RNDI names a compiled function:
 ;   Lambda-list: (RS N)
 ;   Derived type: (FUNCTION (SRND:SRND (UNSIGNED-BYTE 32))
 ;                  (VALUES (MOD 4294967295) &OPTIONAL))
 ;   Documentation:
 ;     get random integer in range [0 n).
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `rndrng`
```
 ; SRND:RNDRNG
 ;   [symbol]
 ; 
 ; RNDRNG names a macro:
 ;   Lambda-list: (RS A B)
 ;   Documentation:
 ;     get a random number in range [a b]
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
```

## `srnd`
```
 ; SRND:SRND
 ;   [symbol]
 ; 
 ; SRND names a compiled function:
 ;   Lambda-list: (RS)
 ;   Derived type: (FUNCTION (T) (VALUES SRND:SRND &OPTIONAL))
 ;   Documentation:
 ;     see make
 ;   Inline proclamation: INLINE (inline expansion available)
 ;   Source file: /home/anders/x/veq/src/rnd/srnd.lisp
 ; 
 ; SRND names the structure-class #<STRUCTURE-CLASS SRND:SRND>:
 ;   Class precedence-list: SRND:SRND, STRUCTURE-OBJECT,
 ;                          SB-PCL::SLOT-OBJECT, T
 ;   Direct superclasses: STRUCTURE-OBJECT
 ;   No subclasses.
 ;   Sealed.
 ;   Slots:
 ;     SRND::S
 ;       Type: VEQ:PN
 ;       Initform: 0
```


NIL 