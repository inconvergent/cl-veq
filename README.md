# VEQ

VEQ is a DSL and a set of utilities for doing dealing with (primarily)
1d/2d/3d/nd point vectors and arrays of point vectors. It includes some
geometry and graphics programming utilities like intersection tests and matrix
transforms.

VEQ was written to be the vector library used in my graph data structure,
[cl-grph](https://github.com/inconvergent/cl-grph), and utilities for
generative art: [auxin](https://github.com/inconvergent/auxin).


## Symbol Docs & Example

See [docs](/docs/00-intro.md) for documentation and symbols.

For more examples go to [examples](examples/ex.lisp).

```lisp
(in-package :veq)

; fvprogn is a compiler for the veq DSL, and and environment that cointains
; many of the vector tools. among other things it handles the !@ triggers for
; common nd point vector operations such as f2!@+ for 2d single float point
; vector addition, and so on

; here are some examples:

(fvprogn
  ; xlet makes it convenient to declare typed value packs ("veqs") with the
  ; trigger [t][d]![var], eg. f2!varname. the variable will the be replaced by
  ; the n values inside the scope.
  (xlet ((f2!a (f2 1 2))                                      ; (values 1f0 2f0)
         (f3!varname 0)                                       ; (values 0.0 0.0 0.0)
         (f2!b (f2 3 100))                                    ; (values 3f0 100f0)
         (line (f2$line 3f0 40f0 7f0 3f0))                    ; #(3f0 40f0 7f0 3f0)
         (xx :s)                                              ; just :s, as you expect
         (i3!integervar) (values 1 7 3)                       ; (values 1 7 3)
         (va (f$_ (loop for v from 0 below 6                  ; #(0.0 1.0 1.0
                        collect (list (ff v) (ff (1+ v))))))) ;   2.0 2.0 3.0
                                                              ;   3.0 4.0 4.0
                                                              ;   5.0 5.0 6.0)
    ; vp pretty prints input code and output values
    (vp (f2len (f2!@*. (f2!@+ a b) 0.1)))
    ; f2!@*. scales the vector by 0.1 by broadcasting [.] the value over the veq

    ; convenience function to print arrays of vectors:
    (2$print line) ; returns line

    (vp (f2!@$+ vb 3f0 100f0))

    ; only return vb from index to below 5
    (vp (f2!@$+ (?@ va 2 5) 3f0 100f0))

    (vp (f2dot (f2$ va) (f2$ line 1)))   ; dot product
    ; can also be written as:
    (f!@+ (f2!@* (f2$ va) (f2$ line 1))
    ; where (f2$ va) is the first veq in va, and (f2$ line 1) is the second
    ; veq in line

    (vp (f2cross (f2$ line 0 1))) ; cross product
    ; equivalent to:
    (vp (f2cross (f2$ line 0) (f2$ line 1)))

    (vp (f2!@$+ (f2$zero 3) 3f0 1f0))

    (vp (f21_@$f2len (f2!@$+ (f2$zero 3) 3f0 1f0)))

    (vp (f21_@$+ vb))

    ; TODO, improve example for new features
    )))
```

## Since v3.0.0 (Nov 2024)

`veq` now tentatively includes packages `rnd` and `srnd` for random number
generation. `rnd` uses native random number generator and `srnd` is for very
fast (low quality) random number generation with explicit state. Eg. for use in
threads for 3d rendering.

Both packages currently only support single floats. and they lack the `f` prefix
used in `veq`. This might change in the future.

See [rnd](/docs/rnd.md) and [srnd](/docs/srnd.md) in the documentation.

