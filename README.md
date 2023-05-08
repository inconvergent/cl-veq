# VEQ

VEQ is a set of convenience utilities for writing (1d/)2d/3d vector mathematics.
It suports single vectors or arrays of vectors, with some broadcasting and
reduction operations.

VEQ was written to be the vector library used in my generative art library
[auxin](https://github.com/inconvergent/auxin), and in
[cl-grph](https://github.com/inconvergent/cl-grph).

## Examples and Documentation

Here are some examples of use:

```lisp
(in-package :veq)
(fvprogn
  (xlet ((f2!a (f2 1f0 2f0))
         (f2!b (f2 3f0 100f0))
         (line (f2$line 3f0 40f0 7f0 3f0))
         (va (f$_ (loop for v from 0 below 6
                        collect (list (ff v) (ff (1+ v)))))))

    ; veq:vp pretty prints input code and output values
    (vp (f2len (f2!@*. (f2+ a b) 0.1f0)))

    ; convenience function to print arrays of vectors:
    (2$print line) ; returns line

    (vp (f2!@$+ vb 3f0 100f0))

    ; only return vb from index to below 5
    (vp (f2!@$+ (?@ va 2 5) 3f0 100f0))

    (vp (f2. (f2$ va 0) (f2$ line 1))) ; dot product

    (vp (f2cross (f2$ line 0 1))) ; cross product
    ; equivalent to:
    (vp (f2cross (f2$ line 0) (f2$ line 1)))

    (vp (f2!@$+ (f2$zero 3) 3f0 1f0))

    (vp (f21_@$f2len (f2!@$+ (f2$zero 3) 3f0 1f0)))

    (vp (f21_@$+ vb))
    ; see documentation for vv macro for more info about !@, _@, .@ compiler
    ; triggers

   ; TODO, improve example for new features
    ))
```

For more examples go to [examples](examples/ex.lisp).

You can also see some usagee in the [tests](test/veq.lisp).

See [docs](DOCS.md) for automatically generated symbol documentation.

