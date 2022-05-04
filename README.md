# VEQ

VEQ is a set of convenience utilities for writing (1d/)2d/3d vector mathematics.
It suports single vectors or arrays of vectors, with some broadcasting and
reduction operations.

VEQ was written to be the vector library used in my generative art library
[weird](https://github.com/inconvergent/weird).

## Examples and Documentation

Here are some examples of use:

```lisp
(in-package :veq)

(vprogn
  ; single vectors
  (f2let ((a (f2 1f0 2f0))
          (b (f2 3f0 100f0)))
    ; prints the euclidian length of 2d vector (a+b)*0.1
    (print (f2len (f2scale (f2+ a b) 0.1f0))))

  ; arrays of vectors
  (let ((a (f2$point 3f0 3f0))
        (line (f2$line 3f0 40f0 7f0 3f0))
        (line* (f2$line 1f0 2f0 3f0 4f0))
        (b (f$_ (loop for v from 0 below 6
                            collect (list (ff v) (ff (1+ v)))))))

    ; convenience function to print arrays of vectors:
    (vpr (2$print line)) ; returns a

    (vpr (f2. (f2$ a 0) (f2$ line 1))) ; dot product
    ;> 30.0

    (vpr (f2cross (f2$ line 0 1))) ; cross product
    ;> -271.0
    ; equivalent to:
    (vpr (f2cross (f2$ line 0) (f2$ line 1)))

    (vpr (f2$+ (f2$zero 3) 3f0 1f0)
         #(3f0 1f0 3f0 1f0 3f0 1f0))

    (vpr (f2$len (f2$+ (f2$zero 3) 3f0 1f0))
        #(3.1622777 3.1622777 3.1622777))

    (vpr (f2$+ (f2$zero 3) (f2rep 3f0))
        #(3f0 3f0 3f0 3f0 3f0 3f0) )

    (vpr (f2$- (f2$zero 3) (f2 3f0 2f0))
        #(-3f0 -2f0 -3f0 -2f0 -3f0 -2f0))))
```

For more examples go to [examples](examples/ex.lisp).

You can also see some usagee in the [tests](test/veq.lisp).

See [docs](DOCS.md) for automatically generated symbol documentation.

## Versions, Issues and feature requests

The most recent stable version is v2.1.0.

This code is still immature, and the interface might change in the future.

To report issues, or suggest features use:
[Github](https://github.com/inconvergent/cl-veq).

