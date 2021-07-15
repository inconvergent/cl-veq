# VEQ

TODO: rewrite this


1. 2d and 3d vectors with corresponding operations:
   ```lisp
   ; create a 2d vector
   (vec:vec 1d0 3d0)

   ; create a 3d vector
   (vec:3vec 1d0 2d0 3d0)
   ```
   `vec` supports common vector operations such as `add`, `mult`, `div`, `sub`,
   as well as cross products `cross`, dot products, `dot`.

   *Note: Functions that operate on 3d vectors are prefixed with `3`.*

   Furthermore there are corresponding functions for scalars, lists of vectors,
   and broadcasting. They are indicated by prefix `l` and `s`:
   ```lisp
   ; add scalar s to a, return result
   (vec:sadd va s)

   ; add vectors in two lists, returns list of new vectors
   (vec:ladd aa bb)

   ; add b to elements of aa, return list of new vectors
   (vec:ladd* aa vb)
   ```

   Most of these functions also have a corresponding function postfixed with `!`,
   which performs the same operation, but assigns the result to the first
   argument. This is faster since there is no need to create a new `vec` struct.

   *Note: This can cause strange behaviour since you can inadvertently change the
   internal state of a struct. Use with caution.*

   Example:
   ```lisp
   ; add b to a, store result in a; also returns a
   (vec:add! va vb)
   ```
   There are also some common geometric operations, such as line interpolation,
   line-line intersection, angles, and line-plane intersection.
   ```lisp
   ; find the position s between a and b. s should be between 0 and 1
   (vec:on-line s va vb)
   ; find intersection of two lines, returns
   ; a bool, and interpolation value along line 1, line 2
   (vec:segx line1 line2)
   ```
   See the code in the package for more details.


