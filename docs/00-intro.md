# VEQ DOCUMENTATION

## Intro

See [01-veq.md](/docs/01-veq.md) for full symbol documentation.

Some symbols are only available inside a `vprogn`, `fvprogn`, `vdef`, `fvdef`,
`vdef*` or `fvdef*` context. These symbols are documented in
[02-fvprogn.md](/docs/02-fvprogn.md).

## VV DSL

The `vv` DSL for vector mathematics is enabled inside all the above contexts.
Or you can use it via the `(vv ...)` macro or the `(proc-vv ...)` compiler
function. For full documentation see [03-vv.md](/docs/03-vv.md)

## Type Names & Conventions

`veq` uses short prefixes for function and macro names. The most common types
are as follows:

| `prefix` | `CL type`            | `short name` | `vec type name`
| ---      | ---                  | ---          | ---
| `f`      | `float`              | `ff`         | `fvec`
| `d`      | `double-float`       | `df`         | `dvec`
| `i`      | `(signed-byte 32)`   | `in`         | `ivec`
| `p`      | `(unsigned-byte 32)` | `pn`         | `pvec`
| `l`      | `list`               | `ll`         | `lvec`
| `s`      | `symbol`             | `sy`         | `svec`
| `k`      | `keyword`            | `kv`         | `kvec`


Symbols with `$` in the name pertain to vec arrays. Vec arrays are
`(simple-array [type])`.

Symbols postfixed with `!` are destructive. Usually on the first argument.

## Examples

For examples see [/examples/ex.lisp](/examples/ex.lisp)
