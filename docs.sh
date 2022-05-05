#!/bin/bash

set -e
touch ./veq.asd
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :veq)'\
     --eval '(handler-case (veq:ext-symbols? :pretty)
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >DOCS.md.tmp 2>&1

cat << EOF > DOCS.md
# VEQ DOCUMENTATION

### Explanation

#### Context

All symbols marked with \`:contex:\` are only valid inside a veq context.  veq
context can be initiated using \`vprogn\`, \`fvprogn\`, \`vdef\`, \`fvdef\`,
\`vdef*\` or \`fvdef*\`. See further documentation below.

See [examples](/examples/ex.lisp) for working examples of some use.

#### Names and Types

Symols prefixed with \`f\` pertain to type \`ff\`, short for \`single-float\`.
The corresponding vector array type is \`fvec\`, short for \`(simple-array ff)\`.

Symols prefixed with \`d\` pertain to type \`df\`, short for \`double-float\`.
The corresponding vector array type is \`dvec\`, short for \`(simple-array df)\`.

Symbols with \`$\` in the name pertain to vector arrays.

Symbols postfixed with \`!\` are destructive or in-place.


#### Abbreviations

\`dsb\` is short for \`destructuring-bind\`.

\`mvb\` is short for \`multiple-value-bind\`.

\`mvc\` is short for \`multiple-value-call\`.


### Symbols

EOF

tail -n +18 DOCS.md.tmp >> DOCS.md
sed -i 's/[[:space:]]*$//' DOCS.md
