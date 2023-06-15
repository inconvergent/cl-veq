#!/bin/bash

sbcl --quit \
     --eval '(ql:quickload :veq :silent t)'\
     --eval '(handler-case (veq:ext-symbols? :pretty)
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >DOCS.md.tmp 2>&1

cat << EOF > DOCS.md
# VEQ DOCUMENTATION

### Explanation

#### FVPROGN

All symbols marked with \`:fvprogn:\` are only valid inside a veq context.  veq
context can be initiated using \`vprogn\`, \`fvprogn\`, \`vdef\`, \`fvdef\`,
\`vdef*\` or \`fvdef*\`. See further documentation under the respective symbols
below.

See [examples](/examples/ex.lisp) for working examples of some use.

#### Names and Types

Symols prefixed with \`f\` pertain to type \`ff\`, short for \`single-float\`.
The corresponding vector array type is \`fvec\`, short for \`(simple-array ff)\`.

Symols prefixed with \`d\` pertain to type \`df\`, short for \`double-float\`.
The corresponding vector array type is \`dvec\`, short for \`(simple-array df)\`.

Symbols with \`$\` in the name pertain to vector arrays.

Symbols postfixed with \`!\` are destructive or in-place. Usually on the first
argument.


#### Abbreviations

\`dsb\` is short for \`destructuring-bind\`.

\`mvb\` is short for \`multiple-value-bind\`.

\`mvc\` is short for \`multiple-value-call\`.


### Symbols

EOF

tail -n +8 DOCS.md.tmp >> DOCS.md
sed -i 's/[[:space:]]*$//g' DOCS.md
sed -i 's:/data/x/veq/::g' DOCS.md
rm  DOCS.md.tmp
