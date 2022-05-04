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

## Symbols
EOF

tail -n +17 DOCS.md.tmp >> DOCS.md
