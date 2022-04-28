#!/bin/bash

set -e
touch ./veq.asd
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(load "veq.asd")'\
     --eval '(handler-case (time (ql:quickload :veq :verbose t))
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
  >compile.sh.tmp 2>&1
