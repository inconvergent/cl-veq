#!/bin/bash

set -e
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(handler-case (ql:quickload :veq :verbose nil)
                           (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
     --eval '(handler-case (asdf:test-system :veq)
                           (error (c) (print c) (sb-ext:quit :unix-status 3)))'

