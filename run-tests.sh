#!/bin/bash

set -e
sbcl --quit \
     --eval '(load "~/quicklisp/setup.lisp")'\
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (ql:quickload :veq :verbose t)
                           (error (c) (format t "STAGE1FAIL: ~a" c)
                                      (sb-ext:quit :unix-status 2)))'\
     --eval '(handler-case (asdf:test-system :veq)
                           (error (c) (format t "STAGE2FAIL: ~a" c)
                                      (sb-ext:quit :unix-status 3)))'

