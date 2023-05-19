#!/bin/bash

set -e
sbcl --quit \
     --eval '(handler-case (ql:quickload :veq :verbose t)
                           (error (c) (format t "STAGE1FAIL: ~a" c)
                                      (uiop:quit 2)))'\
     --eval '(ql:quickload :prove)'\
     --eval '(handler-case (asdf:test-system :veq)
                           (error (c) (format t "STAGE2FAIL: ~a" c)
                                      (uiop:quit 3)))'

