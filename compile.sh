#!/bin/bash

set -e
touch ./veq.asd
time sbcl --quit \
          --eval '(load "veq.asd")'\
          --eval '(handler-case (time (ql:quickload :veq :verbose t))
                    (error (c) (print c) (sb-ext:quit :unix-status 2)))'\
     > compile.sh.log 2>&1
