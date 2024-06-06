#!/usr/local/bin/sbcl --script
(load "~/quicklisp/setup.lisp")
(ql:quickload :veq) (in-package :veq)

(defun make-docs (&optional (cnt 0) (*print-escape* nil))
  (labels ((fn (name) (internal-path-string
                        (format nil "docs/~2,'0d-~a.md"
                                    (incf cnt) (repl name "/" "-")))))
    (with-open-file (fs (fn "veq") :direction :output :if-exists :supersede)
      (princ (-outstr (ext-symbols? "VEQ" :pretty
                        (lambda (n c) (and (not (equal c :context))
                                           (not (equal n 'vv)))))) fs))
    (with-open-file (fs (fn "fvprogn") :direction :output :if-exists :supersede)
      (format fs "# FVPROGN / VPROGN CONTEXT~%~%~a"
              (-outstr (ext-symbols? "VEQ" :pretty
                         (lambda (n c) (equal c :context))))))
    (with-open-file (fs (fn "vv") :direction :output :if-exists :supersede)
      (format fs "# VV DSL~%~%```~%~a~%```~%"
                 (documentation 'veq:vv 'function)))))

(make-docs)

