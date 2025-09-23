#!/usr/local/bin/sbcl --script
(load "~/quicklisp/setup.lisp")
; sorry, this requires auxin to build because im too lazy
(ql:quickload :auxin)
(in-package :veq)

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

(defun make-rnd-docs ()
  (loop for (o . rest) in (dat:import-all-data (internal-path-string "src/packages") ".lisp")
        for pkg = (print (mkstr (car rest)))
        for fn = (internal-path-string (format nil "docs/~(~a~).md" (veq::repl pkg "/" "-")))
        if (and (eq o 'defpackage) (not (equal pkg "VEQ")))
        do (with-open-file (fs fn :direction :output :if-exists :supersede)
             (princ (-outstr (print (ext-symbols? pkg :pretty or))) fs))))

(make-docs)
(make-rnd-docs)
