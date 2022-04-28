
(defpackage #:veq-tests (:use #:cl #:prove) (:export #:run-tests))

(setf prove:*enable-colors* nil)

(in-package #:veq-tests)

(defvar *files* `(#P"test/veq.lisp" #P"test/macro.lisp" #P"test/arr-with.lisp"
                  #P"test/arr.lisp" #P"test/checks.lisp" #P"test/mat.lisp"))

(defun compile-or-fail (f)
  (format t "~%compiling: ~a~%" (veq::mkstr f))
  (with-open-stream (*standard-output* (make-broadcast-stream))
    (compile-file f)))

(defun run-tests ()
  (loop with fails = 0
        for f in *files*
        do (compile-or-fail f)
           (format t "~&~%starting tests in: ~a~%" (veq::mkstr f))
           (unless (prove:run f :reporter :fiveam)
                   (incf fails))
           (format t "~&done: ~a~%" (veq::mkstr f))
        finally (return (unless (< fails 1)
                          (sb-ext:quit :unix-status 7)))))

