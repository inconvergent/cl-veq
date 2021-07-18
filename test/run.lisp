
(defpackage #:veq-tests (:use #:cl #:prove) (:export #:run-tests))

(setf prove:*enable-colors* nil)

(in-package #:veq-tests)

(defun run-tests ()
  (prove:run #P"test/veq.lisp" :reporter :fiveam)
  (prove:run #P"test/checks.lisp" :reporter :fiveam)
  )

