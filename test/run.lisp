
(defpackage #:veq-tests (:use #:cl #:prove) (:export #:run-tests))

(setf prove:*enable-colors* nil)

(in-package #:veq-tests)

(defvar *files* `(#P"test/veq.lisp" #P"test/macro.lisp"
                  #P"test/macro-vv.lisp" #P"test/arr.lisp"
                  #P"test/checks.lisp" #P"test/mat.lisp"))

; TODO: print test file runtime?
(defun run-tests ()
  (loop with fails = 0
        for f in *files*
        do (format t "~&~%starting tests in: ~a~%" (veq::mkstr f))
           (unless (prove:run f :reporter :fiveam)
                   (incf fails))
           (format t "~&done: ~a~%" (veq::mkstr f))
        finally (return (unless (< fails 1)
                          (sb-ext:quit :unix-status 7)))))

(defmacro is-arr (&rest rest) `(is ,@rest :test #'equalp))

