
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1d, 2d, 3d vectors"
  :version "0.1.2"
  :author "anders hoff/inconvergent"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/"
  :serial t
  :depends-on (#:alexandria #:sb-cltl2)
  :components ((:file "packages")
               (:file "utils")
               (:file "vprint")
               (:file "veq")
               (:file "ops")
               (:file "ops-1")
               (:file "ops-2")
               (:file "ops-3")
               (:file "vset")
               (:file "lerp")
               (:file "mima")
               (:file "rows")
               (:file "macros")))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

