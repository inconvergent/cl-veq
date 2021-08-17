
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1d, 2d, 3d vectors"
  :version "0.3.8"
  :author "anders hoff/inconvergent"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:alexandria #:sb-cltl2)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "array-utils" :depends-on ("utils"))
               (:file "mima" :depends-on ("array-utils"))
               (:file "veq" :depends-on ("utils" "array-utils"))
               (:file "vset" :depends-on ("veq"))
               (:file "lerp" :depends-on ("veq"))
               (:file "rows" :depends-on ("veq"))
               (:file "ops-1" :depends-on ("veq"))
               (:file "ops-2" :depends-on ("veq"))
               (:file "ops-3" :depends-on ("veq"))
               (:file "macros" :depends-on ("ops-1" "ops-2" "ops-3"))
               (:file "vprint" :depends-on ("macros"))
               (:file "checks" :depends-on ("macros" "rows"))
               (:file "broadcast" :depends-on ("macros" "rows"))
               (:file "shapes" :depends-on ("macros" "mima"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

