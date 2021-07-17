
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1d, 2d, 3d vectors"
  :version "0.1.4"
  :author "anders hoff/inconvergent"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:alexandria #:sb-cltl2)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "veq" :depends-on ("utils"))
               (:file "ops" :depends-on ("utils"))
               (:file "ops-1" :depends-on ("utils" "veq"))
               (:file "ops-2" :depends-on ("utils" "veq"))
               (:file "ops-3" :depends-on ("utils" "veq"))
               (:file "vset" :depends-on ("utils" "veq"))
               (:file "lerp" :depends-on ("utils" "veq"))
               (:file "mima" :depends-on ("utils" "veq"))
               (:file "rows" :depends-on ("utils" "veq"))
               (:file "macros" :depends-on ("utils" "veq"))
               (:file "vprint" :depends-on ("utils" "macros"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

