
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1d, 2d, 3d vectors"
  :version "0.1.3"
  :author "anders hoff/inconvergent"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:alexandria #:sb-cltl2)
  :components ((:file "packages")
               (:file "utils"
                      :depends-on ("packages"))
               (:file "vprint"
                      :depends-on ("packages" "utils"))
               (:file "veq"
                      :depends-on ("packages" "utils"))
               (:file "ops"
                      :depends-on ("packages" "utils"))
               (:file "ops-1"
                      :depends-on ("packages" "utils" "veq"))
               (:file "ops-2"
                      :depends-on ("packages" "utils" "veq"))
               (:file "ops-3"
                      :depends-on ("packages" "utils" "veq"))
               (:file "vset"
                      :depends-on ("packages" "utils" "veq"))
               (:file "lerp"
                      :depends-on ("packages" "utils" "veq"))
               (:file "mima"
                      :depends-on ("packages" "utils" "veq"))
               (:file "rows"
                      :depends-on ("packages" "utils" "veq"))
               (:file "macros"
                      :depends-on ("packages" "utils" "veq"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

