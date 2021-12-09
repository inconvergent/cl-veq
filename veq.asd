
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1d, 2d, 3d vectors"
  :version "0.4.2"
  :author "anders hoff/inconvergent"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:alexandria #+SBCL #:sb-cltl2)
  :components ((:file "packages")
               (:file "utils" :depends-on ("packages"))
               (:file "array-utils" :depends-on ("utils"))
               (:file "array-mima" :depends-on ("array-utils"))
               (:file "veq" :depends-on ("utils" "array-utils"))
               (:file "vset" :depends-on ("veq"))
               (:file "lspace" :depends-on ("veq"))
               (:file "array-rows" :depends-on ("veq"))
               (:file "ops-1" :depends-on ("veq"))
               (:file "ops-2" :depends-on ("veq"))
               (:file "ops-3" :depends-on ("veq"))
               (:file "macros" :depends-on ("ops-1" "ops-2" "ops-3"))
               (:file "array-print" :depends-on ("macros"))
               (:file "checks" :depends-on ("macros" "array-rows"))
               (:file "array-broadcast" :depends-on ("macros" "array-rows"))
               (:file "shapes" :depends-on ("macros" "array-mima" "array-broadcast"))
               (:file "array-take" :depends-on ("array-broadcast"))
               (:file "easing" :depends-on ("macros"))
               (:file "select-dim" :depends-on ("macros"))
               (:file "extra" :depends-on ("macros"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

