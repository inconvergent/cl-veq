
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1-4d vectors, matrices, and
                arrays of vectors."
  :version "4.0.0"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#+SBCL #:sb-cltl2)
  :components ((:file "packages")
              #+:veq-reader-macros(:file "reader-macros" :depends-on ("packages"))
               (:file "init" :depends-on ("packages"))
               (:file "config" :depends-on ("init"))
               (:file "generic-utils" :depends-on ("config"))
               (:file "types" :depends-on ("generic-utils"))
               (:file "utils" :depends-on ("types"))
               (:file "array-utils" :depends-on ("utils"))
               (:file "docs" :depends-on ("utils"))
               (:file "veq-ops" :depends-on ("docs"))
               (:file "lets" :depends-on ("veq-ops"))
               (:file "ops-vv" :depends-on ("lets"))
               (:file "vset" :depends-on ("veq-ops"))
               (:file "array-rows" :depends-on ("veq-ops"))
               (:file "nsum" :depends-on ("veq-ops"))
               (:file "ops-1" :depends-on ("veq-ops"))
               (:file "ops-2" :depends-on ("veq-ops"))
               (:file "ops-3" :depends-on ("veq-ops"))
               (:file "ops-4" :depends-on ("veq-ops"))
               (:file "fxlspace" :depends-on ("veq-ops"))
               (:file "macros-helpers" :depends-on ("utils" ))
               (:file "macrolets"
                :depends-on ("macros-helpers" "veq-ops" "array-rows"
                             "vset" "fxlspace" "nsum" "array-utils" "lets"
                             "ops-vv" "ops-1" "ops-2" "ops-3" "ops-4"))
               (:file "array-take" :depends-on ("macrolets"))
               (:file "select-dim" :depends-on ("macrolets"))
               (:file "mat" :depends-on ("macrolets"))
               (:file "mat-inv" :depends-on ("macrolets"))
               (:file "mat-cam" :depends-on ("macrolets"))
               (:file "lspace" :depends-on ("macrolets"))
               (:file "array-print" :depends-on ("macrolets"))
               (:file "array-mima" :depends-on ("macrolets"))
               (:file "array-reduce" :depends-on ("macrolets"))
               (:file "checks" :depends-on ("array-mima"))
               (:file "shapes" :depends-on ("array-mima" ))
               (:file "easing" :depends-on ("macrolets"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :version "4.0.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

