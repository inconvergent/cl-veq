
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1-4d vectors, matrices, and
arrays of vectors."
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :version "4.3.0" :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/" :serial nil
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
               (:file "vset" :depends-on ("utils"))
               (:file "lets" :depends-on ("veq-ops"))
               (:file "array-rows" :depends-on ("veq-ops"))
               (:file "ops-1" :depends-on ("veq-ops"))
               (:file "ops-2" :depends-on ("veq-ops"))
               (:file "ops-3" :depends-on ("veq-ops"))
               (:file "ops-4" :depends-on ("veq-ops"))
               (:file "ops-vv-helpers" :depends-on ("lets"))
               (:file "ops-vv" :depends-on ("ops-vv-helpers"))
               (:file "macros-helpers" :depends-on ("utils" ))
               (:file "macrolets"
                :depends-on ("macros-helpers" "veq-ops" "array-rows"
                             "array-utils" "lets"
                             "ops-vv" "ops-1" "ops-2" "ops-3" "ops-4"))
               (:file "select-dim" :depends-on ("macrolets"))
               (:file "fxlspace" :depends-on ("macrolets"))
               (:file "mat" :depends-on ("macrolets"))
               (:file "mat-inv" :depends-on ("macrolets"))
               (:file "mat-cam" :depends-on ("macrolets"))
               (:file "array-print" :depends-on ("macrolets"))
               (:file "array-mima" :depends-on ("macrolets"))
               (:file "checks" :depends-on ("array-mima"))
               (:file "shapes" :depends-on ("array-mima"))
               (:file "easing" :depends-on ("macrolets"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :version "4.2.0"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))

