
(asdf:defsystem #:veq
  :description "DSL and other utilities for vector mathematics."
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :version "5.1.5" :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/" :serial nil
  :depends-on (#+SBCL #:sb-cltl2 #+:veq-simd #:sb-simd)
  :components ((:file "packages")
               #+:veq-reader-macros(:file "reader-macros"
                                    :depends-on ("packages"))
               (:file "init" :depends-on ("packages"))
               (:file "config" :depends-on ("init"))
               (:file "generic-utils" :depends-on ("config"))
               (:file "types" :depends-on ("generic-utils"))
               (:file "utils" :depends-on ("types"))
               (:file "docs" :depends-on ("utils"))
               (:file "vset" :depends-on ("utils"))
               (:file "macros-helpers" :depends-on ("utils" ))
               (:file "ops-helpers" :depends-on ("macros-helpers"))
               (:file "ops" :depends-on ("ops-helpers"))
               (:file "lets" :depends-on ("ops"))
               (:file "macrolets" :depends-on ("macros-helpers" "ops"))
               (:file "ops-vv-helpers" :depends-on ("macrolets"))
               (:file "ops-vv" :depends-on ("ops-vv-helpers"))
               (:file "array-utils" :depends-on ("utils"))
               (:file "array-rows" :depends-on ("utils"))
               (:file "select-dim" :depends-on ("utils"))
               (:file "fxlspace" :depends-on ("macrolets"))
               (:file "mat" :depends-on ("macrolets"))
               (:file "mat-inv" :depends-on ("macrolets"))
               (:file "mat-cam" :depends-on ("macrolets"))
               (:file "array-extra" :depends-on ("macrolets"))
               (:file "checks" :depends-on ("array-extra"))
               (:file "checks-sweep" :depends-on ("checks"))
               (:file "shapes" :depends-on ("array-extra"))
               (:file "easing" :depends-on ("macrolets"))
               (:file "extra" :depends-on ("macrolets"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove #:asdf #:uiop)
  :version "5.1.5"
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/" :serial t
  :components ((:file "run")))

