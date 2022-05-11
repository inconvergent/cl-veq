
(asdf:defsystem #:veq
  :description "reasonably fast operations for 1-4d vectors, matrices, and
                arrays of vectors."
  :version "2.2.1"
  :author "anders hoff / @inconvergent / inconvergent@gmail.com"
  :licence "MIT"
  :in-order-to ((asdf:test-op (asdf:test-op #:veq/tests)))
  :pathname "src/"
  :serial nil
  :depends-on (#:alexandria #+SBCL #:sb-cltl2 #:prove #:str)
  :components ((:file "packages")
               (:file "config" :depends-on ("packages"))
               (:file "utils" :depends-on ("config"))
               (:file "array-utils" :depends-on ("utils"))
               (:file "docs" :depends-on ("utils"))
               (:file "veq-ops" :depends-on ("docs" "array-utils"))
               (:file "vset" :depends-on ("veq-ops"))
               (:file "array-rows" :depends-on ("veq-ops"))
               (:file "nsum" :depends-on ("veq-ops"))
               (:file "ops-1" :depends-on ("veq-ops"))
               (:file "ops-2" :depends-on ("veq-ops"))
               (:file "ops-3" :depends-on ("veq-ops"))
               (:file "ops-4" :depends-on ("veq-ops"))
               (:file "fxlspace" :depends-on ("veq-ops"))
               (:file "lets" :depends-on ("utils"))
               (:file "macros-helpers" :depends-on ("utils" "lets"))
               (:file "macros"
                :depends-on ("macros-helpers" "veq-ops" "array-rows"
                             "vset" "fxlspace" "nsum" "ops-1" "ops-2"
                             "ops-3" "ops-4"))
               (:file "array-take" :depends-on ("macros"))
               (:file "easing" :depends-on ("macros"))
               (:file "select-dim" :depends-on ("macros"))
               (:file "mat" :depends-on ("macros"))
               (:file "mat-inv" :depends-on ("macros"))
               (:file "mat-cam" :depends-on ("macros"))
               (:file "lspace" :depends-on ("macros"))
               (:file "array-print" :depends-on ("macros"))
               (:file "array-mima" :depends-on ("macros"))
               (:file "checks" :depends-on ("array-mima"))
               (:file "array-broadcast" :depends-on ("macros"))
               (:file "array-reduce" :depends-on ("macros"))
               (:file "shapes" :depends-on ("array-mima" "array-broadcast"))))

(asdf:defsystem #:veq/tests
  :depends-on (#:veq #:prove)
  :perform (asdf:test-op (o s) (uiop:symbol-call ':veq-tests '#:run-tests))
  :pathname "test/"
  :serial t
  :components ((:file "run")))

