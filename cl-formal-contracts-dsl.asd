;;;; cl-formal-contracts-dsl.asd - System Definition
;;;;
;;;; Design-by-contract DSL for Common Lisp
;;;; Provides defun/spec macro with :requires, :ensures, and formal verification
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(defsystem "cl-formal-contracts-dsl"
  :name "cl-formal-contracts-dsl"
  :version "1.0.0"
  :author "CLPIC Project"
  :license "MIT"
  :description "Design-by-contract DSL for Common Lisp with formal verification support"
  :long-description "A pure Common Lisp implementation of design-by-contract programming.
Provides defun/spec and defmethod/spec macros for defining functions with:
- Type declarations (:types)
- Preconditions (:requires)
- Postconditions (:ensures)
- Invariants (:invariants)
- Pure function markers (:pure)
- Modification tracking (:modifies)

Contracts generate proof obligations that can be:
- Verified at compile-time (simple cases)
- Checked at runtime (debug builds)
- Exported to ACL2/PVS for formal proof"

  :depends-on ()  ; Pure CL - no external dependencies

  :components
  ((:file "package")
   (:module "src"
    :serial t
    :components
    ((:file "types")
     (:file "verify")
     (:file "runtime")
     (:file "registry")
     (:file "spec"))))

  :in-order-to ((test-op (test-op "cl-formal-contracts-dsl/test"))))

(defsystem "cl-formal-contracts-dsl/test"
  :name "cl-formal-contracts-dsl-test"
  :version "1.0.0"
  :author "CLPIC Project"
  :license "MIT"
  :description "Tests for cl-formal-contracts-dsl"

  :depends-on ("cl-formal-contracts-dsl")

  :components
  ((:module "test"
    :components
    ((:file "test-contracts"))))

  :perform (test-op (op c)
             (funcall (find-symbol "RUN-TESTS" "FORMAL-CONTRACTS-DSL.TEST"))))
