;;;; package.lisp - Package Definitions
;;;;
;;;; Design-by-contract DSL for Common Lisp
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(in-package #:cl-user)

;;; ============================================================================
;;; Main Package
;;; ============================================================================

(defpackage #:formal-contracts-dsl
  (:use #:cl)
  (:nicknames #:fcontracts #:contracts-dsl)
  (:documentation "Design-by-contract DSL for Common Lisp.

Provides macros for defining functions with formally-verified contracts:
- Type declarations with compile-time checking
- Preconditions (requires)
- Postconditions (ensures)
- Invariants (maintains)

Contracts generate proof obligations tracked in a global registry.
Proof obligations can be exported to ACL2 for formal verification.

Example:
  (defun/spec add-balance (account amount)
    (:types ((account account-t) (amount (integer 0 *))))
    (:requires (>= (account-balance account) 0))
    (:ensures (= result (+ old-balance amount)))
    (:body
      (incf (account-balance account) amount)))")

  ;; Contract structure types
  (:export
   #:contract
   #:make-contract
   #:contract-p
   #:contract-name
   #:contract-lambda-list
   #:contract-types
   #:contract-requires
   #:contract-ensures
   #:contract-invariants
   #:contract-modifies
   #:contract-pure-p
   #:contract-documentation
   #:contract-source-file

   ;; Type specification
   #:type-spec
   #:make-type-spec
   #:type-spec-p
   #:type-spec-name
   #:type-spec-type
   #:type-spec-constraints

   ;; Condition types
   #:precondition
   #:make-precondition
   #:precondition-p
   #:precondition-expr
   #:precondition-message

   #:postcondition
   #:make-postcondition
   #:postcondition-p
   #:postcondition-expr
   #:postcondition-old-bindings

   #:invariant-spec
   #:make-invariant-spec
   #:invariant-spec-p
   #:invariant-spec-expr
   #:invariant-spec-scope)

  ;; Main definition macros
  (:export
   #:defun/spec
   #:defmethod/spec
   #:define-contract
   #:with-contracts-enabled
   #:with-contracts-disabled)

  ;; Contract verification
  (:export
   #:verify-contract
   #:verify-all-contracts
   #:verify-types
   #:verify-preconditions
   #:verify-postconditions
   #:verify-invariants
   #:format-verification-report

   ;; Violations
   #:contract-violation
   #:precondition-violation
   #:postcondition-violation
   #:invariant-violation
   #:contract-violation-function
   #:contract-violation-type
   #:contract-violation-expression
   #:contract-violation-message)

  ;; Proof obligation integration
  (:export
   #:proof-obligation
   #:make-proof-obligation
   #:proof-obligation-p
   #:proof-obligation-id
   #:proof-obligation-function
   #:proof-obligation-kind
   #:proof-obligation-formula
   #:proof-obligation-status

   #:register-proof-obligation
   #:get-proof-obligations
   #:clear-proof-obligations
   #:obligation-status
   #:update-obligation-status
   #:mark-proven
   #:mark-axiom
   #:mark-skip

   ;; Reporting
   #:check-all-proof-obligations
   #:obligations-by-function
   #:obligations-by-file
   #:export-obligations-to-acl2
   #:generate-obligation-dashboard)

  ;; Contract query
  (:export
   #:get-contract
   #:list-contracts
   #:contract-defined-p)

  ;; Runtime support
  (:export
   #:assert-precondition
   #:assert-postcondition
   #:assert-invariant
   #:capture-old-values
   #:with-runtime-checks
   #:with-contract-tracing
   #:with-soft-contracts

   ;; Statistics
   #:contract-stats
   #:get-contract-stats
   #:clear-contract-stats
   #:summarize-contract-stats
   #:record-contract-check)

  ;; Special forms
  (:export
   #:result    ; bound to function return value in postconditions
   #:old)      ; function to capture pre-state values

  ;; Configuration
  (:export
   #:*contracts-enabled*
   #:*contract-check-level*
   #:*proof-obligation-registry*
   #:*contract-registry*
   #:*contract-trace*
   #:*contract-soft-mode*
   #:*contracts-version*))
