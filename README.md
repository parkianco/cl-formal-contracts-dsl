# cl-formal-contracts-dsl

A pure Common Lisp implementation of Design-by-Contract programming with formal verification support.

## Features

- **defun/spec** macro for defining functions with contracts
- **Preconditions** (`:requires`) - conditions that must hold on function entry
- **Postconditions** (`:ensures`) - conditions that must hold on function exit
- **Type specifications** (`:types`) - compile-time type checking
- **Invariants** (`:invariants`) - conditions that must hold throughout execution
- **Pure function markers** (`:pure`) - declare side-effect-free functions
- **Runtime checking** - optional runtime contract verification
- **Proof obligations** - generate verifiable proof obligations
- **ACL2 export** - export obligations for formal verification

## Installation

Clone the repository and load with ASDF:

```lisp
(asdf:load-system "cl-formal-contracts-dsl")
```

Or add to your system's dependencies.

## Quick Start

```lisp
(use-package :formal-contracts-dsl)

;; Define a function with contracts
(defun/spec deposit (account amount)
  "Deposit amount into account."
  (:types ((amount (integer 0 *))))
  (:requires (>= amount 0)
             (account-p account))
  (:ensures (= (account-balance account)
               (+ (old (account-balance account)) amount)))
  (:body
    (incf (account-balance account) amount)))

;; Pure function example
(defun/spec square (x)
  "Compute the square of x."
  (:types ((x number)))
  (:pure)
  (:ensures (= result (* x x)))
  (:body
    (* x x)))
```

## Syntax Reference

### defun/spec

```lisp
(defun/spec name (parameters...)
  [documentation-string]
  (:types ((param1 type1) (param2 type2) ...))
  (:requires precondition1 precondition2 ...)
  (:ensures postcondition1 postcondition2 ...)
  (:invariants invariant1 ...)
  (:modifies place1 place2 ...)
  (:pure)
  (:body form1 form2 ...)
  ; or just forms without :body
  form1 form2 ...)
```

### Special Forms in Postconditions

- `result` - the return value of the function
- `(old expr)` - the value of `expr` before the function call

### Configuration

```lisp
;; Enable/disable contract checking
(setf *contracts-enabled* t)  ; default: t

;; Set checking level
(setf *contract-check-level* :full)
;; Options: :none, :pre, :post, :full
```

### Runtime Control

```lisp
;; Temporarily disable contracts
(with-contracts-disabled
  (potentially-violating-code))

;; Enable contract tracing
(with-contract-tracing
  (my-contracted-function args))
```

## Verification

```lisp
;; Verify a single contract
(let ((contract (get-contract 'my-function)))
  (format-verification-report (verify-contract contract)))

;; Verify all registered contracts
(verify-all-contracts)

;; Check proof obligation status
(check-all-proof-obligations)
```

## ACL2 Export

Export proof obligations for formal verification:

```lisp
(export-obligations-to-acl2 "proofs/obligations.lisp")
```

## API Reference

### Main Macros

- `defun/spec` - Define a function with contracts
- `defmethod/spec` - Define a method with contracts
- `define-contract` - Define a standalone contract

### Contract Types

- `contract` - Complete contract specification
- `type-spec` - Type specification for a parameter
- `precondition` - Precondition specification
- `postcondition` - Postcondition specification
- `invariant-spec` - Invariant specification

### Verification

- `verify-contract` - Verify a single contract
- `verify-all-contracts` - Verify all registered contracts
- `format-verification-report` - Format a verification report

### Proof Obligations

- `register-proof-obligation` - Register a proof obligation
- `get-proof-obligations` - Get obligations (with filtering)
- `obligation-status` - Get status of a function's obligations
- `mark-proven` - Mark obligations as proven
- `export-obligations-to-acl2` - Export to ACL2 format

### Runtime

- `assert-precondition` - Assert a precondition
- `assert-postcondition` - Assert a postcondition
- `with-contracts-enabled` - Enable contracts in scope
- `with-contracts-disabled` - Disable contracts in scope
- `with-contract-tracing` - Trace contract checks
- `with-soft-contracts` - Convert violations to warnings

## License

MIT License. See LICENSE file.

## Acknowledgments

Extracted from the CLPIC project (Common Lisp P2P Intellectual Property Chain).
