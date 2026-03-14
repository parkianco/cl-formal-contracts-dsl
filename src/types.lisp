;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; src/types.lisp - Core Type Definitions
;;;;
;;;; Design-by-contract DSL for Common Lisp
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(in-package #:formal-contracts-dsl)

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(defparameter *contracts-version* "1.0.0"
  "Version of the contracts module.")

(defvar *contracts-enabled* t
  "When T, contract checking is active in debug builds.")

(defvar *contract-check-level* :full
  "Level of contract checking:
   :none - No runtime checks
   :pre  - Only preconditions
   :post - Preconditions and postconditions
   :full - All checks including invariants")

;;; ============================================================================
;;; Registries
;;; ============================================================================

(defvar *contract-registry* (make-hash-table :test 'equal)
  "Registry of all defined contracts by function name.")

(defvar *proof-obligation-registry* (make-hash-table :test 'equal)
  "Registry of proof obligations generated from contracts.")

;;; ============================================================================
;;; Condition Types
;;; ============================================================================

(define-condition contract-violation (error)
  ((function-name :initarg :function :reader contract-violation-function)
   (contract-type :initarg :type :reader contract-violation-type)
   (expression :initarg :expr :reader contract-violation-expression)
   (message :initarg :message :reader contract-violation-message))
  (:report (lambda (c s)
             (format s "Contract violation in ~A (~A): ~A~%Expression: ~S"
                     (contract-violation-function c)
                     (contract-violation-type c)
                     (contract-violation-message c)
                     (contract-violation-expression c)))))

(define-condition precondition-violation (contract-violation)
  ()
  (:default-initargs :type :precondition))

(define-condition postcondition-violation (contract-violation)
  ()
  (:default-initargs :type :postcondition))

(define-condition invariant-violation (contract-violation)
  ()
  (:default-initargs :type :invariant))

;;; ============================================================================
;;; Type Specification
;;; ============================================================================

(defstruct (type-spec (:constructor make-type-spec (name type &optional constraints)))
  "Specification for a typed parameter."
  (name nil :type symbol :read-only t)
  (type t :type (or symbol list) :read-only t)
  (constraints nil :type list :read-only t))

(defmethod make-load-form ((obj type-spec) &optional environment)
  "Allow TYPE-SPEC to be serialized to FASL files."
  (declare (ignore environment))
  `(make-type-spec ',(type-spec-name obj)
                   ',(type-spec-type obj)
                   ',(type-spec-constraints obj)))

;;; ============================================================================
;;; Precondition
;;; ============================================================================

(defstruct (precondition (:constructor make-precondition (expr &optional message)))
  "A precondition that must hold on function entry."
  (expr nil :type t :read-only t)
  (message nil :type (or null string) :read-only t))

(defmethod make-load-form ((obj precondition) &optional environment)
  "Allow PRECONDITION to be serialized to FASL files."
  (declare (ignore environment))
  `(make-precondition ',(precondition-expr obj)
                      ,(precondition-message obj)))

;;; ============================================================================
;;; Postcondition
;;; ============================================================================

(defstruct (postcondition (:constructor make-postcondition (expr &optional old-bindings)))
  "A postcondition that must hold on function exit.
   OLD-BINDINGS lists variables whose pre-call values are captured."
  (expr nil :type t :read-only t)
  (old-bindings nil :type list :read-only t))

(defmethod make-load-form ((obj postcondition) &optional environment)
  "Allow POSTCONDITION to be serialized to FASL files."
  (declare (ignore environment))
  `(make-postcondition ',(postcondition-expr obj)
                       ',(postcondition-old-bindings obj)))

;;; ============================================================================
;;; Invariant Specification
;;; ============================================================================

(defstruct (invariant-spec (:constructor make-invariant-spec (expr &optional scope)))
  "An invariant that must hold throughout execution.
   SCOPE specifies where the invariant applies (:function, :loop, :global)."
  (expr nil :type t :read-only t)
  (scope :function :type keyword :read-only t))

(defmethod make-load-form ((obj invariant-spec) &optional environment)
  "Allow INVARIANT-SPEC to be serialized to FASL files."
  (declare (ignore environment))
  `(make-invariant-spec ',(invariant-spec-expr obj)
                        ,(invariant-spec-scope obj)))

;;; ============================================================================
;;; Contract
;;; ============================================================================

(defstruct (contract (:constructor %make-contract))
  "Complete contract specification for a function."
  (name nil :type symbol :read-only t)
  (lambda-list nil :type list :read-only t)
  (types nil :type list)          ; list of type-spec
  (requires nil :type list)       ; list of precondition
  (ensures nil :type list)        ; list of postcondition
  (invariants nil :type list)     ; list of invariant-spec
  (modifies nil :type list)       ; list of modified places
  (pure-p nil :type boolean)      ; true if function has no side effects
  (documentation nil :type (or null string))
  (source-file nil :type (or null pathname))
  (source-line nil :type (or null integer)))

(defun make-contract (name lambda-list &key types requires ensures
                                            invariants modifies pure-p
                                            documentation source-file source-line)
  "Create a new contract specification."
  (%make-contract :name name
                  :lambda-list lambda-list
                  :types (mapcar (lambda (ts)
                                   (if (type-spec-p ts) ts
                                       (apply #'make-type-spec ts)))
                                 types)
                  :requires (mapcar (lambda (pre)
                                      (if (precondition-p pre) pre
                                          (make-precondition pre)))
                                    requires)
                  :ensures (mapcar (lambda (post)
                                     (if (postcondition-p post) post
                                         (make-postcondition post)))
                                   ensures)
                  :invariants (mapcar (lambda (inv)
                                        (if (invariant-spec-p inv) inv
                                            (make-invariant-spec inv)))
                                      invariants)
                  :modifies modifies
                  :pure-p pure-p
                  :documentation documentation
                  :source-file source-file
                  :source-line source-line))

;;; ============================================================================
;;; Proof Obligation
;;; ============================================================================

(defstruct (proof-obligation (:constructor %make-proof-obligation))
  "A proof obligation generated from a contract."
  (id nil :type string :read-only t)
  (function nil :type symbol :read-only t)
  (kind nil :type keyword :read-only t)  ; :precondition, :postcondition, :invariant, :type
  (formula nil :type t :read-only t)
  (hypotheses nil :type list)
  (source-file nil :type (or null pathname))
  (source-line nil :type (or null integer))
  (status :pending :type keyword)  ; :pending, :proven, :axiom, :skip, :failed
  (proof-file nil :type (or null pathname))
  (skip-reason nil :type (or null string))
  (created-at nil :type integer)
  (verified-at nil :type (or null integer)))

(defun make-proof-obligation (function kind formula &key hypotheses
                                                         source-file source-line
                                                         status proof-file skip-reason)
  "Create a new proof obligation."
  (let ((id (format nil "~A-~A-~A"
                    (symbol-name function)
                    kind
                    (sxhash formula))))
    (%make-proof-obligation
     :id id
     :function function
     :kind kind
     :formula formula
     :hypotheses hypotheses
     :source-file source-file
     :source-line source-line
     :status (or status :pending)
     :proof-file proof-file
     :skip-reason skip-reason
     :created-at (get-universal-time)
     :verified-at nil)))

;;; ============================================================================
;;; Contract Clause Parsing
;;; ============================================================================

(defun parse-type-clause (clause)
  "Parse a :types clause into a list of type-spec."
  ;; Format: (:types ((var1 type1) (var2 type2) ...))
  (mapcar (lambda (spec)
            (destructuring-bind (name type &optional constraints)
                (if (listp spec) spec (list spec t))
              (make-type-spec name type constraints)))
          (cadr clause)))

(defun parse-requires-clause (clause)
  "Parse a :requires clause into a list of preconditions."
  ;; Format: (:requires expr1 expr2 ...) or (:requires (expr "message") ...)
  (mapcar (lambda (spec)
            (if (and (listp spec) (stringp (second spec)))
                (make-precondition (first spec) (second spec))
                (make-precondition spec)))
          (cdr clause)))

(defun parse-ensures-clause (clause)
  "Parse an :ensures clause into a list of postconditions."
  ;; Format: (:ensures expr1 expr2 ...)
  ;; Can reference RESULT and (old var) for pre-state values
  (mapcar (lambda (spec)
            (let ((old-bindings (extract-old-references spec)))
              (make-postcondition spec old-bindings)))
          (cdr clause)))

(defun parse-invariants-clause (clause)
  "Parse an :invariants clause."
  (mapcar (lambda (spec)
            (if (and (listp spec) (keywordp (second spec)))
                (make-invariant-spec (first spec) (second spec))
                (make-invariant-spec spec)))
          (cdr clause)))

(defun extract-old-references (expr)
  "Extract all (old var) references from an expression."
  (let ((refs nil))
    (labels ((walk (form)
               (cond
                 ((atom form) nil)
                 ((and (eq (car form) 'old) (symbolp (cadr form)))
                  (pushnew (cadr form) refs))
                 (t (mapc #'walk form)))))
      (walk expr)
      refs)))

;;; ============================================================================
;;; Contract Body Parsing
;;; ============================================================================

(defun parse-spec-body (body)
  "Parse a defun/spec body into contract clauses and actual body.
   Returns (values types requires ensures invariants modifies pure-p doc body)."
  (let ((types nil)
        (requires nil)
        (ensures nil)
        (invariants nil)
        (modifies nil)
        (pure-p nil)
        (doc nil)
        (real-body nil))
    ;; Extract documentation string if present
    (when (and (stringp (car body)) (cdr body))
      (setf doc (pop body)))
    ;; Parse spec clauses
    (loop while (and body (listp (car body)) (keywordp (caar body)))
          for clause = (pop body)
          do (case (car clause)
               (:types (setf types (parse-type-clause clause)))
               (:requires (setf requires (append requires (parse-requires-clause clause))))
               (:ensures (setf ensures (append ensures (parse-ensures-clause clause))))
               (:invariants (setf invariants (append invariants (parse-invariants-clause clause))))
               (:modifies (setf modifies (cdr clause)))
               (:pure (setf pure-p t))
               (:body (setf real-body (cdr clause)))
               ;; Recognized metadata keywords that don't affect code generation
               (:tier nil)
               (:complexity nil)
               (:version nil)
               (t (push clause real-body))))
    ;; Remaining forms are the body
    (when body
      (setf real-body (append real-body body)))
    (values types requires ensures invariants modifies pure-p doc real-body)))
