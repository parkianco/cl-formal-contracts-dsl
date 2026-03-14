;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

;;;; src/verify.lisp - Compile-Time Verification
;;;;
;;;; Static verification of contracts at compile time.
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(in-package #:formal-contracts-dsl)

;;; ============================================================================
;;; Type Verification
;;; ============================================================================

(defun verify-type-spec (type-spec value-form env)
  "Verify that VALUE-FORM satisfies TYPE-SPEC at compile time if possible.
   Returns :proven, :unknown, or (list :failed reason)."
  (declare (ignore env))
  (let ((type (type-spec-type type-spec)))
    (cond
      ;; Constant value - can check directly
      ((constantp value-form)
       (if (typep (eval value-form) type)
           :proven
           (list :failed (format nil "Constant ~S is not of type ~S"
                                 value-form type))))
      ;; THE form - trust the declaration
      ((and (listp value-form) (eq (car value-form) 'the))
       (if (subtypep (second value-form) type)
           :proven
           :unknown))
      ;; Otherwise unknown
      (t :unknown))))

(defun verify-types (contract)
  "Verify all type specifications in a contract.
   Returns a list of (type-spec . status) pairs."
  (mapcar (lambda (ts)
            (cons ts (verify-type-spec ts (type-spec-name ts) nil)))
          (contract-types contract)))

;;; ============================================================================
;;; Precondition Verification
;;; ============================================================================

(defun simplify-boolean (expr)
  "Simplify a boolean expression if possible."
  (cond
    ;; Atomic
    ((atom expr) expr)
    ;; T and NIL propagation
    ((eq (car expr) 'and)
     (let ((args (mapcar #'simplify-boolean (cdr expr))))
       (cond
         ((member nil args) nil)
         ((every (lambda (x) (eq x t)) args) t)
         ((= (length args) 1) (car args))
         (t `(and ,@(remove t args))))))
    ((eq (car expr) 'or)
     (let ((args (mapcar #'simplify-boolean (cdr expr))))
       (cond
         ((member t args) t)
         ((every #'null args) nil)
         ((= (length args) 1) (car args))
         (t `(or ,@(remove nil args))))))
    ((eq (car expr) 'not)
     (let ((arg (simplify-boolean (second expr))))
       (cond
         ((eq arg t) nil)
         ((null arg) t)
         (t `(not ,arg)))))
    ;; Numeric comparisons with constants
    ((and (member (car expr) '(< <= > >= = /=))
          (every #'constantp (cdr expr)))
     (if (apply (car expr) (mapcar #'eval (cdr expr))) t nil))
    ;; Otherwise return as-is
    (t expr)))

(defun verify-precondition (pre hypotheses)
  "Try to verify a precondition given hypotheses.
   Returns :proven, :unknown, or (list :failed reason)."
  (let ((expr (precondition-expr pre)))
    ;; Try simplification
    (let ((simplified (simplify-boolean expr)))
      (cond
        ((eq simplified t) :proven)
        ((null simplified) (list :failed "Precondition simplifies to NIL"))
        ;; Check if precondition is in hypotheses
        ((member expr hypotheses :test #'equal) :proven)
        ;; Check for obvious implications
        ((some (lambda (h)
                 (implies-p h expr))
               hypotheses)
         :proven)
        (t :unknown)))))

(defun implies-p (hypothesis conclusion)
  "Check if HYPOTHESIS implies CONCLUSION (simple cases)."
  (cond
    ;; Same expression
    ((equal hypothesis conclusion) t)
    ;; (>= x y) implies (> x y) when y constant and (> x (- y 1))
    ((and (listp hypothesis) (eq (car hypothesis) '>=)
          (listp conclusion) (eq (car conclusion) '>)
          (equal (second hypothesis) (second conclusion))
          (numberp (third conclusion))
          (numberp (third hypothesis))
          (>= (third hypothesis) (third conclusion)))
     t)
    ;; (> x 0) implies (>= x 0)
    ((and (listp hypothesis) (eq (car hypothesis) '>)
          (listp conclusion) (eq (car conclusion) '>=)
          (equal (second hypothesis) (second conclusion))
          (equal (third conclusion) 0)
          (numberp (third hypothesis))
          (>= (third hypothesis) 0))
     t)
    (t nil)))

(defun verify-preconditions (contract)
  "Verify all preconditions in a contract.
   Returns a list of (precondition . status) pairs."
  (let ((hypotheses (extract-type-hypotheses (contract-types contract))))
    (mapcar (lambda (pre)
              (cons pre (verify-precondition pre hypotheses)))
            (contract-requires contract))))

(defun extract-type-hypotheses (types)
  "Extract hypotheses from type specifications."
  (let ((hyps nil))
    (dolist (ts types)
      (let ((name (type-spec-name ts))
            (type (type-spec-type ts)))
        ;; Generate type predicate
        (push `(typep ,name ',type) hyps)
        ;; Generate numeric bounds if applicable
        (when (and (listp type) (eq (car type) 'integer))
          (when (second type)
            (push `(>= ,name ,(second type)) hyps))
          (when (and (cddr type) (third type) (not (eq (third type) '*)))
            (push `(<= ,name ,(third type)) hyps)))))
    hyps))

;;; ============================================================================
;;; Postcondition Verification
;;; ============================================================================

(defun verify-postcondition (post hypotheses body-form)
  "Try to verify a postcondition.
   Returns :proven, :unknown, or (list :failed reason)."
  (declare (ignore body-form))
  (let ((expr (postcondition-expr post)))
    (let ((simplified (simplify-boolean expr)))
      (cond
        ((eq simplified t) :proven)
        ((null simplified) (list :failed "Postcondition simplifies to NIL"))
        ((member expr hypotheses :test #'equal) :proven)
        (t :unknown)))))

(defun verify-postconditions (contract body-form)
  "Verify all postconditions in a contract."
  (let ((hypotheses (append (extract-type-hypotheses (contract-types contract))
                            (mapcar #'precondition-expr (contract-requires contract)))))
    (mapcar (lambda (post)
              (cons post (verify-postcondition post hypotheses body-form)))
            (contract-ensures contract))))

;;; ============================================================================
;;; Invariant Verification
;;; ============================================================================

(defun verify-invariant (inv hypotheses)
  "Try to verify an invariant."
  (let ((expr (invariant-spec-expr inv)))
    (let ((simplified (simplify-boolean expr)))
      (cond
        ((eq simplified t) :proven)
        ((null simplified) (list :failed "Invariant simplifies to NIL"))
        ((member expr hypotheses :test #'equal) :proven)
        (t :unknown)))))

(defun verify-invariants (contract)
  "Verify all invariants in a contract."
  (let ((hypotheses (append (extract-type-hypotheses (contract-types contract))
                            (mapcar #'precondition-expr (contract-requires contract)))))
    (mapcar (lambda (inv)
              (cons inv (verify-invariant inv hypotheses)))
            (contract-invariants contract))))

;;; ============================================================================
;;; Full Contract Verification
;;; ============================================================================

(defun verify-contract (contract &key body-form)
  "Verify all aspects of a contract.
   Returns a report structure with verification results."
  (let ((type-results (verify-types contract))
        (pre-results (verify-preconditions contract))
        (post-results (verify-postconditions contract body-form))
        (inv-results (verify-invariants contract)))
    (list :contract (contract-name contract)
          :types type-results
          :preconditions pre-results
          :postconditions post-results
          :invariants inv-results
          :proven-count (count-if (lambda (r) (eq (cdr r) :proven))
                                  (append type-results pre-results
                                          post-results inv-results))
          :unknown-count (count-if (lambda (r) (eq (cdr r) :unknown))
                                   (append type-results pre-results
                                           post-results inv-results))
          :failed-count (count-if (lambda (r) (listp (cdr r)))
                                  (append type-results pre-results
                                          post-results inv-results)))))

(defun format-verification-report (report &optional (stream *standard-output*))
  "Format a verification report for human reading."
  (format stream "~&Contract: ~A~%" (getf report :contract))
  (format stream "  Proven: ~D, Unknown: ~D, Failed: ~D~%"
          (getf report :proven-count)
          (getf report :unknown-count)
          (getf report :failed-count))
  (when (getf report :types)
    (format stream "  Types:~%")
    (dolist (r (getf report :types))
      (format stream "    ~A: ~A~%" (type-spec-name (car r)) (cdr r))))
  (when (getf report :preconditions)
    (format stream "  Preconditions:~%")
    (dolist (r (getf report :preconditions))
      (format stream "    ~S: ~A~%" (precondition-expr (car r)) (cdr r))))
  (when (getf report :postconditions)
    (format stream "  Postconditions:~%")
    (dolist (r (getf report :postconditions))
      (format stream "    ~S: ~A~%" (postcondition-expr (car r)) (cdr r))))
  (when (getf report :invariants)
    (format stream "  Invariants:~%")
    (dolist (r (getf report :invariants))
      (format stream "    ~S: ~A~%" (invariant-spec-expr (car r)) (cdr r)))))

;;; ============================================================================
;;; Batch Verification
;;; ============================================================================

(defun verify-all-contracts ()
  "Verify all registered contracts. Returns summary statistics."
  (let ((total 0)
        (proven 0)
        (unknown 0)
        (failed 0)
        (reports nil))
    (maphash (lambda (name contract)
               (declare (ignore name))
               (when (contract-p contract)
                 (let ((report (verify-contract contract)))
                   (incf total)
                   (incf proven (getf report :proven-count))
                   (incf unknown (getf report :unknown-count))
                   (incf failed (getf report :failed-count))
                   (push report reports))))
             *contract-registry*)
    (list :total-contracts total
          :total-proven proven
          :total-unknown unknown
          :total-failed failed
          :reports (nreverse reports))))
