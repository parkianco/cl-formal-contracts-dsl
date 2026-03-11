;;;; test/test-contracts.lisp - Contract DSL Tests
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(defpackage #:formal-contracts-dsl.test
  (:use #:cl #:formal-contracts-dsl)
  (:export #:run-tests))

(in-package #:formal-contracts-dsl.test)

;;; ============================================================================
;;; Test Infrastructure
;;; ============================================================================

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro test (name &body body)
  "Define a test case."
  `(progn
     (incf *test-count*)
     (handler-case
         (progn
           ,@body
           (incf *pass-count*)
           (format t "~&PASS: ~A~%" ',name))
       (error (e)
         (incf *fail-count*)
         (format t "~&FAIL: ~A~%  Error: ~A~%" ',name e)))))

(defmacro assert-equal (expected actual)
  `(unless (equal ,expected ,actual)
     (error "Expected ~S but got ~S" ,expected ,actual)))

(defmacro assert-true (expr)
  `(unless ,expr
     (error "Expected ~S to be true" ',expr)))

(defmacro assert-signals (condition-type &body body)
  `(handler-case
       (progn ,@body
              (error "Expected ~S to be signaled" ',condition-type))
     (,condition-type () t)))

;;; ============================================================================
;;; Basic Type Tests
;;; ============================================================================

(test type-spec-creation
  (let ((ts (make-type-spec 'x 'integer)))
    (assert-true (type-spec-p ts))
    (assert-equal 'x (type-spec-name ts))
    (assert-equal 'integer (type-spec-type ts))))

(test precondition-creation
  (let ((pre (make-precondition '(> x 0) "x must be positive")))
    (assert-true (precondition-p pre))
    (assert-equal '(> x 0) (precondition-expr pre))
    (assert-equal "x must be positive" (precondition-message pre))))

(test postcondition-creation
  (let ((post (make-postcondition '(= result (* 2 x)) '(x))))
    (assert-true (postcondition-p post))
    (assert-equal '(= result (* 2 x)) (postcondition-expr post))
    (assert-equal '(x) (postcondition-old-bindings post))))

(test contract-creation
  (let ((contract (make-contract 'test-fn '(x y)
                                 :types '((x integer) (y integer))
                                 :requires '((> x 0))
                                 :ensures '((> result 0)))))
    (assert-true (contract-p contract))
    (assert-equal 'test-fn (contract-name contract))
    (assert-equal '(x y) (contract-lambda-list contract))))

;;; ============================================================================
;;; defun/spec Tests
;;; ============================================================================

;; Simple function with precondition
(defun/spec double-positive (x)
  "Double a positive integer."
  (:types ((x (integer 0 *))))
  (:requires (> x 0))
  (:ensures (= result (* 2 x)))
  (:body
    (* 2 x)))

(test defun-spec-basic
  (assert-equal 10 (double-positive 5))
  (assert-equal 2 (double-positive 1)))

(test defun-spec-contract-registered
  (assert-true (contract-defined-p 'double-positive))
  (let ((contract (get-contract 'double-positive)))
    (assert-true (contract-p contract))
    (assert-equal 'double-positive (contract-name contract))))

(test defun-spec-precondition-violation
  (let ((*contracts-enabled* t)
        (*contract-check-level* :full))
    (assert-signals precondition-violation
      (double-positive 0))))

;; Function with postcondition using old
(defun/spec increment-bounded (x max)
  "Increment x without exceeding max."
  (:types ((x integer) (max integer)))
  (:requires (<= x max))
  (:ensures (<= result max)
            (>= result (old x)))
  (:body
    (min (1+ x) max)))

(test defun-spec-old-binding
  (assert-equal 5 (increment-bounded 4 10))
  (assert-equal 10 (increment-bounded 10 10)))

;; Pure function
(defun/spec square (x)
  "Compute the square of x."
  (:types ((x number)))
  (:pure)
  (:ensures (= result (* x x)))
  (:body
    (* x x)))

(test defun-spec-pure
  (let ((contract (get-contract 'square)))
    (assert-true (contract-pure-p contract)))
  (assert-equal 25 (square 5))
  (assert-equal 4 (square -2)))

;;; ============================================================================
;;; Verification Tests
;;; ============================================================================

(test verify-types-constant
  (let* ((ts (make-type-spec 'x 'integer))
         (result (verify-type-spec ts 42 nil)))
    (assert-equal :proven result)))

(test simplify-boolean-and
  (assert-equal t (simplify-boolean '(and t t)))
  (assert-equal nil (simplify-boolean '(and t nil)))
  (assert-equal 'x (simplify-boolean '(and t x))))

(test simplify-boolean-or
  (assert-equal t (simplify-boolean '(or nil t)))
  (assert-equal nil (simplify-boolean '(or nil nil)))
  (assert-equal 'x (simplify-boolean '(or nil x))))

(test simplify-boolean-not
  (assert-equal nil (simplify-boolean '(not t)))
  (assert-equal t (simplify-boolean '(not nil))))

(test verify-contract-basic
  (let ((contract (make-contract 'test '(x)
                                 :requires '((> x 0))
                                 :ensures '((> result 0)))))
    (let ((report (verify-contract contract)))
      (assert-true (listp report))
      (assert-equal 'test (getf report :contract)))))

;;; ============================================================================
;;; Runtime Tests
;;; ============================================================================

(test contract-tracing
  (let ((*contract-trace* t)
        (output (make-string-output-stream)))
    (let ((*trace-output* output))
      (trace-contract-check :precondition 'test '(> x 0) t))
    (assert-true (> (length (get-output-stream-string output)) 0))))

(test contract-stats
  (clear-contract-stats)
  (record-contract-check 'test-fn :precondition t)
  (record-contract-check 'test-fn :precondition t)
  (record-contract-check 'test-fn :precondition nil)
  (let ((stats (get-contract-stats 'test-fn)))
    (assert-true (contract-stats-p stats))
    (assert-equal 3 (contract-stats-precondition-checks stats))
    (assert-equal 1 (contract-stats-precondition-failures stats))))

;;; ============================================================================
;;; Proof Obligation Tests
;;; ============================================================================

(test proof-obligation-creation
  (let ((ob (make-proof-obligation 'test-fn :precondition '(> x 0))))
    (assert-true (proof-obligation-p ob))
    (assert-equal 'test-fn (proof-obligation-function ob))
    (assert-equal :precondition (proof-obligation-kind ob))
    (assert-equal :pending (proof-obligation-status ob))))

(test proof-obligation-registry
  (clear-proof-obligations)
  (let ((ob (make-proof-obligation 'test-fn :precondition '(> x 0))))
    (register-proof-obligation ob)
    (let ((found (get-proof-obligations :function 'test-fn)))
      (assert-equal 1 (length found)))))

(test obligation-status-update
  (clear-proof-obligations)
  (let ((ob (make-proof-obligation 'test-fn :precondition '(> x 0))))
    (register-proof-obligation ob)
    (update-obligation-status (proof-obligation-id ob) :proven)
    (assert-equal :all-proven (obligation-status 'test-fn))))

;;; ============================================================================
;;; Control Macro Tests
;;; ============================================================================

(test with-contracts-disabled
  (with-contracts-disabled
    (assert-true (not *contracts-enabled*))))

(test with-contracts-enabled
  (let ((*contracts-enabled* nil))
    (with-contracts-enabled
      (assert-true *contracts-enabled*))))

;;; ============================================================================
;;; Test Runner
;;; ============================================================================

(defun run-tests ()
  "Run all tests and report results."
  (setf *test-count* 0
        *pass-count* 0
        *fail-count* 0)

  (format t "~&~%=== cl-formal-contracts-dsl Test Suite ===~%~%")

  ;; Run all tests
  (test type-spec-creation)
  (test precondition-creation)
  (test postcondition-creation)
  (test contract-creation)
  (test defun-spec-basic)
  (test defun-spec-contract-registered)
  (test defun-spec-precondition-violation)
  (test defun-spec-old-binding)
  (test defun-spec-pure)
  (test verify-types-constant)
  (test simplify-boolean-and)
  (test simplify-boolean-or)
  (test simplify-boolean-not)
  (test verify-contract-basic)
  (test contract-tracing)
  (test contract-stats)
  (test proof-obligation-creation)
  (test proof-obligation-registry)
  (test obligation-status-update)
  (test with-contracts-disabled)
  (test with-contracts-enabled)

  ;; Report summary
  (format t "~&~%=== Test Summary ===~%")
  (format t "Total: ~D, Passed: ~D, Failed: ~D~%"
          *test-count* *pass-count* *fail-count*)

  (if (zerop *fail-count*)
      (format t "~&All tests passed!~%")
      (format t "~&FAILURES DETECTED~%"))

  (values *pass-count* *fail-count*))
