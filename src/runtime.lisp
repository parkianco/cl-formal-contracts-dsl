;;;; src/runtime.lisp - Runtime Contract Checking
;;;;
;;;; Runtime contract checking for debug builds.
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(in-package #:formal-contracts-dsl)

;;; ============================================================================
;;; Runtime Assertion Helpers
;;; ============================================================================

(defun assert-precondition (function-name expr value &optional message)
  "Assert that a precondition holds. Signals precondition-violation if not."
  (unless value
    (error 'precondition-violation
           :function function-name
           :expr expr
           :message (or message "Precondition failed"))))

(defun assert-postcondition (function-name expr value &optional message)
  "Assert that a postcondition holds. Signals postcondition-violation if not."
  (unless value
    (error 'postcondition-violation
           :function function-name
           :expr expr
           :message (or message "Postcondition failed"))))

(defun assert-invariant (function-name expr value &optional message)
  "Assert that an invariant holds. Signals invariant-violation if not."
  (unless value
    (error 'invariant-violation
           :function function-name
           :expr expr
           :message (or message "Invariant failed"))))

;;; ============================================================================
;;; Old Value Capture
;;; ============================================================================

(defmacro capture-old-values (bindings &body body)
  "Capture values of variables before executing BODY.
   BINDINGS is a list of (old-name original-expr) pairs."
  `(let ,(mapcar (lambda (b)
                   (list (first b) (second b)))
                 bindings)
     ,@body))

;;; ============================================================================
;;; Runtime Check Wrapper
;;; ============================================================================

(defmacro with-runtime-checks ((function-name contract) &body body)
  "Execute BODY with runtime contract checking."
  (let ((result-var (gensym "RESULT"))
        (contract-var (gensym "CONTRACT")))
    `(let ((,contract-var ,contract))
       ;; Check preconditions
       (when (and *contracts-enabled*
                  (member *contract-check-level* '(:pre :post :full)))
         (dolist (pre (contract-requires ,contract-var))
           (unless (eval pre)
             (error 'precondition-violation
                    :function ',function-name
                    :expr pre
                    :message "Precondition failed"))))
       ;; Execute body and capture result
       (let ((,result-var (progn ,@body)))
         ;; Check postconditions
         (when (and *contracts-enabled*
                    (member *contract-check-level* '(:post :full)))
           (dolist (post (contract-ensures ,contract-var))
             (let ((result ,result-var))
               (declare (ignorable result))
               (unless (eval post)
                 (error 'postcondition-violation
                        :function ',function-name
                        :expr post
                        :message "Postcondition failed")))))
         ,result-var))))

;;; ============================================================================
;;; Contract Tracing
;;; ============================================================================

(defvar *contract-trace* nil
  "When non-nil, trace contract checks to *trace-output*.")

(defun trace-contract-check (kind function-name expr result)
  "Trace a contract check if tracing is enabled."
  (when *contract-trace*
    (format *trace-output* "~&[CONTRACT] ~A ~A: ~S => ~A~%"
            kind function-name expr result)))

(defmacro with-contract-tracing (&body body)
  "Execute BODY with contract tracing enabled."
  `(let ((*contract-trace* t))
     ,@body))

;;; ============================================================================
;;; Contract Statistics
;;; ============================================================================

(defvar *contract-stats* (make-hash-table :test 'eq)
  "Statistics about contract checks.")

(defstruct contract-stats
  "Statistics for a single function's contract checks."
  (precondition-checks 0 :type integer)
  (precondition-failures 0 :type integer)
  (postcondition-checks 0 :type integer)
  (postcondition-failures 0 :type integer)
  (invariant-checks 0 :type integer)
  (invariant-failures 0 :type integer)
  (total-time-ns 0 :type integer))

(defun record-contract-check (function-name kind success-p &optional time-ns)
  "Record a contract check for statistics."
  (let ((stats (or (gethash function-name *contract-stats*)
                   (setf (gethash function-name *contract-stats*)
                         (make-contract-stats)))))
    (ecase kind
      (:precondition
       (incf (contract-stats-precondition-checks stats))
       (unless success-p
         (incf (contract-stats-precondition-failures stats))))
      (:postcondition
       (incf (contract-stats-postcondition-checks stats))
       (unless success-p
         (incf (contract-stats-postcondition-failures stats))))
      (:invariant
       (incf (contract-stats-invariant-checks stats))
       (unless success-p
         (incf (contract-stats-invariant-failures stats)))))
    (when time-ns
      (incf (contract-stats-total-time-ns stats) time-ns))))

(defun get-contract-stats (function-name)
  "Get contract statistics for a function."
  (gethash function-name *contract-stats*))

(defun clear-contract-stats ()
  "Clear all contract statistics."
  (clrhash *contract-stats*))

(defun summarize-contract-stats ()
  "Return a summary of all contract statistics."
  (let ((total-pre-checks 0)
        (total-pre-failures 0)
        (total-post-checks 0)
        (total-post-failures 0)
        (total-inv-checks 0)
        (total-inv-failures 0)
        (functions 0))
    (maphash (lambda (name stats)
               (declare (ignore name))
               (incf functions)
               (incf total-pre-checks (contract-stats-precondition-checks stats))
               (incf total-pre-failures (contract-stats-precondition-failures stats))
               (incf total-post-checks (contract-stats-postcondition-checks stats))
               (incf total-post-failures (contract-stats-postcondition-failures stats))
               (incf total-inv-checks (contract-stats-invariant-checks stats))
               (incf total-inv-failures (contract-stats-invariant-failures stats)))
             *contract-stats*)
    (list :functions functions
          :precondition-checks total-pre-checks
          :precondition-failures total-pre-failures
          :postcondition-checks total-post-checks
          :postcondition-failures total-post-failures
          :invariant-checks total-inv-checks
          :invariant-failures total-inv-failures)))

;;; ============================================================================
;;; Soft Contract Mode
;;; ============================================================================

(defvar *contract-soft-mode* nil
  "When non-nil, contract violations are warnings instead of errors.")

(defmacro with-soft-contracts (&body body)
  "Execute BODY in soft contract mode (violations become warnings)."
  `(handler-bind
       ((contract-violation
          (lambda (c)
            (warn "~A" c)
            (continue))))
     (let ((*contract-soft-mode* t))
       ,@body)))

;;; ============================================================================
;;; Contract Debugging
;;; ============================================================================

(defun explain-contract-violation (condition)
  "Provide detailed explanation of a contract violation."
  (format nil "Contract Violation Details:
  Function: ~A
  Type: ~A
  Expression: ~S
  Message: ~A

To debug:
  1. Check the preconditions are satisfied before calling
  2. Verify type constraints on arguments
  3. Use (trace ~A) to see call sequence
  4. Enable contract tracing: (setf *contract-trace* t)"
          (contract-violation-function condition)
          (contract-violation-type condition)
          (contract-violation-expression condition)
          (contract-violation-message condition)
          (contract-violation-function condition)))
