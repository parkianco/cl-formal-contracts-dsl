;;;; src/registry.lisp - Proof Obligation Registry
;;;;
;;;; Tracks proof obligations generated from contracts and their status.
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(in-package #:formal-contracts-dsl)

;;; ============================================================================
;;; Obligation Registration
;;; ============================================================================

(defun register-proof-obligation (obligation)
  "Register a proof obligation in the global registry."
  (setf (gethash (proof-obligation-id obligation) *proof-obligation-registry*)
        obligation))

(defun get-proof-obligations (&key function kind status)
  "Get proof obligations, optionally filtered."
  (let ((obligations nil))
    (maphash (lambda (id ob)
               (declare (ignore id))
               (when (and (or (null function) (eq (proof-obligation-function ob) function))
                          (or (null kind) (eq (proof-obligation-kind ob) kind))
                          (or (null status) (eq (proof-obligation-status ob) status)))
                 (push ob obligations)))
             *proof-obligation-registry*)
    (nreverse obligations)))

(defun clear-proof-obligations ()
  "Clear all registered proof obligations."
  (clrhash *proof-obligation-registry*))

;;; ============================================================================
;;; Obligation Status Management
;;; ============================================================================

(defun obligation-status (function-name &optional kind)
  "Get the status of proof obligations for a function.
   Returns :all-proven, :pending, :mixed, or :none."
  (let ((obligations (get-proof-obligations :function function-name :kind kind)))
    (cond
      ((null obligations) :none)
      ((every (lambda (ob) (eq (proof-obligation-status ob) :proven)) obligations)
       :all-proven)
      ((every (lambda (ob) (eq (proof-obligation-status ob) :pending)) obligations)
       :pending)
      (t :mixed))))

(defun update-obligation-status (id new-status &key proof-file skip-reason)
  "Update the status of a proof obligation."
  (let ((ob (gethash id *proof-obligation-registry*)))
    (when ob
      (setf (proof-obligation-status ob) new-status)
      (when proof-file
        (setf (proof-obligation-proof-file ob) proof-file))
      (when skip-reason
        (setf (proof-obligation-skip-reason ob) skip-reason))
      (when (eq new-status :proven)
        (setf (proof-obligation-verified-at ob) (get-universal-time)))
      ob)))

(defun mark-proven (function-name &key kind proof-file)
  "Mark obligations as proven for a function."
  (dolist (ob (get-proof-obligations :function function-name :kind kind))
    (update-obligation-status (proof-obligation-id ob) :proven
                              :proof-file proof-file)))

(defun mark-axiom (function-name &key kind)
  "Mark obligations as axioms (assumed true)."
  (dolist (ob (get-proof-obligations :function function-name :kind kind))
    (update-obligation-status (proof-obligation-id ob) :axiom)))

(defun mark-skip (function-name reason &key kind)
  "Mark obligations as skipped with a justification."
  (dolist (ob (get-proof-obligations :function function-name :kind kind))
    (update-obligation-status (proof-obligation-id ob) :skip
                              :skip-reason reason)))

;;; ============================================================================
;;; Obligation Reporting
;;; ============================================================================

(defun check-all-proof-obligations ()
  "Report on all unproven obligations. Returns (values total unproven)."
  (let ((total 0)
        (unproven 0)
        (by-status (make-hash-table)))
    (maphash (lambda (id ob)
               (declare (ignore id))
               (incf total)
               (incf (gethash (proof-obligation-status ob) by-status 0))
               (when (eq (proof-obligation-status ob) :pending)
                 (incf unproven)))
             *proof-obligation-registry*)
    (format t "~&=== Proof Obligation Report ===~%")
    (format t "Total obligations: ~D~%" total)
    (format t "  Proven:  ~D~%" (gethash :proven by-status 0))
    (format t "  Pending: ~D~%" (gethash :pending by-status 0))
    (format t "  Axiom:   ~D~%" (gethash :axiom by-status 0))
    (format t "  Skip:    ~D~%" (gethash :skip by-status 0))
    (format t "  Failed:  ~D~%" (gethash :failed by-status 0))
    (when (> unproven 0)
      (format t "~%UNPROVEN obligations:~%")
      (maphash (lambda (id ob)
                 (declare (ignore id))
                 (when (eq (proof-obligation-status ob) :pending)
                   (format t "  ~A [~A]: ~S~%"
                           (proof-obligation-function ob)
                           (proof-obligation-kind ob)
                           (proof-obligation-formula ob))))
               *proof-obligation-registry*))
    (values total unproven)))

(defun obligations-by-function ()
  "Group obligations by function name."
  (let ((by-function (make-hash-table :test 'equal)))
    (maphash (lambda (id ob)
               (declare (ignore id))
               (push ob (gethash (proof-obligation-function ob) by-function)))
             *proof-obligation-registry*)
    by-function))

(defun obligations-by-file ()
  "Group obligations by source file."
  (let ((by-file (make-hash-table :test 'equal)))
    (maphash (lambda (id ob)
               (declare (ignore id))
               (let ((file (proof-obligation-source-file ob)))
                 (when file
                   (push ob (gethash (namestring file) by-file)))))
             *proof-obligation-registry*)
    by-file))

;;; ============================================================================
;;; ACL2 Export
;;; ============================================================================

(defun export-obligations-to-acl2 (output-path)
  "Generate ACL2 proof stubs for pending obligations."
  (ensure-directories-exist output-path)
  (let ((pending (get-proof-obligations :status :pending)))
    (with-open-file (out output-path :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
      (format out ";;;; Auto-generated ACL2 proof stubs~%")
      (format out ";;;; Generated: ~A~%" (get-universal-time))
      (format out ";;;; Total pending obligations: ~D~%~%" (length pending))
      (format out "(in-package \"ACL2\")~%~%")
      (format out "(include-book \"contracts-book\")~%~%")
      ;; Group by function
      (let ((by-function (make-hash-table :test 'equal)))
        (dolist (ob pending)
          (push ob (gethash (proof-obligation-function ob) by-function)))
        (maphash (lambda (fn obs)
                   (format out "~%;; === ~A ===~%" fn)
                   (dolist (ob (reverse obs))
                     (export-single-obligation out ob)))
                 by-function)))
    (format t "~&Exported ~D proof stubs to ~A~%" (length pending) output-path)
    output-path))

(defun export-single-obligation (stream ob)
  "Export a single obligation as an ACL2 defthm stub."
  (format stream "~%;; ~A (~A)~%"
          (proof-obligation-id ob)
          (proof-obligation-kind ob))
  (format stream ";; Source: ~A~%"
          (or (proof-obligation-source-file ob) "unknown"))
  (format stream "(defthm ~A-~A~%"
          (string-downcase (symbol-name (proof-obligation-function ob)))
          (proof-obligation-kind ob))
  (format stream "  ~A~%" (formula-to-acl2 (proof-obligation-formula ob)))
  (format stream "  :hints ((\"Goal\" :in-theory (enable ~A))))~%~%"
          (string-downcase (symbol-name (proof-obligation-function ob)))))

(defun formula-to-acl2 (formula)
  "Convert a Lisp formula to ACL2 syntax."
  (cond
    ((null formula) 'nil)
    ((eq formula t) 't)
    ((symbolp formula) (intern (string-downcase (symbol-name formula)) "ACL2"))
    ((atom formula) formula)
    ;; Convert type predicates
    ((and (eq (car formula) 'typep) (= (length formula) 3))
     (let ((var (second formula))
           (type (third formula)))
       (type-to-acl2-predicate var type)))
    ;; Recursive conversion
    (t (mapcar #'formula-to-acl2 formula))))

(defun type-to-acl2-predicate (var type)
  "Convert a type check to an ACL2 predicate."
  (cond
    ((eq type 'integer) `(integerp ,var))
    ((and (listp type) (eq (car type) 'integer))
     (let ((lo (second type))
           (hi (third type)))
       (cond
         ((and lo hi (not (eq hi '*)))
          `(and (integerp ,var) (>= ,var ,lo) (<= ,var ,hi)))
         (lo `(and (integerp ,var) (>= ,var ,lo)))
         (t `(integerp ,var)))))
    ((eq type 'string) `(stringp ,var))
    ((eq type 'list) `(true-listp ,var))
    ((eq type 'boolean) `(booleanp ,var))
    (t `(,(intern (format nil "~A-P" type) "ACL2") ,var))))

;;; ============================================================================
;;; Obligation Import
;;; ============================================================================

(defun import-proof-results (proof-file)
  "Import proof results from an ACL2 certification log."
  (with-open-file (in proof-file :direction :input)
    (let ((line nil)
          (updated 0))
      (loop while (setf line (read-line in nil nil))
            do (when (search "Summary" line)
                 ;; Parse theorem names and status
                 (when (search "SUCCEEDED" line)
                   (let ((name (extract-theorem-name line)))
                     (when name
                       (mark-proven-by-theorem-name name)
                       (incf updated))))))
      (format t "~&Updated ~D obligations from ~A~%" updated proof-file)
      updated)))

(defun extract-theorem-name (line)
  "Extract a theorem name from a line of ACL2 output."
  (let ((start (search "DEFTHM " line :test #'char-equal)))
    (when start
      (let* ((name-start (+ start 7))
             (name-end (position #\Space line :start name-start)))
        (when name-end
          (subseq line name-start name-end))))))

(defun mark-proven-by-theorem-name (theorem-name)
  "Mark obligations matching a theorem name as proven."
  (maphash (lambda (id ob)
             (declare (ignore ob))
             (when (search theorem-name id :test #'char-equal)
               (update-obligation-status id :proven)))
           *proof-obligation-registry*))

;;; ============================================================================
;;; Obligation Dashboard
;;; ============================================================================

(defun generate-obligation-dashboard (&optional (output-path "obligation-report.html"))
  "Generate an HTML dashboard of proof obligations."
  (with-open-file (out output-path :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
    (format out "<!DOCTYPE html>~%<html><head>~%")
    (format out "<title>Proof Obligations Dashboard</title>~%")
    (format out "<style>~%")
    (format out "body { font-family: sans-serif; margin: 20px; }~%")
    (format out "table { border-collapse: collapse; width: 100%; }~%")
    (format out "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }~%")
    (format out ".proven { background-color: #90EE90; }~%")
    (format out ".pending { background-color: #FFE4B5; }~%")
    (format out ".skip { background-color: #E0E0E0; }~%")
    (format out ".failed { background-color: #FFB6C1; }~%")
    (format out "</style></head><body>~%")
    (format out "<h1>Proof Obligation Dashboard</h1>~%")
    (format out "<p>Generated: ~A</p>~%" (get-universal-time))
    ;; Summary
    (let ((total 0) (proven 0) (pending 0) (skip 0) (failed 0))
      (maphash (lambda (id ob)
                 (declare (ignore id))
                 (incf total)
                 (ecase (proof-obligation-status ob)
                   (:proven (incf proven))
                   (:pending (incf pending))
                   ((:axiom :skip) (incf skip))
                   (:failed (incf failed))))
               *proof-obligation-registry*)
      (format out "<h2>Summary</h2>~%")
      (format out "<p>Total: ~D | Proven: ~D | Pending: ~D | Skip/Axiom: ~D | Failed: ~D</p>~%"
              total proven pending skip failed))
    ;; Table
    (format out "<h2>Obligations</h2>~%")
    (format out "<table><tr><th>Function</th><th>Kind</th><th>Formula</th><th>Status</th></tr>~%")
    (maphash (lambda (id ob)
               (declare (ignore id))
               (format out "<tr class=\"~A\">~%"
                       (string-downcase (symbol-name (proof-obligation-status ob))))
               (format out "<td>~A</td>~%" (proof-obligation-function ob))
               (format out "<td>~A</td>~%" (proof-obligation-kind ob))
               (format out "<td><code>~S</code></td>~%" (proof-obligation-formula ob))
               (format out "<td>~A</td></tr>~%" (proof-obligation-status ob)))
             *proof-obligation-registry*)
    (format out "</table></body></html>~%"))
  (format t "~&Dashboard generated: ~A~%" output-path)
  output-path)
