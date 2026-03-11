;;;; src/spec.lisp - Definition Macros
;;;;
;;;; Provides defun/spec, defmethod/spec, and related macros for
;;;; defining functions with formally-verified contracts.
;;;;
;;;; Copyright (c) 2025 CLPIC Project
;;;; MIT License

(in-package #:formal-contracts-dsl)

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

(defun extract-declarations (body)
  "Extract declaration forms from the start of BODY.
   Returns (values declarations remaining-body)."
  (let ((decls nil))
    (loop while (and body
                     (consp (car body))
                     (eq (caar body) 'declare))
          do (push (pop body) decls))
    (values (nreverse decls) body)))

(defun generate-type-declarations (types lambda-list)
  "Generate type declarations from type specs."
  (let ((decls nil))
    (dolist (ts types)
      (let ((name (type-spec-name ts))
            (type (type-spec-type ts)))
        (when (member name lambda-list)
          (push `(type ,type ,name) decls))))
    (when decls
      `((declare ,@(nreverse decls))))))

(defun generate-precondition-checks (function-name requires)
  "Generate runtime precondition check forms."
  (when requires
    `((when (and *contracts-enabled*
                 (member *contract-check-level* '(:pre :post :full)))
        ,@(mapcar (lambda (pre)
                    `(unless ,(precondition-expr pre)
                       (error 'precondition-violation
                              :function ',function-name
                              :expr ',(precondition-expr pre)
                              :message ,(or (precondition-message pre)
                                           "Precondition failed"))))
                  requires)))))

(defun generate-old-bindings (ensures)
  "Generate let bindings for (old var) references."
  (let ((bindings nil))
    (dolist (post ensures)
      (dolist (var (postcondition-old-bindings post))
        (pushnew `(,(intern (format nil "OLD-~A" var)) ,var)
                 bindings :key #'car)))
    bindings))

(defun substitute-old-references (expr)
  "Replace (old var) with OLD-VAR in an expression."
  (cond
    ((atom expr) expr)
    ((and (eq (car expr) 'old) (symbolp (cadr expr)))
     (intern (format nil "OLD-~A" (cadr expr))))
    (t (mapcar #'substitute-old-references expr))))

(defun subst-result-symbol (new-sym expr)
  "Replace any symbol named RESULT in EXPR with NEW-SYM, regardless of package."
  (cond
    ((and (symbolp expr) (string= (symbol-name expr) "RESULT")) new-sym)
    ((consp expr)
     (let ((new-car (subst-result-symbol new-sym (car expr)))
           (new-cdr (subst-result-symbol new-sym (cdr expr))))
       (if (and (eq new-car (car expr)) (eq new-cdr (cdr expr)))
           expr
           (cons new-car new-cdr))))
    (t expr)))

(defun generate-postcondition-checks (function-name ensures result-var)
  "Generate runtime postcondition check forms."
  (when ensures
    `((when (and *contracts-enabled*
                 (member *contract-check-level* '(:post :full)))
        ,@(mapcar (lambda (post)
                    (let* ((check-expr (substitute-old-references
                                        (postcondition-expr post)))
                           (fixed-expr (subst-result-symbol result-var check-expr)))
                      `(unless ,fixed-expr
                         (error 'postcondition-violation
                                :function ',function-name
                                :expr ',(postcondition-expr post)
                                :message "Postcondition failed"))))
                  ensures)))))

;;; ============================================================================
;;; Proof Obligation Registration
;;; ============================================================================

(defun register-contract-obligations (function-name contract)
  "Register all proof obligations from a contract."
  (let ((file *compile-file-truename*))
    ;; Type obligations
    (dolist (ts (contract-types contract))
      (register-proof-obligation
       (make-proof-obligation
        function-name :type
        `(typep ,(type-spec-name ts) ',(type-spec-type ts))
        :source-file file)))
    ;; Precondition obligations
    (dolist (pre (contract-requires contract))
      (register-proof-obligation
       (make-proof-obligation
        function-name :precondition
        (precondition-expr pre)
        :source-file file)))
    ;; Postcondition obligations
    (dolist (post (contract-ensures contract))
      (register-proof-obligation
       (make-proof-obligation
        function-name :postcondition
        (postcondition-expr post)
        :source-file file)))
    ;; Invariant obligations
    (dolist (inv (contract-invariants contract))
      (register-proof-obligation
       (make-proof-obligation
        function-name :invariant
        (invariant-spec-expr inv)
        :source-file file)))))

;;; ============================================================================
;;; defun/spec Macro
;;; ============================================================================

(defmacro defun/spec (name lambda-list &body body)
  "Define a function with statically-verified contracts and proof obligations.

Syntax:
  (defun/spec name (args...)
    [documentation-string]
    (:types ((arg1 type1) (arg2 type2) ...))
    (:requires precondition1 precondition2 ...)
    (:ensures postcondition1 postcondition2 ...)
    (:invariants invariant1 ...)
    (:modifies place1 place2 ...)
    (:pure)
    (:body form1 form2 ...)
    ; or just forms if no :body clause
    form1 form2 ...)

Special forms in postconditions:
  RESULT - the return value of the function
  (old VAR) - the value of VAR before the function call

Example:
  (defun/spec deposit (account amount)
    (:types ((account account) (amount (integer 0 *))))
    (:requires (account-p account)
               (>= amount 0))
    (:ensures (= (account-balance account)
                 (+ (old (account-balance account)) amount)))
    (:body
      (incf (account-balance account) amount)))"

  (multiple-value-bind (types requires ensures invariants modifies pure-p doc real-body)
      (parse-spec-body body)
    ;; Extract user declarations from real-body (they can't go inside progn)
    (multiple-value-bind (user-decls exec-body)
        (extract-declarations real-body)
      (let ((result-var (gensym "RESULT"))
            (old-bindings (generate-old-bindings ensures)))
        ;; Create and register contract
        `(progn
           ;; Register contract at compile/load time
           (eval-when (:compile-toplevel :load-toplevel :execute)
             (let ((contract (make-contract
                              ',name ',lambda-list
                              :types ',types
                              :requires ',(mapcar #'precondition-expr requires)
                              :ensures ',(mapcar #'postcondition-expr ensures)
                              :invariants ',(mapcar #'invariant-spec-expr invariants)
                              :modifies ',modifies
                              :pure-p ,pure-p
                              :documentation ,doc
                              :source-file *compile-file-truename*)))
               (setf (gethash ',name *contract-registry*) contract)
               (register-contract-obligations ',name contract)))

           ;; Define the actual function
           (defun ,name ,lambda-list
             ,@(when doc (list doc))
             ,@(generate-type-declarations types lambda-list)
             ,@user-decls  ;; User declarations at function level
             ;; Capture old values for postconditions
             ,@(when old-bindings
                 `((let ,old-bindings
                     (declare (ignorable ,@(mapcar #'car old-bindings)))
                     ,@(generate-precondition-checks name requires)
                     (let ((,result-var (progn ,@exec-body)))
                       ,@(generate-postcondition-checks name ensures result-var)
                       ,result-var))))
             ;; No old bindings needed
             ,@(unless old-bindings
                 `(,@(generate-precondition-checks name requires)
                   (let ((,result-var (progn ,@exec-body)))
                     ,@(generate-postcondition-checks name ensures result-var)
                     ,result-var)))))))))

;;; ============================================================================
;;; defmethod/spec Macro
;;; ============================================================================

(defmacro defmethod/spec (name &rest args)
  "Define a method with contracts. Syntax similar to defun/spec.

Example:
  (defmethod/spec validate ((tx transaction) (utxos utxo-set))
    (:requires (transaction-p tx))
    (:ensures (or result (not (valid-transaction-p tx utxos))))
    (:body
      ...))"
  ;; Parse method qualifiers and specializers
  (let ((qualifiers nil)
        (lambda-list nil)
        (body nil))
    ;; Collect qualifiers (keywords before lambda-list)
    (loop while (and args (not (listp (car args))))
          do (push (pop args) qualifiers))
    (setf qualifiers (nreverse qualifiers))
    ;; Lambda-list is next
    (setf lambda-list (pop args))
    (setf body args)

    (multiple-value-bind (types requires ensures invariants modifies pure-p doc real-body)
        (parse-spec-body body)
      ;; Extract user declarations from real-body (they can't go inside progn)
      (multiple-value-bind (user-decls exec-body)
          (extract-declarations real-body)
        (let ((result-var (gensym "RESULT"))
              (old-bindings (generate-old-bindings ensures))
              ;; Extract plain parameter names
              (plain-lambda-list (mapcar (lambda (p)
                                           (if (listp p) (car p) p))
                                         lambda-list)))
          `(progn
             ;; Register contract
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (let ((contract (make-contract
                                ',name ',plain-lambda-list
                                :types ',types
                                :requires ',(mapcar #'precondition-expr requires)
                                :ensures ',(mapcar #'postcondition-expr ensures)
                                :invariants ',(mapcar #'invariant-spec-expr invariants)
                                :modifies ',modifies
                                :pure-p ,pure-p
                                :documentation ,doc
                                :source-file *compile-file-truename*)))
                 (setf (gethash (list ',name ',lambda-list) *contract-registry*) contract)
                 (register-contract-obligations ',name contract)))

             ;; Define the method
             (defmethod ,name ,@qualifiers ,lambda-list
               ,@(when doc (list doc))
               ,@(generate-type-declarations types plain-lambda-list)
               ,@user-decls  ;; User declarations at method level
               ,@(when old-bindings
                   `((let ,old-bindings
                       (declare (ignorable ,@(mapcar #'car old-bindings)))
                       ,@(generate-precondition-checks name requires)
                       (let ((,result-var (progn ,@exec-body)))
                         ,@(generate-postcondition-checks name ensures result-var)
                         ,result-var))))
               ,@(unless old-bindings
                   `(,@(generate-precondition-checks name requires)
                     (let ((,result-var (progn ,@exec-body)))
                       ,@(generate-postcondition-checks name ensures result-var)
                       ,result-var))))))))))

;;; ============================================================================
;;; Contract Control Macros
;;; ============================================================================

(defmacro with-contracts-enabled (&body body)
  "Execute BODY with contract checking enabled."
  `(let ((*contracts-enabled* t))
     ,@body))

(defmacro with-contracts-disabled (&body body)
  "Execute BODY with contract checking disabled."
  `(let ((*contracts-enabled* nil))
     ,@body))

(defmacro define-contract (name lambda-list &body clauses)
  "Define a standalone contract that can be attached to functions.

Example:
  (define-contract positive-transfer (from to amount)
    (:requires (> amount 0)
               (not (eq from to)))
    (:ensures (= (+ (balance from) (balance to))
                 (old (+ (balance from) (balance to))))))"
  (multiple-value-bind (types requires ensures invariants modifies pure-p doc _body)
      (parse-spec-body clauses)
    (declare (ignore _body))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ',name *contract-registry*)
             (make-contract ',name ',lambda-list
                            :types ',types
                            :requires ',(mapcar #'precondition-expr requires)
                            :ensures ',(mapcar #'postcondition-expr ensures)
                            :invariants ',(mapcar #'invariant-spec-expr invariants)
                            :modifies ',modifies
                            :pure-p ,pure-p
                            :documentation ,doc)))))

;;; ============================================================================
;;; Contract Query Functions
;;; ============================================================================

(defun get-contract (function-name)
  "Get the contract for a function, if any."
  (gethash function-name *contract-registry*))

(defun list-contracts ()
  "List all registered contract names."
  (let ((names nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (push k names))
             *contract-registry*)
    (nreverse names)))

(defun contract-defined-p (function-name)
  "Check if a contract is defined for a function."
  (not (null (gethash function-name *contract-registry*))))
