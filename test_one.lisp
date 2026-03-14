;; Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
;; SPDX-License-Identifier: BSD-3-Clause

(load "cl-formal-contracts-dsl.asd")
(handler-case
  (progn
    (asdf:test-system :cl-formal-contracts-dsl/test)
    (format t "PASS~%"))
  (error (e)
    (format t "FAIL~%")))
(quit)
