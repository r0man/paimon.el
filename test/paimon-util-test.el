;; paimon-util-test.el --- Paimon util tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Paimon util tests

;;; Code:

(require 'ert)
(require 'paimon-util)

(ert-deftest paimon--human-name-test ()
  (should (null (paimon--human-name nil)))
  (should (equal "Eventtype Color" (paimon--human-name "_eventtype_color"))))

(ert-deftest paimon--transient-arg-multi-value-test ()
  (let ((args '(("--indexes=" "staging" "main") "--status-buckets=10") ))
    (should (not (paimon--transient-arg-multi-value "--status-buckets" args)))
    (should (equal '("staging" "main") (paimon--transient-arg-multi-value "--indexes=" '(("--indexes=" "staging" "main")))))))
