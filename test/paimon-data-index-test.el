;; paimon-data-index-test.el --- Data index tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Data index tests

;;; Code:

(require 'ert)
(require 'paimon-data-index)
(require 'paimon-db)
(require 'paimon-test)

(ert-deftest paimon-data-indexes-load-test ()
  (paimon-test-with-profile _db profile
    (let ((data-indexes (aio-wait-for (paimon-data-indexes-load profile))))
      (should (equal '("_audit"
                       "_internal"
                       "_introspection"
                       "_telemetry"
                       "_thefishbucket"
                       "history"
                       "main"
                       "splunklogger"
                       "summary")
                     (seq-map #'paimon-data-index-name data-indexes))))))

(ert-deftest paimon-data-indexes-synchronize ()
  (paimon-test-with-profile db profile
    (let ((data-indexes (aio-wait-for (paimon-data-indexes-synchronize db profile))))
      (should (equal '("_audit"
                       "_internal"
                       "_introspection"
                       "_telemetry"
                       "_thefishbucket"
                       "history"
                       "main"
                       "splunklogger"
                       "summary")
                     (seq-map #'paimon-data-index-name data-indexes))))))
