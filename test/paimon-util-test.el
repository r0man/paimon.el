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
