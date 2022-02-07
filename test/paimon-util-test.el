;; paimon-util-test.el --- Paimon util tests -*- lexical-binding: t; -*-

;; Copyright (C) 2022 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses.

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
