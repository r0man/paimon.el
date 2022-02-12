;; paimon-data-index-test.el --- Data index tests -*- lexical-binding: t; -*-

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

;; Data index tests

;;; Code:

(require 'ert)
(require 'paimon-data-index)
(require 'paimon-db)
(require 'paimon-test)

(ert-deftest paimon-data-indexes-test ()
  (paimon-test-with-profile db profile
    (should (not (paimon-data-indexes db)))
    (aio-wait-for (paimon-data-indexes-synchronize db profile))
    (should (paimon-data-indexes db))))

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
