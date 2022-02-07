;; paimon-search-test.el --- Search tests -*- lexical-binding: t; -*-

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

;; Search tests

;;; Code:

(require 'aio)
(require 'closql)
(require 'ert)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-search)
(require 'paimon-search-job)
(require 'paimon-test)

(ert-deftest paimon-search--index-names->command-test ()
  (should (equal "" (paimon-search--index-names->command nil)))
  (should (equal "index=_internal OR index=main" (paimon-search--index-names->command '("_internal" "main")))))

(ert-deftest paimon-search--search-command-test ()
  (should (equal "search *" (paimon-search--search-command "*")))
  (should (equal "search index=a OR index=b *" (paimon-search--search-command "*" :indexes'("a" "b")))))

(ert-deftest paimon-search--index-names-test ()
  (paimon-test-with-profile db profile
    (aio-wait-for (paimon-data-indexes-synchronize db profile))
    (should (equal '("_audit"
                     "_internal"
                     "_introspection"
                     "_telemetry"
                     "_thefishbucket"
                     "history"
                     "main"
                     "splunklogger"
                     "summary")
                   (paimon-search--index-names db profile)))))

(ert-deftest paimon-search-create-test ()
  (paimon-test-with-db db
    (save-window-excursion
      (let* ((_profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (should (cl-typep job 'paimon-search-job))
        (should (equal "DONE" (paimon-search-job-dispatch-state job)))
        (should (equal "search index=_internal" (paimon-search-job-search job)))
        (should (cl-typep (paimon-search-job-fields job) 'vector))
        (should (not (zerop (length (paimon-search-job-fields job)))))
        (should (seq-every-p #'hash-table-p (paimon-search-job-fields job)))
        (should (not (zerop (paimon-search-job-results-count job))))
        (should (not (zerop (length (paimon-search-results-by-job job)))))
        (with-current-buffer (get-buffer paimon-search-jobs-buffer-name)
          (should (equal (oref job id) (oref (paimon-search-job-under-point) id)))
          (should (equal "DONE" (seq-elt (tabulated-list-get-entry) 3))))
        (let ((saved (paimon-search-job-by-id db (oref job id))))
          (should (cl-typep (paimon-search-job-fields saved) 'vector)))))))
