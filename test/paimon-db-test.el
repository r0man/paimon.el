;; paimon-db-test.el --- Paimon database tests -*- lexical-binding: t; -*-

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

;; Paimon database tests

;;; Code:

(require 'closql)
(require 'ert)
(require 'paimon)
(require 'paimon-db)
(require 'paimon-search-job)
(require 'paimon-test)

(ert-deftest paimon-db-reset-test ()
  (paimon-test-with-db db
    (should db)
    (should (paimon-db-reset))))

(ert-deftest paimon-db-insert-search-job-test ()
  (paimon-test-with-db db
    (let* ((profile (closql-insert db (paimon-profile)))
           (job (paimon-search-job :id "1" :profile-id (oref profile id))))
      (should (closql-insert db job t)))))

(ert-deftest paimon-db-insert-search-result-test ()
  (paimon-test-with-db db
    (let* ((profile (closql-insert db (paimon-profile)))
           (job (closql-insert db (paimon-search-job :id "1" :profile-id (oref profile id))))
           (result (paimon-search-result :id ["1" 1] :job-id (oref job id) :offset 0)))
      (should (closql-insert db job t))
      (should (closql-insert db result t)))))
