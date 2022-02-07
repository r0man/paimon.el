;; paimon-search-job-test.el --- Search job tests -*- lexical-binding: t; -*-

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

;; Search job tests

;;; Code:

(require 'ert)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-test)

(ert-deftest paimon-search-job-buffer-name-test ()
  (let ((job (paimon-search-job :id "my-id")))
    (should (equal "*paimon-my-id*" (paimon-search-job-buffer-name job)))))

(ert-deftest paimon-search-job-field-names-test ()
  (let ((job (paimon-search-job
              :fields (vector (ht ("name" "a"))
                              (ht ("name" "b"))))))
    (should (equal '("a" "b") (paimon-search-job-field-names job)))))

(ert-deftest paimon-search-job-results-count-test ()
  (paimon-test-with-db db
    (let* ((profile (closql-insert db (paimon-profile)))
           (job-1 (paimon-search-job :id "1" :profile-id (oref profile id)))
           (job-2 (paimon-search-job :id "2" :profile-id (oref profile id))))
      (closql-insert db job-2)
      (closql-insert db job-1)
      (closql-insert db (paimon-search-result :id (vector (oref job-2 id) 1) :job-id (oref job-2 id) :offset 0))
      (should (equal 0 (paimon-search-job-results-count job-1)))
      (should (equal 1 (paimon-search-job-results-count job-2))))))
