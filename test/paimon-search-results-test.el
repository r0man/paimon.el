;; paimon-search-results-test.el --- Search results tests -*- lexical-binding: t; -*-

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

;; Search results tests

;;; Code:

(require 'ert)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-search-results)
(require 'paimon-test)

(ert-deftest paimon-search-results-show-test ()
  (paimon-test-with-db db
    (save-window-excursion
      (let* ((paimon-search-results-limit 5)
             (profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (should (cl-typep job 'paimon-search-job))
        (should (equal "DONE" (paimon-search-job-dispatch-state job)))
        (paimon-search-results-show job)
        (with-current-buffer (get-buffer (paimon-search-results-buffer-name profile))
          (should (equal 0 (oref (paimon-search-result-under-point) offset)))
          (goto-char (point-max))
          (forward-line -1)
          (should (equal 4 (oref (paimon-search-result-under-point) offset))))))))

(ert-deftest paimon-search-results-next-test ()
  (paimon-test-with-db db
    (save-window-excursion
      (let* ((paimon-search-results-limit 5)
             (profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (paimon-search-results-show job)
        (with-current-buffer (get-buffer (paimon-search-results-buffer-name profile))
          (aio-wait-for (paimon-search-results-next job))
          (should (equal 5 (oref (paimon-search-result-under-point) offset)))
          (goto-char (point-max))
          (forward-line -1)
          (should (equal 9 (oref (paimon-search-result-under-point) offset))))))))

(ert-deftest paimon-search-results-previous-test ()
  (paimon-test-with-db db
    (save-window-excursion
      (let* ((paimon-search-results-limit 5)
             (profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (paimon-search-results-show job)
        (with-current-buffer (get-buffer (paimon-search-results-buffer-name profile))
          (aio-wait-for (paimon-search-results-next job))
          (aio-wait-for (paimon-search-results-next job))
          (aio-wait-for (paimon-search-results-previous job))
          (should (equal 5 (oref (paimon-search-result-under-point) offset)))
          (goto-char (point-max))
          (forward-line -1)
          (should (equal 9 (oref (paimon-search-result-under-point) offset)))
          (aio-wait-for (paimon-search-results-previous job))
          (should (equal 0 (oref (paimon-search-result-under-point) offset)))
          (goto-char (point-max))
          (forward-line -1)
          (should (equal 4 (oref (paimon-search-result-under-point) offset)))
          (should (not (paimon-search-results-previous job))))))))
