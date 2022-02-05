;; paimon-search-results-test.el --- Search results tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

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
             (_profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (should (cl-typep job 'paimon-search-job))
        (should (equal "DONE" (paimon-search-job-dispatch-state job)))
        (paimon-search-results-show job)
        (with-current-buffer (get-buffer (paimon-search-job-buffer-name job))
          (should (equal 0 (oref (paimon-search-result-under-point) offset)))
          (end-of-buffer)
          (previous-line)
          (should (equal 4 (oref (paimon-search-result-under-point) offset))))))))

(ert-deftest paimon-search-results-next-test ()
  (paimon-test-with-db db
    (save-window-excursion
      (let* ((paimon-search-results-limit 5)
             (_profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (paimon-search-results-show job)
        (with-current-buffer (get-buffer (paimon-search-job-buffer-name job))
          (aio-wait-for (paimon-search-results-next job))
          (should (equal 5 (oref (paimon-search-result-under-point) offset)))
          (end-of-buffer)
          (previous-line)
          (should (equal 9 (oref (paimon-search-result-under-point) offset))))))))

(ert-deftest paimon-search-results-previous-test ()
  (paimon-test-with-db db
    (save-window-excursion
      (let* ((paimon-search-results-limit 5)
             (_profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (paimon-search-results-show job)
        (with-current-buffer (get-buffer (paimon-search-job-buffer-name job))
          (aio-wait-for (paimon-search-results-next job))
          (aio-wait-for (paimon-search-results-next job))
          (aio-wait-for (paimon-search-results-previous job))
          (should (equal 5 (oref (paimon-search-result-under-point) offset)))
          (end-of-buffer)
          (previous-line)
          (should (equal 9 (oref (paimon-search-result-under-point) offset)))
          (aio-wait-for (paimon-search-results-previous job))
          (should (equal 0 (oref (paimon-search-result-under-point) offset)))
          (end-of-buffer)
          (previous-line)
          (should (equal 4 (oref (paimon-search-result-under-point) offset)))
          (should (not (paimon-search-results-previous job))))))))
