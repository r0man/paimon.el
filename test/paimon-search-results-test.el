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
      (let* ((_profile (closql-insert db (paimon-profile :default 1)))
             (job (aio-wait-for (paimon-search-create "index=_internal"))))
        (should (cl-typep job 'paimon-search-job))
        (should (equal "DONE" (paimon-search-job-dispatch-state job)))
        (paimon-search-results-show job)
        (with-current-buffer (get-buffer (paimon-search-job-buffer-name job))
          (should (equal job paimon-search-results-job))
          (should (cl-typep (paimon-search-result-under-point) paimon-search-result)))))))
