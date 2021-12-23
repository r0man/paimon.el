;; paimon-db-test.el --- Paimon database tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

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
  (paimon-test-with-db _db
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
           (result (paimon-search-result :id ["1" 1] :job-id (oref job id) :offset 0 :data (ht-create))))
      (should (closql-insert db job t))
      (should (closql-insert db result t)))))
