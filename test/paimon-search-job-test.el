;; paimon-search-job-test.el --- search job tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; search job tests

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
