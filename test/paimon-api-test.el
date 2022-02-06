;; paimon-api-test.el --- Paimon API tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Paimon API tests

;;; Code:

(require 'ert)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-profile)
(require 'paimon-search-job)
(require 'paimon-test)

(ert-deftest paimon-api-basic-auth-encode-test ()
  (should (equal "YWRtaW46MTIzNDU2Nzg=" (paimon-api-basic-auth-encode "admin" "12345678"))))

(ert-deftest paimon-api-error-status-test ()
  (should (null (paimon-api-error-status nil)))
  (should (equal 404 (paimon-api-error-status '(error . (:status 404))))))

(ert-deftest paimon-api-authorization-header-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (should (equal '("Authorization" . "Basic YWRtaW46MTIzNDU2Nzg=")
                   (paimon-api-authorization-header
                    (paimon-api :auth-type "basic" :identity "admin" :hostname "localhost"))))
    (should (equal '("Authorization" . "Bearer 12345678")
                   (paimon-api-authorization-header
                    (paimon-api :auth-type "bearer" :identity "admin" :hostname "localhost"))))))

(ert-deftest paimon-api-authorization-tokens-url-test ()
  (should (equal "https://localhost/en-US/manager/launcher/authorization/tokens"
                 (paimon-api-authorization-tokens-url paimon-test-api))))

(ert-deftest paimon-api-data-indexes-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (let ((response (aio-wait-for (paimon-api-data-indexes paimon-test-api))))
      (should (equal 200 (plist-get response :status)))
      (should (hash-table-p (plist-get response :body)))
      (should (equal '("_audit"
                       "_internal"
                       "_introspection"
                       "_telemetry"
                       "_thefishbucket"
                       "history"
                       "main"
                       "splunklogger"
                       "summary")
                     (seq-map (lambda (data) (ht-get data "name"))
                              (ht-get (plist-get response :body) "entry")))))))

(ert-deftest paimon-api-headers-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (should (equal '(("Authorization" . "Basic YWRtaW46MTIzNDU2Nzg="))
                   (paimon-api-headers (paimon-api :auth-type "basic" :identity "admin" :hostname "localhost"))))
    (should (equal '(("Authorization" . "Bearer 12345678"))
                   (paimon-api-headers (paimon-api :auth-type "bearer" :identity "admin" :hostname "localhost"))))))

(ert-deftest paimon-api-job-url-test ()
  (let ((id "1641654074.1715482_441E883E-2B06-437D-97A4-B78C146189E2"))
    (should (equal (format "https://localhost/en-US/app/search/search?sid=%s" id)
                   (paimon-api-job-url paimon-test-api id)))))

(ert-deftest paimon-api-url-test ()
  (should (equal "https://localhost:8089/"
                 (paimon-api-url paimon-test-api)))
  (should (equal "https://localhost:8089/services/search/jobs"
                 (paimon-api-url paimon-test-api "services/search/jobs"))))

(ert-deftest paimon-api-search-job-create-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (let ((response (aio-wait-for (paimon-api-search-job-create paimon-test-api "search"))))
      (should (equal 201 (plist-get response :status)))
      (should (hash-table-p (plist-get response :body)))
      (should (stringp (ht-get (plist-get response :body) "sid"))))))

(ert-deftest paimon-api-search-job-test ()
  (let ((response (aio-wait-for (paimon-api-search-job-create paimon-test-api "search"))))
    (should (equal 201 (plist-get response :status)))
    (should (hash-table-p (plist-get response :body)))
    (let ((response (aio-wait-for (paimon-api-search-job paimon-test-api (ht-get (plist-get response :body) "sid")))))
      (should (equal 200 (plist-get response :status)))
      (should (hash-table-p (plist-get response :body))))))

(ert-deftest paimon-api-search-job-results-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (let ((response (aio-wait-for (paimon-api-search-job-create paimon-test-api "search"))))
      (should (equal 201 (plist-get response :status)))
      (should (hash-table-p (plist-get response :body)))
      (let ((job (plist-get response :body)))
        (aio-wait-for (aio-sleep 1))
        (let ((response (aio-wait-for (paimon-api-search-job-results paimon-test-api (ht-get job "sid")))))
          (should (equal 200 (plist-get response :status)))
          (should (hash-table-p (plist-get response :body))))))))

(ert-deftest paimon-api-search-job-preview-results-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (let ((response (aio-wait-for (paimon-api-search-job-create paimon-test-api "search"))))
      (should (equal 201 (plist-get response :status)))
      (should (hash-table-p (plist-get response :body)))
      (let ((job (plist-get response :body)))
        (aio-wait-for (aio-sleep 1))
        (let ((response (aio-wait-for (paimon-api-search-job-preview-results paimon-test-api (ht-get job "sid")))))
          (should (equal 200 (plist-get response :status)))
          (should (hash-table-p (plist-get response :body))))))))

(ert-deftest paimon-api-search-parse-query-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (let ((response (aio-wait-for (paimon-api-search-parse-query paimon-test-api "search"))))
      (should (equal 200 (plist-get response :status)))
      (should (hash-table-p (plist-get response :body)))
      (let ((command (seq-first (ht-get* (plist-get response :body) "commands"))))
        (should (equal "search" (ht-get command "command")))))))

(ert-deftest paimon-api-search-typeahead-test ()
  (paimon-test-with-auth-info paimon-test-auth-info
    (let ((response (aio-wait-for (paimon-api-search-typeahead paimon-test-api "search"))))
      (should (equal 200 (plist-get response :status)))
      (should (hash-table-p (plist-get response :body))))))

(ert-deftest paimon-api--parse-exit-code-test ()
  (should (equal 7 (paimon-api--parse-exit-code "exited abnormally with code 7\n"))))

(ert-deftest paimon-api-for-profile-test ()
  (paimon-test-with-db db
    (let* ((profile (closql-insert db (paimon-profile)))
           (api (paimon-api-for profile)))
      (should (cl-typep api 'paimon-api))
      (should (equal (oref profile port) (oref api port)))
      (should (equal (oref profile hostname) (oref api hostname)))
      (should (equal (oref profile protocol) (oref api protocol))))))

(ert-deftest paimon-api-for-search-job-test ()
  (paimon-test-with-db db
    (let* ((profile (closql-insert db (paimon-profile)))
           (job (paimon-search-job :profile-id (oref profile id) ))
           (api (paimon-api-for job)))
      (should (cl-typep api 'paimon-api))
      (should (equal (oref profile port) (oref api port)))
      (should (equal (oref profile hostname) (oref api hostname)))
      (should (equal (oref profile protocol) (oref api protocol))))))
