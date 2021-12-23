;; paimon-test.el --- Paimon tests -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Paimon tests

;;; Code:

(require 'ert)
(require 'seq)
(require 'paimon)
(require 'paimon-api)
(require 'paimon-util)

(defvar paimon-test-api
  (paimon-api
   :hostname "localhost"
   :identity "admin"
   :protocol "https"
   :port 8089)
  "The API used for testing.")

(defvar paimon-test-profile
  (paimon-profile
   :auth-type "basic"
   :hostname "localhost"
   :identity "admin")
  "The profile used for testing.")

(defvar paimon-test-auth-info
  [["machine" "localhost" "login" "admin" "password" "12345678" "port" "8089"]
   ["machine" "localhost" "login" "paimon.el" "password" "87654321" "port" "8089"]]
  "Auth info used for testing.")

(defun paimon-test-auth-info-string (auth-info)
  "Encode AUTH-INFO into a string."
  (thread-last auth-info
    (seq-map (lambda (strings) (string-join strings " ")))
    (s-join "\n")))

(defmacro paimon-test-with-auth-info (auth-info &rest body)
  "Evaluate BODY with AUTH-INFO available in the secrets."
  (declare (indent 1))
  (let ((filename (make-temp-file "authinfo")))
    `(let ((auth-sources (list ,filename)))
       (f-write (paimon-test-auth-info-string ,auth-info) 'utf-8 ,filename)
       (unwind-protect
           (progn
             (auth-source-forget+)
             ,@body)
         (delete-file ,filename)))))

(defmacro paimon-test-with-db (db &rest body)
  "Evaluate BODY with DB bound to a temporary database."
  (declare (indent 1))
  `(let* ((temporary-file (concat temporary-file-directory (make-temp-name "paimon.el-")))
          (paimon--db-connection nil)
          (paimon-db-filename temporary-file)
          (,db (paimon-db)))
     ,@body))

(provide 'paimon-test)

;;; paimon-test.el ends here
