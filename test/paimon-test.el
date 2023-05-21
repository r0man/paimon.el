;; paimon-test.el --- Paimon test helpers -*- lexical-binding: t; -*-

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

;; Paimon test helpers

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

(defclass paimon-test-database (paimon-database)
  ((file :initform "/tmp/paimon-test-db.sqlite")))

(defun paimon-test-auth-info-string (auth-info)
  "Encode AUTH-INFO into a string."
  (thread-last
    auth-info
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
  (let ((test-db-sym (gensym "db")))
    `(let ((,test-db-sym (closql-db 'paimon-test-database)))
       (cl-letf (((symbol-function 'paimon-db) (lambda () ,test-db-sym)))
         (unwind-protect
             (let ((,db ,test-db-sym))
               ,@body)
           (progn
             (emacsql-close ,test-db-sym)
             (delete-file (oref ,test-db-sym file))))))))

(defmacro paimon-test-with-profile (db profile &rest body)
  "Evaluate BODY with DB and PROFILE bound for a test."
  (declare (indent 2))
  (let ((db-sym (gensym "db"))
        (profile-sym (gensym "profile")))
    `(paimon-test-with-auth-info paimon-test-auth-info
       (paimon-test-with-db ,db-sym
         (let ((,profile-sym (closql-insert ,db-sym paimon-test-profile t)))
           (let ((,db ,db-sym)
                 (,profile ,profile-sym))
             ,@body))))))

(provide 'paimon-test)

;;; paimon-test.el ends here
