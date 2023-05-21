;;; paimon-db.el --- Paimon database -*- lexical-binding: t -*-

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

;; Paimon database

;;; Code:

(require 'closql)
(require 'eieio)
(require 'emacsql)
(require 'seq)

(defvar paimon--db-table-schemata)

(defconst paimon--db-version 7
  "The latest version of the database.")

;;; Options

(defcustom paimon-db-filename
  (expand-file-name "paimon.el/db.sqlite"  user-emacs-directory)
  "The file used to store the paimon database."
  :package-version '(paimon . "0.1.0")
  :group 'paimon
  :type 'file)

;;; Core

;; (declare-function paimon-database--eieio-childp "paimon-db.el" (obj) t)

(defclass paimon-database (closql-database)
  ((name         :initform "Paimon")
   ;; (object-class :initform 'paimon-repository)
   (file         :initform 'paimon-db-filename)
   (schemata     :initform 'paimon--db-table-schemata)
   (version      :initform 7)))

(defun paimon-db ()
  "Return the Paimon database."
  (closql-db 'paimon-database))

;;; Schemata

(defconst paimon--db-table-schemata
  '((profile
     [(class :not-null)
      (id :not-null :primary-key)
      (auth-type :not-null)
      default
      (hostname :not-null)
      (identity :not-null)
      (port :not-null)
      (protocol :not-null)])
    (data-index
     [(class :not-null)
      (id :not-null :primary-key)
      (profile-id :not-null)
      (acl :not-null)
      (author :not-null)
      (content :not-null)
      (name :not-null)]
     (:foreign-key [profile-id] :references profile [id] :on-delete :cascade))
    (search-job
     [(class :not-null)
      (id :not-null :primary-key)
      (profile-id :not-null)
      created-at
      details
      earliest-time
      fields
      latest-time
      search
      search-level
      status-buckets
      sid]
     (:foreign-key [profile-id] :references profile [id] :on-delete :cascade))
    (search-result
     [(class :not-null)
      (id :not-null :primary-key)
      (job-id :not-null)
      (data :not-null)
      (offset :not-null)]
     (:foreign-key [job-id] :references search-job [id] :on-delete :cascade)))
  "The paimon.el database schema.")

(defun paimon-db--create-profile-indexes (db)
  "Create the indexes for the profiles in DB."
  (emacsql db [:create :index profile-identity :on profile [identity]]))

(defun paimon-db--create-search-job-indexes (db)
  "Create the indexes for the search jobs in DB."
  (emacsql db [:create :index search-job-profile-id :on search-job [profile-id]]))

(defun paimon-db--create-search-result-indexes (db)
  "Create the indexes for the search results in DB."
  (emacsql db [:create :index search-result-index-job-id-offset :on search-result [job-id offset]]))

(cl-defmethod closql--db-init ((db paimon-database))
  "Initialize the DB."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) paimon--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (paimon-db--create-profile-indexes db)
    (paimon-db--create-search-job-indexes db)
    (paimon-db--create-search-result-indexes db)
    (closql--db-set-version db paimon--db-version)))

(defun paimon--db-maybe-update (db version)
  "Update the DB schema to VERSION."
  (emacsql-with-transaction db
    version))

;;; Api

(defun paimon-db-sql (sql &rest args)
  "Execute the SQL statement using ARGS."
  (if (stringp sql)
      (emacsql (paimon-db) (apply #'format sql args))
    (apply #'emacsql (paimon-db) sql args)))

;;;###autoload
(defun paimon-db-reset ()
  "Reset the current database."
  (interactive)
  (delete-file paimon-db-filename)
  (paimon-db))

(provide 'paimon-db)
;;; paimon-db.el ends here
