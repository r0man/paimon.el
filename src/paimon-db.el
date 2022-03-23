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
(require 'emacsql-sqlite)
(require 'seq)

(defvar paimon--db-connection nil
  "The EmacSQL database connection.")

(defvar paimon--db-table-schemata)

;;; Options

(defcustom paimon-database-connector 'sqlite
  "The database connector used by Paimon.

This must be set before `paimon' is loaded.  To use an
alternative connector you must install the respective package
explicitly.

When `sqlite', then use the `emacsql-sqlite' library that is
being maintained in the same repository as `emacsql' itself.

When `libsqlite3', then use the `emacsql-libsqlite' library,
which itself uses a module provided by the `sqlite3' package.
This is still experimental.

When `sqlite3', then use the `emacsql-sqlite3' library, which
uses the official `sqlite3' command line tool, which I do not
recommended because it is not suitable to be used like this, but
has the advantage that you likely don't need a compiler.  See
https://nullprogram.com/blog/2014/02/06/."
  :package-version '(paimon . "0.3.0")
  :group 'paimon
  :type '(choice (const sqlite)
                 (const libsqlite3)
                 (symbol :tag "other")))

(defcustom paimon-db-filename
  (expand-file-name "paimon.el/db.sqlite"  user-emacs-directory)
  "The file used to store the paimon database."
  :package-version '(paimon . "0.1.0")
  :group 'paimon
  :type 'file)

;;; Core

(declare-function paimon-database--eieio-childp "paimon-db.el" (obj) t)

(cl-case paimon-database-connector
  (sqlite
   (defclass paimon-database (emacsql-sqlite-connection closql-database)
     ((object-class :initform 'paimon-repository))))
  (libsqlite3
   (require (quote emacsql-libsqlite3))
   (with-no-warnings
     (defclass paimon-database (emacsql-libsqlite3-connection closql-database)
       ((object-class :initform 'paimon-repository)))))
  (sqlite3
   (require (quote emacsql-sqlite3))
   (with-no-warnings
     (defclass paimon-database (emacsql-sqlite3-connection closql-database)
       ((object-class :initform 'paimon-repository))))))

(defconst paimon--db-version 7
  "The latest version of the database.")

(defconst paimon--sqlite-available-p
  (with-demoted-errors "Paimon database initialization: %S"
    (emacsql-sqlite-ensure-binary)
    t)
  "Set to t if SQLite is available, otherwise nil.")

(defun paimon-db ()
  "Return the Paimon database."
  (unless (and paimon--db-connection (emacsql-live-p paimon--db-connection))
    (make-directory (file-name-directory paimon-db-filename) t)
    (closql-db 'paimon-database 'paimon--db-connection
               paimon-db-filename t)
    (let* ((db paimon--db-connection)
           (version (closql--db-get-version db))
           (version (paimon--db-maybe-update paimon--db-connection version)))
      (cond
       ((> version paimon--db-version)
        (emacsql-close db)
        (user-error
         "The Paimon database was created with a newer Paimon version.  %s"
         "You need to update the Paimon package."))
       ((< version paimon--db-version)
        (emacsql-close db)
        (error "BUG: The Paimon database scheme changed %s"
               "and there is no upgrade path")))))
  paimon--db-connection)

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
  (setq paimon--db-connection nil)
  (paimon-db))

(provide 'paimon-db)
;;; paimon-db.el ends here
