;; paimon-data-index.el --- Data index -*- lexical-binding: t; -*-

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

;; Data index

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'closql)
(require 'eieio)
(require 'ht)
(require 'seq)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-search-result)
(require 'paimon-util)
(require 'subr-x)

(defclass paimon-data-index (closql-object)
  ((closql-table :initform 'data-index)
   (closql-primary-key :initform 'id)
   (id
    :accessor paimon-data-index-id
    :documentation "The id of the data index."
    :initarg :id
    :initform (eval '(paimon-uuid))
    :type string)
   (profile-id
    :accessor paimon-data-index-profile-id
    :documentation "The profile id of the data index."
    :initarg :profile-id
    :type string)
   (acl
    :accessor paimon-data-index-acl
    :documentation "The acl of the data index."
    :initarg :acl
    :initform (ht-create)
    :type hash-table)
   (author
    :accessor paimon-data-index-author
    :documentation "The author of the data index."
    :initarg :author
    :type string)
   (content
    :accessor paimon-data-index-content
    :documentation "The content of the data index."
    :initarg :content
    :initform (ht-create)
    :type hash-table)
   (name
    :accessor paimon-data-index-name
    :documentation "The name of the data index."
    :initarg :name
    :type string))
  "A class representing a Splunk data index.")

(defun paimon-data-indexes (db)
  "Return the data indexes from DB."
  (closql-entries db nil 'paimon-data-index))

(defun paimon-data-indexes-by-profile (db profile)
  "Return the data indexes in DB by PROFILE."
  (seq-map (lambda (row) (closql--remake-instance 'paimon-data-index db row))
           (paimon-db-sql [:select [*] :from data-index :where (= profile-id $s1) :order-by [(asc name)]]
                          (oref profile id))))

(defun paimon-data-index--entry->instance (profile entry)
  "Convert the ENTRY into a data index for PROFILE."
  (paimon-data-index :profile-id (oref profile id)
                     :acl (ht-get entry "acl")
                     :author (ht-get entry "author")
                     :content (ht-get entry "content")
                     :name (ht-get entry "name")))

(aio-defun paimon-data-indexes-load (profile)
  "Load the data indexes from the API."
  (let ((response (aio-await (paimon-api-data-indexes (paimon-api-for profile)))))
    (pcase (plist-get response :status)
      (200 (seq-map (lambda (entry) (paimon-data-index--entry->instance profile entry))
                    (ht-get (plist-get response :body) "entry")))
      (401 (message "Warning: Can't load data indexes because you are not authenticated.") nil)
      (403 (message "Warning: Can't load data indexes because you are not authorized.") nil)
      (_ (user-error "Can't load data indexes: %s" (plist-get response :body))))))

(aio-defun paimon-data-indexes-synchronize (db profile)
  "Synchronize the data indexes in DB with PROFILE."
  (seq-doseq (data-index (aio-await (paimon-data-indexes-load profile)))
    (closql-insert db data-index)))

(provide 'paimon-data-index)

;;; paimon-data-index.el ends here
