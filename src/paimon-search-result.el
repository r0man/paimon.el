;; paimon-search-result.el --- Search Result -*- lexical-binding: t; -*-

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

;; Search Result

;;; Code:

(require 'cl-lib)
(require 'closql)
(require 'eieio)
(require 'ht)
(require 'js)
(require 'paimon-db)

(defcustom paimon-search-result-buffer-template
  "*paimon-search-result-%s*"
  "The search result buffer name."
  :group 'paimon
  :type 'string)

(defcustom paimon-search-results-offset 0
  "Show the search results from the offset."
  :group 'paimon
  :local t
  :type 'number)

(defcustom paimon-search-results-limit 250
  "The number of search results fetched per HTTP request."
  :group 'paimon
  :local t
  :type 'number)

(defclass paimon-search-result (closql-object)
  ((closql-table :initform 'search-result)
   (closql-primary-key :initform 'id)
   (closql-foreign-key :initform 'job-id)
   (id
    :accessor paimon-search-result-id
    :documentation "The id of the search result."
    :initarg :id
    :initform nil
    :type (or null vector))
   (job-id
    :accessor paimon-search-result-job-id
    :documentation "The job id of the search result."
    :initarg :job-id
    :initform nil
    :type (or null string))
   (data
    :accessor paimon-search-result-data
    :documentation "The data of the search result."
    :initarg :data
    :initform (ht-create)
    :type hash-table)
   (offset
    :accessor paimon-search-result-offset
    :documentation "The offset of the search result."
    :initarg :offset
    :initform nil
    :type (or null number)))
  "A class representing a search result.")

(defun paimon-search-result-buffer-name (profile)
  "Return the search result buffer name for PROFILE."
  (format paimon-search-result-buffer-template (oref profile identity)))

(defun paimon-search-result-by-id (db id)
  "Find the search result in DB by ID."
  (closql-get db id 'paimon-search-result))

(defun paimon-search-result-field-value (result field)
  "Return the FIELD value of the search RESULT."
  (with-slots (data) result
    (let ((value (ht-get data field)))
      (if (stringp value)
          value
        (format "%s" (or value ""))))))

(defun paimon-search-result-job (result)
  "Return the job of the search RESULT."
  (with-slots (job-id) result
    (closql-get (closql--oref result 'closql-database) job-id 'paimon-search-job)))

(defun paimon-search-result-under-point ()
  "Return the search result under point."
  (paimon-search-result-by-id (paimon-db) (tabulated-list-get-id)))

(defun paimon-search-result-get-data (result &rest path)
  "Get the object of the RESULT details at PATH."
  (when-let (data (paimon-search-result-data result))
    (apply #'ht-get* data path)))

(defun paimon-search-result-raw (result)
  "Return the raw log line from the search RESULT."
  (paimon-search-result-get-data result "_raw"))

(defun paimon-search-results-by-job (job &optional offset limit)
  "Return the search results of the search JOB using OFFSET and LIMIT."
  (seq-map (lambda (row) (closql--remake-instance 'paimon-search-result (paimon-db) row))
           (paimon-db-sql [:select [*] :from search-result :where (= job-id $s1) :order-by [(asc offset)] :limit $s2 :offset $s3]
                          (oref job id)
                          (or limit paimon-search-results-limit)
                          (or offset paimon-search-results-offset))))

(defun paimon-search-results-search-like (job query &optional offset limit)
  "Search the results of the search JOB matching QUERY using OFFSET and LIMIT."
  (seq-map (lambda (row) (closql--remake-instance 'paimon-search-result (paimon-db) row))
           (paimon-db-sql [:select [*]
                                   :from search-result
                                   :where (and (= job-id $s1) (like data $r4))
                                   :order-by [(asc offset)]
                                   :limit $s2
                                   :offset $s3]
                          (oref job id)
                          (or limit paimon-search-results-limit)
                          (or offset paimon-search-results-offset)
                          query)))

(defun paimon-search-result-render (result)
  "Render the search RESULT."
  (let* ((job (paimon-search-result-job result))
         (profile (paimon-search-job-profile job)))
    (with-current-buffer (get-buffer-create (paimon-search-result-buffer-name profile))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (condition-case error
            (progn (if (fboundp 'json-insert)
                       (json-insert (oref result data))
                     (insert (json-encode (oref result data))))
                   (json-pretty-print-buffer))
          (error (insert "/*\n\n WARNING: Can't render search result in JSON.\n\n")
                 (insert (format " Error: %s\n\n %s\n*/\n\n" (car error) (pp-to-string (cdr error))))
                 (insert (pp-to-string (oref result data)))))
        (paimon-search-result-mode)
        (goto-char (point-min))))))

(defun paimon-search-result-show (result)
  "Show the search RESULT."
  (interactive (list (paimon-search-result-under-point)))
  (when result
    (let* ((job (paimon-search-result-job result))
           (profile (paimon-search-job-profile job))
           (buffer (get-buffer-create (paimon-search-result-buffer-name profile)))
           (split-width-threshold nil))
      (switch-to-buffer-other-window buffer)
      (paimon-search-result-render result))))

(defvar paimon-search-result-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "The key map for the `paimon-search-result-mode'.")

(define-derived-mode paimon-search-result-mode js-mode "Search Result"
  "Special mode for search result buffers."
  (read-only-mode)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'paimon-search-result-mode 'emacs)))

(provide 'paimon-search-result)

;;; paimon-search-result.el ends here
