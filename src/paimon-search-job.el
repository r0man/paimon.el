;; paimon-search-job.el --- Search job -*- lexical-binding: t; -*-

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

;; Search job

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'closql)
(require 'eieio)
(require 'ht)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-profile)
(require 'paimon-search-result)
(require 'paimon-util)
(require 'seq)
(require 'subr-x)

(defvar paimon-search-job-earliest-time-seconds-ago 300
  "The number of minutes ago to use for the earliest search time.")

(defun paimon-search-job-default-earliest-time ()
  "Return the default earliest time, `paimon-search-job-earliest-time-seconds-ago` ago."
  (time-add (current-time) (- paimon-search-job-earliest-time-seconds-ago)))

(defclass paimon-search-job (closql-object)
  ((closql-table :initform 'search-job)
   (closql-primary-key :initform 'id)
   (id
    :accessor paimon-search-job-id
    :documentation "The id of the job."
    :initarg :id
    :initform (eval '(paimon-uuid))
    :type string)
   (profile-id
    :accessor paimon-search-job-profile-id
    :documentation "The search result profile id of the job."
    :initarg :profile-id
    :type string)
   (created-at
    :accessor paimon-search-job-created-at
    :documentation "The time when the search job has been created."
    :initarg :created-at
    :initform (current-time)
    :initform nil
    :type list)
   (details
    :accessor paimon-search-job-details
    :documentation "The details of the job."
    :initarg :details
    :initform nil)
   (earliest-time
    :accessor paimon-search-job-earliest-time
    :documentation "The earliest time of the search job."
    :initarg :earliest-time
    :initform nil
    :type list)
   (fields
    :accessor paimon-search-job-fields
    :documentation "The search result fields of the job."
    :initarg :fields
    :initform nil
    :type (or null vector))
   (latest-time
    :accessor paimon-search-job-latest-time
    :documentation "The latest time of the search job."
    :initarg :latest-time
    :initform nil
    :type list)
   (search
    :accessor paimon-search-job-search
    :documentation "The search of the job."
    :initarg :search
    :initform nil
    :type (or null string))
   (search-level
    :accessor paimon-search-job-search-level
    :documentation "The search level of the job."
    :initarg :search-level
    :initform "smart"
    :type string)
   (sid
    :accessor paimon-search-job-sid
    :documentation "The sid of the job."
    :initarg :sid
    :initform nil
    :type (or null string))
   (status-buckets
    :accessor paimon-search-job-status-buckets
    :documentation "The number of status buckets of the job."
    :initarg :status-buckets
    :initform 0
    :type number))
  "A class representing a search job.")

(defun paimon-search-jobs (db)
  "Return the search jobs from DB."
  (closql-entries db nil 'paimon-search-job))

(defun paimon-search-jobs-by-profile (db profile)
  "Return the search jobs of PROFILE in DB."
  (seq-map (lambda (row) (closql--remake-instance 'paimon-search-job db row))
           (paimon-db-sql [:select [*] :from search-job :where (= profile-id $s1) :order-by [(desc created-at)]]
                          (oref profile id))))

(defun paimon-search-job-buffer-name (job)
  "Return the buffer name for the search JOB."
  (format "*paimon-%s*" (paimon-search-job-id job)))

(defun paimon-search-job-by-id (db id)
  "Find the search job in DB by ID."
  (closql-get db id 'paimon-search-job))

(defun paimon-search-job-delete (job)
  "Delete the search JOB from the database."
  (closql-delete job))

(defun paimon-search-job-font-lock-face (job)
  "Return the face of the search JOB."
  (let ((status (paimon-search-job-dispatch-state job)))
    (cond
     ((paimon-search-job-expired-p job)
      'font-lock-doc-face)
     ((member status '(nil "NONE" "QUEUED" "PARSING" "CREATED" "RUNNING" "FINALIZING"))
      'font-lock-keyword-face)
     ((equal "PAUSED" status)
      'font-lock-regexp-grouping-backslash)
     ((equal "FAILED" status)
      'font-lock-warning-face))))

(defun paimon-search-job-field-names (job)
  "Return the names of the search JOB fields."
  (with-slots (fields) job
    (seq-map (lambda (field) (ht-get field "name")) fields)))

(defun paimon-search-job-under-point ()
  "Return the search job under point."
  (paimon-search-job-by-id (paimon-db) (tabulated-list-get-id)))

(defun paimon-search-job-get-details (job &rest path)
  "Get the object of the JOB details at PATH."
  (when-let (details (paimon-search-job-details job))
    (apply #'ht-get* details path)))

(defun paimon-search-job-published-at (job)
  "Return the event count of the search JOB."
  (when-let (time (paimon-search-job-get-details job "published"))
    (parse-iso8601-time-string time)))

(defun paimon-search-job-event-count (job)
  "Return the event count of the search JOB."
  (paimon-search-job-get-details job "content" "eventCount"))

(defun paimon-search-job-dispatch-state (job)
  "Return the dispatch state of the search JOB."
  (paimon-search-job-get-details job "content" "dispatchState"))

(defun paimon-search-job-ttl (job)
  "Return the TTL of the search JOB."
  (paimon-search-job-get-details job "content" "ttl"))

(defun paimon-search-job-expired-p (job)
  "Return t if the search JOB has been expired, otherwise nil."
  (when-let ((published-at (paimon-search-job-published-at job))
             (ttl (paimon-search-job-ttl job)))
    (time-less-p (time-add published-at ttl) (current-time))))

(defun paimon-search-job-max-field-lengths (job)
  "Return the maximum field lengths of the search JOB."
  (when-let (field-names (paimon-search-job-field-names job))
    (thread-last
      (paimon-search-results-by-job job 0 100)
      (seq-mapcat (lambda (result)
                    (seq-map (lambda (field-name)
                               (let ((value (ht-get (oref result data) field-name)))
                                 (cons field-name (when (stringp value) (length  value)))))
                             field-names)))
      (seq-remove (lambda (element) (null (cdr element))))
      (seq-group-by #'car)
      (seq-map (lambda (element)
                 (cons (car element)
                       (apply #'max (seq-map #'cdr (cdr element)))))))))

(defun paimon-search-job-refresh-p (job)
  "Return t if the search JOB needs to be refreshed, otherwise nil."
  (and (oref job sid)
       (not (paimon-search-job-expired-p job))
       (not (member (paimon-search-job-dispatch-state job) '("DONE" "FAILED")))))

(defun paimon-search-job-results-count (job)
  "Return the number of search results of the search JOB."
  (caar (paimon-db-sql [:select [(funcall count *)] :from search-result :where (= job-id $s1)] (oref job id))))

(aio-defun paimon-search-job-create (job)
  "Create the search JOB asynchronously."
  (with-slots (earliest-time latest-time search search-level sid status-buckets) job
    (let ((response (aio-await (paimon-api-search-job-create
                                (paimon-api-for job) search
                                :earliest-time (paimon--format-time earliest-time)
                                :latest-time (paimon--format-time latest-time)
                                :search-level search-level
                                :status-buckets status-buckets))))
      (pcase (plist-get response :status)
        (201 (setf sid (ht-get (plist-get response :body) "sid"))
             (closql-insert (closql--oref job 'closql-database) job t))
        (_ (user-error "Can't create search job.  %s" (paimon-api-error-message response)))))))

(aio-defun paimon-search-job-synchronize (job)
  "Synchronize the search JOB with the Splunk API."
  (with-slots (id details sid) job
    (let ((response (aio-await (paimon-api-search-job (paimon-api-for job) sid))))
      (pcase (plist-get response :status)
        (200 (setf details (aref (ht-get (plist-get response :body) "entry") 0)) job)
        (404 nil)
        (_ (user-error "Can't synchronize search job %s.  %s"
                       (paimon--bold id)
                       (paimon-api-error-message response)))))))

(aio-defun paimon-search-job-control (job action)
  "Control the search JOB using ACTION."
  (with-slots (id sid) job
    (let ((response (aio-await (paimon-api-search-job-control (paimon-api-for job) sid action))))
      (pcase (plist-get response :status)
        (200 (plist-get response :body))
        (_ (user-error "Can't apply %s action to search job %s.  %s"
                       (paimon--bold action)
                       (paimon--bold id)
                       (paimon-api-error-message response)))))))

(defun paimon-search-job--parse-results (job response)
  "Parse the search results RESPONSE of the search JOB."
  (with-slots (id) job
    (let* ((offset (ht-get response "init_offset"))
           (results (seq-map-indexed
                     (lambda (result index)
                       (paimon-search-result
                        :data result
                        :id (vector id (+ offset index))
                        :job-id id
                        :offset (+ offset index)))
                     (ht-get response "results"))))
      (ht-set! response "results" results)
      response)))

(aio-defun paimon-search-job-get-results (job &optional offset count)
  "Get the results of the search JOB using OFFSET and COUNT. "
  (with-slots (id sid) job
    (let ((response (aio-await (paimon-api-search-job-results (paimon-api-for job) sid :offset offset :count count))))
      (pcase (plist-get response :status)
        (200 (paimon-search-job--parse-results job (plist-get response :body)))
        (_ (user-error "Can't load results for search job %s.  %s"
                       (paimon--bold id)
                       (paimon-api-error-message response)))))))

(aio-defun paimon-search-job-get-results-preview (job &optional offset count)
  "Fetch the log lines of the search JOB using OFFSET and COUNT. "
  (with-slots (id sid) job
    (let ((response (aio-await (paimon-api-search-job-preview-results (paimon-api-for job) sid :offset offset :count count))))
      (pcase (plist-get response :status)
        (200 (paimon-search-job--parse-results job (plist-get response :body)))
        (_ (user-error "Can't load preview results for search job %s.  %s"
                       (paimon--bold id)
                       (paimon-api-error-message response)))))))

(defun paimon-search-job--save-results (db results)
  "Save the JOB search RESULTS in DB."
  (when (seq-first results)
    (emacsql db [:insert-or-replace-into
                 search-result [class id job-id data offset]
                 :values $v1]
             (seq-map (lambda (result)
                        (with-slots (data id job-id offset) result
                          (vector 'paimon-search-result id job-id data offset)))
                      results)))
  results)

(aio-defun paimon-search-job-load-results (job &optional offset limit)
  "Load the search results of the search JOB using OFFSET and LIMIT."
  (with-slots (id fields) job
    (let ((db (closql--oref job 'closql-database))
          (response (aio-await (paimon-search-job-get-results job offset limit))))
      (setf fields (ht-get response "fields"))
      (paimon-search-job--save-results db (ht-get response "results")))))

(aio-defun paimon-search-job-load-results-preview (job &optional offset limit)
  "Load the search preview results of the search JOB using OFFSET and LIMIT."
  (with-slots (id fields) job
    (let ((db (closql--oref job 'closql-database))
          (response (aio-await (paimon-search-job-get-results-preview job offset limit))))
      (setf fields (ht-get response "fields"))
      (paimon-search-job--save-results db (ht-get response "results")))))

(aio-defun paimon-search-job-apply-action (job action)
  "Apply the ACTION on the search JOB and update."
  (when (and job action)
    (aio-await (paimon-search-job-control job action))
    (aio-await (paimon-search-job-synchronize job))))

(defun paimon-search-job-profile (job)
  "Return the profile of the search JOB."
  ;; (paimon-profile-by-id (closql--oref job 'closql-database) (oref job profile-id))
  (paimon-profile-by-id (paimon-db) (oref job profile-id)))

(cl-defmethod paimon-api-for ((job paimon-search-job))
  "Return the API for the search JOB."
  (when-let (profile (paimon-search-job-profile job))
    (paimon-api-for profile)))

(provide 'paimon-search-job)

;;; paimon-search-job.el ends here
