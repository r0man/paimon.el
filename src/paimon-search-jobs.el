;;; paimon-search-jobs.el --- Search job list-*- lexical-binding: t -*-

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

;; Search job list

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'eieio)
(require 'ht)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-profile)
(require 'paimon-search-job)
(require 'paimon-search-results)
(require 'paimon-util)
(require 's)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)

(defcustom paimon-search-jobs-buffer-template
  "*paimon-search-jobs-%s*"
  "The template of the search jobs buffer."
  :group 'paimon
  :safe #'stringp
  :type 'string)

(defcustom paimon-search-jobs-list-sort-key
  '("Created At" . t)
  "Sort the search jobs on this key."
  :group 'paimon
  :safe #'listp
  :type 'list)

(defvar-local paimon-search-jobs-profile nil
  "The profile used for the search job.")

(put 'paimon-search-jobs-profile 'permanent-local t)

(defvar paimon-search-jobs-list-format
  [("Created At" 20 t)
   ("Earliest Time" 20 t)
   ("Latest Time" 20 t)
   ("State" 10 t)
   ("Events" 10 t)
   ("Search" 37 t)]
  "The `tabulated-list-mode' format of the search jobs buffer.")

(defun paimon-search-jobs-buffer-name (profile)
  "Return the search jobs buffer name for PROFILE."
  (format paimon-search-jobs-buffer-template (oref profile identity)))

(defun paimon--trim-search-term (s)
  "Trim the search term from S."
  (when s (s-replace "search " "" s)))

(defun paimon-search-jobs-list-entry-search (job)
  "Return the tabulated list entry search for JOB."
  (paimon--trim-search-term (paimon-search-job-search job)))

(defun paimon-search-jobs-list-entry-dispatch-state (job)
  "Return the tabulated list entry dispatch state for JOB."
  (or (paimon-search-job-dispatch-state job)
      (if (oref job id) "CREATED" "CREATING")))

(defun paimon-search-jobs-list-entry-event-count (job)
  "Return the tabulated list entry event count for JOB."
  (number-to-string (or (paimon-search-job-event-count job) 0)))

(defun paimon-search-jobs-list-entry-created-at (job)
  "Return the tabulated list entry published at time for JOB."
  (paimon--format-time-human (paimon-search-job-created-at job)))

(defun paimon-search-jobs-list-entry-earliest-time (job)
  "Return the tabulated list entry earliest time for JOB."
  (or (paimon--format-time-human (paimon-search-job-earliest-time job)) ""))

(defun paimon-search-jobs-list-entry-latest-time (job)
  "Return the tabulated list entry latest time for JOB."
  (or (paimon--format-time-human (paimon-search-job-latest-time job)) ""))

(defun paimon-search-jobs-list--apply-face (job s)
  "Apply a face to S depending on the JOB status."
  (when s (propertize s 'font-lock-face (paimon-search-job-font-lock-face job))))

(defun paimon-search-jobs-list-entry (job)
  "Convert the search JOB into a tabulated list entry."
  (list (paimon-search-job-id job)
        (thread-last (list (paimon-search-jobs-list-entry-created-at job)
                           (paimon-search-jobs-list-entry-earliest-time job)
                           (paimon-search-jobs-list-entry-latest-time job)
                           (paimon-search-jobs-list-entry-dispatch-state job)
                           (paimon-search-jobs-list-entry-event-count job)
                           (paimon-search-jobs-list-entry-search job))
          (seq-map (lambda (s) (paimon-search-jobs-list--apply-face job s)))
          (apply #'vector ))))

(defun paimon-search-jobs-list-entries ()
  "Return the search job list entries."
  (when-let (profile paimon-search-jobs-profile)
    (when-let (jobs (paimon-search-jobs-by-profile (paimon-db) profile))
      (seq-map #'paimon-search-jobs-list-entry jobs))))

(defun paimon-search-jobs-render (profile &optional remember-pos update)
  "Render the search list entries for PROFILE using REMEMBER-POS and UPDATE."
  (interactive)
  (with-current-buffer (get-buffer-create (paimon-search-jobs-buffer-name profile))
    (tabulated-list-print remember-pos update)))

(defvar paimon-search-jobs--lifecycle-registry (ht-create)
  "The search job lifecycle handlers.")

(defun paimon-search-jobs--lifecycle-registered-p (job)
  "Return t when there is a life cycle handler for JOB registered, otherwise nil."
  (ht-get paimon-search-jobs--lifecycle-registry (oref job id)))

(defun paimon-search-jobs--refresh-results (job)
  "Refresh the results of the search JOB."
  (let ((profile  (paimon-search-job-profile job)))
    (when-let (buffer (get-buffer (paimon-search-results-buffer-name profile)))
      (with-current-buffer buffer
        (setq-local paimon-search-results-job job)
        (paimon-search-results-mode)))))

(aio-defun paimon-search-jobs--lifecycle-done (job)
  "Handle the life cycle of the search JOB when in the done state."
  (let ((limit paimon-search-results-limit)
        (offset paimon-search-results-offset))
    (aio-await (paimon-search-job-load-results job offset limit))
    (paimon-search-jobs--refresh-results job)
    (message "Search job %s done." (paimon--bold (oref job id)))
    nil))

(aio-defun paimon-search-jobs--lifecycle-failed (job)
  "Handle the life cycle of the search JOB when in the failed state."
  (message "Search job %s failed." (paimon--bold (oref job id)))
  nil)

(aio-defun paimon-search-jobs--lifecycle-running (job)
  "Handle the life cycle of the search JOB when in the running state."
  (let ((limit paimon-search-results-limit)
        (offset (paimon-search-job-results-count job)))
    (when-let (results (aio-await (paimon-search-job-load-results-preview job offset limit)))
      (when (zerop offset)
        (paimon-search-jobs--refresh-results job))
      (message "Loaded %s preview search results for job %s."
               (paimon--bold (length results)) (paimon--bold (oref job id))))
    (aio-await (aio-sleep 1))
    t))

(aio-defun paimon-search-jobs--manage-lifecycle-run (job)
  "Start the life-cycle update loop for the search JOB."
  (condition-case error
      (when (paimon-search-job-refresh-p job)
        (while (when-let (job (aio-await (paimon-search-job-synchronize job)))
                 (paimon-search-jobs-render (paimon-search-job-profile job) t)
                 (aio-await
                  (pcase (paimon-search-job-dispatch-state job)
                    ("DONE" (paimon-search-jobs--lifecycle-done job))
                    ("FAILED" (paimon-search-jobs--lifecycle-failed job))
                    ("RUNNING" (paimon-search-jobs--lifecycle-running job))
                    (_ (aio-sleep 1) t)))))
        job)
    (error (message "Search job %s lifecycle handler died: %s %s"
                    (oref job id) (car error) (cdr error)))))

(defun paimon-search-jobs--manage-lifecycle (job)
  "Start the life-cycle update loop for the search JOB."
  (with-slots (id) job
    (let ((promise (paimon-search-jobs--manage-lifecycle-run job)))
      (ht-set paimon-search-jobs--lifecycle-registry id promise)
      promise)))

(defun paimon-search-jobs--manage-lifecycle-p (job)
  "Return t if the life cycle of the search JOB needs to be managed, otherwise nil."
  (and (paimon-search-job-refresh-p job)
       (not (paimon-search-jobs--lifecycle-registered-p job))))

(defun paimon-search-jobs--manage-lifecycles (jobs)
  "Manage the life cycle of the search JOBS."
  (thread-last jobs
    (seq-filter #'paimon-search-jobs--manage-lifecycle-p)
    (seq-map #'paimon-search-jobs--manage-lifecycle)))

(aio-defun paimon-search-jobs--apply-action (job action doing done)
  "Apply the search JOB ACTION and display the DOING and DONE messages."
  (when (and job (not (paimon-search-job-expired-p job)))
    (let ((id (paimon--bold (oref job id))))
      (when doing (message "%s search job %s ..." doing id))
      (aio-await (paimon-search-job-apply-action job action))
      (paimon-search-jobs-render (paimon-search-job-profile job) t t)
      (when done (message "Search job %s %s." id done)))))

(defun paimon-search-jobs-browse (job)
  "Browse the search JOB under point on the web."
  (interactive (list (paimon-search-job-under-point)))
  (when (and job (not (paimon-search-job-expired-p job)))
    (browse-url (paimon-api-job-url (paimon-api-for job) (oref job sid)))))

(defun paimon-search-jobs-cancel (job)
  "Cancel the search JOB under point."
  (interactive (list (paimon-search-job-under-point)))
  (paimon-search-jobs--apply-action job "cancel" "Canceling" "canceled"))

(defun paimon-search-jobs-delete (job)
  "Delete the search JOB."
  (interactive (list (paimon-search-job-under-point)))
  (when job
    (paimon-search-job-delete job)
    (paimon-search-jobs-render (paimon-search-job-profile job) t)))

(defun paimon-search-jobs-finalize (job)
  "Finalize the search JOB under point."
  (interactive (list (paimon-search-job-under-point)))
  (paimon-search-jobs--apply-action job "finalize" "Finalizing" "finalized"))

(defun paimon-search-jobs-pause (job)
  "Pause the search JOB under point."
  (interactive (list (paimon-search-job-under-point)))
  (paimon-search-jobs--apply-action job "pause" "Pausing" "paused"))

(defun paimon-search-jobs-unpause (job)
  "Un-pause the search JOB under point."
  (interactive (list (paimon-search-job-under-point)))
  (paimon-search-jobs--apply-action job "unpause" "Unpausing" "unpaused"))

(defvar paimon-search-jobs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'paimon-search-jobs-cancel)
    (define-key map (kbd "C-c C-o") 'paimon-search-jobs-browse)
    (define-key map (kbd "C-c C-w") 'paimon-search-jobs-browse)
    (define-key map (kbd "D") 'paimon-search-jobs-delete)
    (define-key map (kbd "F") 'paimon-search-jobs-finalize)
    (define-key map (kbd "P") 'paimon-search-jobs-pause)
    (define-key map (kbd "RET") 'paimon-search-results-show)
    (define-key map (kbd "U") 'paimon-search-jobs-unpause)
    (define-key map (kbd "c") 'paimon-search)
    (define-key map (kbd "w") 'paimon-profiles-list)
    map)
  "The key map for the `paimon-search-jobs-mode'.")

(define-derived-mode paimon-search-jobs-mode tabulated-list-mode "Search Jobs"
  "Special mode for search jobs buffers."
  (setq tabulated-list-entries #'paimon-search-jobs-list-entries)
  (setq tabulated-list-format paimon-search-jobs-list-format)
  (setq tabulated-list-sort-key paimon-search-jobs-list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1)
  (paimon-search-jobs--manage-lifecycles (paimon-search-jobs (paimon-db))))

(defun paimon-search-jobs-list (profile)
  "List the search jobs for PROFILE."
  (interactive (list (paimon-profile-current)))
  (let ((buffer (get-buffer-create (paimon-search-jobs-buffer-name profile))))
    (with-current-buffer buffer
      (switch-to-buffer buffer)
      (setq-local paimon-search-jobs-profile profile)
      (paimon-search-jobs-mode))))

(provide 'paimon-search-jobs)

;;; paimon-search-jobs.el ends here
