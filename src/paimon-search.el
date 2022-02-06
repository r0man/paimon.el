;;; paimon-search.el --- Search commands -*- lexical-binding: t -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Search commands

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'eieio)
(require 'f)
(require 'ht)
(require 'parse-time)
(require 'request)
(require 'seq)
(require 'paimon-api)
(require 'paimon-db)
(require 'paimon-profiles)
(require 'paimon-search-job)
(require 'paimon-search-jobs)
(require 'paimon-search-result)
(require 'paimon-search-results)
(require 'paimon-util)
(require 'subr-x)
(require 'transient)

(defun paimon-search-create (query &optional args)
  "Create a Splunk search for QUERY using ARGS."
  (interactive (list (or (and transient-current-prefix (oref transient-current-prefix scope))
                         (read-string "Search: " nil 'paimon-search-history))
                     (transient-args transient-current-command)))
  (let* ((db (paimon-db))
         (earliest-time (transient-arg-value "--earliest-time=" args))
         (latest-time (transient-arg-value "--latest-time=" args))
         (search-level (transient-arg-value "--search-level=" args))
         (profile (or paimon-search-jobs-profile (paimon-profile-current db)))
         (job (closql-insert db (paimon-search-job
                                 :earliest-time (or (paimon--parse-time earliest-time) (paimon-search-job-default-earliest-time))
                                 :latest-time (or (paimon--parse-time latest-time) (current-time))
                                 :profile-id (oref profile id)
                                 :search (concat "search " query)
                                 :search-level (or search-level "smart")))))
    (paimon-search-jobs-list profile)
    (paimon-search-results-show job)
    (aio-with-async
      (message "Creating search job %s ..." (paimon--bold (oref job search)))
      (let ((job (aio-await (paimon-search-job-create job))))
        (message "Search job %s created." (paimon--bold (oref job id)))
        (aio-await (paimon-search-jobs--manage-lifecycle job))))))

(transient-define-infix paimon-search:--earliest-time ()
  :argument "--earliest-time="
  :class 'transient-option
  :description "Sets the earliest time bounds for the search."
  :key "-e"
  :reader #'paimon--read-time)

(transient-define-infix paimon-search:--latest-time ()
  :argument "--latest-time="
  :class 'transient-option
  :description "Sets the latest time bounds for the search."
  :key "-l"
  :reader #'paimon--read-time)

(transient-define-infix paimon-search:--search-level ()
  :argument "--search-level="
  :class 'transient-option
  :choices '("fast" "smart" "verbose")
  :description "Change the search level."
  :key "-L")

(transient-define-infix paimon-search:--status-buckets ()
  :argument "--status-buckets="
  :class 'transient-option
  :description "The number of status buckets to generate."
  :key "-B"
  :reader 'transient-read-number-N0)

(defun paimon-search--description ()
  "Return the description of the `paimon-search' transient command."
  (concat (propertize "Search Splunk" 'face 'transient-heading) "\n\n "
          (propertize (oref transient--prefix scope) 'face 'transient-inactive-argument) "\n\n"
          (propertize "Options" 'face 'transient-heading)))

;;;###autoload
(transient-define-prefix paimon-search (query)
  "Create a Splunk search job for QUERY."
  [:description
   paimon-search--description
   (paimon-search:--earliest-time)
   (paimon-search:--latest-time)
   (paimon-search:--search-level)
   (paimon-search:--status-buckets)]
  ["Actions"
   ("c" "Create search job" paimon-search-create)]
  (interactive (list (read-string "Search: " nil 'paimon-search-history)))
  (transient-setup 'paimon-search nil nil :scope query))

;;;###autoload
(defun paimon-search-at-point ()
  "Create a Splunk search job for the active region or the thing under point."
  (interactive)
  (paimon-search (read-string "Search: " (paimon--thing-at-point) 'paimon-search-history)))

(provide 'paimon-search)

;;; paimon-search.el ends here
