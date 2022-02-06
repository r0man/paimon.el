;;; paimon.el --- An major mode for Splunk -*- lexical-binding: t -*-

;; Copyright (C) 2022 r0man

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>
;; Keywords: paimon, search, tools
;; Package-Requires: ((aio "1.0") (closql "1.2.0") (emacs "27.1") (emacsql "3.0.0") (emacsql-sqlite "3.0.0") (f "0.20.0") (ht "2.4") (parseedn "1.0.6") (transient "0.3.7") (request "0.3.3"))
;; Homepage: https://github.com/r0man/paimon.el
;; Version: 0.1.0

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

;; An Emacs mode for Splunk

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
  "Search for QUERY using ARGS."
  (interactive (list (read-string "Search: " nil 'paimon-search-history)
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

;;;###autoload
(transient-define-prefix paimon-search ()
  "Execute the Datomic query at point against a Nubank service."
  ["Options"
   (paimon-search:--earliest-time)
   (paimon-search:--latest-time)
   (paimon-search:--search-level)
   (paimon-search:--status-buckets)]
  ["Actions"
   ("c" "Create search job" paimon-search-create)])

(defun paimon--db-reset-doc ()
  "Return the reset database documentation string."
  (format "Reset the database in %s." (paimon--bold paimon-db-filename)))

;;;###autoload
(defun paimon ()
  "Show the search jobs of the current profile."
  (interactive)
  (let ((db (paimon-db)))
    (if-let (profile (paimon-profile-current db))
        (paimon-search-jobs-list profile)
      (paimon-search-jobs-list (paimon-profile-setup db)))))

(provide 'paimon)

;;; paimon.el ends here
