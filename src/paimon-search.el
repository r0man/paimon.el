;;; paimon-search.el --- Search commands -*- lexical-binding: t -*-

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

(defun paimon-search--index-names (db profile)
  "Return the search index names in DB for PROFILE."
  (seq-map #'paimon-data-index-name (paimon-data-indexes-by-profile db profile)))

(defun paimon-search--index-names->command (indexes)
  "Format the INDEXES for use in a search command."
  (thread-last indexes
    (seq-map (lambda (index) (format "index=%s" index)))
    (s-join " OR ")))

(cl-defun paimon-search--search-command (query &key indexes)
  "Return the search command for QUERY and INDEXES."
  (thread-last (list "search" (paimon-search--index-names->command indexes) query)
    (seq-remove #'s-blank-p)
    (s-join " ")))

(defun paimon-search-create (query &optional args)
  "Create a Splunk search for QUERY using ARGS."
  (interactive (list (or (and transient-current-prefix (oref transient-current-prefix scope))
                         (read-string "Search: " nil 'paimon-search-history))
                     (transient-args transient-current-command)))
  (let* ((db (paimon-db))
         (earliest-time (transient-arg-value "--earliest-time=" args))
         (latest-time (transient-arg-value "--latest-time=" args))
         (search-level (transient-arg-value "--search-level=" args))
         (indexes (when-let (indexes (paimon--transient-suffix-by-argument "--indexes=" transient-current-suffixes))
                    (oref indexes value)))
         (profile (or paimon-search-jobs-profile (paimon-profile-current db)))
         (job (closql-insert db (paimon-search-job
                                 :earliest-time (or (paimon--parse-time earliest-time) (paimon-search-job-default-earliest-time))
                                 :latest-time (or (paimon--parse-time latest-time) (current-time))
                                 :profile-id (oref profile id)
                                 :search (paimon-search--search-command query :indexes indexes)
                                 :search-level (or search-level "smart")))))
    (paimon-search-jobs-list profile)
    (paimon-search-results-show job)
    (aio-with-async
      (message "Creating search job %s ..." (paimon--bold (oref job search)))
      (let ((job (aio-await (paimon-search-job-create job))))
        (message "Search job %s created. Waiting for results ..." (paimon--bold (oref job id)))
        (aio-await (paimon-search-jobs--manage-lifecycle job))))))

(defun paimon-search--read-index (prompt initial-input history)
  "Read the search index using PROMPT, INITIAL-INPUT and HISTORY."
  (let ((indexes (paimon-search--index-names (paimon-db) (paimon-profile-current))))
    (string-join (completing-read-multiple prompt indexes nil nil initial-input history) ",")))

(transient-define-infix paimon-search--earliest-time ()
  :argument "--earliest-time="
  :class 'transient-option
  :description "The earliest time of the search"
  :key "-e"
  :reader #'paimon--read-time)

(transient-define-infix paimon-search--indexes ()
  :argument "--indexes="
  :class 'paimon-multi-value
  :description "The indexes to search in"
  :choices (lambda (&rest _args) (paimon-search--index-names (paimon-db) (paimon-profile-current)))
  :key "-i")

(transient-define-infix paimon-search--latest-time ()
  :argument "--latest-time="
  :class 'transient-option
  :description "The latest time of the search"
  :key "-l"
  :reader #'paimon--read-time)

(transient-define-infix paimon-search--search-level ()
  :argument "--search-level="
  :class 'transient-option
  :choices '("fast" "smart" "verbose")
  :description "The search level to use"
  :key "-L")

(transient-define-infix paimon-search--status-buckets ()
  :argument "--status-buckets="
  :class 'transient-option
  :description "The number of status buckets to generate"
  :key "-b"
  :reader 'transient-read-number-N0)

(transient-define-infix paimon-search--offset ()
  :argument "--offset="
  :class 'transient-option
  :description "The index of the first item to return"
  :key "-o"
  :reader 'transient-read-number-N0)

(transient-define-infix paimon-search--sort-dir ()
  :argument "--sort-dir="
  :class 'transient-option
  :choices '("asc" "desc")
  :description "The sort order of the results"
  :key "-s")

(transient-define-infix paimon-search--sort-mode ()
  :argument "--sort-mode="
  :class 'transient-option
  :choices '("auto" "alpha" "alpha-case" "num")
  :description "The collated ordering"
  :key "-M")

(transient-define-infix paimon-search--summarize ()
  :argument "--summarize"
  :class 'transient-switch
  :description "Whether to return a summarized result"
  :key "-S")

(defun paimon-search--description ()
  "Return the description of the `paimon-search' transient command."
  (let* ((query (oref transient--prefix scope))
         (indexes (oref (paimon--transient-suffix-by-argument "--indexes=" transient--suffixes) value))
         (profile (paimon-profile-current))
         (command (paimon-search--search-command query :indexes indexes)))
    (concat (propertize "Search Splunk" 'face 'transient-heading)
            (when profile
              (propertize (format " - %s" (paimon-profile-identity profile)) 'face 'transient-inactive-argument))
            "\n\n "
            (propertize command 'face 'transient-inactive-argument) "\n")))

;;;###autoload
(transient-define-prefix paimon-search (query)
  "Create a Splunk search job for QUERY."
  [:description
   paimon-search--description
   "Time"
   (paimon-search--earliest-time)
   (paimon-search--latest-time)
   "\nSort"
   (paimon-search--offset)
   (paimon-search--sort-dir)
   (paimon-search--sort-mode)
   "\nOther"
   (paimon-search--indexes)
   (paimon-search--search-level)
   (paimon-search--status-buckets)
   (paimon-search--summarize)]
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
