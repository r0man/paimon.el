;; paimon-search-results.el --- Search results -*- lexical-binding: t; -*-

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

;; Search results

;;; Code:

(require 'aio)
(require 'ht)
(require 'paimon-search-job)
(require 'paimon-search-result)
(require 'paimon-search-results-layout)
(require 'tabulated-list)

(defcustom paimon-search-results-buffer-template
  "*paimon-search-results-%s*"
  "The template of the search results buffer."
  :group 'paimon
  :safe #'stringp
  :type 'string)

(defvar-local paimon-search-results-job nil
  "The search results job.")

(put 'paimon-search-results-job 'permanent-local t)

(defvar-local paimon-search-results-layout-selected nil
  "The selected layout of the search results buffer.")

(defun paimon-search-results-buffer-name (profile)
  "Return the search jobs buffer name for PROFILE."
  (format paimon-search-results-buffer-template (oref profile identity)))

(defun paimon-search-results-list-entries (job layout &optional query)
  "Return a function that return the tabulated list entries.

JOB    - The search job of the results.
LAYOUT - Format list entries according to this layout.
QUERY  - Run a SQL LIKE query on the data of the result."
  (lambda ()
    (let ((entry-fn (paimon-search-results-entry-fn job layout)))
      (seq-map (lambda (result)
                 (funcall entry-fn result))
               (if query
                   (paimon-search-results-search-like job query)
                 (paimon-search-results-by-job job))))))

(defun paimon-search-results-show (job)
  "Show the results of the search JOB."
  (interactive (list (paimon-search-job-under-point)))
  (with-slots (id search) job
    (let ((profile (paimon-search-job-profile job)))
      (with-current-buffer (get-buffer-create (paimon-search-results-buffer-name profile))
        (setq-local paimon-search-results-job job)
        (paimon-search-results-mode)
        (let ((split-width-threshold nil))
          (switch-to-buffer-other-window (current-buffer)))))))

(defun paimon-search-results--profile ()
  "Return the profile of the the search results mode."
  (when-let (job paimon-search-results-job)
    (paimon-search-job-profile job)))

(defun paimon-search-results-next-line (&optional n)
  "Move N lines forward (backward if N is negative) and show the search result under point."
  (interactive "P")
  (forward-line n)
  (when (get-buffer-window (paimon-search-result-buffer-name (paimon-search-results--profile)))
    (when-let (result (paimon-search-result-under-point))
      (paimon-search-result-render result))))

(defun paimon-search-results-previous-line (&optional n)
  "Move cursor vertically up N lines and show the search result under point."
  (interactive "P")
  (paimon-search-results-next-line (- (or n 1))))

(defun paimon-search-results--load-p (job results offset limit)
  "Return t if the search RESULTS of JOB should be loaded, otherwise nil."
  (< (+ offset (length results)) (min (+ offset limit) (paimon-search-job-event-count job))))

(defun paimon-search-results--pagination-summary (job &optional offset limit)
  "Return the results pagination summary for the search JOB using OFFSET and LIMIT."
  (let ((limit (or limit paimon-search-results-limit))
        (offset (or offset paimon-search-results-offset))
        (event-count (paimon-search-job-event-count job)))
    (format "search result %s - %s of %s in total"
            (paimon--bold (+ 1 offset))
            (paimon--bold (min event-count (+ offset limit)))
            (paimon--bold (paimon-search-job-event-count job)))))

(defun paimon-search-results--message-loading (job &optional offset limit)
  "Display a message that results are loaded for the search JOB using OFFSET and LIMIT."
  (message "Loading %s ..." (paimon-search-results--pagination-summary job offset limit)))

(defun paimon-search-results--message-loaded (job &optional offset limit)
  "Display a message that results are shown for the search JOB using OFFSET and LIMIT."
  (message "Showing %s." (paimon-search-results--pagination-summary job offset limit)))

(aio-defun paimon-search-results-view (job &optional offset limit)
  "Show the search results for JOB at OFFSET using LIMIT."
  (interactive (list paimon-search-results-job))
  (let ((results (paimon-search-results-by-job job offset limit)))
    (when (paimon-search-results--load-p job results offset limit)
      (paimon-search-results--message-loading job)
      (aio-await (if (member (paimon-search-job-dispatch-state job) '("RUNNING"))
                     (paimon-search-job-load-results-preview job offset limit)
                   (paimon-search-job-load-results job offset limit))))
    (revert-buffer)
    (paimon-search-results--message-loaded job)))

(defun paimon-search-results-post-command-hook ()
  "Called after each command to trigger pagination when necessary."
  ;; TODO: At end of buffer. Implement auto loading.
  ;; (when (> (line-number-at-pos (point)) (- (line-number-at-pos (point-max)) 10)))
  )

(defun paimon-search-results-next (job)
  "Show the next results of the search JOB."
  (interactive (list paimon-search-results-job))
  (when job
    (let ((offset (+ paimon-search-results-offset paimon-search-results-limit)))
      (when (< offset (paimon-search-job-event-count job))
        (setq-local paimon-search-results-offset offset)
        (paimon-search-results-view job offset paimon-search-results-limit)))))

(defun paimon-search-results-previous (job)
  "Show the previous results of the search JOB."
  (interactive (list paimon-search-results-job))
  (when job
    (let ((offset (- paimon-search-results-offset paimon-search-results-limit)))
      (when (>= offset 0)
        (setq-local paimon-search-results-offset offset)
        (paimon-search-results-view job offset paimon-search-results-limit)))))

(defun paimon-search-results--setup-list (job layout)
  "Setup the search results for JOB using LAYOUT."
  (setq paimon-search-results-layout-selected layout)
  (setq tabulated-list-entries (paimon-search-results-list-entries job layout))
  (setq tabulated-list-format (paimon-search-results-layout-format job layout))
  (setq tabulated-list-sort-key (paimon-search-results-layout-sort-key job layout))
  (tabulated-list-init-header)
  (tabulated-list-print))

(defun paimon-search-results-switch-layout (job)
  "Switch the search results layout of the search JOB."
  (interactive (list paimon-search-results-job))
  (when job
    (when-let (layout (paimon-search-results-layout-completing-read job))
      (paimon-search-results--setup-list job layout))))

(defun paimon-search-results-filter (job)
  "Filter the results of the search JOB."
  (interactive (list paimon-search-results-job))
  (let* ((profile (paimon-search-job-profile job))
         (hook (lambda (beg end len)
                 (ignore beg end len)
                 (let ((query (minibuffer-contents)))
                   (with-current-buffer (get-buffer-create (paimon-search-results-buffer-name profile))
                     (let ((layout (or paimon-search-results-layout-selected (paimon-search-results-layout-find job)))
                           (wild-card-query (concat "%" (replace-regexp-in-string "\s+" "%" query) "%")))
                       (setq-local paimon-search-results-offset 0)
                       (setq tabulated-list-entries (paimon-search-results-list-entries job layout wild-card-query))
                       (tabulated-list-print)
                       nil))))))
    (minibuffer-with-setup-hook
        (lambda () (add-hook 'after-change-functions hook))
      (read-string "Filter search results: ")
      (remove-hook 'after-change-functions hook))))

(defvar paimon-search-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "L") 'paimon-search-results-switch-layout)
    (define-key map (kbd "N") 'paimon-search-results-next)
    (define-key map (kbd "P") 'paimon-search-results-previous)
    (define-key map (kbd "RET") 'paimon-search-result-show)
    (define-key map (kbd "c") 'paimon-search)
    (define-key map (kbd "f") 'paimon-search-results-filter)
    (define-key map (kbd "l") 'paimon-search-jobs-list)
    (define-key map (kbd "n") 'paimon-search-results-next-line)
    (define-key map (kbd "p") 'paimon-search-results-previous-line)
    (define-key map (kbd "s") 'paimon-search-results-search)
    (define-key map (kbd "w") 'paimon-profiles-list)
    map)
  "The key map for the `paimon-search-results-mode'.")

(define-derived-mode paimon-search-results-mode tabulated-list-mode "Search Results"
  "Special mode for search results buffers."
  (let* ((job paimon-search-results-job)
         (layout (or paimon-search-results-layout-selected (paimon-search-results-layout-find job))))
    (cl-check-type job paimon-search-job)
    (hl-line-mode 1)
    (when (fboundp 'evil-set-initial-state)
      (evil-set-initial-state 'paimon-search-results-mode 'emacs))
    (paimon-search-results--setup-list job layout)
    (add-hook 'post-command-hook #'paimon-search-results-post-command-hook nil t)))

(provide 'paimon-search-results)

;;; paimon-search-results.el ends here
