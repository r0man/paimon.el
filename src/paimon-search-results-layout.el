;; paimon-search-results-layout.el --- Search results layout -*- lexical-binding: t; -*-

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

;; Search results layout

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'ht)
(require 'seq)
(require 'paimon-search-job)
(require 'paimon-search-result)
(require 'paimon-util)

(defclass paimon-search-results-layout ()
  ((id
    :accessor paimon-search-results-layout-id
    :documentation "The id of the search results layout."
    :initarg :id
    :initform 'default
    :type symbol))
  "Search result layout showing all fields.")

(defvar paimon-search-results-layouts
  (list (paimon-search-results-layout))
  "The list of layouts in which to display search results.")

(cl-defgeneric paimon-search-results-entry-fn (job layout)
  "Return a function that return the tabulated-list entry using JOB and LAYOUT.")

(cl-defgeneric paimon-search-results-layout-format (job layout)
  "Return the tabulated-list format of the JOB using LAYOUT.")

(cl-defgeneric paimon-search-results-layout-sort-key (job layout)
  "Return the tabulated-list sort key of the JOB using LAYOUT.")

(cl-defmethod paimon-search-results-entry-fn (job (layout paimon-search-results-layout))
  "Return the tabulated-list format of the JOB using LAYOUT."
  (when-let (readers (paimon-search-results-layout--readers job layout))
    (lambda (result)
      (let ((columns (seq-map (lambda (reader) (funcall reader result)) readers)))
        (unless (seq-every-p #'null columns)
          (list (oref result id)
                (apply #'vector (seq-map (lambda (column) (format "%s" (or column ""))) columns))))))))

(cl-defmethod paimon-search-results-layout-format (job (layout paimon-search-results-layout))
  "Return the tabulated-list format of the JOB using LAYOUT."
  (ignore layout)
  (when-let (max-lengths (paimon-search-job-max-field-lengths job))
    (let ((max-lengths (ht<-alist max-lengths)))
      (thread-last (seq-take (paimon-search-job-field-names job) 10)
        (seq-map (lambda (field)
                   (let ((max-length (ht-get max-lengths field)))
                     (paimon-search-results-layout--format-entry field max-length))))
        (apply #'vector)))))

(cl-defmethod paimon-search-results-layout-sort-key (job (layout paimon-search-results-layout))
  "Return the tabulated-list sort key of the JOB using LAYOUT."
  (ignore job layout))

(defun paimon-search-results-layout-completing-read (job)
  "Completing read a search result layout for the search JOB."
  (let* ((completion-extra-properties '(:annotation-function paimon-search-results-layout--annotation-function))
         (layouts (paimon-search-results-layout--filter-supported job paimon-search-results-layouts))
         (selected (intern-soft (completing-read "Layout: " (seq-map (lambda (layout) (list (oref layout id) layout)) layouts) nil t))))
    (seq-find (lambda (layout) (eql selected (oref layout id))) layouts)))

(defun paimon-search-results-layout-find (job)
  "Find the search result LAYOUT for the search JOB."
  (car (paimon-search-results-layout--filter-supported job paimon-search-results-layouts)))

(defun paimon-search-results-layout--annotate (layout)
  "Return the `completing-read' annotation for LAYOUT."
  (when-let (documentation (paimon-search-results-layout--documentation layout))
    (concat " - " (propertize documentation 'face 'marginalia-documentation))))

(defun paimon-search-results-layout--documentation (layout)
  "Return the documentation of LAYOUT."
  (documentation-property (eieio-object-class layout) 'variable-documentation))

(defun paimon-search-results-layout--format-entry (field max-length)
  "Return the search format entry for FIELD and MAX-LENGTH."
  (list (paimon--human-name field) (or max-length (length field)) t :data (vector field)))

(defun paimon-search-results-layout--readers (job layout)
  "Return the entry reader functions for the search JOB and LAYOUT."
  (seq-map (lambda (entry)
             (let* ((options (seq-drop entry 3))
                    (path (seq-into (plist-get options :data) 'list)))
               (lambda (result)
                 (with-slots (data) result
                   (condition-case nil
                       (when-let (value (apply #'ht-get* data path))
                         (s-trim (format "%s" value)))
                     (wrong-type-argument))))))
           (paimon-search-results-layout-format job layout)))

(defun paimon-search-results-layout--supported-p (job layout results)
  "Return t if the LAYOUT is supported by the RESULTS of the search JOB, otherwise nil."
  (let ((entry-fn (paimon-search-results-entry-fn job layout)))
    (seq-every-p (lambda (result) (funcall entry-fn result)) results)))

(defun paimon-search-results-layout--filter-supported (job layouts)
  "Filter the LAYOUTS supported by the search JOB."
  (let ((results (paimon-search-results-by-job job paimon-search-results-offset 10)))
    (seq-filter (lambda (layout) (paimon-search-results-layout--supported-p job layout results)) layouts)))

(defun paimon-search-results-layout--annotation-function (layout-name)
  "Return the annotation for LAYOUT-NAME using `minibuffer-completion-table'."
  (when-let (layout (cadr (assoc (intern-soft layout-name) minibuffer-completion-table)))
    (paimon-search-results-layout--annotate layout)))

;; Minimal Layout

(defclass paimon-search-results-layout-minimal (paimon-search-results-layout)
  ((id :initform 'minimal))
  "Search result layout showing the time and raw fields.")

(add-to-list 'paimon-search-results-layouts (paimon-search-results-layout-minimal))

(cl-defmethod paimon-search-results-layout-format (job (layout paimon-search-results-layout-minimal))
  "Return the tabulated-list format of the JOB using LAYOUT."
  (ignore job layout)
  (vector (list "Time" 29 t :data ["_time"] :pad-right 2)
          (list "Raw" 100 t :data ["_raw"])))

;; Raw Layout

(defclass paimon-search-results-layout-raw (paimon-search-results-layout)
  ((id :initform 'raw))
  "Search result layout showing the raw field.")

(add-to-list 'paimon-search-results-layouts (paimon-search-results-layout-raw))

(cl-defmethod paimon-search-results-layout-format (job (layout paimon-search-results-layout-raw))
  "Return the tabulated-list format of the JOB using LAYOUT."
  (ignore job layout)
  (vector (list "Raw" 100 t :data ["_raw"])))

(provide 'paimon-search-results-layout)

;;; paimon-search-results-layout.el ends here
