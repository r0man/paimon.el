;; paimon-search-results-layout.el --- Search results layout -*- lexical-binding: t; -*-

;; Copyright © 2022 r0man

;; This file is NOT part of GNU Emacs.

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
    :type symbol)
   (required-fields
    :accessor paimon-search-results-layout-required-fields
    :documentation "The required fields of the search results layout."
    :initarg :required-fields
    :initform nil
    :type list))
  "Search result layout showing all fields.")

(defvar paimon-search-results-layouts
  (list (paimon-search-results-layout))
  "The list of layouts in which to display search results.")

(cl-defgeneric paimon-search-results-layout-entry-fn (job layout)
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
        (list (oref result id)
              (apply #'vector columns))))))

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
  (when-let (field (seq-first (paimon-search-results-layout-format job layout)))
    (cons (seq-first field) t)))

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
                   (format "%s" (or (apply #'ht-get* data path) ""))))))
           (paimon-search-results-layout-format job layout)))

(defun paimon-search-results-layout--supported-p (job layout)
  "Return t if the LAYOUT is supported by the search JOB, otherwise nil."
  (with-slots (required-fields) layout
    (cl-subsetp required-fields (paimon-search-job-field-names job) :test #'equal)))

(defun paimon-search-results-layout--filter-supported (job layouts)
  "Filter the LAYOUTS supported by the search JOB."
  (seq-filter (lambda (layout) (paimon-search-results-layout--supported-p job layout)) layouts))

(defun paimon-search-results-layout--annotation-function (layout-name)
  "Return the annotation for LAYOUT-NAME using `minibuffer-completion-table'."
  (when-let (layout (cadr (assoc (intern-soft layout-name) minibuffer-completion-table)))
    (paimon-search-results-layout--annotate layout)))

;; Minimal Layout

(defclass paimon-search-results-layout-minimal (paimon-search-results-layout)
  ((id :initform 'minimal)
   (required-fields :initform '("_time" "_raw")))
  "Search result layout showing the time and raw fields.")

(add-to-list 'paimon-search-results-layouts (paimon-search-results-layout-minimal))

(cl-defmethod paimon-search-results-layout-format (job (layout paimon-search-results-layout-minimal))
  "Return the tabulated-list format of the JOB using LAYOUT."
  (ignore job layout)
  (vector (list "Time" 29 t :data ["_time"] :pad-right 2)
          (list "Raw" 100 t :data ["_raw"])))

;; Raw Layout

(defclass paimon-search-results-layout-raw (paimon-search-results-layout)
  ((id :initform 'raw)
   (required-fields :initform '("_raw")))
  "Search result layout showing the raw field.")

(add-to-list 'paimon-search-results-layouts (paimon-search-results-layout-raw))

(cl-defmethod paimon-search-results-layout-format (job (layout paimon-search-results-layout-raw))
  "Return the tabulated-list format of the JOB using LAYOUT."
  (ignore job layout)
  (vector (list "Raw" 100 t :data ["_raw"])))

(provide 'paimon-search-results-layout)

;;; paimon-search-results-layout.el ends here
