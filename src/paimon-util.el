;; paimon-util-job.el --- Paimon Util -*- lexical-binding: t; -*-

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

;; Paimon Util

;;; Code:

(require 'eieio)
(require 'ht)
(require 'json)
(require 'org)
(require 'parse-time)
(require 's)
(require 'seq)
(require 'transient)

(defun paimon--human-name (s)
  "Return the human name of S."
  (when s
    (thread-last
      (format "%s" s)
      (replace-regexp-in-string "[_-]" " ")
      (string-trim)
      (s-titleize))))

(defun paimon--format-time (time)
  "Format the TIME."
  (when time (format-time-string "%FT%T%z" time)))

(defun paimon--format-time-human (time)
  "Format the TIME for humans."
  (when time (format-time-string "%F %T" time)))

(defun paimon--parse-time (time)
  "Format the TIME."
  (when (stringp time)
    (parse-iso8601-time-string time)))

(defun paimon--read-time (prompt initial-input _history)
  "Read a date and time using PROMPT, INITIAL-INPUT and HISTORY."
  (when-let (time (org-read-date t 'to-time nil prompt nil initial-input))
    (paimon--format-time time)))

(defun paimon--bold (s)
  "Return S with a bold face."
  (when s (propertize (format "%s" s) 'face 'bold)))

(defun paimon-uuid ()
  "Return a new UUID."
  (let ((s (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        user-mail-address
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (random)
                        (recent-keys)))))
    (format "%s-%s-3%s-%s-%s"
            (substring s 0 8)
            (substring s 8 12)
            (substring s 13 16)
            (substring s 16 20)
            (substring s 20 32))))

(defun paimon--pretty-print-json (object)
  "Pretty print the JSON OBJECT."
  (with-current-buffer (get-buffer-create "*json*")
    (if (fboundp 'json-insert)
        (json-insert object)
      (insert (json-encode object)))
    (json-pretty-print-buffer)
    (js-mode)
    (switch-to-buffer-other-window (current-buffer))))

(defun paimon--thing-at-point ()
  "Return the active region or the thing at point."
  (string-trim (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'line t))))

(defun paimon--transient-arg-multi-value (arg args)
  "Return the value of the multi value ARG as it appears in ARGS."
  (thread-last
    args
    (seq-filter (lambda (x) (and (listp x) (equal arg (car x)))))
    (seq-first)
    (seq-rest)))

(defun paimon--transient-suffix-by-argument (argument suffixes)
  "Find the option `transient-option' in SUFFIXES by ARGUMENT."
  (thread-last
    suffixes
    (seq-filter (lambda (obj)
                  (and (cl-typep obj 'transient-option)
                       (equal argument (oref obj argument)))))
    (seq-first)))

(defmacro paimon-with-errors (&rest body)
  "Evaluate BODY and handle errors."
  (let ((error-sym (gensym "error")))
    `(condition-case ,error-sym
         (progn ,@body)
       (error
        (message (concat (format "%s" (or (cadr ,error-sym) (car ,error-sym)))))
        ,error-sym))))

;; Transient multi value

(defun paimon-multi-value--value-match (re)
  "Match the transient prefix value against RE."
  (when-let ((match (cl-find-if (lambda (v)
                                  (and (stringp v)
                                       (string-match re v)))
                                (oref transient--prefix value))))
    (match-string 1 match)))

(defclass paimon-multi-value (transient-option)
  ((multi-value :initarg :multi-value :initform t)))

(cl-defmethod transient-init-value ((obj paimon-multi-value))
  "Set the initial value of the object OBJ."
  (if-let (value (paimon-multi-value--value-match (format "\\`%s\\(.*\\)" (oref obj argument))))
      (oset obj value (s-split "," value))
    (oset obj value "")))

(cl-defmethod transient-infix-value ((obj paimon-multi-value))
  "Return (cons ARGUMENT VALUE) or nil.

ARGUMENT and VALUE are the values of the respective slots of OBJ.
If VALUE is nil, then return nil.  VALUE may be the empty string,
which is not the same as nil."
  ;; (message "transient-infix-value: %s" obj)
  (when-let ((value (oref obj value)))
    (concat (oref obj argument)
            (if (oref obj multi-value)
                (string-join value ",")
              value))))

(provide 'paimon-util)

;;; paimon-util.el ends here
