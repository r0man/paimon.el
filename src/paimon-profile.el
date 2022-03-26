;; paimon-profile.el --- Paimon profile -*- lexical-binding: t; -*-

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

;; Paimon profile

;;; Code:

(require 'cl-lib)
(require 'closql)
(require 'eieio)
(require 'paimon-api)
(require 'paimon-data-index)
(require 'paimon-db)
(require 'paimon-util)
(require 'request)
(require 'seq)

(defclass paimon-profile (closql-object)
  ((closql-table :initform 'profile)
   (closql-primary-key :initform 'id)
   (id
    :accessor paimon-search-profile-id
    :documentation "The local database id of the profile."
    :initarg :id
    :initform (eval '(paimon-uuid))
    :type string)
   (auth-type
    :accessor paimon-profile-auth-type
    :custom (choice (const "basic") (const "bearer"))
    :documentation "The type of authentication used for the Splunk API."
    :initarg :auth-type
    :initform "basic"
    :type string)
   (default
     :accessor paimon-profile-default
     :custom (choice (const :tag "No" 0) (const :tag "Yes" 1))
     :documentation "Set to t if the profile is the default."
     :initarg :default
     :initform 0
     :type number)
   (hostname
    :accessor paimon-profile-hostname
    :custom string
    :documentation "The hostname of the Splunk API."
    :initarg :hostname
    :initform "localhost"
    :type string)
   (identity
    :accessor paimon-profile-identity
    :custom string
    :documentation "The identity used to lookup the token or password in `auth-sources'."
    :initarg :identity
    :initform "admin"
    :type string)
   (port
    :accessor paimon-profile-port
    :custom number
    :documentation "The port number on which the Splunk API is listening on (default: 8089)."
    :initarg :port
    :initform 8089
    :type number)
   (protocol
    :accessor paimon-profile-protocol
    :custom (choice (const "https") (const "http"))
    :documentation "The protocol to use for communication with the Splunk API."
    :initarg :protocol
    :initform "https"
    :type string))
  "A class representing a profile.")

(defun paimon-profiles (db)
  "Return all profiles from DB."
  (closql-entries db nil 'paimon-profile))

(defun paimon-profile-current (&optional db)
  "Return the current profile from DB or setup a new one."
  (let ((db (or db (paimon-db))))
    (or (thread-last
          (paimon-db-sql [:select [*] :from profile :where (= default 1)])
          (seq-map (lambda (row) (closql--remake-instance 'paimon-profile db row)))
          (seq-first))
        (paimon-profile-setup db))))

(defun paimon-profile-by-id (db id)
  "Find the profile in DB by ID."
  (closql-get db id 'paimon-profile))

(defun paimon-profile-set-default (profile)
  "Change PROFILE to be the default profile."
  (with-slots (id) profile
    (emacsql (paimon-db) [:update profile :set (= default (= id $s1))] id)
    (closql-reload profile)))

(defun paimon-profile--secret-prompt (profile)
  "Return the secret prompt of the PROFILE."
  (pcase (oref profile auth-type)
    ("basic" "Please enter the password for %u at Splunk API %h:%p")
    ("bearer" "Please enter the token for %u at Splunk API %h:%p")))

(defun paimon-profile-secret (profile &optional create)
  "Return the secret of the PROFILE from `auth-sources' using CREATE, or ask the user."
  (with-slots (hostname identity port) profile
    (let* ((auth-source-creation-prompts `((secret . ,(paimon-profile--secret-prompt profile))))
           (auth-source (car (auth-source-search
                              :create create
                              :host hostname
                              :max-tokens 1
                              :port (number-to-string port)
                              :user identity))))
      (when-let (save-function (plist-get auth-source :save-function))
        (funcall save-function))
      (when-let (secret (plist-get auth-source :secret))
        (funcall secret)))))

(defun paimon-profile--read-auth-type ()
  "Read the authentication type from the user."
  (completing-read "Authentication type: " '("basic" "bearer") nil t nil 'paimon-profile-auth-type))

(defun paimon-profile--read-protocol ()
  "Read the authentication type from the user."
  (completing-read "Protocol: " '("https" "http") nil t nil 'paimon-profile-protocol))

(defun paimon-profile--read-hostname ()
  "Read the authentication type from the user."
  (read-string "Hostname: " nil 'paimon-profile-hostname))

(defun paimon-profile--read-port ()
  "Read the authentication type from the user."
  (read-number "Port: " 8089))

(defun paimon-profile--read-identity ()
  "Read the identity from the user."
  (read-string "Identity: " nil 'paimon-profile-identity))

(defun paimon-profile--read ()
  "Read a profile from the user."
  (paimon-profile
   :auth-type (paimon-profile--read-auth-type)
   :protocol (paimon-profile--read-protocol)
   :hostname (paimon-profile--read-hostname)
   :port (paimon-profile--read-port)
   :identity (paimon-profile--read-identity)))

(defun paimon-profile-setup (db &optional profile)
  "Setup the PROFILE in DB."
  (interactive (list (paimon-db)))
  (let ((profile (closql-insert db (or profile (paimon-profile--read)) t)))
    (with-slots (auth-type) profile
      (when (and (equal "bearer" auth-type)
                 (not (paimon-profile-secret profile))
                 (yes-or-no-p "Do you want to grab your authorization token from Splunk?"))
        (browse-url (paimon-api-authorization-tokens-url (paimon-api-for profile))))
      (paimon-profile-secret profile t)
      (paimon-data-indexes-synchronize db profile)
      (if (equal 1 (length (paimon-profiles db)))
          (paimon-profile-set-default profile)
        profile))))

(cl-defmethod paimon-api-for ((profile paimon-profile))
  "Return the API for PROFILE."
  (with-slots (auth-type hostname identity port protocol) profile
    (paimon-api :auth-type auth-type :hostname hostname :identity identity :port port :protocol protocol)))

(provide 'paimon-profile)

;;; paimon-profile.el ends here
