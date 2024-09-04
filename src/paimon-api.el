;; paimon-api.el --- Splunk API -*- lexical-binding: t; -*-

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

;; Splunk API

;;; Code:

(require 'aio)
(require 'cl-lib)
(require 'eieio)
(require 'ht)
(require 'paimon-util)
(require 'request)
(require 's)
(require 'seq)
(require 'subr-x)

(define-error 'paimon-api-error "Splunk API error")

(defvar paimon-api-search-job-actions
  '("cancel" "disablepreview" "enablepreview" "finalize" "pause" "setpriority" "setttl" "touch" "unpause")
  "The search job actions.")

;; Taken from elfeed-curl.el. Thanks skeeto!
(defconst paimon-curl-errors
  '((1 . "Unsupported protocol.")
    (2 . "Failed to initialize.")
    (3 . "URL malformed. The syntax was not correct.")
    (4 . "A feature or option that was needed to perform the desired request was not enabled or was explicitly disabled at build-time.")
    (5 . "Couldn't resolve proxy. The given proxy host could not be resolved.")
    (6 . "Couldn't resolve host. The given remote host was not resolved.")
    (7 . "Failed to connect to host.")
    (8 . "FTP weird server reply. The server sent data curl couldn't parse.")
    (9 . "FTP access denied.")
    (11 . "FTP weird PASS reply.")
    (13 . "FTP weird PASV reply.")
    (14 . "FTP weird 227 format.")
    (15 . "FTP can't get host.")
    (17 . "FTP couldn't set binary.")
    (18 . "Partial file. Only a part of the file was transferred.")
    (19 . "FTP couldn't download/access the given file, the RETR (or similar) command failed.")
    (21 . "FTP quote error. A quote command returned error from the server.")
    (22 . "HTTP page not retrieved.")
    (23 . "Write error.")
    (25 . "FTP couldn't STOR file.")
    (26 . "Read error. Various reading problems.")
    (27 . "Out of memory. A memory allocation request failed.")
    (28 . "Operation timeout.")
    (30 . "FTP PORT failed.")
    (31 . "FTP couldn't use REST.")
    (33 . "HTTP range error. The range \"command\" didn't work.")
    (34 . "HTTP post error. Internal post-request generation error.")
    (35 . "SSL connect error. The SSL handshaking failed.")
    (36 . "FTP bad download resume.")
    (37 . "FILE couldn't read file.")
    (38 . "LDAP bind operation failed.")
    (39 . "LDAP search failed.")
    (41 . "Function not found. A required LDAP function was not found.")
    (42 . "Aborted by callback.")
    (43 . "Internal error. A function was called with a bad parameter.")
    (45 . "Interface error. A specified outgoing interface could not be used.")
    (47 . "Too many redirects.")
    (48 . "Unknown option specified to libcurl.")
    (49 . "Malformed telnet option.")
    (51 . "The peer's SSL certificate or SSH MD5 fingerprint was not OK.")
    (52 . "The server didn't reply anything, which here is considered an error.")
    (53 . "SSL crypto engine not found.")
    (54 . "Cannot set SSL crypto engine as default.")
    (55 . "Failed sending network data.")
    (56 . "Failure in receiving network data.")
    (58 . "Problem with the local certificate.")
    (59 . "Couldn't use specified SSL cipher.")
    (60 . "Peer certificate cannot be authenticated with known CA certificates.")
    (61 . "Unrecognized transfer encoding.")
    (62 . "Invalid LDAP URL.")
    (63 . "Maximum file size exceeded.")
    (64 . "Requested FTP SSL level failed.")
    (65 . "Sending the data requires a rewind that failed.")
    (66 . "Failed to initialise SSL Engine.")
    (67 . "The user name, password, or similar was not accepted and curl failed to log in.")
    (68 . "File not found on TFTP server.")
    (69 . "Permission problem on TFTP server.")
    (70 . "Out of disk space on TFTP server.")
    (71 . "Illegal TFTP operation.")
    (72 . "Unknown TFTP transfer ID.")
    (73 . "File already exists (TFTP).")
    (74 . "No such user (TFTP).")
    (75 . "Character conversion failed.")
    (76 . "Character conversion functions required.")
    (77 . "Problem with reading the SSL CA cert (path? access rights?).")
    (78 . "The resource referenced in the URL does not exist.")
    (79 . "An unspecified error occurred during the SSH session.")
    (80 . "Failed to shut down the SSL connection.")
    (82 . "Could not load CRL file, missing or wrong format (added in 7.19.0).")
    (83 . "Issuer check failed (added in 7.19.0).")
    (84 . "The FTP PRET command failed")
    (85 . "RTSP: mismatch of CSeq numbers")
    (86 . "RTSP: mismatch of Session Identifiers")
    (87 . "unable to parse FTP file list")
    (88 . "FTP chunk callback reported error")
    (89 . "No connection available, the session will be queued")
    (90 . "SSL public key does not matched pinned public key"))
  "Alist mapping curl error code integers to helpful error messages.")

(defclass paimon-api ()
  ((auth-type
    :accessor paimon-api-auth-type
    :custom (choice (const "basic") (const "bearer"))
    :documentation "The authentication type."
    :initarg :auth-type
    :initform "basic"
    :type string)
   (hostname
    :accessor paimon-api-hostname
    :custom string
    :documentation "The hostname of the API."
    :initarg :hostname
    :initform "localhost"
    :type string)
   (identity
    :accessor paimon-api-identity
    :documentation "The identity used for authentication with the API."
    :initarg :identity
    :initform "admin"
    :type string)
   (port
    :accessor paimon-api-port
    :custom number
    :documentation "The port of the API."
    :initarg :port
    :initform 8089
    :type number)
   (protocol
    :accessor paimon-api-protocol
    :custom (choice (const "https") (const "http"))
    :documentation "The protocol of the API."
    :initarg :protocol
    :initform "https"
    :type string))
  "The API class.")

(cl-defgeneric paimon-api-for (object)
  "Return the API for OBJECT.")

(defun paimon-api-basic-auth-encode (username password)
  "Encode USERNAME and PASSWORD according to the HTTP basic authentication scheme."
  (base64-encode-string (format "%s:%s" username password)))

(defun paimon-api-secret (identity hostname port)
  "Find the secret for IDENTITY, HOSTNAME and PORT."
  (let ((port (when port (number-to-string port))))
    (auth-source-pick-first-password :host hostname :max-tokens 1 :port port :user identity)))

(defun paimon-api-authorization-basic-header (api)
  "Return the HTTP basic authorization header for the API."
  (with-slots (identity hostname port) api
    (when-let (password (paimon-api-secret identity hostname port))
      (cons "Authorization" (concat "Basic " (paimon-api-basic-auth-encode identity password))))))

(defun paimon-api-authorization-bearer-header (api)
  "Return the HTTP bearer authorization header for the API."
  (with-slots (identity hostname port) api
    (when-let (token (paimon-api-secret identity hostname port))
      (cons "Authorization" (concat "Bearer " (string-trim token))))))

(defun paimon-api-authorization-header (api)
  "Return the authorization header for the API."
  (with-slots (auth-type) api
    (pcase auth-type
      ("basic" (paimon-api-authorization-basic-header api))
      ("bearer" (paimon-api-authorization-bearer-header api)))))

(defun paimon-api-error-status (error)
  "Return the HTTP status code from the ERROR."
  (plist-get (cdr error) :status))

(defun paimon-api-authorization-tokens-url (api)
  "Return the Splunk API authorization tokens URL."
  (with-slots (hostname port protocol) api
    (let ((local-p (equal "localhost" hostname)))
      (concat (if local-p "http" protocol) "://" hostname
              (when local-p (concat ":8000"))
              "/en-US/manager/launcher/authorization/tokens"))))

(defun paimon-api-job-url (api id)
  "Return the URL for the search job ID using API."
  (with-slots (hostname port protocol) api
    (format "%s://%s/en-US/app/search/search?sid=%s" protocol hostname id)))

(defun paimon-api-headers (api)
  "Return the Splunk API headers."
  (let ((headers nil))
    (when-let (header (paimon-api-authorization-header api))
      (setq headers (cons header headers)))
    headers))

(defun paimon-api-url (api &optional path)
  "Return the Splunk API URL for PATH."
  (with-slots (hostname port protocol) api
    (concat protocol "://" hostname
            (when (numberp port)
              (concat ":" (number-to-string port)))
            "/" path)))

(defun paimon-api--compact-alist (parameters)
  "Remove all elements in PARAMETERS that don't have a value set."
  (seq-remove (lambda (parameter) (null (cdr parameter))) parameters))

(defun paimon-api--parse-content-type (content-type)
  "Parse the CONTENT-TYPE header."
  (unless (s-blank-p content-type)
    (nth 1 (s-match "\\([^/]+/[^;]+\\)\\(;\\(.+\\)\\)?" content-type))))

(defun paimon-api--response-content-type (response)
  "Return the content type of RESPONSE."
  (paimon-api--parse-content-type (request-response-header response "content-type")))

(defun paimon-api--response-body (response)
  "Return the parsed RESPONSE body."
  (let ((content-type (paimon-api--response-content-type response))
        (body (request-response-data response)))
    (cond ((equal "application/json" content-type)
           (if (fboundp 'json-parse-string)
               (json-parse-string body)
             (json-read-from-string body)))
          (t body))))

(defun paimon-api--parse-exit-code (s)
  "Parse the curl exit code from the exit string S."
  (when-let (matches (s-match ".*exited abnormally with code \\([[:digit:]]+\\).*" s))
    (string-to-number (nth 1 matches))))

(defun paimon-api--response-plist (response)
  "Convert the RESPONSE object into a plist."
  (list :body (paimon-api--response-body response)
        :headers (request-response-headers response)
        :status (request-response-status-code response)))

(defun paimon-api--response-error-messages (response)
  "Return the error messages of the RESPONSE."
  (let ((body (plist-get response :body)))
    (when (ht-p body) (ht-get body "messages"))))

(defun paimon-api--response-http-error-message (response)
  "Return the HTTP error messages of the RESPONSE."
  (when (plist-get response :status)
    (let ((message (seq-first (paimon-api--response-error-messages response))))
      (when (ht-p message)
        (format "Splunk API %s.  %s"
                (downcase (ht-get message "type" "error"))
                (paimon--bold (ht-get message "text")))))))

(defun paimon-api--response-curl-error-message (response)
  "Return the Curl error message for RESPONSE."
  (unless (request-response-status-code response)
    (let* ((thrown-error (request-response-error-thrown response))
           (exit-code (paimon-api--parse-exit-code (cdr thrown-error)))
           (description (cdr (assoc exit-code paimon-curl-errors))))
      (when description (format "Splunk API error. %s" description)))))

(defun paimon-api-error-message (response)
  "Return the Splunk API error message of the RESPONSE."
  (when-let (message (seq-first (paimon-api--response-error-messages response)))
    (when (ht-p message)
      (format "Splunk API %s.  %s"
              (downcase (ht-get message "type" "error"))
              (ht-get message "text")))))

(defun paimon-api--handle-response-error (response)
  "Handle an error RESPONSE."
  (if (request-response-status-code response)
      (paimon-api--response-plist response)
    (user-error (paimon-api--response-curl-error-message response))))

(defun paimon-api--handle-response (response)
  "Handle the HTTP RESPONSE."
  (pcase (request-response-symbol-status response)
    ('abort (user-error "Splunk API error. Request aborted"))
    ('error (paimon-api--handle-response-error response))
    ('parse-error (user-error "Splunk API error. Can't parse HTTP response"))
    ('success (paimon-api--response-plist response))
    ('timeout (user-error "Splunk API error. Request timed out"))))

(defun paimon-api--search-job-path (id &rest rest)
  "Return the search job URL path for ID, appended by REST."
  (cl-check-type id string "Invalid search job id")
  (apply #'concat (format "services/search/jobs/%s" id) rest))

(cl-defun paimon-api-request (api path &key body params parser method timeout)
  "Send a Splunk API request.

  BODY The body of the HTTP request.
  METHOD The method of the HTTP request.
  PARAMS The query parameters of the HTTP request.
  PARSER is a function to be applied on the HTTP response.
  PATH The path in the URL of the HTTP request.
  TIMEOUT The number of seconds for the timeout."
  (let ((promise (aio-promise))
        (request-backend 'curl)
        (request-curl-options (when (equal "localhost" (oref api hostname))
                                ;; The Splunk Docker image uses a self-signed certificate
                                (list "--insecure"))))
    (request (paimon-api-url api path)
      :data (cond ((stringp body) body)
                  ((listp body) (paimon-api--compact-alist body)))
      :complete (cl-function (lambda (&rest args &key response &allow-other-keys)
                               (aio-resolve promise (lambda () (paimon-api--handle-response response)))))
      :headers (paimon-api-headers api)
      :params (paimon-api--compact-alist params)
      :parser parser
      :timeout timeout
      :type (or method "GET"))
    promise))

(cl-defun paimon-api-data-indexes (api &key data-type)
  "List the recognized indexes on the API.

DATA-TYPE - Specifies the type (\"all\"|\"event\"|\"metric\") of index."
  (paimon-api-request api "services/data/indexes"
                      :params `(("datatype" . ,data-type)
                                ("output_mode" . "json"))))

(cl-defun paimon-api-search-job-create (api search &key earliest-time latest-time offset search-level status-buckets sort-dir sort-mode summarize)
  "Create a search job.

  API The Splunk API.
  EARLIEST-TIME The earliest time of the search.
  LATEST-TIME The latest time of the search.
  SEARCH The search query."
  (let ((sort-mode (when (stringp sort-mode) (string-replace "-" "_" sort-mode))))
    (paimon-api-request
     api "services/search/jobs"
     :method "POST"
     :body `(("adhoc_search_level" . ,(or search-level "smart"))
             ("status_buckets" . ,status-buckets)
             ("earliest_time" . ,earliest-time)
             ("latest_time" . ,latest-time)
             ("offset" . ,offset)
             ("output_mode" . "json")
             ("search" . ,search)
             ("sort_dir" . ,sort-dir)
             ("sort_mode" . ,sort-mode)
             ("summarize" ,(if summarize "true" "false"))))))

(defun paimon-api-search-job (api id)
  "Get a search job by ID from the Splunk API."
  (paimon-api-request api (paimon-api--search-job-path id) :params `(("output_mode" . "json"))))

(cl-defun paimon-api-search-job-control (api id action)
  "Control the search job by ID using ACTION and the Splunk API."
  (cl-assert (member action paimon-api-search-job-actions) t "Invalid search job action")
  (paimon-api-request api (paimon-api--search-job-path id "/control")
                      :method "POST"
                      :body `(("action" . ,action)
                              ("output_mode" . "json"))))

(cl-defun paimon-api-search-job-results (api id &key count offset)
  "Get the search job results by ID from the Splunk API."
  (paimon-api-request api (paimon-api--search-job-path id "/results")
                      :params `(("count" . ,count)
                                ("offset" . ,(or offset 0))
                                ("output_mode" . "json"))))

(cl-defun paimon-api-search-job-preview-results (api id &key count offset)
  "Get the search job preview results by ID from the Splunk API."
  (paimon-api-request api (paimon-api--search-job-path id "/results_preview")
                      :params `(("count" . ,count)
                                ("offset" . ,(or offset 0))
                                ("output_mode" . "json"))))

(cl-defun paimon-api-search-parse-query (api query)
  "Get the AST of the search QUERY from the API."
  (paimon-api-request api "services/search/parser"
                      :params `(("output_mode" . "json")
                                ("q" . ,query))))

(cl-defun paimon-api-search-typeahead (api prefix &key count)
  "Get the type ahead results for PREFIX from the API."
  (paimon-api-request api "services/search/typeahead"
                      :params `(("count" . ,(or count 5))
                                ("output_mode" . "json")
                                ("prefix" . ,prefix))))

(provide 'paimon-api)

;;; paimon-api.el ends here
