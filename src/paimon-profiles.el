;; paimon-profiles.el --- Paimon profiles -*- lexical-binding: t; -*-

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

;; Paimon profiles

;;; Code:

(require 'cl-lib)
(require 'closql)
(require 'eieio-custom)
(require 'paimon-db)
(require 'paimon-profile)
(require 'paimon-search-jobs)
(require 's)
(require 'seq)
(require 'tabulated-list)

(defvar paimon-profiles-buffer-name "*paimon-profiles*"
  "The name of the profiles buffer.")

(defvar paimon-profiles-list-format
  [(" " 1 t)
   ("Protocol" 10 t)
   ("Hostname" 30 t)
   ("Port" 10 t)
   ("Auth Type" 20 t)
   ("Identity" 20 t)
   ("Secret" 20 t)]
  "The `tabulated-list-mode' format of profiles.")

(defcustom paimon-profiles-list-sort-key
  '("Hostname" . t)
  "The sort key of the profiles."
  :group 'paimon
  :safe #'listp
  :type 'list)

(defvar paimon-profiles--toggled-secrets nil
  "List of profile ids for which to show the secrets.")

(defun paimon-profiles--secret (profile)
  "Return the secret of the PROFILE, hidden or in clear text."
  (with-slots (id) profile
    (let ((secret (paimon-profile-secret profile)))
      (cond ((s-blank-p secret)
             "n/a")
            ((member id paimon-profiles--toggled-secrets)
             secret)
            ((stringp secret)
             (replace-regexp-in-string "." "*" (s-truncate 20 secret)))))))

(defun paimon-profiles-list-entry (profile)
  "Return the list entry of the PROFILE."
  (with-slots (auth-type default hostname id identity protocol port) profile
    (list id (vector (if (= 1 default) "*" "")
                     protocol
                     hostname
                     (number-to-string port)
                     auth-type
                     identity
                     (paimon-profiles--secret profile)))))

(defun paimon-profiles-list-entries ()
  "Return the list entries of all profiles."
  (seq-map #'paimon-profiles-list-entry (paimon-profiles (paimon-db))))

(defun paimon-profiles-under-point ()
  "Return the profile under point."
  (paimon-profile-by-id (paimon-db) (tabulated-list-get-id)))

(defun paimon-profiles-create ()
  "Create a new profile."
  (interactive)
  (paimon-profile-setup (paimon-db))
  (revert-buffer))

(defun paimon-profiles-edit (profile)
  "Edit the PROFILE."
  (interactive (list (paimon-profiles-under-point)))
  (eieio-customize-object profile))

(defun paimon-profiles-change-default (profile)
  "Change PROFILE to be the default profile."
  (interactive (list (paimon-profiles-under-point)))
  (paimon-profile-set-default profile)
  (revert-buffer))

(defun paimon-profiles-delete (profile &optional force)
  "Delete the PROFILE.  If FORCE is t, don't ask the user."
  (interactive (list (paimon-profiles-under-point)))
  (when (or force (yes-or-no-p "Really delete the profile?"))
    (closql-delete profile)
    (revert-buffer)))

(defun paimon-profiles-show (profile)
  "Show the PROFILE."
  (interactive (list (paimon-profiles-under-point)))
  (paimon-search-jobs-list profile))

(defun paimon-profiles-toggle-secret (profile)
  "Delete the PROFILE.  If FORCE is t, don't ask the user."
  (interactive (list (paimon-profiles-under-point)))
  (with-slots (id) profile
    (setq paimon-profiles--toggled-secrets
          (if (member id paimon-profiles--toggled-secrets)
              (remove id paimon-profiles--toggled-secrets)
            (cons id paimon-profiles--toggled-secrets)))
    (revert-buffer)))

(defvar paimon-profiles-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "D") 'paimon-profiles-delete)
    (define-key map (kbd "RET") 'paimon-profiles-show)
    (define-key map (kbd "c") 'paimon-profiles-create)
    (define-key map (kbd "d") 'paimon-profiles-change-default)
    (define-key map (kbd "e") 'paimon-profiles-edit)
    (define-key map (kbd "t") 'paimon-profiles-toggle-secret)
    map)
  "The key map for the `paimon-profiles-mode'.")

(define-derived-mode paimon-profiles-mode tabulated-list-mode "Profiles"
  "Special mode for profiles."
  (hl-line-mode 1)
  (setq tabulated-list-entries #'paimon-profiles-list-entries)
  (setq tabulated-list-format paimon-profiles-list-format)
  (setq tabulated-list-sort-key paimon-profiles-list-sort-key)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'paimon-profiles-mode 'emacs)))

;;;###autoload
(defun paimon-profiles-list ()
  "List the profiles."
  (interactive)
  (let ((buffer (get-buffer-create paimon-profiles-buffer-name)))
    (pop-to-buffer buffer)
    (paimon-profiles-mode)))

(provide 'paimon-profiles)

;;; paimon-profiles.el ends here
