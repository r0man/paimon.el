;;; paimon.el --- A major mode for Splunk -*- lexical-binding: t -*-

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

;; A major mode for Splunk

;;; Code:

(require 'paimon-db)
(require 'paimon-profiles)
(require 'paimon-search)
(require 'paimon-search-jobs)

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
