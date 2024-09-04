;;; paimon.el --- A major mode for Splunk -*- lexical-binding: t -*-

;; Copyright (C) 2022 r0man

;; Author: r0man <roman@burningswell.com>
;; Homepage: https://github.com/r0man/paimon.el
;; Keywords: paimon, search, tools
;; Maintainer: r0man <roman@burningswell.com>
;; Package-Requires: ((aio "1.0") (closql "2.0.0") (emacs "28.1") (emacsql "4.0.2") (f "0.20.0") (ht "2.4") (transient "0.7.5") (request "0.3.2") (compat "30.0.0.0"))
;; Package-Version: 0.2.0-git
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

;; A major mode for Splunk

;;; Code:

(require 'compat)
(require 'paimon-db)
(require 'paimon-profiles)
(require 'paimon-search)
(require 'paimon-search-jobs)

;;;###autoload
(defun paimon ()
  "Show the search jobs of the current profile."
  (interactive)
  (paimon-search-jobs-list (paimon-profile-current (paimon-db))))

(provide 'paimon)

;;; paimon.el ends here
