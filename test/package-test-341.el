;;; package-test-341.el --- Test the elfeed-score Emacs package   -*- lexical-binding: t -*-

;; Copyright (C) 2021-2025 Michael Herstine <sp1ff@pobox.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'package)
(require 'cl-lib)

(setq
 test-dir
 (concat
  (make-temp-name
   (concat (temporary-file-directory) ".elfeed-package-tests")) "/")
 srcdir (getenv "abs_srcdir")
 builddir (getenv "abs_top_builddir"))

(cl-assert srcdir t "Please specify the env var abs_srcdir.")
(cl-assert builddir t "Please specify the env var abs_top_buildir.")

(setq elpa-dir (concat test-dir "elpa")
      info-dir (concat test-dir "info"))

(if (file-exists-p test-dir)
    (delete-directory test-dir t))
(make-directory elpa-dir t)
(make-directory info-dir)

(toggle-debug-on-error)
(setq package-user-dir elpa-dir
      Info-default-directory-list (cons info-dir Info-default-directory-list))

(setq elfeed-package (concat srcdir "/elfeed-3.4.1.tar")
      elfeed-score-package (concat builddir (format "/elfeed-score-%s.tar" version)))

(message "Installing %s..." elfeed-package)
(package-install-file elfeed-package)
(message "Installing %s...done." elfeed-package)
(message "installing %s..." elfeed-score-package)
(package-install-file elfeed-score-package)
(message "installing %s...done." elfeed-score-package)

(setq
 elfeed-feeds
 '("https://sachachua.com/blog/category/emacs-news/feed/"
   ("http://rayhosler.wordpress.com/feed/" cycling))
 elfeed-db-directory test-dir
 elfeed-score-score-file (concat test-dir "elfeed.score")
 elfeed-score-rule-stats-file (concat test-dir "elfeed.stats")
 elfeed-search-print-entry-function #'elfeed-score-print-entry
 elfeed-score-log-level 'debug)

(load-file (concat (file-name-as-directory srcdir) "smoke-test-core.el"))
