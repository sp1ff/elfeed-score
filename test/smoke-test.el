;;; smoke-test.el --- "smoke" tests for `elfeed-score'   -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025 Michael Herstine <sp1ff@pobox.com>

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
(require 'cl-lib)

(defvar srcdir (getenv "srcdir"))
(cl-assert srcdir t "Please specify the env var srcdir.")

(defvar
 test-dir
 (concat
  (make-temp-name (concat (temporary-file-directory) ".elfeed-smoke-tests")) "/"))

(if (file-exists-p test-dir)
    (delete-directory test-dir t))
(make-directory test-dir)

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
