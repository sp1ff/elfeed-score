;;; elfeed-score-test-maintenance.el --- ERT tests for elfeed-score maintenance functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>

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

;; None.

;;; Code:
(require 'elfeed-score-maint)

(ert-deftest elfeed-score-test-getting-date-or-hits ()
  "Test retrieving dates & hit counts."

  (let ((r1 (elfeed-score-title-rule--create
             :text "Bar" :value 1 :type 's))
        (r2 (elfeed-score-feed-rule--create
             :text "feed" :value 1 :type 's :attr 't))
        (r3 (elfeed-score-content-rule--create
             :text "lorem" :value 1 :type 's)))

    (elfeed-score-rule-stats-on-match r1)

    (let ((s1 (elfeed-score-rule-stats-get r1))
          (s2 (elfeed-score-rule-stats-get r2)))
      (should (equal 1 (elfeed-score-rule-stats-hits s1)))
      (should (< 0.0 (elfeed-score-rule-stats-date s1)))
      (should (not s2)))

    (elfeed-score-rule-stats-on-match r1)
    (elfeed-score-rule-stats-on-match r3)

    (let ((s1 (elfeed-score-rule-stats-get r1))
          (s2 (elfeed-score-rule-stats-get r2))
          (s3 (elfeed-score-rule-stats-get r3)))
      (should (equal 2 (elfeed-score-rule-stats-hits s1)))
      (should (< 0.0 (elfeed-score-rule-stats-date s1)))
      (should (not s2))
      (should (equal 1 (elfeed-score-rule-stats-hits s3)))
      (should (< 0.0 (elfeed-score-rule-stats-date s3))))))

(provide 'elfeed-score-test-maintenace)

;;; elfeed-score-test-maintenance.el ends here
