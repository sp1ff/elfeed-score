;;; elfeed-score-test-maintenance.el --- ERT tests for elfeed-score maintenance functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Michael Herstine <sp1ff@pobox.com>

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

;;; Code:

(require 'elfeed-score)

(ert-deftest elfeed-score-test-getting-date-or-hits ()
  "Test `elfeed-score--get-{last-match-date,hits}.'"

  (let ((r1 (elfeed-score-title-rule--create
             :text "Bar" :value 1 :type 's))
        (r2 (elfeed-score-feed-rule--create
             :text "feed" :value 1 :type 's :attr 't :date 0.0
             :hits 0))
        (r3 (elfeed-score-content-rule--create
             :text "lorem" :value 1 :type 's :date 1604184127.0
             :hits 123))
        (r4 (elfeed-score-title-or-content-rule--create
             :text "lorem ipsum" :title-value 2 :content-value 1
             :type 's :date 1604184127.0 :hits 123))
        (r5 (elfeed-score-authors-rule--create
             :text "Hancock" :value 1 :type 's :date 1604184127.0
             :hits 123)))

    (should (equal (elfeed-score--get-last-match-date r1) 0.0))
    (should (equal (elfeed-score--get-last-match-date r2) 0.0))
    (should (equal (elfeed-score--get-last-match-date r3) 1604184127.0))
    (should (equal (elfeed-score--get-last-match-date r4) 1604184127.0))
    (should (equal (elfeed-score--get-last-match-date r5) 1604184127.0))

    (should (equal (elfeed-score--get-hits r1) 0))
    (should (equal (elfeed-score--get-hits r2) 0))
    (should (equal (elfeed-score--get-hits r3) 123))
    (should (equal (elfeed-score--get-hits r4) 123))
    (should (equal (elfeed-score--get-hits r5) 123))))

(provide 'elfeed-score-test-maintenace)

;;; elfeed-score-test-maintenance.el ends here
