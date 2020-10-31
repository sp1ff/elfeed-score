;;; elfeed-score-tests.el --- ERT tests for elfeed-score  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Michael Herstine <sp1ff@pobox.com>

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

;; These tests require 'elfeed-db-tests.

;;; Code:

(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-score)
(require 'elfeed-db-tests)

(defun elfeed-score-test-generate-feed (title &optional url)
  "Generate a random feed with title TITLE.

Warning: run this in `with-elfeed-test'.

This function differs from `elfeed-test-generate-feed' in that it
allows the caller to specify the feed title and, optionally,
URL (which is convenient for testing scoring)."
  (let* ((url (if url url (elfeed-test-generate-url)))
         (id url)
         (feed (elfeed-db-get-feed id)))
    (prog1 feed
      (push url elfeed-feeds)
      (setf (elfeed-feed-title feed) title)
      (setf (elfeed-feed-url feed) url))))

(cl-defun elfeed-score-test-generate-entry (feed title content
                                                 &key (within "1 year")
                                                 tags (authors '((:name "spaceman sp1ff"))))
  "Generate a random entry with feed FEED, title TITLE & content CONTENT.
Use WITHIN to scope the date.  TAGS specifies tags to be applied in addition
to 'unread.

Warning: run this in `with-elfeed-test'.

This function differs from `elfeed-test-generate-entry' in that
it allows the caller to specify the entry title & content (which
is convenient for testing scoring)."

  (let* ((feed-id (elfeed-feed-id feed))
         (namespace (elfeed-url-to-namespace feed-id))
         (link (elfeed-test-generate-url))
         (entry (elfeed-entry--create
                 :id (cons namespace link)
                 :title title
                 :link link
                 :date (elfeed-test-generate-date within)
                 :tags (append tags (list 'unread))
                 :feed-id feed-id
                 :content (elfeed-ref content))))
    (prog1 entry
      (setf (elfeed-meta entry :authors) authors))))

(defmacro with-elfeed-score-test (&rest body)
  "Run BODY with a fresh, empty set of scoring rules."
  (declare (indent defun))
  `(let ((elfeed-score--title-rules nil)
         (elfeed-score--feed-rules nil)
         (elfeed-score--content-rules nil)
         (elfeed-score--title-or-content-rules nil)
         (elfeed-score--tag-rules nil)
         (elfeed-score--adjust-tags-rules nil)
         (elfeed-score--score-mark nil))
     (progn ,@body)))

(ert-deftest elfeed-score-test-test-sort ()
  "Test `elfeed-score-sort'.

`elfeed-score-sort' sorts first on score, then on date. So we should test:

    1. higher score sorts first
    2. lower score sorts later
    3. equal scores, later date sort first"

  (let ((entry-a (elfeed-entry--create
		              :date 1576368912.0
		              :meta '(:authors ((:name "John Smith")))))
	      (entry-b (elfeed-entry--create
		              :date 1576368912.0
		              :meta '(:authors ((:name "John Smith"))
				                           :elfeed-score/score 0)))
	      (entry-c (elfeed-entry--create
		              :date 1576368912.0
		              :meta '(:authors ((:name "John Smith"))
				                           :elfeed-score/score 1)))
	      (entry-d (elfeed-entry--create
		              :date 1576368913.0
		              :meta '(:authors ((:name "John Smith"))
				                           :elfeed-score/score 1))))

    ;; `entry-a': Saturday, December 14, 2019 4:15:12 PM GMT-08:00, no score
    ;; `entry-b': Saturday, December 14, 2019 4:15:12 PM GMT-08:00, score of 0
    ;; `entry-c': Saturday, December 14, 2019 4:15:12 PM GMT-08:00, score of 1
    ;; `entry-d': Saturday, December 14, 2019 4:15:13 PM GMT-08:00, score of 1
    (should (elfeed-score-sort entry-c entry-a))
    (should (not (elfeed-score-sort entry-a entry-c)))
    (should (elfeed-score-sort entry-c entry-b))
    (should (not (elfeed-score-sort entry-b entry-c)))
    (should (not (elfeed-score-sort entry-a entry-b)))
    (should (not (elfeed-score-sort entry-b entry-a)))
    (should (elfeed-score-sort entry-d entry-c))
    (should (not (elfeed-score-sort entry-c entry-d)))))

(ert-deftest elfeed-score-test-format-score ()
  "Unit tests for `elfeed-score-format-score'."

  (let ((elfeed-score-score-format '("%d " 6 :right)))
    (should (equal (elfeed-score-format-score 11) "   11 "))))

(ert-deftest elfeed-score-test-match-tags ()
  "Test `elfeed-score--match-tags'."

  (should (eq t   (elfeed-score--match-tags '(foo bar) '(t . (foo)))))
  (should (eq t   (elfeed-score--match-tags '(foo bar) nil)))
  (should (eq nil (elfeed-score--match-tags '(foo bar) '(t . (splat)))))
  (should (eq nil (elfeed-score--match-tags '(foo bar) '(nil . (foo))))))

(provide 'elfeed-score-tests)

;;; elfeed-score-tests.el ends here
