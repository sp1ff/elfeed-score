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

(cl-defun elfeed-score-test-generate-entry (feed title content &optional (within "1 year"))
  "Generate a random entry with feed FEED, title TITLE & content CONTENT.
Use WITHIN to scope the date.

Warning: run this in `with-elfeed-test'.

This function differs from `elfeed-test-generate-entry' in that
it allows the caller to specify the entry title & content (which
is convenient for testing scoring)."

  (let* ((feed-id (elfeed-feed-id feed))
         (namespace (elfeed-url-to-namespace feed-id))
         (link (elfeed-test-generate-url)))
    (elfeed-entry--create
     :id (cons namespace link)
     :title title
     :link link
     :date (elfeed-test-generate-date within)
     :tags (list 'unread)
     :feed-id feed-id
     :content (elfeed-ref content))))

(defmacro with-elfeed-score-test (&rest body)
  "Run BODY with a fresh, empty set of scoring rules."
  (declare (indent defun))
  `(let ((elfeed-score--title-rules nil)
         (elfeed-score--feed-rules nil)
         (elfeed-score--content-rules nil)
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

(ert-deftest elfeed-score-test-score-files-0 ()
  "Smoke test reading/writing score files"

  (let* ((score-entries
          '((version 1)
            ("title"
             ("hoping" -1000 s)
             ("long way( home)?" +100 r))
            ("feed"
             ("foo.com" +100 s u)
             ("title" -100 s t))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-2 (elfeed-score--parse-score-file score-file)))
    (should (equal score-entries-2
                   (list :mark -2500
                         :feeds (list (elfeed-score-feed-rule--create
                                       :text "foo.com" :value 100 :type 's
                                       :attr 'u)
                                      (elfeed-score-feed-rule--create
                                       :text "title" :value -100 :type 's
                                       :attr 't))
                         :titles (list (elfeed-score-title-rule--create
                                        :text "hoping" :value -1000 :type 's)
                                       (elfeed-score-title-rule--create
                                        :text "long way( home)?" :value 100
                                        :type 'r))
                         :content nil))))
  (let* ((score-entries
          '((version 2)
            ("title"
             ("hoping" -1000 s)
             ("long way( home)?" +100 r))
            ("feed"
             ("foo.com" +100 s u)
             ("title" -100 s t))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-2 (elfeed-score--parse-score-file score-file)))
    (should (equal score-entries-2
                   (list :mark -2500
                         :feeds (list (elfeed-score-feed-rule--create
                                       :text "foo.com" :value 100 :type 's
                                       :attr 'u)
                                      (elfeed-score-feed-rule--create
                                       :text "title" :value -100 :type 's
                                       :attr 't))
                         :titles (list (elfeed-score-title-rule--create
                                        :text "hoping" :value -1000 :type 's)
                                       (elfeed-score-title-rule--create
                                        :text "long way( home)?" :value 100
                                        :type 'r))
                         :content nil
                         :title-or-content nil)))))

(ert-deftest elfeed-score-test-test-scoring-on-title-0 ()
  "Test scoring against entry title-- substring matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
     (with-elfeed-score-test
      (let* ((elfeed-score--title-rules
              (list (elfeed-score-title-rule--create :text "Bar" :value 1 :type 's)))
             (score (elfeed-score--score-entry entry)))
        (should (eq score 1))))
     ;; case-sensitive
     (with-elfeed-score-test
      (let* ((elfeed-score--title-rules
              (list (elfeed-score-title-rule--create :text "Bar" :value 1 :type 'S)))
             (score (elfeed-score--score-entry entry)))
        (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-scoring-on-title-1 ()
  "Test scoring against entry title-- regexp matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(r\\|z\\)" :value 1 :type 'r)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(\\|z\\)r" :value 1 :type 'R)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-scoring-on-feed-title-0 ()
  "Test scoring against entry feed title-- substring matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-score-test-generate-feed
                   "Feed" "http://www.feed.com/rss"))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (should (equal "Feed" (elfeed-feed-title (elfeed-entry-feed entry))))
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "feed" :value 1 :type 's :attr 't)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "feed" :value 1 :type 'S :attr 't)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-scoring-on-feed-title-1 ()
  "Test scoring against entry feed title-- regepx matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-score-test-generate-feed
                   "Feed" "http://www.feed.com/rss"))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "f\\(eed\\|oo\\)" :value 1 :type 'r :attr 't)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "f\\(eed\\|oo\\)" :value 1 :type 'R :attr 't)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))
       ))))

(ert-deftest elfeed-score-test-test-scoring-on-feed-url-0 ()
  "Test scoring against entry feed URL-- substring matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-score-test-generate-feed
                   "Feed" "http://www.feed.com/rss"))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "feed.com" :value 1 :type 's :attr 'u)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "Feed.com" :value 1 :type 'S :attr 'u)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-scoring-on-feed-url-1 ()
  "Test scoring against entry feed URL-- regexp matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-score-test-generate-feed
                   "Feed" "http://www.feed.com/rss"))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "f\\(eed\\|oo\\)\\.com" :value 1 :type 'r :attr 'u)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--feed-rules
                (list (elfeed-score-feed-rule--create :text "F\\(eed\\|oo\\)\\.com" :value 1 :type 'R :attr 'u)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-scoring-on-content-0 ()
  "Test scoring based on content-- substring matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--content-rules
                (list (elfeed-score-content-rule--create :text "lorem" :value 1 :type 's)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--content-rules
                (list (elfeed-score-content-rule--create :text "lorem" :value 1 :type 'S)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-scoring-on-content-1 ()
  "Test scoring based on content-- regexp matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--content-rules
                (list (elfeed-score-content-rule--create :text "lo\\(rem\\|om\\)" :value 1 :type 'r)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--content-rules
                (list (elfeed-score-content-rule--create :text "lo\\(rem\\|om\\)" :value 1 :type 'R)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-scoring-on-title-or-content-0 ()
  "Test scoring based on title-or-content-- substring matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "Lorem ipsum"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--title-or-content-rules
                (list (elfeed-score-title-or-content-rule--create
                       :text "lorem ipsum" :title-value 2 :content-value 1
                       :type 's)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 3))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score--title-or-content-rules
                (list (elfeed-score-title-or-content-rule--create
                       :text "lorem ipsum" :title-value 2 :content-value 1
                       :type 'S)))
               (score (elfeed-score--score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-test-marking-as-read-0 ()
  "Test marking entries as read if they score low enough."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       (with-elfeed-score-test
        (let* ((elfeed-score--title-rules
                (list (elfeed-score-title-rule--create :text "foo" :value -1 :type 's)))
               (elfeed-score--score-mark 0))
          (elfeed-score--score-entry entry)
          (should (not (elfeed-tagged-p 'unread entry)))))))))

(provide 'elfeed-score-tests)

;;; elfeed-score-tests.el ends here
