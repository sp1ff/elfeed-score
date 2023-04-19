;;; test-scoring.el --- ERT tests for elfeed-score scoring  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2023 Michael Herstine <sp1ff@pobox.com>

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

;; Scoring tests.

;;; Code:

(require 'elfeed-score-tests)

(ert-deftest test-scoring-on-title-0 ()
  "Test scoring against entry title-- substring matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum
                    :tags '(foo splat))))
       (elfeed-db-add entry)
       ;; case-insensitive
     (with-elfeed-score-test
      (let* ((elfeed-score-serde-title-rules
              (list (elfeed-score-title-rule--create :text "Bar" :value 1 :type 's)))
             (score (elfeed-score-scoring-score-entry entry))
             (stats (elfeed-score-rule-stats-get (car elfeed-score-serde-title-rules))))
        (should (eq score 1))
        (should (eq 1 (elfeed-score-rule-stats-hits stats)))))
     ;; case-sensitive
     (with-elfeed-score-test
      (let* ((elfeed-score-serde-title-rules
              (list (elfeed-score-title-rule--create :text "Bar" :value 1 :type 'S)))
             (score (elfeed-score-scoring-score-entry entry)))
        (should (eq score 0))))
     ;; case-insensitive, scoped by tags
     (with-elfeed-score-test
      (let* ((elfeed-score-serde-title-rules
              (list (elfeed-score-title-rule--create :text "bar" :value 1 :type 's
                                                     :tags '(t . (foo bar)))))
             (score (elfeed-score-scoring-score-entry entry)))
        (should (eq score 1))))
     (with-elfeed-score-test
      (let* ((elfeed-score-serde-title-rules
              (list (elfeed-score-title-rule--create :text "bar" :value 1 :type 's
                                                     :tags '(nil . (foo bar)))))
             (score (elfeed-score-scoring-score-entry entry)))
        (should (eq score 0))))))))

(ert-deftest test-scoring-on-title-1 ()
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
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(r\\|z\\)" :value 1 :type 'r)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(r\\|z\\)" :value 1 :type 'R)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest test-scoring-on-title-2 ()
  "Test scoring against entry title-- whole-word matching."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(r\\|z\\)" :value 1 :type 'w)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(r\\|z\\)" :value 1 :type 'W)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0)))))))
  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo barsplat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(r\\|z\\)" :value 1 :type 'w)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "Ba\\(\\|z\\)r" :value 1 :type 'W)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))
       ))))

(ert-deftest test-scoring-on-feed-title-0 ()
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
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "feed" :value 1 :type 's :attr 't)))
               (score (elfeed-score-scoring-score-entry entry))
               (stats (elfeed-score-rule-stats-get (car elfeed-score-serde-feed-rules))))
          (should (eq score 1))
          (should (eq 1 (elfeed-score-rule-stats-hits stats)))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "feed" :value 1 :type 'S :attr 't)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest test-scoring-on-feed-title-1 ()
  "Test scoring against entry feed title-- regexp matching."

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
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "f\\(eed\\|oo\\)" :value 1 :type 'r :attr 't)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "f\\(eed\\|oo\\)" :value 1 :type 'R :attr 't)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))
       ))))

(ert-deftest test-scoring-on-feed-url-0 ()
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
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "feed.com" :value 1 :type 's :attr 'u)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "Feed.com" :value 1 :type 'S :attr 'u)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest test-scoring-on-feed-url-1 ()
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
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "f\\(eed\\|oo\\)\\.com" :value 1 :type 'r :attr 'u)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-feed-rules
                (list (elfeed-score-feed-rule--create :text "F\\(eed\\|oo\\)\\.com" :value 1 :type 'R :attr 'u)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest test-scoring-on-content-0 ()
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
        (let* ((elfeed-score-serde-content-rules
                (list (elfeed-score-content-rule--create :text "lorem"
                                                         :value 1 :type 's)))
               (score (elfeed-score-scoring-score-entry entry))
               (stats (elfeed-score-rule-stats-get (car elfeed-score-serde-content-rules))))
          (should (eq score 1))
          (should (eq 1 (elfeed-score-rule-stats-hits stats)))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-content-rules
                (list (elfeed-score-content-rule--create :text "lorem"
                                                         :value 1 :type 'S)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest test-scoring-on-content-1 ()
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
        (let* ((elfeed-score-serde-content-rules
                (list (elfeed-score-content-rule--create :text "lo\\(rem\\|om\\)" :value 1 :type 'r)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 1))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-content-rules
                (list (elfeed-score-content-rule--create :text "lo\\(rem\\|om\\)" :value 1 :type 'R)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest test-scoring-on-title-or-content-0 ()
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
        (let* ((elfeed-score-serde-title-or-content-rules
                (list (elfeed-score-title-or-content-rule--create
                       :text "lorem ipsum" :title-value 2 :content-value 1
                       :type 's)))
               (score (elfeed-score-scoring-score-entry entry))
               (stats (elfeed-score-rule-stats-get (car elfeed-score-serde-title-or-content-rules))))
          (should (eq score 3))
          (should (eq 2 (elfeed-score-rule-stats-hits stats)))))
       ;; case-sensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-or-content-rules
                (list (elfeed-score-title-or-content-rule--create
                       :text "lorem ipsum" :title-value 2 :content-value 1
                       :type 'S)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest test-scoring-on-title-or-content-1 ()
  "Test scoring based on title-or-content-- substring matching,
tags scoping."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "Lorem ipsum"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum
                    :tags '(foo bar))))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-or-content-rules
                (list (elfeed-score-title-or-content-rule--create
                       :text "lorem ipsum" :title-value 1 :content-value 0
                       :type 's)
                      (elfeed-score-title-or-content-rule--create
                       :text "lorem ipsum" :title-value 1 :content-value 0
                       :type 's :tags '(t . (foo splat)))
                      (elfeed-score-title-or-content-rule--create
                       :text "lorem ipsum" :title-value 1 :content-value 0
                       :type 's :tags '(t . (splat)))))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 2))))))))

(ert-deftest test-scoring-on-authors-1 ()
  "Test scoring based on authors-- substring matching, tags scoping."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "Lorem ipsum"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum
                    :authors '((:name "John Hancock"))
                    :tags '(foo bar))))
       (elfeed-db-add entry)
       ;; case-insensitive
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-authors-rules
                (list (elfeed-score-authors-rule--create
                       :text "Hancock" :value 1 :type 's)
                      (elfeed-score-authors-rule--create
                       :text "John" :value 1
                       :type 'S :tags '(t . (foo splat)))
                      (elfeed-score-authors-rule--create
                       :text "john hancock" :value 1
                       :type 's :tags '(t . (splat)))))
               (score (elfeed-score-scoring-score-entry entry))
               (stats (elfeed-score-rule-stats-get (car elfeed-score-serde-authors-rules))))
          (should
           (eq 1 (elfeed-score-rule-stats-hits stats)))
          (should (eq score 2))))))))

(ert-deftest test-scoring-missed-match-20200217 ()
  "Test whole-word scoring.

Thought I had a bug; turns out I didn't understand `word-search-regexp'"

  (let ((test-title "AR/VR engineers 1400% rise! Hired: AR/VR engineers replace blockchain programmers as hottest commodity! Thanks God I`m AR engineer"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed test-title "blah")))
       (elfeed-db-add entry)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-or-content-rules
                (list
                 (elfeed-score-title-or-content-rule--create
                  :text "b\\(lockchain\\|itcoin\\|tc\\)"
                  :title-value 1 :content-value 1 :type 'r)
                 (elfeed-score-title-or-content-rule--create
                  :text "blockchain"
                  :title-value 1 :content-value 1 :type 'w)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 2))))))))

(ert-deftest test-scoring-marking-as-read-0 ()
  "Test marking entries as read if they score low enough."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "foo" :value -1 :type 's)))
               (elfeed-score-serde-score-mark 0))
          (elfeed-score-scoring-score-entry entry)
          (should (not (elfeed-tagged-p 'unread entry)))))))))

(ert-deftest test-scoring-tags-20200314 ()
  "Test scoring by a rule with multiple tag matches."
  (let ((test-title "Traits, dynamic dispatch and upcasting"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed test-title "some content")))
       (elfeed-db-add entry)
       (elfeed-tag entry '@dev 'rust)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-or-content-rules
                (list
                 (elfeed-score-title-or-content-rule--create
                  :text "\\(traits\\|upcasting\\)"
                  :title-value 2
                  :content-value 1
                  :type 'r
                  :tags '(t . (@dev rust splat)))))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 2))))))))

(ert-deftest test-scoring-by-link-0 ()
  "Smoke-test Link rules."
  (with-elfeed-test
   (let* ((feed (elfeed-test-generate-feed))
          (entry (elfeed-score-test-generate-entry
                  feed "Improved PR template by making it more succinct."
                  "some content"
                  :link "https://github.com/klee/klee/commit/295353")))
     (elfeed-db-add entry)
     (with-elfeed-score-test
      (let* ((elfeed-score-serde-link-rules
              (list
               (elfeed-score-link-rule--create
                :text "github\\.com/klee"
                :value 1
                :type 'r)))
             (score (elfeed-score-scoring-score-entry entry)))
        (should (eq score 1)))))))

(ert-deftest test-sticky-scores ()
  "Check sticky scores.

Exercise `elfeed-score-scoring-set-score-on-entry' & its `sticky'
parameter."
  
  (with-elfeed-test
   (let* ((feed (elfeed-test-generate-feed))
          (entry1 (elfeed-score-test-generate-entry
                  feed
                  "Test fix to issue #10 - entry #1"
                  "Test entry #1 for my fix to issue #10"))
          (entry2 (elfeed-score-test-generate-entry
                  feed
                  "Test fix to issue #10 - entry #2"
                  "Test entry #2 for my fix to issue #10")))
     (elfeed-db-add entry1)
     (elfeed-db-add entry2)
     ;; Feature flag set to t, `sticky' param set to t: should set the
     ;; score...
     (let ((elfeed-score-scoring-manual-is-sticky t))
       (elfeed-score-scoring-set-score-on-entry entry1 100 t)
       ;; and ignore an invocation with the `sticky' parameter defauled.
       (elfeed-score-scoring-set-score-on-entry entry1 200)
       (should (eq (elfeed-score-scoring-get-score-from-entry entry1) 100))
       ;; *but*, if I pass `sticky' = t, this time, it should take!
       (elfeed-score-scoring-set-score-on-entry entry1 200 t)
       (should (eq (elfeed-score-scoring-get-score-from-entry entry1) 200)))
     ;; Feature flag set to nil, `sticky' param set to t: should set the
     ;; score...
     (let ((elfeed-score-scoring-manual-is-sticky nil))
       (elfeed-score-scoring-set-score-on-entry entry2 100 t)
       ;; and accept an invocation with the `sticky' parameter defauled.
       (elfeed-score-scoring-set-score-on-entry entry2 200)
       (should (eq (elfeed-score-scoring-get-score-from-entry entry1) 200))))))

(ert-deftest test-broken-udf ()
  "Check that we handle an error-ing UDF."
  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum
                    :tags '(foo splat))))
       (elfeed-db-add entry)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list (elfeed-score-title-rule--create :text "Splat" :value 1 :type 's)))
               (elfeed-score-serde-udf-rules
                (list
                 (elfeed-score-udf-rule--create :function (lambda (_) (error "My bad")))
                 (elfeed-score-udf-rule--create :function (lambda (_) nil))
                 (elfeed-score-udf-rule--create :function (lambda (_) 2))))
               (score (elfeed-score-scoring-score-entry entry))
               (stats1 (elfeed-score-rule-stats-get (car elfeed-score-serde-title-rules)))
               (stats2 (elfeed-score-rule-stats-get (nth 0 elfeed-score-serde-udf-rules)))
               (stats3 (elfeed-score-rule-stats-get (nth 1 elfeed-score-serde-udf-rules)))
               (stats4 (elfeed-score-rule-stats-get (nth 2 elfeed-score-serde-udf-rules))))
          (should (eq score 3))
          (should (eq 1 (elfeed-score-rule-stats-hits stats1)))
          (should (eq 0 (elfeed-score-rule-stats-hits stats2)))
          (should (eq 1 (elfeed-score-rule-udf-stats-errors stats2)))
          (should (eq nil stats3)) ;; never matches, no errors => no stats
          (should (eq 1 (elfeed-score-rule-stats-hits stats4)))))))))

(ert-deftest test-issue-22 ()
  "Regression test for Github issue #22."

  (let* ((lorem-ipsum "Lorem ipsum dolor sit amet")
         (entry-title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry
                    feed entry-title lorem-ipsum)))
       (elfeed-db-add entry)
       ;; case-insensitive
     (with-elfeed-score-test
      (let* ((elfeed-score-serde-title-rules
              ;; This was a bug where tags meant to be set above a certain score were applied to ALL entries.
              ;; So, setup a list with a matching title rule that will give the entry a score of 10. Then
              ;; setup two adjust-tags rules: one that should match & one that should not.
              (list (elfeed-score-title-rule--create :text "Bar" :value 10 :type 's)))
             (elfeed-score-serde-adjust-tags-rules
              (list
               (elfeed-score-adjust-tags-rule--create :threshold '(t . 10) :tags '(t . (a)))
               (elfeed-score-adjust-tags-rule--create :threshold '(t . 11) :tags '(t . (b)))))
             (score (elfeed-score-scoring-score-entry entry))
             (tags-stats (elfeed-score-rule-stats-get (car elfeed-score-serde-adjust-tags-rules))))
        ;; Ho-kay. The entry score should be 10...
        (should (eq score 10))
        ;; we should have ONE adjust-tags rule hit...
        (should (eq 1 (elfeed-score-rule-stats-hits tags-stats)))
        (let ((tags (elfeed-entry-tags entry)))
          (should (memq 'a tags))
          (should (not (memq 'b tags))))))))))

(provide 'test-scoring)
;;; test-scoring.el ends here
