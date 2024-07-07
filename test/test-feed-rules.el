;;; elfeed-score-test-feed-rules -- ERT tests for elfeed-score -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2024 Michael Herstine <sp1ff@pobox.com>

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

;; I'm in the process of breaking up the test suite.

;;; Code:

(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-score)
(require 'elfeed-db-tests)
(require 'elfeed-score-tests)

(ert-deftest elfeed-score-test-feed-rules-match-feeds ()
  "Smoke tests for `elfeed-score-scoring--match-feeds'."

  (with-elfeed-test
   (let ((feed (elfeed-score-test-generate-feed "feed" "http://www.feed.com/rss")))
     (should (elfeed-score-scoring--match-feeds feed '(t (u s "bar.com")  (t s "fee"))))
     (should (not (elfeed-score-scoring--match-feeds feed '(nil (u s "bar.com")  (t s "fee")))))
     (should (elfeed-score-scoring--match-feeds feed '(t t s "fee")))
     (should (not (elfeed-score-scoring--match-feeds feed '(nil (t s "fee"))))))))

(ert-deftest elfeed-score-test-feed-rules-smoke ()
  "Smoke tests for feed-scoped rules."

  (let ((title "foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-score-test-generate-feed "feed" "http://www.feed.com/rss"))
            (entry (elfeed-score-test-generate-entry feed title "some content")))
       (elfeed-db-add entry)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-title-rules
                (list
                 (elfeed-score-title-rule--create
                  :text "foo" :value 1 :type 's
                  :feeds '(t . ((u s "quux.com") (t s "feed"))))))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq 1 score))))))))

(provide 'elfeed-score-test-feed-rules)

;;; elfeed-score-test-feed-rules ends here
