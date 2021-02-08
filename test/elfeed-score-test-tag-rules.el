;;; elfeed-score-test-tag-rules -- ERT tests for elfeed-score  -*- lexical-binding: t; -*-

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

;; I'm in the process of breaking up the test suite.

;; These tests require 'elfeed-db-tests as well as 'elfeed-score-tests.

;;; Code:

(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-score)
(require 'elfeed-db-tests)
(require 'elfeed-score-tests)

(ert-deftest elfeed-score-test-tag-rules-smoke ()
  "Smoke tests for `elfeed-score' tag-based rules."

  (let ((title "Foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry feed title "some content")))
       (elfeed-db-add entry)
       (elfeed-tag entry 'a 'b 'c)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-tag-rules
                (list
                 (elfeed-score-tag-rule--create
                  :tags '(t . a)
                  :value 1)
                 (elfeed-score-tag-rule--create
                  :tags '(t . (z a))
                  :value 1)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 2))))))))

(ert-deftest elfeed-score-test-tag-rules-smoke-1 ()
  "Smoke tests for `elfeed-score' tag-based rules."

  (let ((title "Foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry feed title "some content")))
       (elfeed-db-add entry)
       (elfeed-tag entry 'a 'b 'c)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-tag-rules
                (list
                 (elfeed-score-tag-rule--create
                  :tags '(nil . a)
                  :value 1)
                 (elfeed-score-tag-rule--create
                  :tags '(nil . (z a))
                  :value 1)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 0))))))))

(ert-deftest elfeed-score-test-tag-rules-smoke-2 ()
  "Smoke tests for `elfeed-score' tag-based rules."

  (let ((title "Foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry feed title "some content")))
       (elfeed-db-add entry)
       (elfeed-tag entry 'a 'b 'c)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-tag-rules
                (list
                 (elfeed-score-tag-rule--create
                  :tags '(t . a)
                  :value 1)
                 (elfeed-score-tag-rule--create
                  :tags '(nil . (z a))
                  :value 1)))
               (score (elfeed-score-scoring-score-entry entry)))
          (should (eq score 1))))))))

(ert-deftest elfeed-score-test-tag-rules-adjust ()
  "Test tag adjustment."
  (let ((title "Foo bar splat"))
    (with-elfeed-test
     (let* ((feed (elfeed-test-generate-feed))
            (entry (elfeed-score-test-generate-entry feed title "some content")))
       (elfeed-db-add entry)
       (elfeed-tag entry 'a 'b 'c)
       (with-elfeed-score-test
        (let* ((elfeed-score-serde-adjust-tags-rules
                (list
                 (elfeed-score-adjust-tags-rule--create
                  :threshold '(t . 100)
                  :tags '(t . (da niu)))
                 (elfeed-score-adjust-tags-rule--create
                  :threshold '(nil . 250)
                  :tags '(nil . (a b))))))
          (elfeed-score-scoring--adjust-tags entry 110)))
       (should (elfeed-tagged-p 'da entry))
       (should (elfeed-tagged-p 'niu entry))
       (should (elfeed-tagged-p 'c entry))
       (should (not (elfeed-tagged-p 'a entry)))
       (should (not (elfeed-tagged-p 'b entry)))))))

(provide 'elfeed-score-test-tag-rules)

;;; elfeed-score-test-tag-rules ends here
