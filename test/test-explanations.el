;;; test-explanations.el -- ERT tests for the score explanation logic -*- lexical-binding: t; -*-

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

;; These tests require 'elfeed-db-tests.  The test suite is minimal to
;; begin with.

;;; Code:

(require 'elfeed-score)
(require 'elfeed-score-tests)

(ert-deftest explanations-smoke-tests ()
  "Basic tests for `elfeed-score' explanations."
  (with-elfeed-test
   (let* ((feed (elfeed-score-test-generate-feed "foo"))
          (elfeed-score-scoring-explanation-buffer-name "*explanation-smoke-tests*")
          (entry (elfeed-score-test-generate-entry
                  feed "bar" "Lorem ipsum"
                  :authors '((:name "John Hancock"))
                  :tags '(a b c)
                  :link "https://foo.com")))
     (elfeed-db-add entry)
     (with-elfeed-score-test
      (let* ((elfeed-score-serde-title-rules
              (list (elfeed-score-title-rule--create :text "bar" :value 1 :type 's)))
             (elfeed-score-serde-content-rules
              (list
               (elfeed-score-content-rule--create :text "lorem" :value 1 :type 's)))
             (elfeed-score-serde-title-or-content-rules
              (list (elfeed-score-title-or-content-rule--create :text "bar"
                                                                :title-value 1
                                                                :content-value 1
                                                                :type 's)))
             (elfeed-score-serde-feed-rules
              (list
               (elfeed-score-feed-rule--create :text "foo" :value 1
                                               :type 's :attr 't)))
             (elfeed-score-serde-authors-rules
              (list
               (elfeed-score-authors-rule--create :text "hancock" :value 1 :type 's)))
             (elfeed-score-serde-link-rules
              (list (elfeed-score-link-rule--create :text "foo" :value 1 :type 's)))
             (elfeed-score-serde-tag-rules
              (list (elfeed-score-tag-rule--create :tags '(b c d) :value 1)))
             (elfeed-score-serde-udf-rules
              (list (elfeed-score-udf-rule--create :function (lambda (_) 1)))))
        (elfeed-score-scoring-explain-entry entry)
        (let ((text
               (with-current-buffer "*explanation-smoke-tests*"
                 (buffer-string))))
          (should
           (string=
            text
            (concat
             (propertize "bar" 'face 'elfeed-score-scoring-explain-text-face)
             " matches 8 rules for a score of 8:
(NB your score file is dirty; these matches correspond to the rules currently in-memory)
    1. title{bar}: \"bar\": 1
    2. feed{t/foo}: \"foo\": 1
    3. content{lorem}: \"Lorem\": 1
    4. title-or-content{t/bar}: \"bar\": 1
    5. authors{hancock} \"Hancock\": 1
    6. tags{(b c d)}: 1
    7. link{foo}: \"foo\": 1
    8. udf{<anonymous closure>}: \"bar\": 1
")))))))))

(provide 'test-explanations)

;;; test-explanations.el ends here
