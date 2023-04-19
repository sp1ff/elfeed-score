;;; test-udf-rules -- ERT tests for elfeed-score-udf-rule -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2023 Michael Herstine <sp1ff@pobox.com>

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

(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-score)
(require 'elfeed-db-tests)
(require 'elfeed-score-tests)

(ert-deftest elfeed-score-test-udf-rules-smoke ()
  "Smoke tests for UDF rules."

  (let ((r1 (elfeed-score-udf-rule--create :function (lambda (_) 1))))
    (should (equal (elfeed-score-udf-rule-display-name r1) "<anonymous closure>")))

  (defun test-fn (_) 1)
  (let ((r2 (elfeed-score-udf-rule--create :function 'test-fn)))
    (should (equal (elfeed-score-udf-rule-display-name r2) "test-fn")))

  (let ((bc (byte-compile 'test-fn)))
    (let ((r3 (elfeed-score-udf-rule--create :function bc))
          (r4 (elfeed-score-udf-rule--create :function bc :display-name "compiled-test-fn")))
      (should (equal (elfeed-score-udf-rule-display-name r3) "<anonymous byte-compiled function>"))
      (should (equal (elfeed-score-udf-rule-display-name r4) "compiled-test-fn"))
      (let ((title "foo bar splat"))
        (with-elfeed-test
         (let* ((feed (elfeed-score-test-generate-feed "feed" "http://www.feed.com/rss"))
                (entry (elfeed-score-test-generate-entry feed title "some content")))
           (elfeed-db-add entry)
           (with-elfeed-score-test
            (let* ((elfeed-score-serde-udf-rules
                       (list
                        (elfeed-score-udf-rule--create :function (lambda (_) 1))
                        (elfeed-score-udf-rule--create :function 'test-fn)
                        r4))
                   (score (elfeed-score-scoring-score-entry entry)))
              (should (eq 3 score))))))))))

(provide 'test-udf-rules)

;;; test-udf-rules.el ends here
