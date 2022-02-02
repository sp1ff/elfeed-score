;;; elfeed-score-tests.el --- ERT tests for elfeed-score  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2022 Michael Herstine <sp1ff@pobox.com>

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

;; Unless & until I get my patch accepted into Emacs, I'll just
;; redefine `ert-run-tests-batch' here & customize the variables
;; in my test scripts.
(defvar ert-batch-print-length 10
  "`print-length' setting used in `ert-run-tests-batch'.

When formatting lists in test results, `print-length' will be
temporarily set to this value.")

(defvar ert-batch-print-level 5
  "`print-level' setting used in `ert-run-tests-batch'.

When formatting lists in test results, `print-level' will be
temporarily set to this value.")

(defun ert-run-tests-batch (&optional selector)
  "Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object."
  (unless selector (setq selector 't))
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (cl-ecase event-type
       (run-started
        (unless ert-quiet
          (cl-destructuring-bind (stats) event-args
            (message "Running %s tests (%s, selector `%S')"
                     (length (ert--stats-tests stats))
                     (ert--format-time-iso8601 (ert--stats-start-time stats))
                     selector))))
       (run-ended
        (cl-destructuring-bind (stats abortedp) event-args
          (let ((unexpected (ert-stats-completed-unexpected stats))
                (skipped (ert-stats-skipped stats))
		(expected-failures (ert--stats-failed-expected stats)))
            (message "\n%sRan %s tests, %s results as expected, %s unexpected%s (%s, %f sec)%s\n"
                     (if (not abortedp)
                         ""
                       "Aborted: ")
                     (ert-stats-total stats)
                     (ert-stats-completed-expected stats)
                     unexpected
                     (if (zerop skipped)
                         ""
                       (format ", %s skipped" skipped))
                     (ert--format-time-iso8601 (ert--stats-end-time stats))
                     (float-time
                      (time-subtract
                       (ert--stats-end-time stats)
                       (ert--stats-start-time stats)))
                     (if (zerop expected-failures)
                         ""
                       (format "\n%s expected failures" expected-failures)))
            (unless (zerop unexpected)
              (message "%s unexpected results:" unexpected)
              (cl-loop for test across (ert--stats-tests stats)
                       for result = (ert-test-most-recent-result test) do
                       (when (not (ert-test-result-expected-p test result))
                         (message "%9s  %S%s"
                                  (ert-string-for-test-result result nil)
                                  (ert-test-name test)
                                  (if (getenv "EMACS_TEST_VERBOSE")
                                      (ert-reason-for-test-result result)
                                    ""))))
              (message "%s" ""))
            (unless (zerop skipped)
              (message "%s skipped results:" skipped)
              (cl-loop for test across (ert--stats-tests stats)
                       for result = (ert-test-most-recent-result test) do
                       (when (ert-test-result-type-p result :skipped)
                         (message "%9s  %S%s"
                                  (ert-string-for-test-result result nil)
                                  (ert-test-name test)
                                  (if (getenv "EMACS_TEST_VERBOSE")
                                      (ert-reason-for-test-result result)
                                    ""))))
              (message "%s" "")))))
       (test-started
        )
       (test-ended
        (cl-destructuring-bind (stats test result) event-args
          (unless (ert-test-result-expected-p test result)
            (cl-etypecase result
              (ert-test-passed
               (message "Test %S passed unexpectedly" (ert-test-name test)))
              (ert-test-result-with-condition
               (message "Test %S backtrace:" (ert-test-name test))
               (with-temp-buffer
                 (insert (backtrace-to-string
                          (ert-test-result-with-condition-backtrace result)))
                 (if (not ert-batch-backtrace-right-margin)
                     (message "%s"
                              (buffer-substring-no-properties (point-min)
                                                              (point-max)))
                   (goto-char (point-min))
                   (while (not (eobp))
                     (let ((start (point))
                           (end (line-end-position)))
                       (setq end (min end
                                      (+ start
                                         ert-batch-backtrace-right-margin)))
                       (message "%s" (buffer-substring-no-properties
                                      start end)))
                     (forward-line 1))))
               (with-temp-buffer
                 (ert--insert-infos result)
                 (insert "    ")
                 (let ((print-escape-newlines t)
                       (print-level ert-batch-print-level)
                       (print-length ert-batch-print-length))
                   (ert--pp-with-indentation-and-newline
                    (ert-test-result-with-condition-condition result)))
                 (goto-char (1- (point-max)))
                 (cl-assert (looking-at "\n"))
                 (delete-char 1)
                 (message "Test %S condition:" (ert-test-name test))
                 (message "%s" (buffer-string))))
              (ert-test-aborted-with-non-local-exit
               (message "Test %S aborted with non-local exit"
                        (ert-test-name test)))
              (ert-test-quit
               (message "Quit during %S" (ert-test-name test)))))
          (unless ert-quiet
            (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                   (format-string (concat "%9s  %"
                                          (prin1-to-string (length max))
                                          "s/" max "  %S (%f sec)")))
              (message format-string
                       (ert-string-for-test-result result
                                                   (ert-test-result-expected-p
                                                    test result))
                       (1+ (ert--stats-test-pos stats test))
                       (ert-test-name test)
                       (ert-test-result-duration result))))))))
   nil))

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
                                                 tags (authors '((:name "spaceman sp1ff")))
                                                 link)
  "Generate a random entry with feed FEED, title TITLE & content CONTENT.
Use WITHIN to scope the date.  TAGS specifies tags to be applied in addition
to 'unread.

Warning: run this in `with-elfeed-test'.

This function differs from `elfeed-test-generate-entry' in that
it allows the caller to specify the entry title & content (which
is convenient for testing scoring)."

  (let* ((feed-id (elfeed-feed-id feed))
         (namespace (elfeed-url-to-namespace feed-id))
         (link (or link (elfeed-test-generate-url)))
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
  `(let ((elfeed-score-serde-title-rules nil)
         (elfeed-score-serde-feed-rules nil)
         (elfeed-score-serde-content-rules nil)
         (elfeed-score-serde-title-or-content-rules nil)
         (elfeed-score-serde-tag-rules nil)
         (elfeed-score-serde-adjust-tags-rules nil)
         (elfeed-score-serde-score-mark nil))
     (progn ,@body)))

(defun time-as-float (x)
  (+ (* (nth 0 x) 65536)
       (nth 1 x)
       (/ (nth 2 x) 1000000.0)))

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
  "Test `elfeed-score-scoring--match-tags'."

  (should (eq t   (elfeed-score-scoring--match-tags '(foo bar) '(t . (foo)))))
  (should (eq t   (elfeed-score-scoring--match-tags '(foo bar) nil)))
  (should (eq nil (elfeed-score-scoring--match-tags '(foo bar) '(t . (splat)))))
  (should (eq nil (elfeed-score-scoring--match-tags '(foo bar) '(nil . (foo))))))

(provide 'elfeed-score-tests)

;;; elfeed-score-tests.el ends here
