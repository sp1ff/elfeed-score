;;; test-stats.el --- ERT tests for elfeed-score-rule-stats  -*- lexical-binding: t -*-

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

;; One aspect of the hash table I use to hold rule stats is that it
;; doesn't hold a reference to its keys; IOW they can be garbage
;; collected while in the table if there are no references to them
;; other than the table-- this is how I keep the table from being
;; cluttered with entries for rules that no longer exist.

;; For reasons that are unclear to me, I cannot force garbage
;; collection within ERT tests, so I have to validate that outside
;; this test suite.

;;; Code:
(require 'elfeed-score-rule-stats)
(require 'elfeed-score-serde)
(require 'elfeed-score-rules)

(ert-deftest test-stats-smoke ()
  "Test for smoke in elfeed-score-rule-stats."

  ;; Whip-up a rule
  (let ((rule (elfeed-score-title-rule--create :text "foo" :value 100 :type 's)))
    (elfeed-score-rule-stats-on-match rule 100.0)
    (let ((stats (elfeed-score-rule-stats-get rule)))
      (should (eql 1 (elfeed-score-rule-stats-hits stats)))
      (should (eql 100.0 (elfeed-score-rule-stats-date stats))))))

(ert-deftest test-stats-serde ()
  "Test SERDE for rule statistics."

  (setq elfeed-score-rule-stats--table (elfeed-score-rule-stats--make-table))
  (should (eq 0 (hash-table-count elfeed-score-rule-stats--table)))

  ;; Whip up a few rules
  (let ((r1 (elfeed-score-title-rule--create
             :text "Bar" :value 1 :type 's))
        (r2 (elfeed-score-feed-rule--create
             :text "feed" :value 1 :type 's :attr 't)))
    (elfeed-score-rule-stats-on-match r1)
    (elfeed-score-rule-stats-on-match r2)
    (should (eq 2 (hash-table-count elfeed-score-rule-stats--table)))
    (let ((s1 (elfeed-score-rule-stats-get r1))
          (s2 (elfeed-score-rule-stats-get r2))
          (stats-file (make-temp-file "test-stats-")))
      (elfeed-score-rule-stats-write stats-file)
      (setq elfeed-score-rule-stats--table (elfeed-score-rule-stats--make-table))
      (should (eq 0 (hash-table-count elfeed-score-rule-stats--table)))
      (elfeed-score-rule-stats-read stats-file)
      (should (eq 2 (hash-table-count elfeed-score-rule-stats--table)))
      (should (equal s1 (elfeed-score-rule-stats-get r1)))
      (should (equal s2 (elfeed-score-rule-stats-get r2))))
    (elfeed-score-serde-cleanup-stats)))

(ert-deftest test-stats-cleanup ()
  "Test `elfeed-score-rule-stats-clean'."
  (let ((r1 (elfeed-score-title-rule--create
             :text "Bar" :value 1 :type 's))
        (r2 (elfeed-score-feed-rule--create
             :text "feed" :value 1 :type 's :attr 't)))
    (elfeed-score-rule-stats-on-match r1)
    (elfeed-score-rule-stats-on-match r2)
    (should (eq 2 (hash-table-count elfeed-score-rule-stats--table)))
    (elfeed-score-rule-stats-clean (list r1))
    (should (eq 1 (hash-table-count elfeed-score-rule-stats--table)))
    (let ((s1 (elfeed-score-rule-stats-get r1)))
      (should s1))))

(ert-deftest test-sexp-to-file ()
  "Test `elfeed-score-rule-stats--sexp-to-file."

  ;; Trivial test
  (elfeed-score-rule-stats--sexp-to-file '(a b c) "test-sexp")
  (should (file-exists-p "test-sexp"))
  (let ((x
         (car
          (read-from-string
           (with-temp-buffer
             (insert-file-contents "test-sexp")
             (buffer-string))))))
    (should (equal x '(a b c))))
  (add-name-to-file "test-sexp" "test-sexp-2")
  (elfeed-score-rule-stats--sexp-to-file '(1 2 3) "test-sexp-2" ";; Hello\n")
  (let ((x
         (car
          (read-from-string
           (with-temp-buffer
             (insert-file-contents "test-sexp")
             (buffer-string))))))
    (should (equal x '(1 2 3))))
  (delete-file "test-sexp")
  (delete-file "test-sexp-2"))

;; `file-name-concat' was only introduced in Emacs 28.1, and we want to support
;; 26.1. Write a replacement in Emacs Lisp.
(defun file-name-concat-1 (&rest components)
  "Concatenate file name COMPONENTS into a single path."
  (let ((result ""))
    (dolist (component components)
      (setq result (expand-file-name component result)))
    result))

(ert-deftest test-issue-32 ()
  "Regression test against elfeed-score issue #32
\"Parent dirs in elfeed-score-rule-stats-file are not created
automatically\"

If `elfeed-score-rule-stats-write' is invoked with an argument
for which some parent directories don't exist, and the caller has
permissions, they should be created automatically."

  ;; Whip-up a few rules...
  (let ((r1 (elfeed-score-title-rule--create
             :text "Bar" :value 1 :type 's))
        (r2 (elfeed-score-feed-rule--create
             :text "feed" :value 1 :type 's :attr 't)))
    ;; & generate some stats for 'em.
    (elfeed-score-rule-stats-on-match r1)
    (elfeed-score-rule-stats-on-match r2)
    (should (eq 2 (hash-table-count elfeed-score-rule-stats--table)))
    ;; and send 'em to a target file whose parent directories don't exist (yet)
    (let ((stats-file
           (file-name-concat-1 (make-temp-file "iss-32-" t)
                               "a"
                               "b"
                               "test-stats-file")))
      (elfeed-score-rule-stats-write stats-file)
      (should (file-exists-p stats-file)))
    (elfeed-score-serde-cleanup-stats)))

(provide 'test-stats)

;;; test-stats.el ends here.
