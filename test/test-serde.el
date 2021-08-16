;;; test-serde.el --- ERT tests for elfeed-score SERDE  -*- lexical-binding: t; -*-

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

;; These tests require 'elfeed-db-tests.

;;; Code:

(require 'elfeed-db)
(require 'elfeed-lib)
(require 'elfeed-score)
(require 'elfeed-db-tests)
(require 'elfeed-score-tests)

(ert-deftest score-files-smoke-test ()
  "Smoke test reading/writing score files.

Test reading a trival score file (format version 1)."

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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file))
         (null-stats (elfeed-score-rule-stats--create)))
    (should (equal score-entries-read
                   (list :mark -2500
                         :feeds
                         (list
                          (cons
                           (elfeed-score-feed-rule--create
                            :text "foo.com" :value 100 :type 's :attr 'u)
                           null-stats)
                          (cons
                           (elfeed-score-feed-rule--create
                            :text "title" :value -100 :type 's :attr 't)
                           null-stats))
                         :titles
                         (list
                          (cons
                           (elfeed-score-title-rule--create
                            :text "hoping" :value -1000 :type 's)
                           null-stats)
                          (cons
                           (elfeed-score-title-rule--create
                            :text "long way( home)?" :value 100 :type 'r)
                           null-stats))
                         :content nil :version 1)))))

(ert-deftest test-score-files-1 ()
  "Smoke test reading/writing score files.

Test reading a trival score file (format version 2)."

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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file))
         (null-stats (elfeed-score-rule-stats--create)))
    (should (equal score-entries-read
                   (list :mark -2500
                         :feeds
                         (list
                          (cons
                           (elfeed-score-feed-rule--create
                            :text "foo.com" :value 100 :type 's :attr 'u)
                           null-stats)
                          (cons
                           (elfeed-score-feed-rule--create
                            :text "title" :value -100 :type 's :attr 't)
                           null-stats))
                         :titles
                         (list
                          (cons
                           (elfeed-score-title-rule--create
                            :text "hoping" :value -1000 :type 's)
                           null-stats)
                          (cons
                           (elfeed-score-title-rule--create
                            :text "long way( home)?" :value 100 :type 'r)
                           null-stats))
                         :content nil
                         :title-or-content nil :version 2)))))

(ert-deftest test-score-files-2 ()
  "Smoke test reading/writing score files.

Test reading a trival score file (format version 2 with
tag-scoping rules)."

  (let* ((score-entries
          '((version 2)
            ("title"
             ("hoping" -1000 s nil (t . (foo bar)))
             ("long way( home)?" +100 r))
            ("content"
             ("now is the time..." +100 s nil (t . (foo bar))))
            ("title-or-content"
             ("now is the time..." 150 100 s nil (t . (foo bar))))
            ("feed"
             ("foo.com" +100 s u)
             ("title" -100 s t nil (t . (foo bar))))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file))
         (null-stats (elfeed-score-rule-stats--create)))
    (should
     (equal
      score-entries-read
      (list
       :mark -2500
       :feeds
       (list
        (cons
         (elfeed-score-feed-rule--create
          :text "foo.com" :value 100 :type 's :attr 'u)
         null-stats)
        (cons
         (elfeed-score-feed-rule--create
          :text "title" :value -100 :type 's
          :attr 't :tags '(t . (foo bar)))
         null-stats))
       :titles
       (list
        (cons
         (elfeed-score-title-rule--create
          :text "hoping" :value -1000 :type 's
          :tags (list t 'foo 'bar))
         null-stats)
        (cons
         (elfeed-score-title-rule--create
          :text "long way( home)?" :value 100 :type 'r)
         null-stats))
       :content
       (list
        (cons
         (elfeed-score-content-rule--create
          :text "now is the time..." :value 100
          :type 's :tags (list t 'foo 'bar))
         null-stats))
       :title-or-content
       (list
        (cons
         (elfeed-score-title-or-content-rule--create
          :text "now is the time..." :title-value 150
          :content-value 100 :type 's :tags '(t . (foo bar)))
         null-stats))
       :version 2)))))

(ert-deftest test-score-files-3 ()
  "Smoke test reading/writing score files.

Format version defaulted; tag-scoping rules."
  (let* ((score-entries
          '(("title"
             (:text "hoping" :value -1000 :type s :tags (t . (foo bar)))
             (:text "long way( home)?" :value +100 :type r))
            ("feed"
             (:text "foo.com" :value +100 :type s :attr u)
             (:text "title" :value -100 :type s :attr t))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should
     (equal
      score-entries-read
      (list
       :mark -2500
       :adjust-tags nil
       :feeds
       (list
        (cons
         (elfeed-score-feed-rule--create
          :text "foo.com" :value 100 :type 's :attr 'u)
         nil)
        (cons
         (elfeed-score-feed-rule--create
          :text "title" :value -100 :type 's :attr 't)
         nil))
       :titles
       (list
        (cons
         (elfeed-score-title-rule--create
          :text "hoping" :value -1000 :type 's
          :tags (list t 'foo 'bar))
         nil)
        (cons
         (elfeed-score-title-rule--create
          :text "long way( home)?" :value 100 :type 'r)
         nil))
       :content nil
       :title-or-content nil
       :authors nil
       :tag nil
       :link nil
       :version elfeed-score-serde-current-format)))))

(ert-deftest test-score-files-4 ()
  "Smoke test reading/writing score files.

Format version defaulted, includes adjust-tags rules."

  (let* ((score-entries
          '(("title"
             (:text "hoping" :value -1000 :type s :tags (t . (foo bar)))
             (:text "long way( home)?" :value +100 :type r))
            ("feed"
             (:text "foo.com" :value +100 :type s :attr u)
             (:text "title" :value -100 :type s :attr t))
            ("tag"
             (:tags (t . (a b c)) :value +10)
             (:tags (nil . z) :value -1))
            ("adjust-tags"
             (:threshold (t . 100) :tags (t . important))
             (:threshold (nil . -100) :tags (nil . important)))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should
     (equal
      score-entries-read
      (list
       :mark -2500
       :adjust-tags
       (list
        (cons
         (elfeed-score-adjust-tags-rule--create
          :threshold '(t . 100)
          :tags '(t . important))
         nil)
        (cons
         (elfeed-score-adjust-tags-rule--create
          :threshold '(nil . -100)
          :tags '(nil . important))
         nil))
       :feeds
       (list
        (cons
         (elfeed-score-feed-rule--create
          :text "foo.com" :value 100 :type 's :attr 'u)
         nil)
        (cons
         (elfeed-score-feed-rule--create
          :text "title" :value -100 :type 's :attr 't)
         nil))
       :titles
       (list
        (cons
         (elfeed-score-title-rule--create
          :text "hoping" :value -1000 :type 's
          :tags (list t 'foo 'bar))
         nil)
        (cons
         (elfeed-score-title-rule--create
          :text "long way( home)?" :value 100 :type 'r)
         nil))
       :content nil
       :title-or-content nil
       :authors nil
       :tag
       (list
        (cons
         (elfeed-score-tag-rule--create
          :tags '(t . (a b c)) :value 10)
         nil)
        (cons
         (elfeed-score-tag-rule--create
          :tags '(nil . z) :value -1)
         nil))
       :link nil
       :version elfeed-score-serde-current-format)))))

(ert-deftest test-score-files-5 ()
  "Smoke test reading/writing score files.

Format version 4."

  (let* ((score-entries
          '((version 4)
            ("title"
             ("hoping" -1000 s nil (t . (foo bar)))
             ("long way( home)?" +100 r))
            ("feed"
             ("foo.com" +100 s u)
             ("title" -100 s t))
            ("authors"
             ("esr" +100 s))
            ("tag"
             ((t . (a b c)) +10)
             ((nil . z) -1))
            ("adjust-tags"
             ((t . 100) (t . important))
             ((nil . -100) (nil . important)))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file))
         (null-stats (elfeed-score-rule-stats--create)))
    (should
     (equal
      score-entries-read
      (list
       :mark -2500
       :adjust-tags
       (list
        (cons (elfeed-score-adjust-tags-rule--create
               :threshold '(t . 100) :tags '(t . important))
              null-stats)
        (cons (elfeed-score-adjust-tags-rule--create
               :threshold '(nil . -100) :tags '(nil . important))
              null-stats))
       :feeds
       (list
        (cons
         (elfeed-score-feed-rule--create
          :text "foo.com" :value 100 :type 's :attr 'u)
         null-stats)
        (cons
         (elfeed-score-feed-rule--create
          :text "title" :value -100 :type 's :attr 't)
         null-stats))
       :titles
       (list
        (cons
         (elfeed-score-title-rule--create
          :text "hoping" :value -1000 :type 's :tags (list t 'foo 'bar))
         null-stats)
        (cons
         (elfeed-score-title-rule--create
          :text "long way( home)?" :value 100 :type 'r)
         null-stats))
       :content nil
       :title-or-content nil
       :authors
       (list
        (cons
         (elfeed-score-authors-rule--create
          :text "esr" :value 100 :type 's)
         null-stats))
       :tag
       (list
        (cons
         (elfeed-score-tag-rule--create
          :tags '(t . (a b c)) :value 10)
         null-stats)
        (cons
         (elfeed-score-tag-rule--create
          :tags '(nil . z) :value -1)
         null-stats))
       :version 4)))))

(ert-deftest test-score-files-6 ()
  "Smoke test reading/writing score files.

Format version 6; tag- and feed-scoping rules; authors rule."

  (let* ((score-entries
          '((version 5)
            ("title"
             ("hoping" -1000 s nil (t . (foo bar)) 0 (t . ((t . "foo"))))
             ("long way( home)?" +100 r))
            ("feed"
             ("foo.com" +100 s u nil nil 0)
             ("title" -100 s t))
            ("authors"
             ("esr" +100 s nil nil 100 (t . ((t . "foo") (u . "http://bar.com/feed")))))
            ("tag"
             ((t . (a b c)) +10)
             ((nil . z) -1))
            ("adjust-tags"
             ((t . 100) (t . important))
             ((nil . -100) (nil . important)))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file))
         (null-stats (elfeed-score-rule-stats--create)))
    (should
     (equal
      score-entries-read
      (list
       :mark -2500
       :adjust-tags
       (list
        (cons (elfeed-score-adjust-tags-rule--create
               :threshold '(t . 100) :tags '(t . important))
              null-stats)
        (cons (elfeed-score-adjust-tags-rule--create
               :threshold '(nil . -100) :tags '(nil . important))
              null-stats))
       :feeds
       (list
        (cons
         (elfeed-score-feed-rule--create
          :text "foo.com" :value 100 :type 's :attr 'u)
         null-stats)
        (cons
         (elfeed-score-feed-rule--create
          :text "title" :value -100 :type 's :attr 't)
         null-stats))
       :titles
       (list
        (cons
         (elfeed-score-title-rule--create
          :text "hoping" :value -1000 :type 's
          :tags '(t foo bar)
          :feeds '(t (t . "foo")))
         null-stats)
        (cons
         (elfeed-score-title-rule--create
          :text "long way( home)?" :value 100 :type 'r)
         null-stats))
       :content nil
       :title-or-content nil
       :authors
       (list
        (cons
         (elfeed-score-authors-rule--create
          :text "esr" :value 100 :type 's
          :feeds '(t (t . "foo") (u . "http://bar.com/feed")))
         (elfeed-score-rule-stats--create
          :hits 100)))
       :tag
       (list
        (cons
         (elfeed-score-tag-rule--create
          :tags '(t . (a b c)) :value 10)
         null-stats)
        (cons
         (elfeed-score-tag-rule--create
          :tags '(nil . z) :value -1)
         null-stats))
       :version 5)))))

(ert-deftest test-issue-7 ()
  "Check that the error in issue #7 is caught."

  (let* ((score-entries
          '((version 5)
            ("authors" nil)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text)))
    (should-error (elfeed-score-serde--parse-score-file score-file))))

(ert-deftest test-format-version-6 ()
  "Smoke tests for format version 6."
  (let* ((score-entries
          '((version 6)
            ("title"
             (:text "hoping" :value -1000 :type s))))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file))
         (null-stats (elfeed-score-rule-stats--create)))
    (should (equal score-entries-read
                   (list
                    :mark nil
                    :adjust-tags nil
                    :feeds nil
                    :titles
                    (list
                     (cons
                      (elfeed-score-title-rule--create
                       :text "hoping" :value -1000 :type 's)
                      null-stats))
                    :content nil :title-or-content nil
                    :authors nil :tag nil :version 6)))))

(ert-deftest test-format-version-7 ()
  "Smoke tests for format version 7."
  (let* ((score-entries
          '((version 7)
            ("title"
             (:text "hoping" :value -1000 :type s))
            ("link"
             (:text "foo" :value 100 :type r))))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file))
         (null-stats (elfeed-score-rule-stats--create)))
    (should (equal score-entries-read
                   (list :mark nil :adjust-tags nil :feeds nil
                         :titles
                         (list
                          (cons
                           (elfeed-score-title-rule--create
                            :text "hoping" :value -1000 :type 's)
                           null-stats))
                         :content nil :title-or-content nil
                         :authors nil :tag nil
                         :link
                         (list
                          (cons
                           (elfeed-score-link-rule--create
                            :text "foo" :value 100 :type 'r)
                           null-stats))
                         :version 7)))))

(ert-deftest test-writes-latest-version ()
  "Be sure that the score file is written in the lastest format version."

  (let* ((score-file (make-temp-file "elfeed-score-test-")))
    (elfeed-score-serde-write-score-file score-file)
    (let ((sexp
           (car
            (read-from-string
             (with-temp-buffer
               (insert-file-contents score-file)
               (buffer-string))))))
      (should (eq elfeed-score-serde-current-format
                  (car (alist-get 'version sexp)))))))

(ert-deftest test-issue-12 ()
  "Test my fix to issue #12."
  (let* ((score-entries
          '((version 5)
            ("title"
             ("hoping" -1000 s nil (t . (foo bar)) 0 (t . ((t . "foo"))))
             ("long way( home)?" +100 r))
            ("feed"
             ("foo.com" +100 s u nil nil 0)
             ("title" -100 s t))
            ("authors"
             ("esr" +100 s nil nil 100 (t . ((t . "foo")
                                             (u . "http://bar.com/feed")))))
            ("tag"
             ((t . (a b c)) +10)
             ((nil . z) -1))
            ("adjust-tags"
             ((t . 100) (t . important))
             ((nil . -100) (nil . important)))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (backup-name (format "%s.~5~" score-file)))
    ;; Pre-create the backup-file
    (with-temp-buffer
      (insert score-text)
      (write-file backup-name nil))
    ;; This will attempt to make a backup, but stumble across the one
    ;; we just made; should *not* error out
    (elfeed-score-serde--parse-score-file score-file)))

(ert-deftest test-issue-12-a ()
  "Test a secondary aspect to my fix to issue #12.

When elfeed-score reads a score file in an archaic format, it
should immediately re-write the file in the new format (as well
as make a backup copy of the file in the old format--
cf. `test-issue-12'."
  (let* ((score-entries
          '((version 5)
            ("title"
             ("hoping" -1000 s nil (t . (foo bar)) 0 (t . ((t . "foo"))))
             ("long way( home)?" +100 r))
            ("feed"
             ("foo.com" +100 s u nil nil 0)
             ("title" -100 s t))
            ("authors"
             ("esr" +100 s nil nil 100 (t . ((t . "foo")
                                             (u . "http://bar.com/feed")))))
            ("tag"
             ((t . (a b c)) +10)
             ((nil . z) -1))
            ("adjust-tags"
             ((t . 100) (t . important))
             ((nil . -100) (nil . important)))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (backup-name (format "%s.~5~" score-file)))
    (elfeed-score-serde-load-score-file score-file)
    ;; This should leave us with:
    ;; 1. a copy of the score file in `backup-name'
    (should (file-exists-p backup-name))
    ;; 2. `score-file' updated to `elfeed-score-serde-current-format'
    (should
     (eq elfeed-score-serde-current-format
         (plist-get (elfeed-score-serde--parse-score-file score-file) :version)))))

(ert-deftest test-issue-14 ()
  "Test my fix to issue #14 (and that there is no regression)."
  (let* ((score-entries
          '((version 7)
            ("link"
             (:text "reddit" :value 30 :type s)
             (:text "twitter" :value 30 :type s))))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text)))
    (with-elfeed-score-test
     (elfeed-score-serde-load-score-file score-file)
     (should elfeed-score-serde-link-rules))))

(ert-deftest test-format-version-8 ()
  "Smoke tests for format version 8."
  (let* ((score-entries
          '((version 8)
            ("title"
             (:text "hoping" :value -1000 :type s))
            ("link"
             (:text "foo" :value 100 :type r))))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should (equal score-entries-read
                   (list :mark nil :adjust-tags nil :feeds nil
                         :titles
                         (list
                          (cons
                           (elfeed-score-title-rule--create
                            :text "hoping" :value -1000 :type 's)
                           nil))
                         :content nil :title-or-content nil
                         :authors nil :tag nil
                         :link
                         (list
                          (cons
                           (elfeed-score-link-rule--create
                            :text "foo" :value 100 :type 'r)
                           nil))
                         :version 8)))))

(ert-deftest test-add-rule ()
  "Tests for `elfeed-score-serde-add-rule'."
  (with-elfeed-score-test
   (let* ((score-entries
           '((version 8)
             ("title"
              (:text "hoping" :value -1000 :type s))
             ("link"
              (:text "foo" :value 100 :type r))))
          (score-text (pp-to-string score-entries))
          (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
          (elfeed-score-serde-score-file score-file)
          (last-read-time))
     (elfeed-score-serde-load-score-file elfeed-score-serde-score-file)
     (setq last-read-time elfeed-score-serde--last-load-time)
     (sleep-for 0.25)
     (elfeed-score-serde-add-rule
      (elfeed-score-title-rule--create
       :text "paradise found" :value 150 :type 's))
     (should (< last-read-time elfeed-score-serde--last-load-time))
     (should
      (equal
       elfeed-score-serde-title-rules
       (list
        (elfeed-score-title-rule--create
         :text "paradise found" :value 150 :type 's)
        (elfeed-score-title-rule--create
         :text "hoping" :value -1000 :type 's)))))))

(ert-deftest test-add-rules-no-file ()
  "Tests for `elfeed-score-serde-add-rule' with no score file."
  (with-elfeed-score-test
   (let ((elfeed-score-serde-score-file nil))
     (elfeed-score-serde-add-rule
      (elfeed-score-title-rule--create
       :text "paradise found" :value 150 :type 's))
     (should
      (equal
       elfeed-score-serde-title-rules
       (list
        (elfeed-score-title-rule--create
         :text "paradise found" :value 150 :type 's)))))))

(provide 'test-serde)
;;; test-serde.el ends here
