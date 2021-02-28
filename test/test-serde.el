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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should (equal score-entries-read
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
                         :content nil)))))

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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should (equal score-entries-read
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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
       (should (equal score-entries-read
                      (list :mark -2500
                            :feeds (list (elfeed-score-feed-rule--create
                                          :text "foo.com" :value 100 :type 's
                                          :attr 'u)
                                         (elfeed-score-feed-rule--create
                                          :text "title" :value -100 :type 's
                                          :attr 't :tags '(t . (foo bar))))
                            :titles (list (elfeed-score-title-rule--create
                                           :text "hoping" :value -1000 :type 's
                                           :tags (list t 'foo 'bar))
                                          (elfeed-score-title-rule--create
                                           :text "long way( home)?" :value 100
                                           :type 'r))
                            :content (list (elfeed-score-content-rule--create
                                            :text "now is the time..." :value 100
                                            :type 's :tags (list t 'foo 'bar)))
                            :title-or-content
                            (list (elfeed-score-title-or-content-rule--create
                                   :text "now is the time..." :title-value 150
                                   :content-value 100 :type 's :tags '(t . (foo bar)))))))))

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
       (should (equal score-entries-read
                      (list :mark -2500
                            :adjust-tags nil
                            :feeds (list (elfeed-score-feed-rule--create
                                          :text "foo.com" :value 100 :type 's
                                          :attr 'u)
                                         (elfeed-score-feed-rule--create
                                          :text "title" :value -100 :type 's
                                          :attr 't))
                            :titles (list (elfeed-score-title-rule--create
                                           :text "hoping" :value -1000 :type 's
                                           :tags (list t 'foo 'bar))
                                          (elfeed-score-title-rule--create
                                           :text "long way( home)?" :value 100
                                           :type 'r))
                            :content nil
                            :title-or-content nil
                            :authors nil
                            :tag nil
                            :link nil)))))

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
    (should (equal score-entries-read
                   (list :mark -2500
                         :adjust-tags
                         (list (elfeed-score-adjust-tags-rule--create
                                :threshold '(t . 100)
                                :tags '(t . important))
                               (elfeed-score-adjust-tags-rule--create
                                :threshold '(nil . -100)
                                :tags '(nil . important)))
                         :feeds (list (elfeed-score-feed-rule--create
                                       :text "foo.com" :value 100 :type 's
                                       :attr 'u)
                                      (elfeed-score-feed-rule--create
                                       :text "title" :value -100 :type 's
                                       :attr 't))
                         :titles (list (elfeed-score-title-rule--create
                                        :text "hoping" :value -1000 :type 's
                                        :tags (list t 'foo 'bar))
                                       (elfeed-score-title-rule--create
                                        :text "long way( home)?" :value 100
                                        :type 'r))
                         :content nil
                         :title-or-content nil
                         :authors nil
                         :tag (list (elfeed-score-tag-rule--create
                                     :tags '(t . (a b c))
                                     :value 10)
                                    (elfeed-score-tag-rule--create
                                     :tags '(nil . z)
                                     :value -1))
                         :link nil)))))

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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should (equal score-entries-read
                   (list :mark -2500
                         :adjust-tags
                         (list (elfeed-score-adjust-tags-rule--create
                                :threshold '(t . 100)
                                :tags '(t . important))
                               (elfeed-score-adjust-tags-rule--create
                                :threshold '(nil . -100)
                                :tags '(nil . important)))
                         :feeds (list (elfeed-score-feed-rule--create
                                       :text "foo.com" :value 100 :type 's
                                       :attr 'u)
                                      (elfeed-score-feed-rule--create
                                       :text "title" :value -100 :type 's
                                       :attr 't))
                         :titles (list (elfeed-score-title-rule--create
                                        :text "hoping" :value -1000 :type 's
                                        :tags (list t 'foo 'bar))
                                       (elfeed-score-title-rule--create
                                        :text "long way( home)?" :value 100
                                        :type 'r))
                         :content nil
                         :title-or-content nil
                         :authors (list (elfeed-score-authors-rule--create
                                         :text "esr" :value 100 :type 's))
                         :tag (list (elfeed-score-tag-rule--create
                                     :tags '(t . (a b c))
                                     :value 10)
                                    (elfeed-score-tag-rule--create
                                     :tags '(nil . z)
                                     :value -1)))))))

(ert-deftest test-score-files-6 ()
  "Smoke test reading/writing score files.

Format version 6; tag- and feed-scorping rules; authors rule."

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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should (equal score-entries-read
                   (list
                    :mark -2500
                    :adjust-tags
                    (list (elfeed-score-adjust-tags-rule--create
                           :threshold '(t . 100)
                           :tags '(t . important))
                          (elfeed-score-adjust-tags-rule--create
                           :threshold '(nil . -100)
                           :tags '(nil . important)))
                    :feeds `(,(elfeed-score-feed-rule--create
                               :text "foo.com" :value 100 :type 's
                               :attr 'u :hits 0)
                             ,(elfeed-score-feed-rule--create
                               :text "title" :value -100 :type 's
                               :attr 't))
                    :titles `(,(elfeed-score-title-rule--create
                                :text "hoping" :value -1000 :type 's
                                :tags '(t foo bar) :hits 0
                                :feeds '(t (t . "foo")))
                              ,(elfeed-score-title-rule--create
                                :text "long way( home)?" :value 100
                                :type 'r))
                    :content nil
                    :title-or-content nil
                    :authors
                    `(,(elfeed-score-authors-rule--create
                        :text "esr" :value 100 :type 's
                        :hits 100
                        :feeds '(t (t . "foo") (u . "http://bar.com/feed"))))
                    :tag (list (elfeed-score-tag-rule--create
                                :tags '(t . (a b c))
                                :value 10)
                               (elfeed-score-tag-rule--create
                                :tags '(nil . z)
                                :value -1)))))))

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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should (equal score-entries-read
                   (list :mark nil :adjust-tags nil :feeds nil
                         :titles
                         (list
                          (elfeed-score-title-rule--create
                           :text "hoping" :value -1000 :type 's))
                         :content nil :title-or-content nil
                         :authors nil :tag nil)))))

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
         (score-entries-read (elfeed-score-serde--parse-score-file score-file)))
    (should (equal score-entries-read
                   (list :mark nil :adjust-tags nil :feeds nil
                         :titles
                         (list
                          (elfeed-score-title-rule--create
                           :text "hoping" :value -1000 :type 's))
                         :content nil :title-or-content nil
                         :authors nil :tag nil
                         :link
                         (list
                          (elfeed-score-link-rule--create
                           :text "foo" :value 100 :type 'r)))))))

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
         ;; Pre-create the backup-file
         (backup-name (format "%s.~5~" score-file)))
    (with-temp-buffer
      (insert score-text)
      (write-file backup-name nil))
    ;; This will attempt to make a backup, but stumble across the one
    ;; we just made; should *not* error out
    (elfeed-score-serde--parse-score-file score-file)))

(provide 'test-serde)
;;; test-serde.el ends here
