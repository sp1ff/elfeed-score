;;; elfeed-score-test-serde.el --- ERT tests for elfeed-score SERDE  -*- lexical-binding: t; -*-

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
                         :content nil)))))

(ert-deftest elfeed-score-test-score-files-1 ()
  "Smoke test reading/writing score files"

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

(ert-deftest elfeed-score-test-score-files-2 ()
  "Smoke test reading/writing score files"

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
         (score-entries-2 (elfeed-score--parse-score-file score-file)))
       (should (equal score-entries-2
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

(ert-deftest elfeed-score-test-score-files-3 ()
  "Smoke test reading/writing score files"

  (let* ((score-entries
          '(("title"
             ("hoping" -1000 s nil (t . (foo bar)))
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
                            :tag nil)))))

(ert-deftest elfeed-score-test-score-files-4 ()
  "Smoke test reading/writing score files"

  (let* ((score-entries
          '(("title"
             ("hoping" -1000 s nil (t . (foo bar)))
             ("long way( home)?" +100 r))
            ("feed"
             ("foo.com" +100 s u)
             ("title" -100 s t))
            ("tag"
             ((t . (a b c)) +10)
             ((nil . z) -1))
            ("adjust-tags"
             ((t . 100) (t . important))
             ((nil . -100) (nil . important)))
            (mark -2500)))
         (score-text (pp-to-string score-entries))
         (score-file (make-temp-file "elfeed-score-test-" nil nil score-text))
         (score-entries-2 (elfeed-score--parse-score-file score-file)))
    (should (equal score-entries-2
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
                                     :value -1)))))))

(ert-deftest elfeed-score-test-score-files-5 ()
  "Smoke test reading/writing score files"

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
         (score-entries-2 (elfeed-score--parse-score-file score-file)))
    (should (equal score-entries-2
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

(ert-deftest elfeed-score-test-score-files-6 ()
  "Smoke test reading/writing score files"

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
         (score-entries-2 (elfeed-score--parse-score-file score-file)))
    (should (equal score-entries-2
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

(provide 'elfeed-score-test-serde)

;;; elfeed-score-test-serde.el ends here
