;;; smoke-test-core.el --- Core "smoke" test logic  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2023 Michael Herstine <sp1ff@pobox.com>

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

;; This file is intended to be loaded by other test files, after the
;; basic configuration variables have been set-up.  It begins by
;; calling `elfeed-score-enable' and then carries out a number of
;; operations, using `cl-assert' to check results.

;;; Code:

(require 'cl-lib)
(require 'elfeed)
(require 'elfeed-score)
(require 'elfeed-score-tests)

;; NB the tests below depend on the fact that all three of these rules
;; will match at least once (which they will, assuming r/Emacs has any
;; content, which seems a safe bet).
(with-temp-file
    elfeed-score-score-file
  (insert "((version 5)
(\"title\"
  (\".*\" 100 r))
(\"content\"
 (\".*\" 10 r))
(\"feed\"
 (\"emacs-news\" 75 s t)))
"))

(elfeed-score-enable)
(elfeed)
(define-key elfeed-search-mode-map "=" elfeed-score-map)

(cl-assert (file-exists-p (concat test-dir "elfeed.score.~5~")))

(message "Beginning update...")
(elfeed-search-fetch nil)

(defconst max-count 2048)
(let ((count 0))
  (while (and (> (elfeed-queue-count-total) 0) (< count max-count))
    (sit-for 2)
    (while (accept-process-output nil 2))
    (setq count (1+ count))
    (message "Beginning update...%s" (make-string count ?.)))
  (message
   "Beginning update...%s(%s[%d])"
   (make-string count ?.)
   (if (eq count max-count) "FAILED" "complete")
   count))

(cl-assert (not (elfeed-score-serde-score-file-dirty-p)))

;; We should have re-written the score file in the current format.
(setq
 sexp
 (car
  (read-from-string
   (with-temp-buffer
     (insert-file-contents elfeed-score-score-file)
     (buffer-string)))))

(cl-assert (eq elfeed-score-serde-current-format (car (alist-get 'version sexp)))
           t "Incorrect format %s!?" (car (alist-get 'version sexp)))
(cl-assert (eq 1 (length (cdr (assoc "title" sexp)))))
(cl-assert (eq 1 (length (cdr (assoc "content" sexp)))))

(let ((plist (cadr (assoc "title" sexp))))
  (cl-assert (eq 100 (plist-get plist :value)))
  (cl-assert (eq 'r (plist-get plist :type))))

(let ((plist (cadr (assoc "content" sexp))))
  (cl-assert (eq 10 (plist-get plist :value)))
  (cl-assert (eq 'r (plist-get plist :type))))

;; We should also have the stats in-memory...
(let ((stats (elfeed-score-rule-stats-get (car elfeed-score-serde-title-rules))))
  ;; We're matching on ".*", so if there are any RSS entries, the rule
  ;; should have matched at least once.
  (cl-assert (> (elfeed-score-rule-stats-hits stats) 0)))

;; *and* have written them to disk on completion of fetch.  There is a
;; small gap between `elfeed-curl-queue-active' dropping to zero & the
;; hook being invoked, meaning that we *could* be here without the
;; stats file being on disk (and I have actually oberved
;; this). Cf. `elfeed-curl--queue-wrap'.
(let ((count 0))
  (while (and (not (file-exists-p elfeed-score-rule-stats-file))
              (< count max-count))
    (sit-for 2)
    (while (accept-process-output nil 0))
    (setq count (1+ count))
    (message "Checking for stats on disk...%s" (make-string count ?.)))
  (message
   "Checking for stats on disk...%s(%s)"
   (make-string count ?.)
   (if (eq count max-count) "FAILED" "complete")))

;; *and* have written them to disk on completion of fetch.
(setq
 tab
 (plist-get
  (car
   (read-from-string
    (with-temp-buffer
      (insert-file-contents elfeed-score-rule-stats-file)
      (buffer-string))))
  :stats))

(cl-assert (hash-table-p tab))
(cl-assert (eq 3 (hash-table-count tab)))

;; Note the mtime for the stats file (for use below)
(setq
 mtime0
 (time-as-float
  (file-attribute-modification-time
   (file-attributes elfeed-score-rule-stats-file))))

;; Check that manually-set scores are sticky
(with-current-buffer (elfeed-search-buffer)
  (elfeed-search-first-entry)
  (let ((entry (car (elfeed-search-selected))))
    (elfeed-score-set-score -1)
    (elfeed-score-score-search)
    (cl-assert (eq -1 (elfeed-score-scoring-get-score-from-entry entry)))
    (let ((elfeed-score-scoring-manual-is-sticky nil))
      (elfeed-score-score-search)
      (cl-assert (not (eq -1 (elfeed-score-scoring-get-score-from-entry entry)))))))

;; I just scored the current search, *twice*-- this should have
;; updated the scores.
(let ((stats (elfeed-score-rule-stats-get (car elfeed-score-serde-title-rules))))
  ;; We're matching on ".*", so if there are any RSS entries, the rule
  ;; should have matched at least once.
  (cl-assert (> (elfeed-score-rule-stats-hits stats) 2)))

;; Also, let's check to see if the stats file was updated. Depending
;; on how many rule hits there were, we should have some "dirty" stats
;; (i.e. in memory but not on disk) and/or the stats file should be
;; changed.

(setq
 mtime1
 (time-as-float
  (file-attribute-modification-time
   (file-attributes elfeed-score-rule-stats-file))))

(cl-assert
 (or
  (> elfeed-score-rule-stats--dirty-stats 0)
  (> mtime1 mtime0)))

;; OK-- let's make sure old rules age out of the stats
;; datastructure. Maintain the property that all these rules will
;; match at least once. This updates the :value for the "title" rule
;; from 100 => 200 & adds a udf rule.
(with-temp-file
    elfeed-score-score-file
  (insert "((version 9)
(\"title\"
  (:text \".*\" :value 200 :type r :comment \"foo!\"))
(\"content\"
 (:text \".*\" :value 10 :type r))
(\"feed\"
 (:text \"emacs-reddit\" :value 75 :type s :attr t)))
"))

(elfeed-score-load-score-file elfeed-score-score-file)
(elfeed-score-serde-cleanup-stats)

(let ((stats (elfeed-score-rule-stats-get
              (elfeed-score-title-rule--create :text ".*" :value 100 :type 'r))))
  (cl-assert (not stats)))

(elfeed-score-unload)
(cl-assert (eq 0 elfeed-score-rule-stats--dirty-stats))
(cl-assert
 (not
  (memq
   #'elfeed-score-scoring-score-entry
   elfeed-new-entry-hook)))
(cl-assert
 (not
  (memq
   #'elfeed-score-rule-stats-update-hook
   elfeed-update-hooks)))

;; Re-load `elfeed-score'...
(elfeed-score-enable)

;; and at least run the "display" functions
(elfeed-score-maint-display-rules-by-last-match)
(elfeed-score-maint-display-rules-by-match-hits)

;; Make sure the deser still contains comments:
(cl-assert
 (equal
  (elfeed-score-title-rule-comment
   (car elfeed-score-serde-title-rules))
  "foo!"))

;; OK-- now let's use the interactive rule functions to add some
;; rules, non-interactively. In this case, all defaults will be
;; respected, but the score file should be re-written & the current
;; view re-scored.
(switch-to-buffer "*elfeed-search*")
(goto-char (point-min))
(forward-line) ;; skip the manually-scored entry

;; The tests are pretty repetitive, so let's wrap 'em up in a macro.
(defmacro with-add-rule-test (&rest body)
  "Test calling an interactive add rule function programmatically with BODY.

Although these commands are of course meant to be invoked
interactively, testing them programmatically has turned out to be
pretty useful; during development these tests uncovered a number
of implementation bugs.

Invoke this macro in the *elfeed-search* buffer with point over
the Elfeed entry to be used in the test."

  (declare (indent defun))

  `(let* (
          ;; on my development machine, this test isn't reliable-- the
          ;; score file *is* re-written (I've checked by hand), but
          ;; the test runs quickly enough that the timestamps are
          ;; identical.  It would be more reliable to grab the
          ;; checksum.
          (mtime1
           (time-as-float
            (file-attribute-modification-time
             (file-attributes elfeed-score-rule-stats-file))))
          (mtime2
           (time-as-float
            (file-attribute-modification-time
             (file-attributes elfeed-score-serde-score-file))))
          (entry (elfeed-search-selected t))
          (score (elfeed-score-scoring-get-score-from-entry entry)))
     ,@body
     (cl-assert (> (elfeed-score-scoring-get-score-from-entry entry)
                   score))
     ;; See above.
     (cl-assert (> (time-as-float
                    (file-attribute-modification-time
                     (file-attributes elfeed-score-rule-stats-file)))
                   mtime1))
     (cl-assert (>= (time-as-float
                     (file-attribute-modification-time
                      (file-attributes elfeed-score-serde-score-file)))
                    mtime2))))

;; title
(with-add-rule-test
 (progn
   (elfeed-score-maint-add-title-rule 1001)
   (let ((rule (car elfeed-score-serde-title-rules)))
     (cl-assert (equal (elfeed-entry-title entry)
                       (elfeed-score-title-rule-text rule)))
     (cl-assert (eq 1001 (elfeed-score-title-rule-value rule)))
     (cl-assert (eq 's (elfeed-score-title-rule-type rule)))
     (cl-assert (not (elfeed-score-title-rule-tags rule)))
     (cl-assert (not (elfeed-score-title-rule-feeds rule))))))

;; content
(forward-line 2)
(with-add-rule-test
 (progn
   ;; match text will be the entry's content
   (elfeed-search-show-entry entry) ;; can only add content rules in show view!
   (elfeed-score-maint-add-content-rule 1002)
   (let ((rule (car elfeed-score-serde-content-rules)))
     (cl-assert (equal (elfeed-deref (elfeed-entry-content entry))
                       (elfeed-score-content-rule-text rule)))
     (cl-assert (eq 1002 (elfeed-score-content-rule-value rule)))
     (cl-assert (eq 's (elfeed-score-content-rule-type rule)))
     (cl-assert (not (elfeed-score-content-rule-tags rule)))
     (cl-assert (not (elfeed-score-content-rule-feeds rule))))
   ;; Need to get *out* of the "show" buffer...
   ;; For some reason, this doesn't take effect (tho it does interactively?)
   ;; (elfeed-show-tag--unread)
   (elfeed-kill-buffer)
   (setq elfeed-search-filter "@6-months-ago")
   (elfeed-search-update :force)
   ;; ;; and *then* score the current search.
   (elfeed-score-scoring-score-search)))

;; feed
(forward-line)
(with-add-rule-test
 (let ((feed (elfeed-feed-url (elfeed-entry-feed entry))))
   (elfeed-score-maint-add-feed-rule 1003)
    (let ((rule (car elfeed-score-serde-feed-rules)))
      (cl-assert (equal feed (elfeed-score-feed-rule-text rule)))
      (cl-assert (eq 1003 (elfeed-score-feed-rule-value rule)))
      (cl-assert (eq 's (elfeed-score-feed-rule-type rule)))
      (cl-assert (not (elfeed-score-feed-rule-tags rule))))))

;; authors
(forward-line)
(with-add-rule-test
 (let ((authors (elfeed-score-scoring--concatenate-authors
                 (elfeed-meta entry :authors))))
   (elfeed-score-maint-add-authors-rule 1004)
   (let ((rule (car elfeed-score-serde-authors-rules)))
     (cl-assert (equal authors (elfeed-score-authors-rule-text rule)))
     (cl-assert (eq 1004 (elfeed-score-authors-rule-value rule)))
     (cl-assert (eq 's (elfeed-score-authors-rule-type rule)))
     (cl-assert (not (elfeed-score-authors-rule-tags rule)))
     (cl-assert (not (elfeed-score-authors-rule-tags rule))))))

;; tag
(forward-line 3)
(elfeed-tag (elfeed-search-selected t) 'foo)
(with-add-rule-test
 (let ((tags (elfeed-entry-tags entry)))
   (elfeed-score-maint-add-tag-rule 1005)
   (let ((rule (car elfeed-score-serde-tag-rules)))
     (cl-assert (eq (elfeed-score-tag-rule-value rule) 1005))
     (cl-assert (equal (cons t tags) (elfeed-score-tag-rule-tags rule))))))

;; link
(forward-line 4)
(with-add-rule-test
 (let ((link (elfeed-entry-link entry)))
   (elfeed-score-maint-add-link-rule 1006)
   (let ((rule (car elfeed-score-serde-link-rules)))
     (cl-assert (equal link (elfeed-score-link-rule-text rule)))
     (cl-assert (eq 1006 (elfeed-score-link-rule-value rule)))
     (cl-assert 's (elfeed-score-link-rule-type rule))
     (cl-assert (not (elfeed-score-link-rule-tags rule)))
     (cl-assert (not (elfeed-score-link-rule-feeds rule))))))

;; title-or-content
(forward-line 4)
(with-add-rule-test
 (elfeed-score-maint-add-title-or-content-rule 1007 1008)
 (let ((rule (car elfeed-score-serde-title-or-content-rules)))
   (cl-assert (equal (elfeed-entry-title entry)
                     (elfeed-score-title-or-content-rule-text rule)))
   (cl-assert (eq 1007 (elfeed-score-title-or-content-rule-title-value rule)))
   (cl-assert (eq 1008 (elfeed-score-title-or-content-rule-content-value rule)))
   (cl-assert 's (elfeed-score-title-or-content-rule-type rule))
   (cl-assert (not (elfeed-score-title-or-content-rule-tags rule)))
   (cl-assert (not (elfeed-score-title-or-content-rule-feeds rule)))))

;; negative test-- try adding a rule with a dirty score file:
(with-temp-file
    elfeed-score-score-file
  (insert "((version 9)
(\"title\"
  (:text \".*\" :value 200 :type r))
(\"content\"
 (:text \".*\" :value 10 :type r :comment \"content rule!\"))
(\"feed\"
 (:text \"emacs-reddit\" :value 75 :type s :attr t)))
"))

(forward-line 5)
(let ((errored-out nil))
  (condition-case err
      (elfeed-score-maint-add-title-rule 1009)
    (error
     (setq errored-out t)))
  (cl-assert errored-out))

(with-temp-file
    elfeed-score-score-file
  (insert "((version 10)
(\"title\"
  (:text \".*\" :value 200 :type r :comment \"foo!\"))
(\"content\"
 (:text \".*\" :value 10 :type r))
(\"feed\"
 (:text \"emacs-reddit\" :value 75 :type s :attr t))
(\"udf\"
 (:function (lambda (x) (unless (cl-typep x 'elfeed-entry) (error \"bad type!\")) 1))))
"))

;; Re-load just to get back to a known-good state.
(elfeed-score-load-score-file elfeed-score-score-file)
(elfeed-score-score-search)
