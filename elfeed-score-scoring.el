;;; elfeed-score-scoring.el --- Logic for scoring (and explaining) `elfeed' entries  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Michael Herstine <sp1ff@pobox.com>

;; Package-Requires: ((emacs "24.4") (elfeed "3.3.0"))
;; Version: 0.7.2
;; URL: https://github.com/sp1ff/elfeed-score

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

;; Scoring logic for `elfeed-score'.

;;; Code:

(require 'elfeed-search)
(require 'elfeed-score-log)
(require 'elfeed-score-rules)
(require 'elfeed-score-serde)

(define-obsolete-variable-alias 'elfeed-score/default-score
  'elfeed-score-default-score "0.2.0" "Move to standard-compliant naming.")

(define-obsolete-variable-alias 'elfeed-score-default-score
  'elfeed-score-scoring-default-score "0.7.0" "Re-factoring elfeed-score.el.")

(defcustom elfeed-score-scoring-default-score 0
  "Default score for an Elfeed entry."
  :group 'elfeed-score
  :type 'int)

(define-obsolete-variable-alias 'elfeed-score/meta-kw
  'elfeed-score-meta-keyword "0.2.0" "Move to standard-compliant naming.")

(define-obsolete-variable-alias 'elfeed-score-meta-keyword
  'elfeed-score-scoring-meta-keyword "0.7.0" "Re-factoring elfeed-score.el.")

(defcustom elfeed-score-scoring-meta-keyword :elfeed-score/score
  "Default keyword for storing scores in Elfeed entry metadata."
  :group 'elfeed-score
  :type 'symbol)

(define-obsolete-variable-alias 'elfeed-score-explanation-buffer-name
  'elfeed-score-scoring-explanation-buffer-name "0.7.0"
  "Re-factoring elfeed-score.el.")

(defcustom elfeed-score-scoring-explanation-buffer-name
  "*elfeed-score-explanations*"
  "Name of the buffer to be used for scoring explanations."
  :group 'elfeed-score
  :type 'string)

(defun elfeed-score-scoring-set-score-on-entry (entry score)
  "Set the score on ENTRY to SCORE."
  (setf (elfeed-meta entry elfeed-score-meta-keyword) score))

(defun elfeed-score-scoring-get-score-from-entry (entry)
  "Retrieve the score from ENTRY."
  (elfeed-meta entry elfeed-score-meta-keyword elfeed-score-default-score))

(defun elfeed-score-scoring--match-text (match-text search-text match-type)
  "Test SEARCH-TEXT against MATCH-TEXT according to MATCH-TYPE.
Return nil on failure, the matched text on match."
  (cond
   ((or (eq match-type 's)
        (eq match-type 'S))
    (let ((case-fold-search (eq match-type 's)))
      (if (string-match (regexp-quote match-text) search-text)
          (match-string 0 search-text)
        nil)))
   ((or (eq match-type 'r)
        (eq match-type 'R)
        (not match-type))
    (let ((case-fold-search (eq match-type 'r)))
      (if (string-match match-text search-text)
          (match-string 0 search-text)
        nil)))
   ((or (eq match-type 'w)
        (eq match-type 'W))
    (let ((case-fold-search (eq match-type 'w)))
      (if  (string-match (word-search-regexp match-text) search-text)
          (match-string 0 search-text))))
   (t
    (error "Unknown match type %s" match-type))))

(defun elfeed-score-scoring--match-tags (entry-tags tag-rule)
  "Test a ENTRY-TAGS against TAG-RULE.

ENTRY-TAGS shall be a list of symbols, presumably the tags applied to the Elfeed
entry being scored.  TAG-RULE shall be a list of the form (boolean . (symbol...))
or nil, and is presumably a tag scoping for a scoring rule."

  (if tag-rule
      (let ((flag (car tag-rule))
            (rule-tags (cdr tag-rule))
            (apply nil))
        ;; Special case allowing this method to be called like (... (t . symbol))
        (if (symbolp rule-tags)
            (setq rule-tags (list rule-tags)))
        (while (and rule-tags (not apply))
          (if (memq (car rule-tags) entry-tags)
              (setq apply t))
          (setq rule-tags (cdr rule-tags)))
        (if flag
            apply
          (not apply)))
    t))

(defun elfeed-score-scoring--get-feed-attr (feed attr)
  "Retrieve attribute ATTR from FEED."
  (cond
   ((eq attr 't) (elfeed-feed-title feed))
   ((eq attr 'u) (elfeed-feed-url feed))
   ((eq attr 'a) (elfeed-feed-author feed))
   (t (error "Unknown feed attribute %s" attr))))

(defun elfeed-score-scoring--match-feeds (entry-feed feed-rule)
  "Test ENTRY-FEED against FEED-RULE.

ENTRY-FEED shall be an <elfeed-feed> instance.  FEED-RULE shall
be a list of the form (BOOLEAN (ATTR TYPE TEXT)...), or nil, and
is presumably the feed scoping for a scoring rule."

  (if feed-rule
      (let ((flag (car feed-rule))
            (rule-feeds (cdr feed-rule))
            (match))
        ;; Special case allowing this method to be called like (... (t 't 's "title))
        (if (symbolp (car rule-feeds))
            (setq rule-feeds (list rule-feeds)))
        (while (and rule-feeds (not match))
          (let* ((feed (car rule-feeds))
                 (attr (nth 0 feed))
                 (match-type (nth 1 feed))
                 (match-text (nth 2 feed))
                 (feed-text (elfeed-score-scoring--get-feed-attr entry-feed attr)))
            (if (elfeed-score-scoring--match-text match-text feed-text match-type)
                (setq match t)))
          (setq rule-feeds (cdr rule-feeds)))
        (if flag match (not match)))
    t))

(defun elfeed-score-scoring--apply-title-rules (entry on-match)
  "Apply all title scoring rules to ENTRY; call ON-MATCH for each match.

ON-MATCH will be invoked with the matching rule & the matchted text."
  (let ((title (elfeed-entry-title entry)))
    (dolist (rule elfeed-score-serde-title-rules)
      (let* ((match-text   (elfeed-score-title-rule-text  rule))
		         (match-type   (elfeed-score-title-rule-type  rule))
             (tag-rule     (elfeed-score-title-rule-tags  rule))
             (feed-rule    (elfeed-score-title-rule-feeds rule))
             (matched-text (and
                            (elfeed-score-scoring--match-tags (elfeed-entry-tags entry) tag-rule)
                            (elfeed-score-scoring--match-feeds (elfeed-entry-feed entry) feed-rule)
                            (elfeed-score-scoring--match-text match-text title match-type))))
        (if matched-text (funcall on-match rule matched-text))))))

(defun elfeed-score-scoring--explain-title (entry)
  "Apply all title scoring rules to ENTRY; return an explanation.

The explanation will be a list of two-tuples (i.e. a list with
two elements), one for each matching rule.  The first element of
each two-tuple will be the matching rule & the second the
matched text."
  (let ((hits '()))
    (elfeed-score-scoring--apply-title-rules
     entry
     (lambda (rule matched-text)
       (setq hits (cons (elfeed-score-make-title-explanation :matched-text matched-text :rule rule) hits))))
    hits))

(defun elfeed-score-scoring--score-on-title (entry)
  "Run all title scoring rules against ENTRY; return the summed values."
  (let ((score 0))
    (elfeed-score-scoring--apply-title-rules
     entry
     (lambda (rule matched-text)
       (let* ((value (elfeed-score-title-rule-value rule)))
         (elfeed-score-log
          'debug
          "title rule '%s' matched text '%s' for entry %s('%s'); adding %d to
its score"
          (elfeed-score-rules-pp-rule-to-string rule)
          matched-text
          (elfeed-entry-id entry)
          (elfeed-entry-title entry) value)
		     (setq score (+ score value))
         (setf (elfeed-score-title-rule-date rule) (float-time))
         (setf (elfeed-score-title-rule-hits rule)
               (1+ (elfeed-score-title-rule-hits rule))))))
    score))

(defun elfeed-score-scoring--concatenate-authors (authors-list)
  "Given AUTHORS-LIST, list of plists; return string of all authors concatenated."
  (mapconcat (lambda (author) (plist-get author :name)) authors-list ", "))

(defun elfeed-score-scoring--apply-authors-rules (entry on-match)
  "Apply all authors rules to ENTRY; invoke ON-MATCH for each match.

ON-MATCH will be invoked with the matching rule & the matched text."
  (let ((authors-string
         (elfeed-score-scoring--concatenate-authors
          (elfeed-meta entry :authors))))
    (dolist (rule elfeed-score-serde-authors-rules)
      (let* ((match-text   (elfeed-score-authors-rule-text  rule))
		         (match-type   (elfeed-score-authors-rule-type  rule))
             (tag-rule     (elfeed-score-authors-rule-tags  rule))
             (feed-rule    (elfeed-score-authors-rule-feeds rule))
             (matched-text (and
                            (elfeed-score-scoring--match-tags (elfeed-entry-tags entry) tag-rule)
                            (elfeed-score-scoring--match-feeds (elfeed-entry-feed entry) feed-rule)
                            (elfeed-score-scoring--match-text match-text authors-string match-type))))
        (if matched-text (funcall on-match rule matched-text))))))

(defun elfeed-score-scoring--explain-authors (entry)
  "Apply the authors rules to ENTRY; return an explanation.

The explanation will be a list of two-tuples (i.e. a list with
two elements), one for each rule.  The first element of each
two-tuple will be the rule & the second the matched text."
  (let ((hits '()))
    (elfeed-score-scoring--apply-authors-rules
     entry
     (lambda (rule match-text)
       (setq hits (cons (elfeed-score-make-authors-explanation :matched-text match-text :rule rule) hits))))
    hits))

(defun elfeed-score-scoring--score-on-authors (entry)
  "Run all title scoring rules against ENTRY; return the summed values."
  (let ((score 0))
    (elfeed-score-scoring--apply-authors-rules
     entry
     (lambda (rule match-text)
       (let ((value (elfeed-score-authors-rule-value rule)))
         (elfeed-score-log
          'debug
          "title rule '%s' matched text '%s' for entry %s('%s'); \
adding %d to its score"
          (elfeed-score-rules-pp-rule-to-string rule)
          match-text
          (elfeed-entry-id entry)
          (elfeed-entry-title entry)
          value)
		     (setq score (+ score value))
         (setf (elfeed-score-authors-rule-date rule) (float-time))
         (setf (elfeed-score-authors-rule-hits rule)
               (1+ (elfeed-score-authors-rule-hits rule))))))
    score))

(defun elfeed-score-scoring--apply-feed-rules (entry on-match)
  "Run all feed rules against ENTRY; invoke ON-MATCH for each match.

ON-MATCH will be invoked with the applicable rule as well as the matched text."
  (let ((feed (elfeed-entry-feed  entry)))
    (dolist (rule elfeed-score-serde-feed-rules)
      (let* ((match-text   (elfeed-score-feed-rule-text rule))
		         (match-type   (elfeed-score-feed-rule-type rule))
             (attr         (elfeed-score-feed-rule-attr rule))
             (feed-text    (elfeed-score-scoring--get-feed-attr feed attr))
             (tag-rule     (elfeed-score-feed-rule-tags rule))
             (matched-text (and
                            (elfeed-score-scoring--match-tags (elfeed-entry-tags entry) tag-rule)
                            (elfeed-score-scoring--match-text match-text feed-text match-type))))
        (if matched-text (funcall on-match rule matched-text))))))

(defun elfeed-score-scoring--explain-feed (entry)
  "Apply the feed scoring rules to ENTRY, return an explanation.

The explanation will be a list of two-tuples (i.e. a list with
two elements), one for each rule that matches.  The first element
will be the rule that matched & the second the matched text."
  (let ((hits '()))
    (elfeed-score-scoring--apply-feed-rules
     entry
     (lambda (rule match-text)
       (setq hits (cons (elfeed-score-make-feed-explanation :matched-text match-text :rule rule) hits))))
    hits))

(defun elfeed-score-scoring--score-on-feed (entry)
  "Run all feed scoring rules against ENTRY; return the summed values."
  (let ((score 0))
    (elfeed-score-scoring--apply-feed-rules
     entry
     (lambda (rule match-text)
       (let ((value (elfeed-score-feed-rule-value rule)))
         (elfeed-score-log
          'debug
          "feed rule '%s' matched text '%s' for entry %s('%s'); \
adding %d to its score"
          (elfeed-score-rules-pp-rule-to-string rule)
          match-text
          (elfeed-entry-id entry)
          (elfeed-entry-title entry) value)
		     (setq score (+ score value))
		     (setf (elfeed-score-feed-rule-date rule) (float-time))
         (setf (elfeed-score-feed-rule-hits rule)
               (1+ (elfeed-score-feed-rule-hits rule))))))
    score))

(defun elfeed-score-scoring--apply-content-rules (entry on-match)
  "Apply the content scoring rules to ENTRY; invoke ON-MATCH for each match.

ON-MATCH will be invoked with each matching rule and the ENTRY
text on which it matched."
  (let ((content (elfeed-deref (elfeed-entry-content entry))))
    (if content
        (dolist (rule elfeed-score-serde-content-rules)
          (let* ((match-text    (elfeed-score-content-rule-text  rule))
		             (match-type    (elfeed-score-content-rule-type  rule))
                 (tag-rule      (elfeed-score-content-rule-tags  rule))
                 (feed-rule     (elfeed-score-content-rule-feeds rule))
                 (matched-text  (and
                                 (elfeed-score-scoring--match-tags (elfeed-entry-tags entry) tag-rule)
                                 (elfeed-score-scoring--match-feeds (elfeed-entry-feed entry) feed-rule)
                                 (elfeed-score-scoring--match-text match-text content match-type))))
            (if matched-text (funcall on-match rule matched-text)))))))

(defun elfeed-score-scoring--explain-content (entry)
  "Apply the content scoring rules to ENTRY, return an explanation.

The explanation is a list of two-tuples (i.e. a list with two elements); the
first member of each list element will be the rule that matched & the second
the matched text."

  (let ((hits '()))
    (elfeed-score-scoring--apply-content-rules
     entry
     (lambda (rule match-text)
       (setq hits (cons (elfeed-score-make-content-explanation :matched-text match-text :rule rule) hits))))
    hits))

(defun elfeed-score-scoring--score-on-content (entry)
  "Run all content scoring rules against ENTRY; return the summed values."
  (let ((score 0))
    (elfeed-score-scoring--apply-content-rules
     entry
     (lambda (rule match-text)
       (let ((value (elfeed-score-content-rule-value rule)))
         (elfeed-score-log
          'debug "content rule '%s' matched text '%s' for entry %s('%s'); \
adding %d to its score"
          (elfeed-score-rules-pp-rule-to-string rule)
          match-text
          (elfeed-entry-id entry)
          (elfeed-entry-title entry)
          value)
		     (setq score (+ score value))
		     (setf (elfeed-score-content-rule-date rule)
               (float-time))
         (setf (elfeed-score-content-rule-hits rule)
               (1+ (elfeed-score-content-rule-hits rule))))))
    score))

(defun elfeed-score-scoring--apply-title-or-content-rules (entry on-match)
  "Apply the title-or-content rules to ENTRY; invoke ON-MATCH for each match.

ON-MATCH will be invoked with the matching rule, the matched
text, and a boolean value indicating whether this is a title
match (t) or a content match (nil)."

  (let ((title (elfeed-entry-title entry))
        (content (elfeed-deref (elfeed-entry-content entry))))
    (dolist (rule elfeed-score-serde-title-or-content-rules)
      (let* ((match-text      (elfeed-score-title-or-content-rule-text        rule))
		         (match-type      (elfeed-score-title-or-content-rule-type        rule))
             (tag-rule        (elfeed-score-title-or-content-rule-tags        rule))
             (feed-rule       (elfeed-score-title-or-content-rule-feeds       rule))
             (matched-tags    (elfeed-score-scoring--match-tags  (elfeed-entry-tags entry) tag-rule))
             (matched-feeds   (elfeed-score-scoring--match-feeds (elfeed-entry-feed entry) feed-rule))
             (matched-title
              (and
               matched-tags
               matched-feeds
               (elfeed-score-scoring--match-text match-text title match-type)))
             (matched-content
              (and
               content
               matched-tags
               matched-feeds
               (elfeed-score-scoring--match-text match-text content match-type)))
             (got-title-match (and matched-tags matched-feeds matched-title))
             (got-content-match (and content matched-tags matched-feeds matched-content)))
        (if got-title-match (funcall on-match rule matched-title t))
        (if got-content-match (funcall on-match rule matched-content nil))))))

(defun elfeed-score-scoring--explain-title-or-content (entry)
  "Apply the title-or-content scoring rules to ENTRY, return an explanation.

The explanation is a list of three-tuples: rule, matched text, t
for a title match & nil for a content match."
  (let ((hits '()))
    (elfeed-score-scoring--apply-title-or-content-rules
     entry
     (lambda (rule match-text title-match)
       (setq
        hits
        (cons
         (elfeed-score-make-title-or-content-explanation
          :matched-text match-text :rule rule :attr (if title-match 't 'c))
         hits))))
    hits))

(defun elfeed-score-scoring--score-on-title-or-content (entry)
  "Run all title-or-content rules against ENTRY; return the summed values."
  (let ((score 0))
    (elfeed-score-scoring--apply-title-or-content-rules
     entry
     (lambda (rule match-text title-match)
       (if title-match
           (let ((value (elfeed-score-title-or-content-rule-title-value rule)))
             (elfeed-score-log 'debug "title-or-content rule '%s' matched text\
 '%s' in the title of entry '%s'; adding %d to its score"
                               (elfeed-score-rules-pp-rule-to-string rule)
                               match-text (elfeed-entry-id entry) value)
		         (setq score (+ score value))
             (setf (elfeed-score-title-or-content-rule-date rule)
                   (float-time))
             (setf (elfeed-score-title-or-content-rule-hits rule)
                   (1+ (elfeed-score-title-or-content-rule-hits rule))))
         (let ((value (elfeed-score-title-or-content-rule-content-value rule)))
           (elfeed-score-log 'debug "title-or-content rule '%s' matched text\
 '%s' in the content of entry '%s'; adding %d to its score"
                             (elfeed-score-rules-pp-rule-to-string rule)
                             match-text (elfeed-entry-id entry)
                             value)
		       (setq score (+ score value))
		       (setf (elfeed-score-title-or-content-rule-date rule)
                 (float-time))
           (setf (elfeed-score-title-or-content-rule-hits rule)
                 (1+ (elfeed-score-title-or-content-rule-hits rule)))))))
    score))

(defun elfeed-score-scoring--apply-tag-rules (entry on-match)
  "Apply the tag scoring rules to ENTRY; invoke ON-MATCH for each match.

On match, ON-MATCH will be called with the matching rule."
  (let ((tags (elfeed-entry-tags entry)))
    (dolist (rule elfeed-score-serde-tag-rules)
      (let* ((rule-tags  (elfeed-score-tag-rule-tags rule))
             (got-match  (elfeed-score-scoring--match-tags tags rule-tags)))
        (if got-match (funcall on-match rule))))))

(defun elfeed-score-scoring--explain-tags (entry)
  "Record with tags rules match ENTRY.  Return a list of the rules that matched."
  (let ((hits '()))
    (elfeed-score-scoring--apply-tag-rules
     entry
     (lambda (rule)
       (setq hits (cons (elfeed-score-make-tags-explanation :rule rule) hits))))
    hits))

(defun elfeed-score-scoring--score-on-tags (entry)
  "Run all tag scoring rules against ENTRY; return the summed value."

  (let ((score 0))
    (elfeed-score-scoring--apply-tag-rules
     entry
     (lambda (rule)
       (let ((rule-value (elfeed-score-tag-rule-value rule)))
         (elfeed-score-log
          'debug "tag rule '%s' matched entry %s('%s'); adding %d to its score"
          (elfeed-score-rules-pp-rule-to-string rule)
          (elfeed-entry-id entry)
          (elfeed-entry-title entry)
          rule-value)
         (setq score (+ score rule-value))
         (setf (elfeed-score-tag-rule-date rule) (float-time))
         (setf (elfeed-score-tag-rule-hits rule)
               (1+ (elfeed-score-tag-rule-hits rule))))))
    score))

(defun elfeed-score-scoring--adjust-tags (entry score)
  "Run all tag adjustment rules against ENTRY for score SCORE."
  (dolist (adj-tags elfeed-score-serde-adjust-tags-rules)
    (let* ((thresh           (elfeed-score-adjust-tags-rule-threshold adj-tags))
           (threshold-switch (car thresh))
           (threshold-value  (cdr thresh)))
      (if (or (and threshold-switch )
              (and (not threshold-switch) (<= score threshold-value)))
          (let* ((rule-tags   (elfeed-score-adjust-tags-rule-tags adj-tags))
                 (rule-switch (car rule-tags))
                 (actual-tags (cdr rule-tags))) ;; may be a single tag or a list!
            (if rule-switch
                (progn
                  ;; add `actual-tags'...
                  (elfeed-score-log 'debug "Tag adjustment rule %s matched score %d for entry \
%s(%s); adding tag(s) %s"
                                    rule-tags score (elfeed-entry-id entry)
                                    (elfeed-entry-title entry) actual-tags)
                  (apply #'elfeed-tag entry actual-tags)
                  (setf (elfeed-score-adjust-tags-rule-date adj-tags) (float-time))
                  (setf (elfeed-score-adjust-tags-rule-hits adj-tags)
                        (1+ (elfeed-score-adjust-tags-rule-hits adj-tags))))
              (progn
                ;; else rm `actual-tags'
                (elfeed-score-log 'debug "Tag adjustment rule %s matched score %d for entry \
%s(%s); removing tag(s) %s"
                                  rule-tags score (elfeed-entry-id entry)
                                  (elfeed-entry-title entry) actual-tags)
                (apply #'elfeed-untag entry actual-tags)
                (setf (elfeed-score-adjust-tags-rule-date adj-tags) (float-time))
                (setf (elfeed-score-adjust-tags-rule-hits adj-tags)
                      (1+ (elfeed-score-adjust-tags-rule-hits adj-tags))))))))))

(defun elfeed-score-scoring-score-entry (entry)
  "Score an Elfeed ENTRY.

This function will return the entry's score, update it's meta-data, and
update the \"last matched\" time of the salient rules."

  (let ((score (+ elfeed-score-default-score
                  (elfeed-score-scoring--score-on-title            entry)
                  (elfeed-score-scoring--score-on-feed             entry)
                  (elfeed-score-scoring--score-on-content          entry)
                  (elfeed-score-scoring--score-on-title-or-content entry)
                  (elfeed-score-scoring--score-on-authors          entry)
                  (elfeed-score-scoring--score-on-tags             entry))))
    (elfeed-score-scoring-set-score-on-entry entry score)
    (elfeed-score-scoring--adjust-tags entry score)
	  (if (and elfeed-score-serde-score-mark
		         (< score elfeed-score-serde-score-mark))
	      (elfeed-untag entry 'unread))
    score))

(defun elfeed-score-scoring--pp-rule-match-to-string (match)
  "Pretty-print a rule explanation MATCH & return the resulting string."

  (cl-typecase match
    (elfeed-score-title-explanation
     (elfeed-score-rules-pp-title-explanation match))
    (elfeed-score-feed-explanation
     (elfeed-score-rules-pp-feed-explanation match))
    (elfeed-score-content-explanation
     (elfeed-score-rules-pp-content-explanation match))
    (elfeed-score-title-or-content-explanation
     (elfeed-score-rules-pp-title-or-content-explanation match))
    (elfeed-score-authors-explanation
     (elfeed-score-rules-pp-authors-explanation match))
    (elfeed-score-tags-explanation
     (elfeed-score-rules-pp-tags-explanation match))
    (t
     (error "Don't know how to pretty-print %S" match))))

(defun elfeed-score-scoring--get-match-contribution (match)
  "Retrieve the score contribution for MATCH."

  (cl-typecase match
    (elfeed-score-title-explanation
     (elfeed-score-rules-title-explanation-contrib match))
    (elfeed-score-feed-explanation
     (elfeed-score-rules-feed-explanation-contrib match))
    (elfeed-score-content-explanation
     (elfeed-score-rules-content-explanation-contrib match))
    (elfeed-score-title-or-content-explanation
     (elfeed-score-rules-title-or-content-explanation-contrib match))
    (elfeed-score-authors-explanation
     (elfeed-score-rules-authors-explanation-contrib match))
    (elfeed-score-tags-explanation
     (elfeed-score-rules-tags-explanation-contrib match))
    (t
     (error "Don't know how to evaluate %S" match))))

(define-obsolete-function-alias
  'elfeed-score-explain-entry
  #'elfeed-score-scoring-explain-entry
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-scoring-explain-entry (entry)
  "Explain an Elfeed ENTRY.

This function will apply all scoring rules to an entry, but will
not change anything (e.g.  update ENTRY's meta-data, or the
last-matched timestamp in the matching rules); instead, it will
provide a human-readable description of what would happen if
ENTRY were to be scored, presumably for purposes of debugging or
understanding of scoring rules."

  ;; Generate the list of matching rules
  (let* ((matches
          (append
           (elfeed-score-scoring--explain-title            entry)
           (elfeed-score-scoring--explain-feed             entry)
           (elfeed-score-scoring--explain-content          entry)
           (elfeed-score-scoring--explain-title-or-content entry)
           (elfeed-score-scoring--explain-authors          entry)
           (elfeed-score-scoring--explain-tags             entry)))
         (candidate-score
          (cl-reduce
           '+
           matches
           :key #'elfeed-score-scoring--get-match-contribution
           :initial-value elfeed-score-default-score)))
    (with-current-buffer-window
        elfeed-score-scoring-explanation-buffer-name
        nil nil
      (goto-char (point-max))
      (insert (format "\"%s\" matches %d rules"(elfeed-entry-title entry) (length matches)))
      (if (> (length matches) 0)
          (progn
            (insert (format " for a score of %d:\n" candidate-score))
            (cl-dolist (match matches)
              (insert
               (format
                "%s\n"
                (elfeed-score-scoring--pp-rule-match-to-string match)))))))))

(provide 'elfeed-score-scoring)
;;; elfeed-score-scoring.el ends here
