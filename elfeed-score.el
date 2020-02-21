;;; elfeed-score.el --- Gnus-style scoring for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "24.1") (elfeed "3.3.0") (cl-lib "0.6.1"))
;; Keywords: news
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

;; `elfeed-score' is an add-on for `elfeed', an RSS reader for
;; Emacs.  It brings Gnus-style scoring to your RSS feeds.  Elfeed, by
;; default, displays feed entries by date.  This package allows you to
;; setup rules for assigning numeric scores to entries, and sorting
;; entries with higher scores ahead of those with lower, regardless of
;; date.  The idea is to prioritize content important to you.

;; After installing this file, enable scoring by invoking
;; `elfeed-score-enable'.  This will setup the Elfeed new entry hook,
;; the Elfeed sort function, and load the score file (if it exists).
;; Turn off scoring by invoking `elfeed-score-unload'.

;;; Code:

(require 'elfeed-search)

(defconst elfeed-score-version "0.3.0")

(defgroup elfeed-score nil
  "Gnus-sytle scoring for Elfeed entries."
  :group 'comm)

(define-obsolete-variable-alias 'elfeed-score/default-score
  'elfeed-score-default-score "0.2.0" "Move to standard-compliant naming.")

(defcustom elfeed-score-default-score 0
  "Default score for an Elfeed entry."
  :group 'elfeed-score
  :type 'int)

(define-obsolete-variable-alias 'elfeed-score/meta-kw
  'elfeed-score-meta-keyword "0.2.0" "Move to standard-compliant naming.")

(defcustom elfeed-score-meta-keyword :elfeed-score/score
  "Default keyword for storing scores in Elfeed entry metadata."
  :group 'elfeed-score
  :type 'symbol)

(define-obsolete-variable-alias 'elfeed-score/score-file
  'elfeed-score-score-file "0.2.0" "Move to standard-compliant naming.")

(defcustom elfeed-score-score-file
  (concat (expand-file-name user-emacs-directory) "elfeed.score")
  "Location at which to persist scoring rules.

Set this to nil to disable automatic serialization &
de-serialization of scoring rules."
  :group 'elfeed-score
  :type 'file)

(defcustom elfeed-score-score-format '("%d " 6 :right)
  "Format for scores when displayed in the Elfeed search buffer.
This is a three-tuple: the `format' format string, target width,
and alignment.  This should be (string integer keyword)
for (format width alignment).  Possible alignments are :left and
:right."
  :group 'elfeed-score
  :type '(list string integer (choice (const :left) (const :right))))

(define-obsolete-variable-alias 'elfeed-score/debug
  'elfeed-score-debug "0.2.0" "Move to standard-compliant naming.")

(defvar elfeed-score-debug nil
  "Control debug output.

Setting this to a non-nil value will produce copious debugging
information to the \"*Messages*\" buffer.")

(defun elfeed-score--debug (fmt &rest params)
  "Produce a formattted (FMT) message based on PARAMS at debug level.

Print a formatted message if `elfeed-score-debug' is non-nil."
  (if elfeed-score-debug (apply 'message fmt params)))

(define-obsolete-function-alias 'elfeed-score/sort
  'elfeed-score-sort "0.2.0" "Move to standard-compliant naming.")

(defun elfeed-score-sort (a b)
  "Return non-nil if A should sort before B.

`elfeed-score' will substitute this for the Elfeed scoring function."

  (let ((a-score (elfeed-meta a elfeed-score-meta-keyword
                              elfeed-score-default-score))
        (b-score (elfeed-meta b elfeed-score-meta-keyword
                              elfeed-score-default-score)))
    (if (> a-score b-score)
        t
      (let ((a-date  (elfeed-entry-date a))
            (b-date  (elfeed-entry-date b)))
        (and (eq a-score b-score) (> a-date b-date))))))

(define-obsolete-function-alias 'elfeed-score/set-score
  'elfeed-score-set-score "0.2.0" "Move to standard-compliant naming.")

(defun elfeed-score-set-score (&optional score ignore-region)
  "Set the score of one or more Elfeed entries to SCORE.

Their scores will be set to `elfeed-score-default-score' by
default.

If IGNORE-REGION is nil (as it will be when called
interactively), then all entries in the current region will have
their scores re-set.  If the region is not active, then only the
entry under point will be affected.  If IGNORE-REGION is t, then
only the entry under point will be affected, regardless of the
region's state."

  (interactive "P")

  (let ((score
         (if score
             (prefix-numeric-value score)
           elfeed-score-default-score))
        (entries (elfeed-search-selected ignore-region)))
    (dolist (entry entries)
      (elfeed-score--debug "entry '%s' => %d" (elfeed-entry-title entry) score)
      (setf (elfeed-meta entry elfeed-score-meta-keyword) score))))

(define-obsolete-function-alias 'elfeed-score/get-score
  'elfeed-score-get-score "0.2.0" "Move to standard-compliant naming.")

(defun elfeed-score-get-score ()
  "Return the score of the entry under point.

If called intractively, print a message."

  (interactive)

  (let* ((entry (elfeed-search-selected t))
         (score (elfeed-meta entry elfeed-score-meta-keyword
                             elfeed-score-default-score)))
    (if (called-interactively-p 'any)
        (message "%s has a score of %d." (elfeed-entry-title entry) score))
    score))

(defun elfeed-score-format-score (score)
  "Format SCORE for printing in `elfeed-search-mode'.

The customization `elfeed-score-score-format' sets the
formatting.  This implementation is based on that of
`elfeed-search-format-date'."
  (cl-destructuring-bind (format target alignment) elfeed-score-score-format
    (let* ((string (format format score))
           (width (string-width string)))
      (cond
       ((> width target)
        (if (eq alignment :left)
            (substring string 0 target)
          (substring string (- width target) width)))
       ((< width target)
        (let ((pad (make-string (- target width) ?\s)))
          (if (eq alignment :left)
              (concat string pad)
            (concat pad string))))
       (string)))))

(cl-defstruct (elfeed-score-title-rule
               (:constructor nil)
               (:constructor elfeed-score-title-rule--create))
  "Rule for scoring against entry titles.

    - text :: The rule's match text; either a string or a regular
              expression (on which more below)
    - value :: Integral value (positive or negative) to be added to
               an entry's score if this rule matches
    - type :: (optional) One of the symbols s S r R; s/r denotes
              substring/regexp match; lower-case means case-insensitive
              and upper case sensitive.  Defaults to r (case-insensitive
              regexp match)
    - date :: time (in seconds since epoch) when this rule last matched
    - tags :: cons cell of the form (a . b) where A is either #t or #f and
              B is a list of symbols. The latter is interpreted as a list
              of tags scoping the rule and the former as a bolean switch
              possibly negating the scoping. E.g. (#t . (a b)) means \"apply
              this rule if either of tags a & b are present\". Making the
              first element means \"do not apply this rule if any of a and b
              are present\"."
  text value type date tags)

(cl-defstruct (elfeed-score-feed-rule
               (:constructor nil)
               (:constructor elfeed-score-feed-rule--create))
  "Rule for scoring against entry feeds.

    - :text :: The rule's match text; either astring or a regular
               expression (on which more below)
    - :value :: Integral value (positive or negative) to be added to
                an entry's score if this rule matches
    - :type :: (optional) One of the symbols s S r R; s/r denotes
               substring/regexp match; lower-case means case-insensitive
               and upper case sensitive.  Defaults to r (case-insensitive
               regexp match)
    - :attr :: Defines the feed attribute against which matching shall be
               performed: 't for title & 'u for URL.
    - :date :: time (in seconds since epoch) when this rule last matched
    - :tags :: cons cell of the form (a . b) where A is either #t or #f and
               B is a list of symbols. The latter is interpreted as a list
               of tags scoping the rule and the former as a bolean switch
               possibly negating the scoping. E.g. (#t . (a b)) means \"apply
               this rule if either of tags a & b are present\". Making the
               first element means \"do not apply this rule if any of a and b
               are present\"."
  text value type attr date tags)

(cl-defstruct (elfeed-score-content-rule
               (:constructor nil)
               (:constructor elfeed-score-content-rule--create))
  "Rule for scoring against entry content

    - :text :: The rule's match text; either a string or a regular
               expression (on which more below)
    - :value :: Integral value (positive or negative) to be added to
                an entry's score if this rule matches
    - :type :: (optional) One of the symbols s S r R; s/r denotes
               substring/regexp match; lower-case means case-insensitive
               and upper case sensitive.  Defaults to r (case-insensitive
               regexp match)
    - :date :: time (in seconds since epoch) when this rule last matched
    - tags :: cons cell of the form (a . b) where A is either #t or #f and
              B is a list of symbols. The latter is interpreted as a list
              of tags scoping the rule and the former as a bolean switch
              possibly negating the scoping. E.g. (#t . (a b)) means \"apply
              this rule if either of tags a & b are present\". Making the
              first element means \"do not apply this rule if any of a and b
              are present\"."
  text value type date tags)

(cl-defstruct (elfeed-score-title-or-content-rule
               (:constructor nil)
               (:constructor elfeed-score-title-or-content-rule--create))
  "Rule for scoring the same text against both entry title & content.

I found myself replicating the same rule for both title &
content, with a higher score for title.  This rule permits
defining a single rule for both.

    - :text :: The rule's match text; either a string or a
               regular expression (on which more below)
    - :title-value :: Integral value (positive or negative) to be
                      added to an entry's score should this rule match the
                      entry's title
    - :content-value :: Integral value (positive or negative) to be
                      added to an entry's score should this rule match the
                      entry's value
    - :type :: (optional) One of the symbols s S r R; s/r denotes
               substring/regexp match; lower-case means case-insensitive
               and upper case sensitive.  Defaults to r (case-insensitive
               regexp match)
    - :date :: time (in seconds since epoch) when this rule last matched
    - tags :: cons cell of the form (a . b) where A is either #t or #f and
              B is a list of symbols. The latter is interpreted as a list
              of tags scoping the rule and the former as a bolean switch
              possibly negating the scoping. E.g. (#t . (a b)) means \"apply
              this rule if either of tags a & b are present\". Making the
              first element means \"do not apply this rule if any of a and b
              are present\"."
  text title-value content-value type date tags)

(defun elfeed-score--parse-title-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of title rules.

Each sub-list shall have the form '(TEXT VALUE TYPE DATE)."
  (let (title-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-title-rule--create
                     :text  (nth 0 item)
                     :value (nth 1 item)
                     :type  (nth 2 item)
                     :date  (nth 3 item)
                     :tags  (nth 4 item))))
        (unless (member struct title-rules)
          (setq title-rules (append title-rules (list struct))))))
    title-rules))

(defun elfeed-score--parse-content-rule-sexps (sexps)
  "Parse a lsit of lists SEXPS into a list of content rules."
  (let (content-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-content-rule--create
                     :text  (nth 0 item)
                     :value (nth 1 item)
                     :type  (nth 2 item)
                     :date  (nth 3 item)
                     :tags  (nth 4 item))))
        (unless (member struct content-rules)
          (setq content-rules (append content-rules (list struct))))))
    content-rules))

(defun elfeed-score--parse-feed-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of feed rules."
  (let (feed-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-feed-rule--create
                     :text  (nth 0 item)
                     :value (nth 1 item)
                     :type  (nth 2 item)
                     :attr  (nth 3 item)
                     :date  (nth 4 item)
                     :tags  (nth 5 item))))
        (unless (member struct feed-rules)
          (setq feed-rules (append feed-rules (list struct))))))
    feed-rules))

(defun elfeed-score--parse-scoring-sexp-1 (sexp)
  "Interpret the S-expression SEXP as scoring rules version 1.

Parse version 1 of the scoring S-expression.  This function will
fail if SEXP has a \"version\" key with a value other than 1 (the
caller may want to remove it via `assoc-delete-all' or some
such).  Return a property list with the following keys:

    - :title : list of elfeed-score-title-rule structs
    - :content : list of elfeed-score-content-rule structs
    - :feed : list of elfeed-score-feed-rule structs
    - :mark : score below which entries shall be marked read"

  (let (mark titles feeds content)
    (dolist (raw-item sexp)
      (let ((key  (car raw-item))
	          (rest (cdr raw-item)))
	      (cond
         ((string= key "version")
          (unless (eq 1 (car rest))
            (error "Unsupported score file version %s" (car rest))))
	       ((string= key "title")
          (setq titles (elfeed-score--parse-title-rule-sexps rest)))
         ((string= key "content")
          (setq content (elfeed-score--parse-content-rule-sexps rest)))
         ((string= key "feed")
          (setq feeds (elfeed-score--parse-feed-rule-sexps rest)))
	       ((eq key 'mark)
          ;; set `mark' to (cdr rest) if (not mark) or (< mark (cdr rest))
          (let ((rest (car rest)))
            (if (or (not mark)
                    (< mark rest))
                (setq mark rest))))
	       (t
	        (error "Unknown score file key %s" key)))))
    (list
     :mark mark
	   :feeds feeds
	   :titles titles
     :content content)))

(defun elfeed-score--parse-title-or-content-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of title-or-content rules.

Each sub-list shall have the form '(TEXT TITLE-VALUE
CONTENT-VALUE TYPE DATE)."
  (let (toc-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-title-or-content-rule--create
                     :text          (nth 0 item)
                     :title-value   (nth 1 item)
                     :content-value (nth 2 item)
                     :type          (nth 3 item)
                     :date          (nth 4 item)
                     :tags          (nth 5 item))))
        (unless (member struct toc-rules)
          (setq toc-rules (append toc-rules (list struct))))))
    toc-rules))

(defun elfeed-score--parse-scoring-sexp-2 (sexp)
  "Interpret the S-expression SEXP as scoring rules version 2.

Parse version 2 of the scoring S-expression.  Return a property list
with the following keys:

    - :title : list of elfeed-score-title-rule structs
    - :content : list of elfeed-score-content-rule structs
    - :title-or-content: list of elfeed-score-title-or-content-rule
                         structs
    - :feed : list of elfeed-score-feed-rule structs
    - :mark : score below which entries shall be marked read"

  (let (mark titles feeds content tocs)
    (dolist (raw-item sexp)
      (let ((key  (car raw-item))
	          (rest (cdr raw-item)))
	      (cond
         ((string= key "version")
          (unless (eq 2 (car rest))
            (error "Unsupported score file version %s" (car rest))))
	       ((string= key "title")
          (setq titles (elfeed-score--parse-title-rule-sexps rest)))
         ((string= key "content")
          (setq content (elfeed-score--parse-content-rule-sexps rest)))
         ((string= key "feed")
          (setq feeds (elfeed-score--parse-feed-rule-sexps rest)))
         ((string= key "title-or-content")
          (setq tocs (elfeed-score--parse-title-or-content-rule-sexps rest)))
	       ((eq key 'mark)
          ;; set `mark' to (cdr rest) if (not mark) or (< mark (cdr rest))
          (let ((rest (car rest)))
            (if (or (not mark)
                    (< mark rest))
                (setq mark rest))))
	       (t
	        (error "Unknown score file key %s" key)))))
    (list
     :mark mark
	   :feeds feeds
	   :titles titles
     :content content
     :title-or-content tocs)))

(defun elfeed-score--parse-scoring-sexp (sexps)
  "Parse raw S-expressions (SEXPS) into scoring rules."
  (let ((version
         (cond
          ((assoc 'version sexps)
           (cadr (assoc 'version sexps)))
          ((assoc "version" sexps)
           (cadr (assoc "version" sexps)))
          (t
           ;; I'm going to assume this is a new, hand-authored scoring
           ;; file, and attempt to parse it according to the latest
           ;; version spec.
           2))))
    ;; I use `cl-delete' instead of `assoc-delete-all' because the
    ;; latter would entail a dependency on Emacs 26.2, which I would
    ;; prefer not to do.
    (cl-delete "version" sexps :test 'equal :key 'car)
    (cl-delete 'version sexps :test 'equal :key 'car)
    (cond
     ((eq version 1)
      (elfeed-score--parse-scoring-sexp-1 sexps))
     ((eq version 2)
      (elfeed-score--parse-scoring-sexp-2 sexps))
     (t
      (error "Unknown version %s" version)))))

(defun elfeed-score--parse-score-file (score-file)
  "Parse SCORE-FILE.

Internal.  This is the core score file parsing routine.  Opens
SCORE-FILE, reads the contents as a Lisp form, and parses that
into a property list with the following properties:

    - :content
    - :feeds
    - :mark
    - :titles"

  (let ((sexp
         (car
		      (read-from-string
		       (with-temp-buffer
			       (insert-file-contents score-file)
			       (buffer-string))))))
    (elfeed-score--parse-scoring-sexp sexp)))

(defvar elfeed-score--title-rules nil
  "List of structs each defining a scoring rule for entry titles.")

(defvar elfeed-score--feed-rules nil
  "List of structs each defining a scoring rule for entry feeds.")

(defvar elfeed-score--content-rules nil
  "List of structs each defining a scoring rule for entry content.")

(defvar elfeed-score--title-or-content-rules nil
  "List of structs each defining a scoring rule for entry title or content.")

(defvar elfeed-score--score-mark nil
  "Score at or below which entries shall be marked as read.")

(defun elfeed-score--load-score-file (score-file)
  "Load SCORE-FILE into our internal scoring rules.

Internal.  Read SCORE-FILE, store scoring rules in our internal datastructures,"

  (let ((score-entries (elfeed-score--parse-score-file score-file)))
    (setq elfeed-score--title-rules            (plist-get score-entries :titles)
          elfeed-score--feed-rules             (plist-get score-entries :feeds)
          elfeed-score--content-rules          (plist-get score-entries :content)
          elfeed-score--title-or-content-rules (plist-get score-entries :title-or-content)
          elfeed-score--score-mark             (plist-get score-entries :mark))))

(defun elfeed-score--match-text (match-text search-text match-type)
  "Test SEARCH-TEXT against MATCH-TEXT according to MATCH-TYPE.
Return nil on failure, t on match."
  (cond
   ((or (eq match-type 's)
        (eq match-type 'S))
    (let ((case-fold-search (eq match-type 's)))
      (string-match-p (regexp-quote match-text) search-text)))
   ((or (eq match-type 'r)
        (eq match-type 'R)
        (not match-type))
    (let ((case-fold-search (eq match-type 'r)))
      (string-match-p match-text search-text)))
   ((or (eq match-type 'w)
        (eq match-type 'W))
    (let ((case-fold-search (eq match-type 'w)))
      (string-match-p (word-search-regexp match-text) search-text)))
   (t
    (error "Unknown match type %s" match-type))))

(defun elfeed-score--match-tags (entry-tags tag-rule)
  "Test a ENTRY-TAGS against TAG-RULE.

ENTRY-TAGS shall be a list of symbols, presumably the tags applied to the Elfeed
entry bing scored.  TAG-RULE shall be a list of the form (boolean . (symbol...))
or nil, and is presumably a tag scoping for a scoring rule."

  (if tag-rule
      (let ((flag (car tag-rule))
            (rule-tags (cdr tag-rule))
            (apply nil))
        (while (and rule-tags (not apply))
          (if (memq (car rule-tags) entry-tags)
              (setq apply t))
          (setq rule-tags (cdr rule-tags)))
        (if flag
            apply
          (not apply)))
    t))

(defun elfeed-score--score-on-title (entry)
  "Run all title scoring rules against ENTRY; return the summed values."
  (let ((title (elfeed-entry-title entry))
        (score 0))
    (dolist (score-title elfeed-score--title-rules)
	    (let* ((match-text (elfeed-score-title-rule-text  score-title))
		         (value      (elfeed-score-title-rule-value score-title))
		         (match-type (elfeed-score-title-rule-type  score-title))
             (tag-rule   (elfeed-score-title-rule-tags  score-title))
             (got-match (and
                         (elfeed-score--match-tags (elfeed-entry-tags entry) tag-rule)
                         (elfeed-score--match-text match-text title match-type))))
        (if got-match
            (progn
              (elfeed-score--debug "'%s' + %d (title)" title value)
		          (setq score (+ score value))
              (setf (elfeed-score-title-rule-date score-title) (float-time))))))
    score))

(defun elfeed-score--score-on-feed (entry)
  "Run all feed scoring rules against ENTRY; return the summed values."
  (let ((feed (elfeed-entry-feed  entry))
        (score 0))
    (dolist (score-feed elfeed-score--feed-rules)
	    (let* ((match-text (elfeed-score-feed-rule-text  score-feed))
		         (value      (elfeed-score-feed-rule-value score-feed))
		         (match-type (elfeed-score-feed-rule-type  score-feed))
             (attr       (elfeed-score-feed-rule-attr  score-feed))
             (feed-text (cond
                         ((eq attr 't)
                          (elfeed-feed-title feed))
                         ((eq attr 'u)
                          (elfeed-feed-url feed))
                         ((eq attr 'a)
                          (elfeed-feed-author feed))
                         (t
                          (error "Unknown feed attribute %s" attr))))
             (tag-rule   (elfeed-score-feed-rule-tags  score-feed))
             (got-match (and
                         (elfeed-score--match-tags (elfeed-entry-tags entry) tag-rule)
                         (elfeed-score--match-text match-text feed-text match-type))))
        (if got-match
            (progn
              (elfeed-score--debug "%s + %d (feed)" (elfeed-entry-title entry) value)
		          (setq score (+ score value))
		          (setf (elfeed-score-feed-rule-date score-feed) (float-time))))))
    score))

(defun elfeed-score--score-on-content (entry)
  "Run all content scoring rules against ENTRY; return the summed values."
  (let ((content (elfeed-deref (elfeed-entry-content entry)))
        (score 0))
    (if content
        (dolist (score-content elfeed-score--content-rules)
	        (let* ((match-text   (elfeed-score-content-rule-text  score-content))
		             (value        (elfeed-score-content-rule-value score-content))
		             (match-type   (elfeed-score-content-rule-type  score-content))
                 (tag-rule     (elfeed-score-content-rule-tags  score-content))
                 (got-match    (and
                                (elfeed-score--match-tags (elfeed-entry-tags entry)
                                                          tag-rule)
                                (elfeed-score--match-text match-text
                                                          content match-type))))
            (if got-match
                (progn
                  (elfeed-score--debug "%s + %d (content)" (elfeed-entry-title entry)
                                       value)
		              (setq score (+ score value))
		              (setf (elfeed-score-content-rule-date score-content)
                        (float-time)))))))
    score))

(defun elfeed-score--score-on-title-or-content (entry)
  "Run all title-or-content rules against ENTRY; return the summed values."
  (let ((title (elfeed-entry-title entry))
        (content (elfeed-deref (elfeed-entry-content entry)))
        (score 0))
    (dolist (score-title elfeed-score--title-or-content-rules)
	    (let* ((match-text (elfeed-score-title-or-content-rule-text        score-title))
		         (value      (elfeed-score-title-or-content-rule-title-value score-title))
		         (match-type (elfeed-score-title-or-content-rule-type        score-title))
             (tag-rule   (elfeed-score-title-or-content-rule-tags        score-title))
             (got-match (and
                         (elfeed-score--match-tags (elfeed-entry-tags entry) tag-rule)
                         (elfeed-score--match-text match-text title match-type))))
        (if got-match
            (progn
              (elfeed-score--debug "'%s' + %d (ToC/title: rule %s)" title value match-text)
		          (setq score (+ score value))
              (setf (elfeed-score-title-or-content-rule-date score-title)
                    (float-time))))))
    (if content
        (dolist (score-content elfeed-score--title-or-content-rules)
	        (let* ((match-text (elfeed-score-title-or-content-rule-text score-content))
		             (value (elfeed-score-title-or-content-rule-content-value
                         score-content))
		             (match-type (elfeed-score-title-or-content-rule-type
                              score-content))
                 (tag-rule (elfeed-score-title-or-content-rule-tags score-content))
                 (got-match (and
                             (elfeed-score--match-tags (elfeed-entry-tags entry)
                                                       tag-rule)
                             (elfeed-score--match-text match-text content match-type))))
            (if got-match
                (progn
                  (elfeed-score--debug "%s + %d (ToC/content: rule %s)"
                                       (elfeed-entry-title entry) value match-text)
		              (setq score (+ score value))
		              (setf (elfeed-score-title-or-content-rule-date score-content)
                        (float-time)))))))
    score))

(defun elfeed-score--score-entry (entry)
  "Score an Elfeed ENTRY.

This function will return the entry's score, udpate it's meta-data, and
udpate the \"last matched\" time of the salient rules."

  (let ((score (+ elfeed-score-default-score
                  (elfeed-score--score-on-title entry)
                  (elfeed-score--score-on-feed entry)
                  (elfeed-score--score-on-content entry)
                  (elfeed-score--score-on-title-or-content entry))))
    (setf (elfeed-meta entry elfeed-score-meta-keyword) score)
	  (if (and elfeed-score--score-mark
		         (< score elfeed-score--score-mark))
	      (elfeed-untag entry 'unread))
    score))

(define-obsolete-function-alias 'elfeed-score/load-score-file
  'elfeed-score-load-score-file "0.2.0" "Move to standard-compliant naming.")

(defun elfeed-score-load-score-file (score-file)
  "Load SCORE-FILE into the current scoring rules."

  (interactive
   (list
    (read-file-name "score file: " nil elfeed-score-score-file t
                    elfeed-score-score-file)))
  (elfeed-score--load-score-file score-file))

(define-obsolete-function-alias 'elfeed-score/write-score-file
  'elfeed-score-write-score-file "0.2.0" "Move to standard-compliant naming.")

(defun elfeed-score-write-score-file (score-file)
  "Write the current scoring rules to SCORE-FILE."
  (interactive
   (list
    (read-file-name "score file: " nil elfeed-score-score-file t
                    elfeed-score-score-file)))
  (write-region
   (format
    ";;; Elfeed score file                                     -*- lisp -*-\n%s"
	  (pp-to-string
	   (list
	    (list 'version 2)
      (append
       '("title")
	     (mapcar
	      (lambda (x)
          (let ((body
                 (list
                  (elfeed-score-title-rule-text  x)
                  (elfeed-score-title-rule-value x)
                  (elfeed-score-title-rule-type  x)
                  (elfeed-score-title-rule-date  x)))
                (tags (elfeed-score-title-rule-tags x)))
            (if tags
                (append body (list tags))
              body)))
	      elfeed-score--title-rules))
      (append
       '("content")
	     (mapcar
	      (lambda (x)
          (let ((body
		             (list
		              (elfeed-score-content-rule-text  x)
		              (elfeed-score-content-rule-value x)
		              (elfeed-score-content-rule-type  x)
		              (elfeed-score-content-rule-date  x)))
                (tags (elfeed-score-content-rule-tags x)))
            (if tags
                (append body (list tags))
              body)))
	      elfeed-score--content-rules))
      (append
       '("title-or-content")
       (mapcar
        (lambda (x)
          (let ((body
                 (list
                  (elfeed-score-title-or-content-rule-text x)
                  (elfeed-score-title-or-content-rule-title-value x)
                  (elfeed-score-title-or-content-rule-content-value x)
                  (elfeed-score-title-or-content-rule-type x)
                  (elfeed-score-title-or-content-rule-date x)))
                (tags (elfeed-score-title-or-content-rule-tags x)))
            (if tags
                (append body (list tags))
              body)))
        elfeed-score--title-or-content-rules))
      (append
       '("feed")
	     (mapcar
	      (lambda (x)
          (let ((body
		             (list
		              (elfeed-score-feed-rule-text  x)
		              (elfeed-score-feed-rule-value x)
		              (elfeed-score-feed-rule-type  x)
                  (elfeed-score-feed-rule-attr  x)
		              (elfeed-score-feed-rule-date  x)))
                (tags (elfeed-score-feed-rule-tags x)))
            (if tags
                (append body (list tags))
              body)))
	      elfeed-score--feed-rules))
      (list 'mark elfeed-score--score-mark))))
   nil score-file))

(define-obsolete-function-alias 'elfeed-score/score
  'elfeed-score-score "0.2.0" "Move to standard-compliant naming.")

(defun elfeed-score-score (&optional ignore-region)
  "Score some entries.

Score all selected entries, unless IGNORE-REGION is non-nil, in
which case only the entry under point will be scored.  If the
region is not active, only the entry under point will be scored."

  (interactive "P")

  (let ((entries (elfeed-search-selected ignore-region)))
    (dolist (entry entries)
      (let ((score (elfeed-score--score-entry entry)))
        (elfeed-score--debug "entry '%s' => %d" (elfeed-entry-title entry) score)))
    (if elfeed-score-score-file
       (elfeed-score-write-score-file elfeed-score-score-file))
    (elfeed-search-update t)))

(define-obsolete-function-alias 'elfeed-score/score-search
  'elfeed-score-score-search "0.2.0" "Move to standard-compliant naming.")

(defun elfeed-score-score-search ()
  "Score the current set of search results."

  (interactive)

  (dolist (entry elfeed-search-entries)
    (let ((score (elfeed-score--score-entry entry)))
      (elfeed-score--debug "entry %s => %s" (elfeed-entry-title entry) score)))
  (if elfeed-score-score-file
	    (elfeed-score-write-score-file elfeed-score-score-file))
  (elfeed-search-update t))

(defvar elfeed-score-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "e" #'elfeed-score-set-score)
      (define-key map "g" #'elfeed-score-get-score)
      (define-key map "l" #'elfeed-score-load-score-file)
      (define-key map "s" #'elfeed-score-score)
      (define-key map "v" #'elfeed-score-score-search)
      (define-key map "w" #'elfeed-score-write-score-file)))
  "Keymap for `elfeed-score' commands.")

(defvar elfeed-score--old-sort-function nil
  "Original value of `elfeed-search-sort-function'.")

(defvar elfeed-score--old-print-entry-function nil
  "Original value of `elfed-search-print-entry-function'.")

(defun elfeed-score-print-entry (entry)
  "Print ENTRY to the Elfeed search buffer.
This implementation is derived from `elfeed-search-print-entry--default'."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left))
	 (score (elfeed-score-format-score
		 (elfeed-meta entry elfeed-score-meta-keyword
                              elfeed-score-default-score))))
    (insert score)
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")"))))

;;;###autoload
(defun elfeed-score-enable ()
  "Enable `elfeed-score'."

  (interactive)

  ;; Begin scoring on every new entry...
  (add-hook 'elfeed-new-entry-hook #'elfeed-score--score-entry)
  ;; sort based on score...
  (setq elfeed-score--old-sort-function elfeed-search-sort-function
        elfeed-search-sort-function #'elfeed-score-sort
        elfeed-score--old-print-entry-function elfeed-search-print-entry-function)
  ;; & load the default score file, if it's defined & exists.
  (if (and elfeed-score-score-file
           (file-exists-p elfeed-score-score-file))
      (elfeed-score-load-score-file elfeed-score-score-file))

  (defun elfeed-score-unload ()
    "Unload `elfeed-score'."

    (interactive)

    (if elfeed-score-score-file
	      (elfeed-score-write-score-file elfeed-score-score-file))
    (setq elfeed-search-sort-function elfeed-score--old-sort-function
          elfeed-search-print-entry-function elfeed-score--old-print-entry-function)
    (remove-hook 'elfeed-new-entry-hook #'elfeed-score--score-entry)))


(provide 'elfeed-score)
;;; elfeed-score.el ends here
