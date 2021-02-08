;;; elfeed-score-rules.el --- Rules for `elfeed-score'  -*- lexical-binding: t; -*-

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

;; This package contains the definitions of the `elfeed-score' rule
;; types.

;;; Code:

(require 'cl-lib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             title rules                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (elfeed-score-title-rule
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                elfeed-score-title-rule--create
                (&key text value type date tags (hits 0) feeds
                      &aux
                      (_
                       (unless (and (stringp text) (> (length text) 0))
                         (error "Title rule text must be a non-empty string"))
                       (unless (numberp value)
                         (error "Title rule value must be a number"))
                       (unless (and (symbolp type)
                                    (or (eq type 's)
                                        (eq type 'S)
                                        (eq type 'r)
                                        (eq type 'R)
                                        (eq type 'w)
                                        (eq type 'W)))
                         (error "Title type must be one of '{s,S,r,R,wW}"))))))
  "Rule for scoring against entry titles.

    - text :: The rule's match text; either a string or a regular
              expression (on which more below)

    - value :: Integral value (positive or negative) to be added
               to an entry's score if this rule matches

    - type :: One of the symbols s S r R w W; s/r/w denotes
              substring/regexp/whole word match; lower-case means
              case-insensitive and upper case sensitive.
              Defaults to r (case-insensitive regexp match)

    - date :: time (in seconds since epoch) when this rule last
              matched

    - tags :: cons cell of the form (A . B) where A is either t
              or nil and B is a list of symbols. The latter is
              interpreted as a list of tags scoping the rule and
              the former as a boolean switch possibly negating the
              scoping. E.g. (t . (a b)) means \"apply this rule
              if either of tags a & b are present\". Making the
              first element nil means \"do not apply this rule if
              any of a and b are present\".

    - hits :: the number of times since upgrading to score file
              version 5 that this rule has been matched

    - feeds :: cons cell of the form (A . B) where A is either t
               or nil and B is a list of three-tuples. Each
               three-tuple will be matched against an entry's
               feed:

                 1. attribute: one of 't, 'u, or 'a for title,
                 URL, or author, resp.
                 2. match type: one of 's, 'S, 'r, 'R, 'w, or
                 'W (the usual match types)
                 3. match text

               So, e.g. '(t s \"foo\") means do a
               case-insensitive substring match for \"foo\"
               against the feed title.

               The first element of the cons cell is interpreted as a boolean
               switch possibly negating the scoping. For
               instance, (t . '((t s \"foo\") (u s
               \"https://bar.com/feed\"))) means \"apply this rule
               only to feeds entitled foo or from
               https://bar/com/feed\" Making the first element nil
               means \"do not apply this rule if the feed is
               either foo or bar\"."
  text value type date tags (hits 0) feeds)

(cl-defstruct (elfeed-score-title-explanation
               (:constructor nil)
               (:constructor elfeed-score-make-title-explanation))
  "An explanation of a title rule match."
  matched-text rule)

(define-obsolete-function-alias 'elfeed-score-pp-title-explanation
  #'elfeed-score-rules-pp-title-explanation "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-pp-title-explanation (match)
  "Pretty-print MATCH to string."
  (let ((rule (elfeed-score-title-explanation-rule match)))
    (format "title{%s}: \"%s\": %d"
            (elfeed-score-title-rule-text rule)
            (elfeed-score-title-explanation-matched-text match)
            (elfeed-score-title-rule-value rule))))

(define-obsolete-function-alias 'elfeed-score-title-explanation-contrib
  #'elfeed-score-rules-title-explanation-contrib "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-title-explanation-contrib (match)
  "Return the score contribution due to MATCH."
  (elfeed-score-title-rule-value
   (elfeed-score-title-explanation-rule match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            feed rules                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (elfeed-score-feed-rule
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                elfeed-score-feed-rule--create
                (&key text value type attr date tags (hits 0)
                      &aux
                      (_
                       (unless (and (stringp text) (> (length text) 0))
                         (error "Feed rule text must be a non-empty string"))
                       (unless (numberp value)
                         (error "Feed rule value must be a number"))
                       (unless (and (symbolp type)
                                    (or (eq type 's)
                                        (eq type 'S)
                                        (eq type 'r)
                                        (eq type 'R)
                                        (eq type 'w)
                                        (eq type 'W)))
                         (error "Feed type must be one of '{s,S,r,R,wW}"))
                       (unless (and (symbolp attr)
                                    (or (eq type 't)
                                        (eq type 'u)))
                         (error "Feed attribute must be one of '{t,u}"))))))
  "Rule for scoring against entry feeds.

    - text :: The rule's match text; either a string or a regular
              expression (on which more below)

    - value :: Integral value (positive or negative) to be added to
               an entry's score if this rule matches

    - type :: One of the symbols s S r R w W; s/r/w denotes
              substring/regexp/whole word match; lower-case means
              case-insensitive and upper case sensitive.
              Defaults to r (case-insensitive regexp match)

    - attr :: Defines the feed attribute against which matching
              shall be performed: 't for title & 'u for URL.

    - date :: time (in seconds since epoch) when this rule last
              matched

    - tags :: cons cell of the form (a . b) where A is either t
              or nil and B is a list of symbols. The latter is
              interpreted as a list of tags scoping the rule and
              the former as a boolean switch possibly negating the
              scoping. E.g. (t . (a b)) means \"apply this rule
              if either of tags a & b are present\". Making the
              first element nil means \"do not apply this rule if
              any of a and b are present\".

    - hits :: the number of times since upgrading to score file
              version 5 that this rule has been matched"
  text value type attr date tags (hits 0))

(cl-defstruct (elfeed-score-feed-explanation
               (:constructor nil)
               (:constructor elfeed-score-make-feed-explanation))
  "An explanation of a feed rule match"
  matched-text rule)

(define-obsolete-function-alias 'elfeed-score-pp-feed-explanation
  #'elfeed-score-rules-pp-feed-explanation "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-pp-feed-explanation (match)
  "Pretty-print MATCH to string."
  (let ((rule (elfeed-score-feed-explanation-rule match)))
    (format "feed{%s/%s}: \"%s\": %d"
            (elfeed-score-feed-rule-attr rule)
            (elfeed-score-feed-rule-text rule)
            (elfeed-score-feed-explanation-matched-text match)
            (elfeed-score-feed-rule-value rule))))

(define-obsolete-function-alias 'elfeed-score-feed-explanation-contrib
  #'elfeed-score-rules-feed-explanation-contrib "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-feed-explanation-contrib (match)
  "Return the score contribution due to MATCH."
  (elfeed-score-feed-rule-value
   (elfeed-score-feed-explanation-rule match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          content rules                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (elfeed-score-content-rule
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                elfeed-score-content-rule--create
                (&key text value type date tags (hits 0) feeds
                      &aux
                      (_
                       (unless (and (stringp text) (> (length text) 0))
                         (error "Content rule text must be a non-empty string"))
                       (unless (numberp value)
                         (error "Content rule value must be a number"))
                       (unless (and (symbolp type)
                                    (or (eq type 's)
                                        (eq type 'S)
                                        (eq type 'r)
                                        (eq type 'R)
                                        (eq type 'w)
                                        (eq type 'W)))
                         (error "Content type must be one of '{s,S,r,R,wW}"))))))
  "Rule for scoring against entry content

    - text :: The rule's match text; either a string or a regular
              expression (on which more below)

    - value :: Integral value (positive or negative) to be added to
               an entry's score if this rule matches

    - type :: One of the symbols s S r R w W; s/r/w denotes
              substring/regexp/whole word match; lower-case
              means case-insensitive and upper case sensitive.
              Defaults to r (case-insensitive regexp match)

    - date :: time (in seconds since epoch) when this rule last
              matched

    - tags :: cons cell of the form (a . b) where A is either t
              or nil and B is a list of symbols. The latter is
              interpreted as a list of tags scoping the rule and
              the former as a boolean switch possibly negating the
              scoping. E.g. (t . (a b)) means \"apply this rule
              if either of tags a & b are present\". Making the
              first element nil means \"do not apply this rule if
              any of a and b are present\".

    - hits :: the number of times since upgrading to score file
              version 5 that this rule has been matched

    - feeds :: cons cell of the form (A . B) where A is either t
               or nil and B is a list of three-tuples. Each
               three-tuple will be matched against an entry's
               feed:

                 1. attribute: one of 't, 'u, or 'a for title,
                 URL, or author, resp.
                 2. match type: one of 's, 'S, 'r, 'R, 'w, or
                 'W (the usual match types)
                 3. match text

               So, e.g. '(t s \"foo\") means do a
               case-insensitive substring match for \"foo\"
               against the feed title.

               The first element of the cons cell is interpreted as a boolean
               switch possibly negating the scoping. For
               instance, (t . '((t s \"foo\") (u s
               \"https://bar.com/feed\"))) means \"apply this rule
               only to feeds entitled foo or from
               https://bar/com/feed\" Making the first element nil
               means \"do not apply this rule if the feed is
               either foo or bar\"."
  text value type date tags (hits 0) feeds)

(cl-defstruct (elfeed-score-content-explanation
               (:constructor nil)
               (:constructor elfeed-score-make-content-explanation))
  "An explanation of a matched content rule."
  matched-text rule)

(define-obsolete-function-alias 'elfeed-score-content-explanation-contrib
  #'elfeed-score-rules-content-explanation-contrib "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-pp-content-explanation (match)
  "Pretty-print MATCH to string."
  (let ((rule (elfeed-score-content-explanation-rule match)))
    (format "content{%s}: \"%s\": %d"
            (elfeed-score-content-rule-text rule)
            (elfeed-score-content-explanation-matched-text match)
            (elfeed-score-content-rule-value rule))))

(define-obsolete-function-alias 'elfeed-score-pp-content-explanation
  #'elfeed-score-rules-pp-content-explanation "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-content-explanation-contrib (match)
  "Return the score contribution due to MATCH."
  (elfeed-score-content-rule-value
   (elfeed-score-content-explanation-rule match)))

(define-obsolete-function-alias 'elfeed-score-feed-explanation-contrib
  #'elfeed-score-rules-feed-explanation-contrib "0.7.0"
  "Re-factoring elfeed-score.el.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      title-or-content rules                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (elfeed-score-title-or-content-rule
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                elfeed-score-title-or-content-rule--create
                (&key text title-value content-value type date tags
                      (hits 0) feeds
                      &aux
                      (_
                       (unless (and (stringp text) (> (length text) 0))
                         (error "Title-or-content rule text must be a \
non-empty string"))
                       (unless (numberp title-value)
                         (error "Title-or-content rule title value must \
be a number"))
                       (unless (numberp content-value)
                         (error "Title-or-content rule content value must \
be a number"))
                       (unless (and (symbolp type)
                                    (or (eq type 's)
                                        (eq type 'S)
                                        (eq type 'r)
                                        (eq type 'R)
                                        (eq type 'w)
                                        (eq type 'W)))
                         (error "Title-or-content type must be one of \
'{s,S,r,R,wW}"))))))
  "Rule for scoring the same text against both entry title & content.

I found myself replicating the same rule for both title &
content, with a higher score for title.  This rule permits
defining a single rule for both.

    - text :: The rule's match text; either a string or a regular
              expression (on which more below)

    - title-value :: Integral value (positive or negative) to be
                     added to an entry's score should this rule
                     match the entry's title

    - content-value :: Integral value (positive or negative) to
                       be added to an entry's score should this
                       rule match the entry's value

    - type :: One of the symbols s S r R w W; s/r/w denotes
              substring/regexp/whole word match; lower-case means
              case-insensitive and upper case sensitive.
              Defaults to r (case-insensitive regexp match)

    - date :: time (in seconds since epoch) when this rule last matched

    - tags :: cons cell of the form (a . b) where A is either t
              or nil and B is a list of symbols. The latter is
              interpreted as a list of tags scoping the rule and
              the former as a boolean switch possibly negating the
              scoping. E.g. (t . (a b)) means \"apply this rule
              if either of tags a & b are present\". Making the
              first nil element means \"do not apply this rule if
              any of a and b are present\".

    - hits :: the number of times since upgrading to score file version
              5 that this rule has been matched

    - feeds :: cons cell of the form (A . B) where A is either t
               or nil and B is a list of three-tuples. Each
               three-tuple will be matched against an entry's
               feed:

                 1. attribute: one of 't, 'u, or 'a for title,
                 URL, or author, resp.
                 2. match type: one of 's, 'S, 'r, 'R, 'w, or
                 'W (the usual match types)
                 3. match text

               So, e.g. '(t s \"foo\") means do a
               case-insensitive substring match for \"foo\"
               against the feed title.

               The first element of the cons cell is interpreted as a boolean
               switch possibly negating the scoping. For
               instance, (t . '((t s \"foo\") (u s
               \"https://bar.com/feed\"))) means \"apply this rule
               only to feeds entitled foo or from
               https://bar/com/feed\" Making the first element nil
               means \"do not apply this rule if the feed is
               either foo or bar\"."
  text title-value content-value type date tags (hits 0) feeds)

(cl-defstruct (elfeed-score-title-or-content-explanation
               (:constructor nil)
               (:constructor elfeed-score-make-title-or-content-explanation))
  "An explanation of a title-or-content rule match."
  matched-text rule attr)

(define-obsolete-function-alias
  'elfeed-score-pp-title-or-content-explanation
  #'elfeed-score-rules-pp-title-or-content-explanation
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-pp-title-or-content-explanation (match)
  "Pretty-print MATCH to string."
  (let ((rule (elfeed-score-title-or-content-explanation-rule match))
        (attr (elfeed-score-title-or-content-explanation-attr match)))
    (format "title-or-content{%s/%s}: \"%s\": %d"
            attr
            (elfeed-score-title-or-content-rule-text rule)
            (elfeed-score-title-or-content-explanation-matched-text match)
            (if (eq 't attr)
                (elfeed-score-title-or-content-rule-title-value rule)
              (elfeed-score-title-or-content-rule-content-value rule)))))

(define-obsolete-function-alias
  'elfeed-score-title-or-content-explanation-contrib
  #'elfeed-score-rules-title-or-content-explanation-contrib
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-title-or-content-explanation-contrib (match)
  "Return the score contribution due to MATCH."
  (let ((rule (elfeed-score-title-or-content-explanation-rule match)))
    (if (eq 't (elfeed-score-title-or-content-explanation-attr match))
        (elfeed-score-title-or-content-rule-title-value rule)
      (elfeed-score-title-or-content-rule-content-value rule))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          authors rules                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (elfeed-score-authors-rule
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                elfeed-score-authors-rule--create
                (&key text value type date tags (hits 0) feeds
                      &aux
                      (_
                       (unless (and (stringp text) (> (length text) 0))
                         (error "Authors rule text must be a non-empty string"))
                       (unless (numberp value)
                         (error "Authors rule value must be a number"))
                       (unless (and (symbolp type)
                                    (or (eq type 's)
                                        (eq type 'S)
                                        (eq type 'r)
                                        (eq type 'R)
                                        (eq type 'w)
                                        (eq type 'W)))
                         (error "Authors rulle type must be one of \
'{s,S,r,R,w,W}"))))))
  "Rule for scoring against the names of all the authors

    - text :: The rule's match text; either a string or a regular
              expression (on which more below)

    - value :: Integral value (positive or negative) to be added
               to an entry's score should this rule match one of
               the authors

    - type :: One of the symbols s S r R w W; s/r/w denotes
              substring/regexp/whole word match; lower-case means
              case-insensitive and upper case sensitive.
              Defaults to r (case-insensitive regexp match)

    - date :: time (in seconds since epoch) when this rule last
              matched

    - tags :: cons cell of the form (a . b) where A is either t
              or nil and B is a list of symbols. The latter is
              interpreted as a list of tags scoping the rule and
              the former as a boolean switch possibly negating the
              scoping. E.g. (t . (a b)) means \"apply this rule
              if either of tags a & b are present\". Making the
              first nil element means \"do not apply this rule if
              any of a and b are present\".

    - hits :: the number of times since upgrading to score file
              version 5 that this rule has been matched

    - feeds :: cons cell of the form (A . B) where A is either t
               or nil and B is a list of three-tuples. Each
               three-tuple will be matched against an entry's
               feed:

                 1. attribute: one of 't, 'u, or 'a for title,
                 URL, or author, resp.
                 2. match type: one of 's, 'S, 'r, 'R, 'w, or
                 'W (the usual match types)
                 3. match text

               So, e.g. '(t s \"foo\") means do a
               case-insensitive substring match for \"foo\"
               against the feed title.

               The first element of the cons cell is interpreted as a boolean
               switch possibly negating the scoping. For
               instance, (t . '((t s \"foo\") (u s
               \"https://bar.com/feed\"))) means \"apply this rule
               only to feeds entitled foo or from
               https://bar/com/feed\" Making the first element nil
               means \"do not apply this rule if the feed is
               either foo or bar\"."
  text value type date tags (hits 0) feeds)

(cl-defstruct (elfeed-score-authors-explanation
               (:constructor nil)
               (:constructor elfeed-score-make-authors-explanation))
  "An explanation of an authors rule match"
  matched-text rule)

(define-obsolete-function-alias 'elfeed-score-pp-authors-explanation
  #'elfeed-score-rules-pp-authors-explanation "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-pp-authors-explanation (match)
  "Pretty-print MATCH to string."
  (let ((rule (elfeed-score-authors-explanation-rule match)))
    (format "authors{%s} \"%s\": %d"
            (elfeed-score-authors-rule-text rule)
            (elfeed-score-authors-explanation-matched-text match)
            (elfeed-score-authors-rule-value rule))))

(define-obsolete-function-alias
  'elfeed-score-authors-explanation-contrib
  #'elfeed-score-rules-authors-explanation-contrib "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-authors-explanation-contrib (match)
  "Return the score contribution due to MATCH."
  (elfeed-score-authors-rule-value
   (elfeed-score-authors-explanation-rule match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                            tags rules                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (elfeed-score-tag-rule
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                elfeed-score-tag-rule--create
                (&key tags value date (hits 0)
                      &aux
                      (_
                       (unless (and (listp tags))
                         (error "Tags rules must begin with a cons cell"))
                       (unless (numberp value)
                         (error "Tags rule value must be a number"))))))
  "Rule for scoring based on the presence or absence of a tag or tags.

    - tags :: cons cell of the form (A . B) where A is either t
              or nil and B is a symbol or list of symbols. The
              latter is interpreted as a list of tags selecting
              the entries to which this rule shall apply & the
              former as a boolean switch possibly negating this
              selection.  E.g. (t . (a b)) means \"apply this if
              either of the tags a & b are present\". Making the
              first element nil means \" do not apply this rule
              if any of a and b are present\".

    - value :: integral value (positive or negative) by which to
               adjust the entry score if this rule matches

    - date :: time (in seconds since epoch) when this rule last
              matched

    - hits :: the number of times since upgrading to score file
              version 5 that this rule has been matched"
  tags value date (hits 0))

(cl-defstruct (elfeed-score-tags-explanation
               (:constructor nil)
               (:constructor elfeed-score-make-tags-explanation))
  "An explanation of a tags rule match."
  rule)

(define-obsolete-function-alias
  'elfeed-score-pp-tags-explanation
  #'elfeed-score-rules-pp-tags-explanation
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-pp-tags-explanation (match)
  "Pretty-print MATCH to string."
  (let ((rule (elfeed-score-tags-explanation-rule match)))
    (format "tags{%s}: %d"
            (elfeed-score-tag-rule-tags rule)
            (elfeed-score-tag-rule-value rule))))

(define-obsolete-function-alias
  'elfeed-score-tags-explanation-contrib
  #'elfeed-score-rules-tags-explanation-contrib
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-rules-tags-explanation-contrib (match)
  "Return the score contribution due to MATCH."
  (elfeed-score-tag-rule-value
   (elfeed-score-tags-explanation-rule match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        adjust-tags rules                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (elfeed-score-adjust-tags-rule
               ;; Disable the default ctor (the name violates Emacs
               ;; package naming conventions)
               (:constructor nil)
               ;; Abuse the &aux keyword to validate our parameters; I
               ;; can only specify type in the slot description if I
               ;; specify a default value for the slot, which doesn't
               ;; always make sense.
               (:constructor
                elfeed-score-adjust-tags-rule--create
                (&key threshold tags date (hits 0)
                      &aux
                      (_
                       (unless (listp threshold)
                         (error "Adjust tags rules must begin with a cons cell"))
                       (unless (listp tags)
                         (error "Adjust tags rules must specify tags with \
a cons cell"))))))
  "Rule for adjusting tags based on score.

    - threshold :: a cons cell of the form (A . B) where A is a
                   boolean and B is an integral value. If A is t,
                   then this rule will apply to all entries whose
                   score is greater than or equal to B. If A is
                   nil, this rule will apply to all entries whose
                   score is less than or equal to B.

    - tags :: a cons cell of the form (A . B) where A is a
              boolean and B is a symbol or list of symbols. If A
              is t, and this rule matches, the tags in B will be
              added to the entry. If A is nil & this rule
              matches, the list of tags in B shall be removed
              from the entry.

    - date :: time (in seconds since epoch) when this rule last
              matched

    - hits :: the number of times since upgrading to score file
              version 5 that this rule has been matched"
  threshold tags date (hits 0))

(defun elfeed-score-rules-pp-rule-to-string (rule)
  "Pretty-print RULE; return as a string."
  (cl-typecase rule
    (elfeed-score-title-rule
     (format "title{%s}" (elfeed-score-title-rule-text rule)))
    (elfeed-score-feed-rule
     (format "feed{%s}" (elfeed-score-feed-rule-text rule)))
    (elfeed-score-content-rule
     (format "content{%s}" (elfeed-score-content-rule-text rule)))
    (elfeed-score-title-or-content-rule
     (format "title-or-content{%s}" (elfeed-score-title-or-content-rule-text rule)))
    (elfeed-score-authors-rule
     (format "authors{%s}" (elfeed-score-authors-rule-text rule)))
    (elfeed-score-tag-rule
     (format "tag{%s}" (prin1-to-string (elfeed-score-tag-rule-tags rule))))
    (elfeed-score-adjust-tags-rule
     (format "adjust-tags{%s}" (prin1-to-string (elfeed-score-adjust-tags-rule-tags rule))))
    (otherwise (error "Don't know how to pretty-print %S" rule))))

(provide 'elfeed-score-rules)
;;; elfeed-score-rules.el ends here
