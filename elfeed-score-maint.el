;;; elfeed-score-maint.el --- Helpers for maintaining `elfeed-score' rules  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Michael Herstine <sp1ff@pobox.com>

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

;; This package contains utility functions for reporting on
;; `elfeed-score' rules.

;;; Code:

(require 'elfeed-score-rules)
(require 'elfeed-score-serde)
(require 'elfeed-score-rule-stats)

(defun elfeed-score-maint--get-last-match-date (rule)
  "Retrieve the time at which RULE was last matched.

Return the time, in seconds since epoch, at which RULE was most
recently matched against an entry (floating point).  Note that
RULE may be any rule struct."

  (let ((stats (elfeed-score-rule-stats-get rule)))
    (if stats
        (elfeed-score-rule-stats-date stats)
      0.0)))

(defun elfeed-score-maint--get-hits (rule)
  "Retrieve the number of times RULE has matched an entry.

Note that RULE may be an instance of any rule structure."

  (let ((stats (elfeed-score-rule-stats-get rule)))
    (if stats
        (elfeed-score-rule-stats-hits stats)
      0)))

(defun elfeed-score-maint--sort-rules-by-last-match (rules)
  "Sort RULES in decreasing order of last match.

Note that RULES need not be homogeneous; it may contain rule
structs of any kind understood by
`elfeed-score-maint--get-last-match-date'."
  (sort
   rules
   (lambda (lhs rhs)
     (message "%s|%s" lhs rhs)
     (> (elfeed-score-maint--get-last-match-date lhs)
        (elfeed-score-maint--get-last-match-date rhs)))))

(defun elfeed-score-maint--sort-rules-by-hits (rules)
  "Sort RULES in decreasing order of match hits.

Note that RULES need not be homogeneous; it may contain rule
structs of any kind understood by
`elfeed-score-maint--get-hits'."
  (sort
   rules
   (lambda (lhs rhs)
     (> (elfeed-score-maint--get-hits lhs)
        (elfeed-score-maint--get-hits rhs)))))

(defun elfeed-score-maint--display-rules-by-last-match (rules title)
  "Sort RULES in decreasing order of last match; display results as TITLE."
  (let ((rules (elfeed-score-maint--sort-rules-by-last-match rules))
	      (results '())
	      (max-text 0))
    (cl-dolist (rule rules)
      (let* ((pp (elfeed-score-rules-pp-rule-to-string rule))
	           (lp (length pp)))
	      (if (> lp max-text) (setq max-text lp))
	      (setq
	       results
	       (append
          results
          (list (cons (format-time-string "%a, %d %b %Y %T %Z" (elfeed-score-maint--get-last-match-date rule)) pp))))))
    (with-current-buffer-window title nil nil
      (let ((fmt (format "%%28s: %%-%ds\n" max-text)))
	      (cl-dolist (x results)
	        (insert (format fmt (car x) (cdr x))))
        (special-mode)))))

(defun elfeed-score-maint--display-rules-by-match-hits (rules title)
  "Sort RULES in decreasing order of match hits; display results as TITLE."
  (let ((rules (elfeed-score-maint--sort-rules-by-hits rules))
	      (results '())
	      (max-text 0)
        (max-hits 0))
    (cl-dolist (rule rules)
      (let* ((pp (elfeed-score-rules-pp-rule-to-string rule))
	           (lp (length pp))
             (hits (elfeed-score-maint--get-hits rule)))
	      (if (> lp max-text) (setq max-text lp))
        (if (> hits max-hits) (setq max-hits hits))
	      (setq results (append results (list (cons hits pp))))))
    (with-current-buffer-window title nil nil
      (let ((fmt (format "%%%dd: %%-%ds\n" (ceiling (log max-hits 10)) max-text)))
	      (cl-dolist (x results)
	        (insert (format fmt (car x) (cdr x))))
        (special-mode)))))

(defun elfeed-score-maint--rules-for-keyword (key)
  "Retrieve the list of rules corresponding to keyword KEY."
  (cond
   ((eq key :title) elfeed-score-serde-title-rules)
   ((eq key :feed) elfeed-score-serde-feed-rules)
   ((eq key :content) elfeed-score-serde-content-rules)
   ((eq key :title-or-content) elfeed-score-serde-title-or-content-rules)
   ((eq key :authors) elfeed-score-serde-authors-rules)
   ((eq key :tag) elfeed-score-serde-tag-rules)
   ((eq key :adjust-tags) elfeed-score-serde-adjust-tags-rules)
   (t
    (error "Unknown keyword %S" key))))

(define-obsolete-function-alias
  'elfeed-score-display-rules-by-last-match
  #'elfeed-score-maint-display-rules-by-last-match
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-maint-display-rules-by-last-match (&optional category)
  "Display all scoring rules in descending order of last match.

CATEGORY may be used to narrow the scope of rules displayed.  If
nil, display all rules.  If one of the following symbols, display
only that category of rules:

    :title
    :feed
    :content
    :title-or-content
    :authors
    :tag
    :adjust-tags

Finally, CATEGORY may be a list of symbols in the preceding
list, in which case the union of the corresponding rule
categories will be displayed."

  (interactive)
  (let ((rules
	       (cond
	        ((not category)
	         (append elfeed-score-serde-title-rules elfeed-score-serde-feed-rules
		               elfeed-score-serde-content-rules
		               elfeed-score-serde-title-or-content-rules
		               elfeed-score-serde-authors-rules elfeed-score-serde-tag-rules
		               elfeed-score-serde-adjust-tags-rules))
	        ((symbolp category)
	         (elfeed-score-maint--rules-for-keyword category))
	        ((listp category)
	         (cl-loop for sym in category
		                collect (elfeed-score-maint--rules-for-keyword sym)))
	        (t
	         (error "Invalid argument %S" category)))))
    (elfeed-score-maint--display-rules-by-last-match rules "elfeed-score Rules by Last Match")))

(define-obsolete-function-alias
  'elfeed-score-display-rules-by-match-hits
  #'elfeed-score-maint-display-rules-by-match-hits
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-maint-display-rules-by-match-hits (&optional category)
  "Display all scoring rules in descending order of match hits.

CATEGORY may be used to narrow the scope of rules displayed.  If
nil, display all rules.  If one of the following symbols, display
only that category of rules:

    :title
    :feed
    :content
    :title-or-content
    :authors
    :tag
    :adjust-tags

Finally, CATEGORY may be a list of symbols in the preceding
list, in which case the union of the corresponding rule
categories will be displayed."

  (interactive)
  (let ((rules
	       (cond
	        ((not category)
	         (append elfeed-score-serde-title-rules elfeed-score-serde-feed-rules
		               elfeed-score-serde-content-rules
		               elfeed-score-serde-title-or-content-rules
		               elfeed-score-serde-authors-rules elfeed-score-serde-tag-rules
		               elfeed-score-serde-adjust-tags-rules))
	        ((symbolp category)
	         (elfeed-score-maint--rules-for-keyword category))
	        ((listp category)
	         (cl-loop for sym in category
		                collect (elfeed-score-maint--rules-for-keyword sym)))
	        (t
	         (error "Invalid argument %S" category)))))
    (elfeed-score-maint--display-rules-by-match-hits rules "elfeed-score Rules by Match Hits")))

(defcustom elfeed-score-maint-default-match-type 's
  "Default match type for interactively added rules.

Must be one of 's, 'S, 'r, 'R, 'w or 'W, for case-insensitive
substring match, case-sensitive substring, regexp or whole-word
match, respectively.  Set to nil to always prompt."
  :group 'elfeed-score
  :type '(choice (const s) (const S) (const r) (const R) (const w) (const W)))

(defcustom elfeed-score-maint-default-scope-to-feed 'no
  "Control whether intreractively added rules are scoped to the current feed.

Must  be one of 'yes, 'no, or 'ask."
  :group 'elfeed-score
  :type '(choice (const yes) (const no) (const ask)))

(defcustom elfeed-score-maint-default-scope-to-tags 'no
  "Control whether intreractively added rules are scoped to the current tagset.

Must  be one of 'yes, 'no, or 'ask."
  :group 'elfeed-score
  :type '(choice (const yes) (const no) (const ask)))

(defun elfeed-score-maint-add-title-rule (value &optional ignore-defaults _called-interactively)
  "Add a title rule with VALUE, possibly IGNORE-DEFAULTS.

Interactively add a new title rule based on the current Elfeed
entry.  This defun can be interactively invoked in a few ways:

    No prefix argument: the match value & text must be supplied
    interactively.  Other rule attributes will be gathered
    according to their corresponding \"default\" customization
    variables (on which more below).

    A numeric prefix argument will be interpreted as the match
    value; the match text must be supplied interactively.  Other
    rule attributes will be gathered according to their
    corresponding \"default\" customization variables (on which
    more below).

    One or more \\[C-]u prefix arguments; the match value & text
    must be supplied interactively. All defaults will be ignored
    & the other rule attributes must be interactively entered.

If called non-interactively, defaults will be respected, except
that anything set to 'ask will be interpreted as 'no.  The entry
title will be used as the match text.  Consider calling
`elfeed-score-serde-add-rule' directly, in that case."

  (interactive
   (append
    (cond
     ;; NB (listp nil) => t, so this conditional has to appear before listp
     ;; Could still be '-... what to do with that?
     ((or (not current-prefix-arg) (eq current-prefix-arg '-))
      (list
       (read-number "Value: " (prefix-numeric-value current-prefix-arg))
       nil))
     ((listp current-prefix-arg)
      (list
       (read-number "Value: " (prefix-numeric-value current-prefix-arg))
       t))
     ((integerp current-prefix-arg)
      (list current-prefix-arg nil)))
    (list (not (or executing-kbd-macro noninteractive)))))

  (if (elfeed-score-serde-score-file-dirty-p)
      (if (and _called-interactively
               (yes-or-no-p "The score file has been modified since last"
                            "loaded; reload now? "))
          (elfeed-score-serde-load-score-file elfeed-score-serde-score-file)))

  (let ((entry (or elfeed-show-entry (elfeed-search-selected t))))
    (unless entry
      (error "No Elfeed entry here?"))
    (let* ((title (elfeed-entry-title entry))
           (match-text
            (if _called-interactively
                (read-string "Match text: " title)
              title))
           (match-type
            (if (or ignore-defaults (not _called-interactively))
                elfeed-score-maint-default-match-type
              (completing-read
               "Match type: "
               '(("s" s) ("S" S) ("r" r) ("R" R) ("w" w) ("W" W)) nil t "s")))
           (scope-to-feed
            (if (or ignore-defaults (not _called-interactively))
                (eq elfeed-score-maint-default-scope-to-feed 'yes)
              (y-or-n-p "Scope this rule to this entry's feed? ")))
           (scope-to-tags
            (if (or ignore-defaults (not _called-interactively))
                (eq elfeed-score-maint-default-scope-to-tags 'yes)
              (cl-mapcar
               #'intern
               (split-string
                (read-from-minibuffer
	               "Scope by tags (clear to not scope): "
	               (string-join
		              (cl-mapcar
		               (lambda (x) (pp-to-string x))
		               (elfeed-entry-tags entry))
		              " "))))))
           (rule
             (elfeed-score-title-rule--create
              :text match-text
              :value value
              :type (intern match-type)
              :tags
              (if scope-to-tags
                  (cons t scope-to-tags))
              :feeds
              (if scope-to-feed
                  (cons
                   t
                   (list
                    (list 'u 'S (elfeed-entry-feed-id entry))))))))
      (elfeed-score-serde-add-rule rule)))
  (elfeed-score-scoring-score-search))

(provide 'elfeed-score-maint)
;;; elfeed-score-maint.el ends here
