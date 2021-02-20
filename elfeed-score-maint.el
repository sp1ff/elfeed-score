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

(defun elfeed-score-maint--get-last-match-date (rule)
  "Retrieve the time at which RULE was last matched.

Return the time, in seconds since epoch, at which RULE was most
recently matched against an entry (floating point).  Note that
RULE may be any rule struct."

  (let ((date
	 (cl-typecase rule
	   (elfeed-score-title-rule
	    (elfeed-score-title-rule-date rule))
	   (elfeed-score-feed-rule
	    (elfeed-score-feed-rule-date rule))
	   (elfeed-score-content-rule
	    (elfeed-score-content-rule-date rule))
	   (elfeed-score-title-or-content-rule
	    (elfeed-score-title-or-content-rule-date rule))
	   (elfeed-score-authors-rule
	    (elfeed-score-authors-rule-date rule))
	   (elfeed-score-tag-rule
	    (elfeed-score-tag-rule-date rule))
	   (elfeed-score-adjust-tags-rule
	    (elfeed-score-adjust-tags-rule-date rule))
	   (otherwise (error "Unknown rule type %S" rule)))))
    (or date 0.0)))

(defun elfeed-score-maint--get-hits (rule)
  "Retrieve the number of times RULE has matched an entry.

Note that RULE may be an instance of any rule structure."

  (let ((hits
         (cl-typecase rule
	         (elfeed-score-title-rule
	          (elfeed-score-title-rule-hits rule))
	         (elfeed-score-feed-rule
	          (elfeed-score-feed-rule-hits rule))
	         (elfeed-score-content-rule
	          (elfeed-score-content-rule-hits rule))
	         (elfeed-score-title-or-content-rule
	          (elfeed-score-title-or-content-rule-hits rule))
	         (elfeed-score-authors-rule
	          (elfeed-score-authors-rule-hits rule))
	         (elfeed-score-tag-rule
	          (elfeed-score-tag-rule-hits rule))
	         (elfeed-score-adjust-tags-rule
	          (elfeed-score-adjust-tags-rule-hits rule))
	         (otherwise (error "Unknown rule type %S" rule)))))
    (or hits 0)))

(defun elfeed-score-maint--sort-rules-by-last-match (rules)
  "Sort RULES in decreasing order of last match.

Note that RULES need not be homogeneous; it may contain rule
structs of any kind understood by
`elfeed-score-maint--get-last-match-date'."
  (sort
   rules
   (lambda (lhs rhs)
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

(provide 'elfeed-score-maint)
;;; elfeed-score-maint.el ends here
