;;; elfeed-score-serde.el --- SERDE `elfeed-score'  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Michael Herstine <sp1ff@pobox.com>

;; Package-Requires: ((emacs "24.4") (elfeed "3.3.0"))
;; Version: 0.7.1
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

;; This package provides `elfeed-score' serialization &
;; deserialization facilities.

;;; Code:
(require 'elfeed-score-rules)

(define-obsolete-variable-alias 'elfeed-score/score-file
  'elfeed-score-score-file "0.2.0" "Move to standard-compliant naming.")

(define-obsolete-variable-alias
  'elfeed-score-score-file
  'elfeed-score-serde-score-file
  "0.7.0"
  "Refactoring elfeed-score.el.")

(defcustom elfeed-score-serde-score-file
  (concat (expand-file-name user-emacs-directory) "elfeed.score")
  "Location at which to persist scoring rules.

Set this to nil to disable automatic serialization &
deserialization of scoring rules."
  :group 'elfeed-score
  :type 'file)

(defvar elfeed-score-serde--title-rules nil
  "List of structs each defining a scoring rule for entry titles.")

(defvar elfeed-score-serde--feed-rules nil
  "List of structs each defining a scoring rule for entry feeds.")

(defvar elfeed-score-serde--authors-rules nil
  "List of structs each defining a scoring rule for entry authors.")

(defvar elfeed-score-serde--content-rules nil
  "List of structs each defining a scoring rule for entry content.")

(defvar elfeed-score-serde--title-or-content-rules nil
  "List of structs each defining a scoring rule for entry title or content.")

(defvar elfeed-score-serde--tag-rules nil
  "List of structs each defining a scoring rule for entry tags.")

(defvar elfeed-score-serde--score-mark nil
  "Score at or below which entries shall be marked as read.")

(defvar elfeed-score-serde--adjust-tags-rules nil
  "List of structs defining rules to be run after scoring to adjust entry tags based on score.")

(defun elfeed-score-serde--parse-title-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of title rules.

Each sub-list shall have the form (TEXT VALUE TYPE DATE TAGS HITS
FEEDS).  NB Over the course of successive score file versions,
new fields have been added at the end so as to maintain backward
compatibility (i.e. this function can be used to read all
versions of the title rule serialization format.)"
  (let (title-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-title-rule--create
                     :text  (nth 0 item)
                     :value (nth 1 item)
                     :type  (nth 2 item)
                     :date  (nth 3 item)
                     :tags  (nth 4 item)
                     :hits  (let ((hits (nth 5 item))) (or hits 0))
                     :feeds (nth 6 item))))
        (unless (member struct title-rules)
          (setq title-rules (append title-rules (list struct))))))
    title-rules))

(defun elfeed-score-serde--parse-feed-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of feed rules.

Each sub-list shall have the form (TEXT VALUE TYPE ATTR DATE TAGS
HITS).  NB Over the course of successive score file versions, new
fields have been added at the end so as to maintain backward
compatibility (i.e. this function can be used to read all
versions of the feed rule serialization format.)"
  (let (feed-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-feed-rule--create
                     :text  (nth 0 item)
                     :value (nth 1 item)
                     :type  (nth 2 item)
                     :attr  (nth 3 item)
                     :date  (nth 4 item)
                     :tags  (nth 5 item)
                     :hits  (let ((hits (nth 6 item))) (or hits 0)))))
        (unless (member struct feed-rules)
          (setq feed-rules (append feed-rules (list struct))))))
    feed-rules))

(defun elfeed-score-serde--parse-content-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of content rules.

Each sub-list shall have the form (TEXT VALUE TYPE DATE TAGS HITS
FEEDS).  NB Over the course of successive score file versions,
new fields have been added at the end so as to maintain backward
compatibility (i.e. this function can be used to read all
versions of the content rule serialization format.)"
  (let (content-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-content-rule--create
                     :text  (nth 0 item)
                     :value (nth 1 item)
                     :type  (nth 2 item)
                     :date  (nth 3 item)
                     :tags  (nth 4 item)
                     :hits  (let ((hits (nth 5 item))) (or hits 0))
                     :feeds (nth 6 item))))
        (unless (member struct content-rules)
          (setq content-rules (append content-rules (list struct))))))
    content-rules))

(defun elfeed-score-serde--parse-title-or-content-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of title-or-content rules.

Each sub-list shall have the form '(TEXT TITLE-VALUE
CONTENT-VALUE TYPE DATE TAGS HITS FEEDS).  NB Over the course of
successive score file versions, new fields have been added at the
end so as to maintain backward compatibility (i.e. this function
can be used to read all versions of the title-or-content rule
serialization format.)"
  (let (toc-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-title-or-content-rule--create
                     :text          (nth 0 item)
                     :title-value   (nth 1 item)
                     :content-value (nth 2 item)
                     :type          (nth 3 item)
                     :date          (nth 4 item)
                     :tags          (nth 5 item)
                     :hits          (let ((hits (nth 6 item))) (or hits 0))
                     :feeds         (nth 7 item))))
        (unless (member struct toc-rules)
          (setq toc-rules (append toc-rules (list struct))))))
    toc-rules))

(defun elfeed-score-serde--parse-authors-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of authors rules.

Each sub-list shall have the form '(TEXT VALUE TYPE DATE TAGS
HITS FEEDS).  NB Over the course of successive score file
versions, new fields have been added at the end so as to maintain
backward compatibility (i.e. this function can be used to read
all versions of the authors rule serialization format.)"
  (let (authors-rules)
    (dolist (item sexps)
      (let ((struct (elfeed-score-authors-rule--create
                     :text  (nth 0 item)
                     :value (nth 1 item)
                     :type  (nth 2 item)
                     :date  (nth 3 item)
                     :tags  (nth 4 item)
                     :hits  (let ((hits (nth 5 item))) (or hits 0))
                     :feeds (nth 6 item))))
        (unless (member struct authors-rules)
          (setq authors-rules (append authors-rules (list struct))))))
    authors-rules))

(defun elfeed-score-serde--parse-scoring-sexp-1 (sexp)
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
          (setq titles (elfeed-score-serde--parse-title-rule-sexps rest)))
         ((string= key "content")
          (setq content (elfeed-score-serde--parse-content-rule-sexps rest)))
         ((string= key "feed")
          (setq feeds (elfeed-score-serde--parse-feed-rule-sexps rest)))
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

(defun elfeed-score-serde--parse-scoring-sexp-2 (sexp)
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
          (setq titles (elfeed-score-serde--parse-title-rule-sexps rest)))
         ((string= key "content")
          (setq content (elfeed-score-serde--parse-content-rule-sexps rest)))
         ((string= key "feed")
          (setq feeds (elfeed-score-serde--parse-feed-rule-sexps rest)))
         ((string= key "title-or-content")
          (setq tocs (elfeed-score-serde--parse-title-or-content-rule-sexps rest)))
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

(defun elfeed-score-serde--parse-tag-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of tag rules."
  (let ((tag-rules))
    (dolist (item sexps)
      (let ((struct (elfeed-score-tag-rule--create
                     :tags  (nth 0 item)
                     :value (nth 1 item)
                     :date  (nth 2 item))))
        (unless (member struct tag-rules)
          (setq tag-rules (append tag-rules (list struct))))))
    tag-rules))

(defun elfeed-score-serde--parse-adjust-tags-rule-sexps (sexps)
  "Parse a list of lists SEXPS into a list of adjust-tags rules."
  (let ((adj-tag-rules))
    (dolist (item sexps)
      (let ((struct (elfeed-score-adjust-tags-rule--create
                     :threshold (nth 0 item)
                     :tags      (nth 1 item)
                     :date      (nth 2 item))))
        (unless (member struct adj-tag-rules)
          (setq adj-tag-rules (append adj-tag-rules (list struct))))))
    adj-tag-rules))

(defun elfeed-score-serde--parse-scoring-sexp-3 (sexp)
  "Interpret the S-expression SEXP as scoring rules version 3.

Parse version 3 of the scoring S-expression.  Return a property list
with the following keys:

    - :title : list of elfeed-score-title-rule structs
    - :content : list of elfeed-score-content-rule structs
    - :title-or-content: list of elfeed-score-title-or-content-rule
                         structs
    - :feed : list of elfeed-score-feed-rule structs
    - :tag : list of elfeed-score-tag-rule structs
    - :mark : score below which entries shall be marked read
    - :adjust-tags : list of elfeed-score-adjust-tags-rule structs"

  (let (mark titles feeds content tocs tags adj-tags)
    (dolist (raw-item sexp)
      (let ((key  (car raw-item))
	          (rest (cdr raw-item)))
	      (cond
         ((string= key "version")
          (unless (eq 3 (car rest))
            (error "Unsupported score file version %s" (car rest))))
	       ((string= key "title")
          (setq titles (elfeed-score-serde--parse-title-rule-sexps rest)))
         ((string= key "content")
          (setq content (elfeed-score-serde--parse-content-rule-sexps rest)))
         ((string= key "feed")
          (setq feeds (elfeed-score-serde--parse-feed-rule-sexps rest)))
         ((string= key "title-or-content")
          (setq tocs (elfeed-score-serde--parse-title-or-content-rule-sexps rest)))
         ((string= key "tag")
          (setq tags (elfeed-score-serde--parse-tag-rule-sexps rest)))
         ((string= key "adjust-tags")
          (setq adj-tags (elfeed-score-serde--parse-adjust-tags-rule-sexps rest)))
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
     :adjust-tags adj-tags
	   :feeds feeds
	   :titles titles
     :content content
     :title-or-content tocs
     :tag tags)))

(defun elfeed-score-serde--parse-scoring-sexp-4 (sexp)
  "Interpret the S-expression SEXP as scoring rules version 4.

Parse version 4 of the scoring S-expression.  Return a property list
with the following keys:

    - :title : list of elfeed-score-title-rule structs
    - :content : list of elfeed-score-content-rule structs
    - :title-or-content: list of elfeed-score-title-or-content-rule
                         structs
    - :feed : list of elfeed-score-feed-rule structs
    - :authors : list of elfeed-score-authors-rule-structs
    - :tag : list of elfeed-score-tag-rule structs
    - :mark : score below which entries shall be marked read
    - :adjust-tags : list of elfeed-score-adjust-tags-rule structs"

  (let (mark titles feeds content tocs authors tags adj-tags)
    (dolist (raw-item sexp)
      (let ((key  (car raw-item))
	          (rest (cdr raw-item)))
	      (cond
         ((string= key "version")
          (unless (or (eq 4 (car rest)) (eq 5 (car rest)))
            (error "Unsupported score file version %s" (car rest))))
	       ((string= key "title")
          (setq titles (elfeed-score-serde--parse-title-rule-sexps rest)))
         ((string= key "content")
          (setq content (elfeed-score-serde--parse-content-rule-sexps rest)))
         ((string= key "feed")
          (setq feeds (elfeed-score-serde--parse-feed-rule-sexps rest)))
         ((string= key "title-or-content")
          (setq tocs (elfeed-score-serde--parse-title-or-content-rule-sexps rest)))
	       ((string= key "authors")
          (setq authors (elfeed-score-serde--parse-authors-rule-sexps rest)))
         ((string= key "tag")
          (setq tags (elfeed-score-serde--parse-tag-rule-sexps rest)))
         ((string= key "adjust-tags")
          (setq adj-tags (elfeed-score-serde--parse-adjust-tags-rule-sexps rest)))
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
     :adjust-tags adj-tags
	   :feeds feeds
	   :titles titles
     :content content
     :title-or-content tocs
     :authors authors
     :tag tags)))

(defun elfeed-score-serde--parse-scoring-sexp (sexps)
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
           4))))
    ;; I use `cl-delete' instead of `assoc-delete-all' because the
    ;; latter would entail a dependency on Emacs 26.2, which I would
    ;; prefer not to do.
    (cl-delete "version" sexps :test 'equal :key 'car)
    (cl-delete 'version sexps :test 'equal :key 'car)
    (cond
     ((eq version 1)
      (elfeed-score-serde--parse-scoring-sexp-1 sexps))
     ((eq version 2)
      (elfeed-score-serde--parse-scoring-sexp-2 sexps))
     ((eq version 3)
      (elfeed-score-serde--parse-scoring-sexp-3 sexps))
     ((eq version 4)
      (elfeed-score-serde--parse-scoring-sexp-4 sexps))
     ((eq version 5)
      ;; This is not a typo-- I want to call
      ;; `elfeed-score-serde--parse-scoring-sexp-4' even when the score file
      ;; format is 5.  The difference in the two formats is that
      ;; version 5 adds new fields to the end of several rule types;
      ;; the first-level structure of the s-expression doesn't
      ;; change.  So if an old version of `elfeed-score' tried to read
      ;; version 5, it wouldn't encounter an error, it would just
      ;; silently ignore those new fields. I don't think this is what
      ;; the user would want (especially since their new attributes
      ;; would be lost the first time their old `elfeed-score' writes
      ;; out their scoring rules), so I bumped the version to 5 (to
      ;; keep old versions from even trying to read it) but I can
      ;; still use the same first-level parsing logic.
      (elfeed-score-serde--parse-scoring-sexp-4 sexps))
     (t
      (error "Unknown version %s" version)))))

(defun elfeed-score-serde--parse-score-file (score-file)
  "Parse SCORE-FILE.

Internal.  This is the core score file parsing routine.  Opens
SCORE-FILE, reads the contents as a Lisp form, and parses that
into a property list with the following properties:

    - :content
    - :feeds
    - :authors
    - :mark
    - :titles
    - :adjust-tags
    - :title-or-content
    - :tags"

  (let ((sexp
         (car
		      (read-from-string
		       (with-temp-buffer
			       (insert-file-contents score-file)
			       (buffer-string))))))
    (elfeed-score-serde--parse-scoring-sexp sexp)))

(define-obsolete-function-alias 'elfeed-score/write-score-file
  #'elfeed-score-serde-write-score-file "0.2.0" "Move to standard-compliant naming.")

(define-obsolete-function-alias
  'elfeed-score-write-score-file
  #'elfeed-score-serde-write-score-file
  "0.7.0"
  "Re-factoring elfeed-score.el.")

(defun elfeed-score-serde-write-score-file (score-file)
  "Write the current scoring rules to SCORE-FILE."
  (interactive
   (list
    (read-file-name "score file: " nil elfeed-score-score-file t
                    elfeed-score-score-file)))
  (write-region
   (format
    ";;; Elfeed score file                                     -*- lisp -*-\n%s"
    (let ((print-level nil)
          (print-length nil))
	    (pp-to-string
	     (list
	      (list 'version 5)
        (append
         '("title")
	       (mapcar
	        (lambda (x)
            (list
             (elfeed-score-title-rule-text  x)
             (elfeed-score-title-rule-value x)
             (elfeed-score-title-rule-type  x)
             (elfeed-score-title-rule-date  x)
             (elfeed-score-title-rule-tags  x)
             (elfeed-score-title-rule-hits  x)
             (elfeed-score-title-rule-feeds x)))
	        elfeed-score-serde--title-rules))
        (append
         '("content")
	       (mapcar
	        (lambda (x)
            (list
		         (elfeed-score-content-rule-text  x)
		         (elfeed-score-content-rule-value x)
		         (elfeed-score-content-rule-type  x)
		         (elfeed-score-content-rule-date  x)
             (elfeed-score-content-rule-tags  x)
             (elfeed-score-content-rule-hits  x)
             (elfeed-score-content-rule-feeds x)))
	        elfeed-score-serde--content-rules))
        (append
         '("title-or-content")
         (mapcar
          (lambda (x)
            (list
             (elfeed-score-title-or-content-rule-text x)
             (elfeed-score-title-or-content-rule-title-value x)
             (elfeed-score-title-or-content-rule-content-value x)
             (elfeed-score-title-or-content-rule-type x)
             (elfeed-score-title-or-content-rule-date x)
             (elfeed-score-title-or-content-rule-tags x)
             (elfeed-score-title-or-content-rule-hits x)
             (elfeed-score-title-or-content-rule-feeds x)))
          elfeed-score-serde--title-or-content-rules))
        (append
         '("tag")
         (mapcar
          (lambda (x)
            (list
             (elfeed-score-tag-rule-tags  x)
             (elfeed-score-tag-rule-value x)
             (elfeed-score-tag-rule-date  x)
             (elfeed-score-tag-rule-hits  x)))
          elfeed-score-serde--tag-rules))
        (append
         '("authors")
	       (mapcar
	        (lambda (x)
            (list
             (elfeed-score-authors-rule-text  x)
             (elfeed-score-authors-rule-value x)
             (elfeed-score-authors-rule-type  x)
             (elfeed-score-authors-rule-date  x)
             (elfeed-score-authors-rule-tags  x)
             (elfeed-score-authors-rule-hits  x)
             (elfeed-score-authors-rule-feeds x)))
	        elfeed-score-serde--authors-rules))
        (append
         '("feed")
	       (mapcar
	        (lambda (x)
            (list
		         (elfeed-score-feed-rule-text  x)
		         (elfeed-score-feed-rule-value x)
		         (elfeed-score-feed-rule-type  x)
             (elfeed-score-feed-rule-attr  x)
		         (elfeed-score-feed-rule-date  x)
             (elfeed-score-feed-rule-tags  x)
             (elfeed-score-feed-rule-hits  x)))
	        elfeed-score-serde--feed-rules))
        (list 'mark elfeed-score-serde--score-mark)
        (append
         '("adjust-tags")
         (mapcar
          (lambda (x)
            (list
             (elfeed-score-adjust-tags-rule-threshold x)
             (elfeed-score-adjust-tags-rule-tags      x)
             (elfeed-score-adjust-tags-rule-date      x)
             (elfeed-score-adjust-tags-rule-hits      x)))
          elfeed-score-serde--adjust-tags-rules))))))
   nil score-file))

(defun elfeed-score-serde--load-score-file (score-file)
  "Load SCORE-FILE into our internal scoring rules.

Internal.  Read SCORE-FILE, store scoring rules in our internal datastructures,"

  (let ((score-entries (elfeed-score-serde--parse-score-file score-file)))
    (setq elfeed-score-serde--title-rules             (plist-get score-entries :titles)
          elfeed-score-serde--feed-rules              (plist-get score-entries :feeds)
          elfeed-score-serde--content-rules           (plist-get score-entries :content)
          elfeed-score-serde--title-or-content-rules  (plist-get score-entries :title-or-content)
          elfeed-score-serde--tag-rules               (plist-get score-entries :tag)
          elfeed-score-serde--authors-rules           (plist-get score-entries :authors)
          elfeed-score-serde--score-mark              (plist-get score-entries :mark)
          elfeed-score-serde--adjust-tags-rules       (plist-get score-entries :adjust-tags))))

(provide 'elfeed-score-serde)
;;; elfeed-score-serde.el ends here
