;;; elfeed-score.el --- Gnus-style scoring for Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Michael Herstine <sp1ff@pobox.com>

;; Author: Michael Herstine <sp1ff@pobox.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "24") (elfeed "3.3.0"))
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

(defun elfeed-score--parse-score-file (score-file)
  "Parse SCORE-FILE.

Internal.  This is the core score file parsing routine.  Opens
SCORE-FILE, reads the contents as a Lisp form, and parses that
into a property list with the following properties:

    - :content
    - :feeds
    - :mark
    - :titles"

  (let ((raw-entries
         (car
		      (read-from-string
		       (with-temp-buffer
			       (insert-file-contents score-file)
			       (buffer-string)))))
	      mark titles feeds content)
    (dolist (raw-item raw-entries)
      (let ((key  (car raw-item))
	          (rest (cdr raw-item)))
	      (cond
         ((string= key "version")
          (unless (eq 1 (car rest))
            (error "Unsupported score file version %s" (car rest))))
	       ((string= key "title")
          (dolist (item rest)
            (let ((item-plist (list
                               :text  (nth 0 item)
                               :value (nth 1 item)
                               :type  (nth 2 item)
                               :date  (nth 3 item))))
              (unless (member item-plist titles)
                (setq titles (append titles (list item-plist)))))))
         ((string= key "content")
          (dolist (item rest)
            (let ((item-plist (list
                               :text  (nth 0 item)
                               :value (nth 1 item)
                               :type  (nth 2 item)
                               :date  (nth 3 item))))
              (unless (member item-plist titles)
                (setq content (append content (list item-plist)))))))
         ((string= key "feed")
          (dolist (item rest)
            (let ((item-plist (list
                               :text  (nth 0 item)
                               :value (nth 1 item)
                               :type  (nth 2 item)
                               :attr  (nth 3 item)
                               :date  (nth 4 item))))
              (unless (member item-plist feeds)
                (setq feeds (append feeds (list item-plist)))))))
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

(defvar elfeed-score--score-titles nil
  "List of property lists each defining a scoring rule for entry titles.

The properties are:

    - :text :: The rule's match text; either a string or a regular
               expression (on which more below)
    - :value :: Integral value (positive or negative) to be added to
                an entry's score if this rule matches
    - :type :: (optional) One of the symbols s S r R; s/r denotes
               substring/regexp match; lower-case means case-insensitive
               and upper case sensitive.  Defaults to r (case-insensitive
               regexp match)
    - :date :: time (in seconds since epoch) when this rule last matched")

(defvar elfeed-score--score-feeds nil
  "List of property lists each defining a scoring rule for entry feeds.

The properties are:

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
    - :date :: time (in seconds since epoch) when this rule last matched")

(defvar elfeed-score--score-content nil
    "List of property lists each defining a scoring rule for entry content.

The properties are:

    - :text :: The rule's match text; either a string or a regular
               expression (on which more below)
    - :value :: Integral value (positive or negative) to be added to
                an entry's score if this rule matches
    - :type :: (optional) One of the symbols s S r R; s/r denotes
               substring/regexp match; lower-case means case-insensitive
               and upper case sensitive.  Defaults to r (case-insensitive
               regexp match)
    - :date :: time (in seconds since epoch) when this rule last matched")

(defvar elfeed-score--score-mark nil
  "Score at or below which entries shall be marked as read.")

(defun elfeed-score--load-score-file (score-file)
  "Load SCORE-FILE into our internal scoring rules.

Internal.  Read SCORE-FILE, store scoring rules in our internal datastructures,"

  (let ((score-entries (elfeed-score--parse-score-file score-file)))
    (setq elfeed-score--score-titles  (plist-get score-entries :titles)
          elfeed-score--score-feeds   (plist-get score-entries :feeds)
          elfeed-score--score-content (plist-get score-entries :content)
          elfeed-score--score-mark    (plist-get score-entries :mark))))

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
   (t
    (error "Unknown match type %s" match-type))))

(defun elfeed-score--score-entry (entry)
  "Score an Elfeed ENTRY.

This function will return the entry's score, udpate it's meta-data, and
udpate the \"last matched\" time of the salient rules."

  (let ((title   (elfeed-entry-title entry))
        (feed    (elfeed-entry-feed  entry))
        (content (elfeed-deref (elfeed-entry-content entry)))
	      (score   elfeed-score-default-score))
    ;; score on the entry title
	  (dolist (score-title elfeed-score--score-titles)
	    (let* ((match-text (plist-get score-title :text))
		         (value      (plist-get score-title :value))
		         (match-type (plist-get score-title :type))
             (got-match (elfeed-score--match-text match-text title match-type)))
        (if got-match
            (progn
              (elfeed-score--debug "'%s' + %d (title)" title value)
		          (setq score (+ score value))
		          (plist-put score-title :date (float-time))))))
    ;; score on the entry feed
    (dolist (score-feed elfeed-score--score-feeds)
	    (let* ((match-text (plist-get score-feed :text))
		         (value      (plist-get score-feed :value))
		         (match-type (plist-get score-feed :type))
             (attr       (plist-get score-feed :attr))
             (feed-text (cond
                         ((eq attr 't)
                          (elfeed-feed-title feed))
                         ((eq attr 'u)
                          (elfeed-feed-url feed))
                         ((eq attr 'a)
                          (elfeed-feed-author feed))
                         (t
                          (error "Unknown feed attribute %s" attr))))
             (got-match (elfeed-score--match-text match-text feed-text match-type)))
        (if got-match
            (progn
              (elfeed-score--debug "%s + %d (feed)" title value)
		          (setq score (+ score value))
		          (plist-put score-feed :date (float-time))))))
    ;; score on the entry content
    (if content
        (dolist (score-content elfeed-score--score-content)
	        (let* ((match-text   (plist-get score-content :text))
		             (value        (plist-get score-content :value))
		             (match-type   (plist-get score-content :type))
                 (got-match    (elfeed-score--match-text match-text
                                                         content match-type)))
            (if got-match
                (progn
                  (elfeed-score--debug "%s + %d (content)" title value)
		              (setq score (+ score value))
		              (plist-put score-content :date (float-time)))))))
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
	    (list "version" 1)
      (append
       '("title")
	     (mapcar
	      (lambda (x)
		      (list
		       (plist-get x :text)
		       (plist-get x :value)
		       (plist-get x :type)
		       (plist-get x :date)))
	      elfeed-score--score-titles))
      (append
       '("content")
	     (mapcar
	      (lambda (x)
		      (list
		       (plist-get x :text)
		       (plist-get x :value)
		       (plist-get x :type)
		       (plist-get x :date)))
	      elfeed-score--score-content))
      (append
       '("feed")
	     (mapcar
	      (lambda (x)
		      (list
		       (plist-get x :text)
		       (plist-get x :value)
		       (plist-get x :type)
           (plist-get x :attr)
		       (plist-get x :date)))
	      elfeed-score--score-feeds))
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
