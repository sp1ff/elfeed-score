;;; elfeed-score-log-tests.el --- ERT tests for elfeed-score logging   -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2025 Michael Herstine <sp1ff@pobox.com>

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

;; None.

;;; Code:

(require 'elfeed-score)

(defun elfeed-score-log-tests--log-buffer-text ()
  "Retrieve the contents of the log buffer as a string."
  (with-current-buffer (elfeed-score-log-buffer)
    (buffer-string)))

(defun elfeed-score-log-tests--log-buffer-lines ()
  "Return the number of lines in the log buffer."
  (with-current-buffer (elfeed-score-log-buffer)
    (save-excursion
      (goto-char (point-max))
      (line-number-at-pos))))

(ert-deftest elfeed-score-log-tests-smoke ()
  "Smoke tests for `elfeed-score' logging."

  (kill-buffer (elfeed-score-log-buffer))
  (should (not (get-buffer elfeed-score-log-buffer-name)))
  (let ((elfeed-score-log-level 'warn))
    (elfeed-score-log 'info "Hello, world!")
    (should (not (get-buffer elfeed-score-log-buffer-name)))
    (elfeed-score-log 'warn "Hello, world!")
    (should (get-buffer elfeed-score-log-buffer-name))))

(ert-deftest elfeed-score-log-tests-truncate ()
  "Test truncation for the log buffer."

  (kill-buffer (elfeed-score-log-buffer))
  (let ((elfeed-score-log-level 'info)
        (elfeed-score-log-max-buffer-size 2))
    (elfeed-score-log 'info "line 1")
    (should
     (eq
      0
      (string-match
       "^\\[[-0-9: ]\\{19\\}\\] \\[info\\]: line 1
$"
       (elfeed-score-log-tests--log-buffer-text))))
    (should (eq (elfeed-score-log-tests--log-buffer-lines) 2))
    (elfeed-score-log 'info "line 2")
    (should
     (eq
      0
      (string-match
       "^\\[[-0-9: ]\\{19\\}\\] \\[info\\]: line 1
\\[[-0-9: ]\\{19\\}\\] \\[info\\]: line 2
$"
       (elfeed-score-log-tests--log-buffer-text))))
    (should (eq (elfeed-score-log-tests--log-buffer-lines) 3))
    (elfeed-score-log 'info "line 3")
    (should
     (eq
      0
      (string-match
       "^\\[[-0-9: ]\\{19\\}\\] \\[info\\]: line 2
\\[[-0-9: ]\\{19\\}\\] \\[info\\]: line 3
$"
       (elfeed-score-log-tests--log-buffer-text))))
    (message "buffer: ``%s''" (elfeed-score-log-tests--log-buffer-text))
    (should (eq (elfeed-score-log-tests--log-buffer-lines) 3))))

(provide 'elfeed-score-log-tests)

;;; elfeed-score-log-tests.el ends here
