;;; test-struct-serde.el -- ERT tests for CL struct serde  -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2025 Michael Herstine <sp1ff@pobox.com>

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

;; This file strictly tests `elfeed-score-serde-struct-to-plist' &
;; `elfeed-score-plist-to-struct'.

;;; Code:
(require 'cl-lib)
(require 'ert)
(require 'elfeed-score-serde)

(ert-deftest test-struct-serde-defaults ()
  "Test serialization of defaulted slots."

  (cl-defstruct person-with-defs
    name (age 0) (sex 'male))

  (let* ((x (make-person-with-defs :age 11))
         (plist (elfeed-score-serde-struct-to-plist x)))
    (should (equal plist '(:age 11)))))

(ert-deftest test-struct-serde-smoke ()
  "Smoke test (de)serializing  CL structs."

  (cl-defstruct person name age sex)

  (let* ((dave (make-person :name "Dave" :sex 'male))
         (plist (elfeed-score-serde-struct-to-plist dave))
         (clone (elfeed-score-serde-plist-to-struct plist :type 'person)))
    (should (person-p clone))
    (should (eq (person-name dave) (person-name clone))))

  (cl-defstruct (careful-person (:type vector) (:initial-offset 2) :named)
    (name nil :read-only t :type 'string)
    age
    (sex 'unknown))

  (let ((karen (make-careful-person :name "Karen" :age 51)))
    (setf (aref karen 1) 11)
    (let* ((plist (elfeed-score-serde-struct-to-plist karen :type 2))
           (clone (elfeed-score-serde-plist-to-struct plist :type 'careful-person)))
      (should (careful-person-p clone))
      (should (string= "Karen" (careful-person-name clone)))
      (should (eq 51 (careful-person-age clone)))
      (should (eq 'unknown (careful-person-sex clone)))
      (should (eq 11 (aref clone 1)))
      (should (not (aref clone 0)))))

  (cl-defstruct (astronaut (:include careful-person (age 45)))
    helmet-size
    (favorite-beverage 'tang))

  (let ((buzz (make-astronaut :name "Buzz")))
    (let* ((plist (elfeed-score-serde-struct-to-plist
                   buzz :tag-type t :type 'astronaut))
           (clone (elfeed-score-serde-plist-to-struct plist :type 'astronaut)))
      (should (astronaut-p clone))
      (should (eq 45 (astronaut-age clone)))))

  (cl-defstruct (un-named-struct (:type list))
    x y z)

  (let* ((s (make-un-named-struct :x 1 :y 2 :z 3))
         (plist (elfeed-score-serde-struct-to-plist
                 s :type 'un-named-struct :type-tag t)))
    (should (plist-get plist :_type))))

(provide 'test-struct-serde)
;;; test-struct-serde.el ends here
