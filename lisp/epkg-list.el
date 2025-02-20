;;; epkg-list.el --- List Epkg packages  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2025 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.epkg@jonas.bernoulli.dev>
;; Homepage: https://github.com/emacscollective/epkg
;; Keywords: tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;; This file contains code taken from GNU Emacs, which is
;; Copyright (C) 1976-2023 Free Software Foundation, Inc.

;;; Commentary:

;; The library implements support for listing packages in a buffer.

;;; Code:

(require 'epkg)
(require 'epkg-desc)

;;; Options

(defcustom epkg-list-exclude-types '(shelved)
  "Package types that are excluded from most package lists.

Most commands that list packages exclude any package whose
type matches one of the types listed here.  The command
`epkg-list-packages-of-type' does not respect this option,
and you can tell the other commands to ignore it as well
by using a prefix argument."
  :group 'epkg
  :type `(repeat (choice :tag "Type"
                         ,@(mapcar (##list 'const %)
                                   (closql--list-subabbrevs 'epkg-package)))))

(defcustom epkg-list-columns
  `(("Package"     25 t   nil name    epkg-list-format-name)
    ("Type"        12 t   nil class   nil)
    (,(if (char-displayable-p ?\x2605) "\x2605 " "*")
     4 epkg-list-sort-by-stars (:right-align t) stars nil)
    ("Melpa"
     7 epkg-list-sort-by-downloads (:right-align t) downloads nil)
    ("Description" 99 nil nil summary nil))
  "Slots displayed in the package menu.

The value is a list of columns.  Each element has the form
\(HEADER WIDTH SORT PROPS SLOT FORMAT).

HEADER is the string displayed in the header.
WIDTH is the width of the column.
SORT is a boolean or a function.  If it is t, then the column
  can be sorted alphanumerically, if it is nil then it can not.
  If it is a function then that is used as `sort's PREDICATE.
PROPS is an alist, supported keys are `:right-align'
  and `:pad-right'.
SLOT is an Epkg object slot or `type'.
FORMAT is a function.  It is called with one argument the slot
  value and has to return a representation of that.  If FORMAT
  is nil, then the value is inserted as-is.

If an elements SLOT is `downloads', then the respective SORT
should be `epkg-list-sort-by-downloads'.
If an elements SLOT is `stars', then the respective SORT
should be `epkg-list-sort-by-stars'."
  :group 'epkg
  :type `(repeat
          (list :tag "Column"
                (string  :tag "Header Label")
                (integer :tag "Column Width")
                (choice  :tag "Sort predicate"
                         (const :tag "don't sort" nil)
                         (const :tag "default" t)
                         function)
                (plist :tag "Properties"
                       :key-type (choice :tag "Property"
                                         (const :right-align)
                                         (const :pad-right)
                                         (symbol))
                       :value-type (sexp :tag "Value"))
                (choice  :tag "Slot symbol" ,@epkg--custom-slot-choices)
                (choice  :tag "Format value"
                         (const :tag "as is" nil)
                         (const epkg-list-format-name)
                         function))))

(defcustom epkg-list-mode-hook '(hl-line-mode)
  "Hook run after entering Epkg-List mode."
  :group 'epkg
  :type 'hook
  :options '(hl-line-mode))

(defface epkg-list-name
  '((t :inherit link :underline nil))
  "Face used on package names in the package list."
  :group 'epkg)

;;; Commands

;;;###autoload
(defun epkg-list-packages (&optional all)
  "Display a list of packages.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used."
  (interactive (list current-prefix-arg))
  (epkg--list-packages
   (epkg-sql [:select $i1 :from packages :where class :in $v2]
             (epkg--list-columns-vector)
             (epkg--list-where-class-in all))))

;;;###autoload
(defun epkg-list-matching-packages (pattern &optional all)
  "Display a list of packages whose name or summary matches PATTERN.
PATTERN should be a string with SQLite LIKE pattern syntax.
If it does not contain any wildcards ('%' or '_'), it will be
surrounded by '%' automatically.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used."
  (interactive (list (read-string "pattern: ") current-prefix-arg))
  (epkg--list-packages
   (epkg-sql [:select $i1 :from packages
              :where (or (like summary $s2)
                         (like name $s2))
              :and class :in $v3]
             (epkg--list-columns-vector)
             (intern (if (string-match-p "[%_]" pattern)
                         pattern
                       (concat "%" pattern "%")))
             (epkg--list-where-class-in all))))

;;;###autoload
(defun epkg-list-keyworded-packages (keyword &optional all)
  "Display a list of packages that have KEYWORD set.

Only keywords that are members of `finder-known-keywords' are
offered as completion candidates, but you can also enter other
keywords.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used."
  (interactive (list (intern (completing-read
                              "List packages with keyword: "
                              (progn (require 'finder nil t)
                                     (bound-and-true-p finder-known-keywords))))
                     current-prefix-arg))
  (epkg--list-packages
   (epkg-sql [:select $i1 :from [packages keywords]
              :where (= name package)
              :and (= keyword $s2)
              :and class :in $v3]
             (epkg--list-columns-vector)
             keyword
             (epkg--list-where-class-in all))))

;;;###autoload
(defun epkg-list-packages-by-author (author &optional all)
  "Display a list of packages authored or maintained by AUTHOR.

AUTHOR may be a name or an email address.  Packages whose
Author(s) or Maintainer(s) header keywords contain that author
are listed.

Respect option `epkg-list-exclude-types' unless a prefix argument
is used."
  (interactive (list (read-string "List packages by author: ")
                     current-prefix-arg))
  (epkg--list-packages
   (let ((emailp (string-search "@" author)))
     (epkg-sql [:select :distinct $i1
                :from [packages authors maintainers]
                :where (and (in class $v2)
                            (or (and (= authors:package packages:name)
                                     (= $i3 $s5))
                                (and (= maintainers:package packages:name)
                                     (= $i4 $s5))))]
               (epkg--list-columns-vector t)
               (epkg--list-where-class-in all)
               (if emailp 'authors:email 'authors:name)
               (if emailp 'maintainers:email 'maintainers:name)
               author))))

;;;###autoload
(defun epkg-list-packages-of-type (type)
  "Display a list of all packages of a certain type.

To list all packages of a certain type as well as its subtypes
use `TYPE*' instead of just `TYPE'."
  (interactive (list (epkg-read-type "List packages of type: " nil t)))
  (epkg--list-packages
   (epkg-sql [:select $i1 :from packages :where class :in $v2]
             (epkg--list-columns-vector)
             (closql-where-class-in
              (if (eq type 'all*)
                  'epkg-package-p
                (setq type (symbol-name type))
                (intern (if (string-suffix-p "*" type)
                            (format "epkg-%s-package--eieio-childp"
                                    (substring type 0 -1))
                          (format "epkg-%s-package-p" type))))))))

(defun epkg--list-packages (rows)
  (with-current-buffer (get-buffer-create "*Epkgs*")
    (epkg-list-mode)
    (setq tabulated-list-entries
          (mapcar (lambda (row)
                    (list (car row)
                          (vconcat
                           (cl-mapcar (lambda (val col)
                                        (if-let ((pp (nth 5 col)))
                                            (funcall pp val)
                                          (if val (format "%s" val) "")))
                                      row epkg-list-columns))))
                  rows))
    (tabulated-list-print)
    (switch-to-buffer (current-buffer))))

;;; Mode

(defvar-keymap epkg-list-mode-map
  :doc "Local keymap for Epkg-List mode buffers."
  :parent tabulated-list-mode-map
  "RET"       #'epkg-list-describe-package
  "C-c C-f"   #'epkg-find-file
  "C-c 4 C-f" #'epkg-find-file-other-window
  "C-c 5 C-f" #'epkg-find-file-other-frame)

(define-derived-mode epkg-list-mode tabulated-list-mode "Epkgs"
  "Major mode for browsing a list of packages."
  (when (boundp 'x-stretch-cursor)
    (setq-local x-stretch-cursor nil))
  (setq tabulated-list-padding 0)
  (setq tabulated-list-sort-key (cons "Package" nil))
  (setq tabulated-list-format
        (vconcat (mapcar (pcase-lambda (`(,label ,width ,pred ,props ,_ ,_))
                           `(,label ,width ,pred ,@props))
                         epkg-list-columns)))
  (tabulated-list-init-header))

;;; Utilities

(defun epkg-list-format-name (name)
  (list name
        'face 'epkg-list-name
        'follow-link t
        'action #'epkg-list-describe-package))

(defun epkg--list-columns-vector (&optional qualify)
  (let ((lst (mapcar (pcase-lambda (`(,_ ,_ ,_ ,_ ,slot ,_)) slot)
                     epkg-list-columns)))
    (vconcat (if qualify
                 (mapcar (##if (eq % 'name) 'packages:name %) lst)
               lst))))

(defun epkg--list-where-class-in (all)
  (closql-where-class-in
   (if all
       'epkg-package--eieio-childp
     (mapcar (##closql--expand-abbrev 'epkg-package %)
             (cl-set-difference (closql--list-subabbrevs 'epkg-package)
                                epkg-list-exclude-types)))))

(defvar-local epkg-list--download-column nil)

(defun epkg-list-sort-by-downloads (a b)
  (let ((col (or epkg-list--download-column
                 (setq epkg-list--download-column
                       (cl-position-if
                        (##eq (nth 2 %) 'epkg-list-sort-by-downloads)
                        (append tabulated-list-format nil))))))
    (> (or (ignore-errors (string-to-number (aref (cadr a) col))) 0)
       (or (ignore-errors (string-to-number (aref (cadr b) col))) 0))))

(defvar-local epkg-list--stars-column nil)

(defun epkg-list-sort-by-stars (a b)
  (let ((col (or epkg-list--stars-column
                 (setq epkg-list--stars-column
                       (cl-position-if
                        (##eq (nth 2 %) 'epkg-list-sort-by-stars)
                        (append tabulated-list-format nil))))))
    (> (or (ignore-errors (string-to-number (aref (cadr a) col))) 0)
       (or (ignore-errors (string-to-number (aref (cadr b) col))) 0))))

;;; _
(provide 'epkg-list)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("partial" . "llama--left-apply-partially")
;;   ("rpartial" . "llama--right-apply-partially"))
;; End:
;;; epkg-list.el ends here
