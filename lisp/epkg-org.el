;;; epkg-org.el --- Various Org mode utilities  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
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

;;; Commentary:

;; Epkg itself does not use the utilities defined here.  They are
;; only intended for third-party packages and the Emir tools used
;; to generate reports about the information in the Epkg database.

;;; Code:

(require 'epkg)

(defmacro epkg-with-org-header (header &rest body)
  (declare (indent defun)
           (obsolete epkg-add-org-header "Epkg 3.3.3"))
  `(epkg-add-org-header (progn ,@body) ',header))

(defun epkg-add-org-header (rows header)
  "Prepend HEADER to ROWS if the latter is non-nil.
Usage:

#+name: addheader
#+header: :var header=\"\" :var rows=\"\"
#+begin_src elisp :hlines yes :results none
  (epkg-add-org-header rows header)
#+end_src

#+begin_src emacs-lisp :post addheader(\\='(\"Column 1\" ...),*this*)
  (epkg-sql [:select ...])
#+end_src"
  (cond
   ((equal rows "") nil)
   (rows
    (let ((sort nil)
          (prev nil)
          (n 0))
      (when (numberp (car header))
        (setq sort (apply-partially (lambda (n r) (nth n r)) (car header)))
        (setq header (cdr header)))
      (dolist (row rows)
        (unless (equal (car row) prev)
          (cl-incf n))
        (setq prev (car row)))
      (append (list (cons (format "%s (%s)" (car header) n)
                          (cdr header)))
              (list 'hline)
              (mapcar (lambda (row)
                        (mapcar (##or % "") row))
                      (if sort
                          (cl-sort rows #'string< :key sort)
                        rows)))))))

(defun epkg-org-link (name)
  (let ((pkg (epkg name)))
    (if-let ((repopage (oref pkg repopage)))
        (if-let ((user (oref pkg upstream-user))
                 (name (oref pkg upstream-name)))
            (format "[[%s][%s/%s]]" repopage user name)
          (format "[[%s]]" repopage))
      (and-let* ((homepage (oref pkg homepage)))
        (cl-typecase pkg
          (epkg-gnu-package
           (format "[[%s][gnu:%s]]" homepage name))
          (epkg-nongnu-package
           (format "[[%s][nongnu:%s]]" homepage name))
          (t
           (format "[[%s]]" homepage)))))))

(defun melpa-org-link (name)
  (let ((rcp (epkg-get-recipe 'melpa name)))
    (if-let (repopage (oref rcp repopage))
        (format "[[%s][%s]]" repopage (oref rcp repo))
      (oref rcp url))))

;;; _
(provide 'epkg-org)
;;; epkg-org.el ends here
