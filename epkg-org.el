;;; epkg-org.el --- various Org mode utilities    -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;;; Commentary:

;; Epkg itself does not use the utilities defined here.  They are
;; only intended for third-party packages and the Emir tools used
;; to generate reports about the information in the Epkg database.

;;; Code:

(require 'epkg)

(defmacro epkg-with-org-header (header &rest body)
  (declare (indent defun))
  `(when-let (rows (progn ,@body))
     (let ((header ',header)
           (sort nil)
           (n 0) prev)
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
                         (mapcar (lambda (elt) (or elt "")) row))
                       (if sort
                           (cl-sort rows #'string< :key sort)
                         rows))))))

(defun epkg-org-link (name)
  (let ((pkg (epkg name)))
    (if-let (repopage (oref pkg repopage))
        (if-let ((user (oref pkg upstream-user))
                 (name (oref pkg upstream-name)))
            (format "[[%s][%s/%s]]" repopage user name)
          (format "[[%s]]" repopage))
      (when-let (homepage (oref pkg homepage))
        (cl-typecase pkg
          (epkg-gnu-package
           (format "[[%s][gnu:%s]]" homepage name))
          (epkg-nongnu-package
           (format "[[%s][nongnu:%s]]" homepage name))
          (t
           (format "[[%s]]" homepage)))))))

(defun melpa-org-link (name)
  (let ((rcp (melpa-get name)))
    (if-let (repopage (oref rcp repopage))
        (format "[[%s][%s]]" repopage (oref rcp repo))
      (oref rcp url))))

;;; _
(provide 'epkg-org)
;;; epkg-org.el ends here
