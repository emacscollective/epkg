;;; epkg-melpa.el --- Melpa recipes               -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU GPL see http://www.gnu.org/licenses.

;; This file is not part of GNU Emacs.

;;; Code:

(require 'epkg)

;;; Superclass

(defclass melpa-recipe (closql-object)
  ((closql-table         :initform 'melpa-recipes)
   (closql-primary-key   :initform 'name)
   (closql-foreign-key   :initform 'epkg-package)
   (closql-class-prefix  :initform "melpa-")
   (closql-class-suffix  :initform "-recipe")
   (url-format           :initform nil :allocation :class)
   (repopage-format      :initform nil :allocation :class)
   (name                 :initform nil :initarg :name)
   (url                  :initform nil)
   (repo                 :initform nil)
   (repopage             :initform nil)
   (files                :initform nil)
   (branch               :initform nil)
   (commit               :initform nil)
   (module               :initform nil) ; obsolete
   (version-regexp       :initform nil)
   (old-names            :initform nil)
   (epkg-package         :initform nil))
  :abstract t)

;;; Subclasses

(defclass melpa-git-recipe (melpa-recipe) ())

(defclass melpa-github-recipe (melpa-git-recipe)
  ((url-format      :initform "git@github.com:%r.git")
   (repopage-format :initform "https://github.com/%r")))

(defclass melpa-gitlab-recipe (melpa-git-recipe)
  ((url-format      :initform "git@gitlab.com:%r.git")
   (repopage-format :initform "https://gitlab.com/%r")))

(defclass melpa-hg-recipe (melpa-recipe) ())

(defclass melpa-bitbucket-recipe (melpa-hg-recipe)
  ((url-format      :initform "hg::ssh://hg@bitbucket.org/%r")
   (repopage-format :initform "https://bitbucket.org/%r")))

;;; Interfaces

(defun melpa-recipes (&optional select predicates)
  (closql-query (epkg-db) select predicates 'melpa-recipe))

(defun melpa-get (name)
  (closql-get (epkg-db) name 'melpa-recipe))

;;; Utilities

(defun melpa-json-recipes ()
  (json-encode (mapcar #'melpa--recipe-plist (melpa-recipes))))

(defun melpa--recipe-plist (rcp)
  (let ((type (melpa--recipe-type rcp)))
    `(,(intern (oref rcp name))
      :fetcher ,type
      ,@(if (memq type '(git hg))
            (list :url (oref rcp url))
          (list :repo (oref rcp repo)))
      ,@(cl-mapcan (lambda (slot)
                     (when-let ((value (eieio-oref rcp slot)))
                       (list (intern (format ":%s" slot)) value)))
                   '(files branch commit version-regexp old-names)))))

(defun melpa--recipe-type (rcp)
  (intern (substring (symbol-name (eieio-object-class-name rcp)) 6 -7)))

;;; _
(provide 'epkg-melpa)
;;; epkg-melpa.el ends here
