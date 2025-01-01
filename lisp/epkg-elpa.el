;;; epkg-elpa.el --- Support for Elpa recipes  -*- lexical-binding:t -*-

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

;;; Commentary:

;; Support for querying GNU Elpa, NonGNU Elpa and Melpa recipes.

;;; Code:

(require 'epkg)
(require 'json)

;;; Common

(defclass epkg--platform-recipe () () :abstract t)

(defconst epkg--elpa-recipe-slots
  '( url core
     main-file lisp-dir ignored-files excludes renames
     doc readme news
     shell-command make texinfo
     auto-sync merge branch release-branch rolling-release version-map
     maintainer ; used for a single package
     manual-sync ; whether the package is synced manually
     ))

;;; GNU Elpa

(defclass epkg-gnu-elpa-recipe (closql-object)
  ((closql-table         :initform 'gnu-elpa-recipes)
   (closql-primary-key   :initform 'name)
   (closql-foreign-key   :initform 'epkg-package)
   (closql-class-prefix  :initform "epkg-gnu-elpa-")
   (closql-class-suffix  :initform "-recipe")
   (url-format           :initform nil :allocation :class)
   (repopage-format      :initform nil :allocation :class)
   (name                 :initform nil :initarg :name)
   (released             :initform nil)
   ;; See [[file:~/src/emacs/elpa/gnu/README::*Specifications (elpa-packages)][documentation in GNU Elpa's README]].
   ;; Some are not documented there:
   ;; - auto-sync
   ;; - excludes: If core includes directories, they are copied
   ;;   recursively.  This allows excluding some of these files.
   ;; - rolling-release
   ;; - texinfo
   (url                  :initform nil)
   (core                 :initform nil)
   (main-file            :initform nil)
   (lisp-dir             :initform nil)
   (ignored-files        :initform nil)
   (excludes             :initform nil)
   (renames              :initform nil)
   (doc                  :initform nil)
   (readme               :initform nil)
   (news                 :initform nil)
   (shell-command        :initform nil)
   (make                 :initform nil)
   (texinfo              :initform nil)
   (auto-sync            :initform nil)
   (merge                :initform nil)
   (branch               :initform nil)
   (release-branch       :initform nil)
   (rolling-release      :initform nil)
   (version-map          :initform nil)
   (epkg-package         :initform nil))
  :abstract t)

(defclass epkg-gnu-elpa-core-recipe (epkg-gnu-elpa-recipe) ())

(defclass epkg-gnu-elpa-internal-recipe (epkg-gnu-elpa-recipe) ())

(defclass epkg-gnu-elpa-external-recipe (epkg-gnu-elpa-recipe) ())

(defclass epkg-gnu-elpa-hg-recipe (epkg-gnu-elpa-external-recipe) ())

(defclass epkg-gnu-elpa-git-recipe (epkg-gnu-elpa-external-recipe) ())

(defclass epkg-gnu-elpa-github-recipe
  (epkg-gnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://github.com/%r")
   (repopage-format :initform "https://github.com/%r")))

(defclass epkg-gnu-elpa-gitlab-recipe
  (epkg-gnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://gitlab.com/%r")
   (repopage-format :initform "https://gitlab.com/%r")))

(defclass epkg-gnu-elpa-codeberg-recipe
  (epkg-gnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://codeberg.org/%r")
   (repopage-format :initform "https://codeberg.org/%r")))

(defclass epkg-gnu-elpa-sourcehut-recipe
  (epkg-gnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://git.sr.ht/~%r")
   (repopage-format :initform "https://git.sr.ht/~%r")))

(defclass epkg-gnu-elpa-gnu-recipe
  (epkg-gnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://git.savannah.gnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/%n.git")))

(defclass epkg-gnu-elpa-nongnu-recipe
  (epkg-gnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://git.savannah.nongnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.nongnu.org/cgit/%n.git")))

;;; NonGNU Elpa

(defclass epkg-nongnu-elpa-recipe (closql-object)
  ((closql-table         :initform 'nongnu-elpa-recipes)
   (closql-primary-key   :initform 'name)
   (closql-foreign-key   :initform 'epkg-package)
   (closql-class-prefix  :initform "epkg-nongnu-elpa-")
   (closql-class-suffix  :initform "-recipe")
   (url-format           :initform nil :allocation :class)
   (repopage-format      :initform nil :allocation :class)
   (name                 :initform nil :initarg :name)
   (released             :initform nil)
   (url                  :initform nil)
   (main-file            :initform nil)
   (lisp-dir             :initform nil)
   (ignored-files        :initform nil)
   (excludes             :initform nil)
   (renames              :initform nil)
   (doc                  :initform nil)
   (readme               :initform nil)
   (news                 :initform nil)
   (shell-command        :initform nil)
   (make                 :initform nil)
   (texinfo              :initform nil)
   (auto-sync            :initform nil)
   (merge                :initform nil)
   (branch               :initform nil)
   (release-branch       :initform nil)
   (rolling-release      :initform nil)
   (version-map          :initform nil)
   (epkg-package         :initform nil))
  :abstract t)

(defclass epkg-nongnu-elpa-internal-recipe (epkg-nongnu-elpa-recipe) ())

(defclass epkg-nongnu-elpa-external-recipe (epkg-nongnu-elpa-recipe) ())

(defclass epkg-nongnu-elpa-hg-recipe (epkg-nongnu-elpa-external-recipe) ())

(defclass epkg-nongnu-elpa-git-recipe (epkg-nongnu-elpa-external-recipe) ())

(defclass epkg-nongnu-elpa-github-recipe
  (epkg-nongnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://github.com/%r")
   (repopage-format :initform "https://github.com/%r")))

(defclass epkg-nongnu-elpa-gitlab-recipe
  (epkg-nongnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://gitlab.com/%r")
   (repopage-format :initform "https://gitlab.com/%r")))

(defclass epkg-nongnu-elpa-codeberg-recipe
  (epkg-nongnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://codeberg.org/%r")
   (repopage-format :initform "https://codeberg.org/%r")))

(defclass epkg-nongnu-elpa-sourcehut-recipe
  (epkg-nongnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://git.sr.ht/~%r")
   (repopage-format :initform "https://git.sr.ht/~%r")))

(defclass epkg-nongnu-elpa-gnu-recipe
  (epkg-nongnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://git.savannah.gnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/%n.git")))

(defclass epkg-nongnu-elpa-nongnu-recipe
  (epkg-nongnu-elpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://git.savannah.nongnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.nongnu.org/cgit/%n.git")))

;;; Melpa

(defclass epkg-melpa-recipe (closql-object)
  ((closql-table         :initform 'melpa-recipes)
   (closql-primary-key   :initform 'name)
   (closql-foreign-key   :initform 'epkg-package)
   (closql-class-prefix  :initform "epkg-melpa-")
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
   (version-regexp       :initform nil)
   (old-names            :initform nil)
   (epkg-package         :initform nil))
  :abstract t)

(defclass epkg-melpa-hg-recipe (epkg-melpa-recipe) ())

(defclass epkg-melpa-git-recipe (epkg-melpa-recipe) ())

(defclass epkg-melpa-github-recipe
  (epkg-melpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://github.com/%r")
   (repopage-format :initform "https://github.com/%r")))

(defclass epkg-melpa-gitlab-recipe
  (epkg-melpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://gitlab.com/%r")
   (repopage-format :initform "https://gitlab.com/%r")))

(defclass epkg-melpa-codeberg-recipe
  (epkg-melpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://codeberg.org/%r")
   (repopage-format :initform "https://codeberg.org/%r")))

(defclass epkg-melpa-sourcehut-recipe
  (epkg-melpa-git-recipe epkg--platform-recipe)
  ((url-format      :initform "https://git.sr.ht/~%r")
   (repopage-format :initform "https://git.sr.ht/~%r")))

;;; Interfaces

(defun epkg-list-recipes (elpa &optional select types)
  "Return a list of recipe objects, values or rows, from ELPA.
ELPA is one of `gnu', `nongnu' and `melpa'.  The list is ordered
by the package names in ascending order.  SELECT and TYPES are
like the respective arguments of `epkgs', except that they act on
the appropriate recipe table instead of on the package table."
  (closql-query (epkg-db) select types
                (pcase-exhaustive elpa
                  ('gnu    'epkg-gnu-elpa-recipe)
                  ('nongnu 'epkg-nongnu-elpa-recipe)
                  ('melpa  'epkg-melpa-recipe))))

(defun epkg-get-recipe (elpa name)
  "Return a recipe object for the package named NAME from ELPA.
NAME is the name of a package, a string.  ELPA is one of `gnu',
`nongnu' and `melpa'."
  (closql-get (epkg-db) name
              (pcase-exhaustive elpa
                ('gnu    'epkg-gnu-elpa-recipe)
                ('nongnu 'epkg-nongnu-elpa-recipe)
                ('melpa  'epkg-melpa-recipe))))

;;; Utilities

(defun epkg-recipe-to-package (rcp)
  (epkg (caar
         (epkg-sql
          [:select [packages:name]
           :from [packages (as $i1 recipes)]
           :where (and (= recipes:name $s2)
                       (= recipes:epkg-package
                          packages:name))]
          (cond ((cl-typep rcp 'epkg-gnu-elpa-recipe)    'gnu-elpa-recipes)
                ((cl-typep rcp 'epkg-nongnu-elpa-recipe) 'nongnu-elpa-recipes)
                ((cl-typep rcp 'epkg-melpa-recipe)       'melpa-recipes)
                ;; This table stores a different, but equivalent object.
                ;; Cannot use cl-typep because the type is unknown when
                ;; compiling this
                ((and (fboundp 'package-recipe--eieio-childp)
                      (package-recipe--eieio-childp rcp))
                 'melpa-recipes))
          (oref rcp name)))))

(defun epkg-melpa-json-recipes ()
  (json-encode (mapcar #'epkg-melpa--recipe-plist (epkg-list-recipes 'melpa))))

(defun epkg-melpa--recipe-plist (rcp)
  (let ((type (epkg-melpa--recipe-type rcp)))
    `(,(intern (oref rcp name))
      :fetcher ,type
      ,@(if (memq type '(git hg))
            (list :url (oref rcp url))
          (list :repo (oref rcp repo)))
      ,@(mapcan (lambda (slot)
                  (and-let* ((value (eieio-oref rcp slot)))
                    (list (intern (format ":%s" slot)) value)))
                '(files branch commit version-regexp old-names)))))

(defun epkg-melpa--recipe-type (rcp)
  (intern (substring (symbol-name (eieio-object-class-name rcp)) 6 -7)))

;;; _
(provide 'epkg-elpa)
;;; epkg-elpa.el ends here
