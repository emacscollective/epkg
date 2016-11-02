;;; epkg.el --- browse the Emacsmirror package database  -*- lexical-binding: t -*-

;; Copyright (C) 2016  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://gitlab.com/tarsius/epkg
;; Package-Requires: ((closql "0.3.0") (dash "2.13.0") (emacs "25.1"))
;; Keywords: tools

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

;; This package provides access to a local copy of the Emacsmirror
;; package database.  It provides low-level functions for querying
;; the database and a `package.el'-like user interface for browsing
;; the database.  Epkg itself is not a package manager.

;; For more information see https://gitlab.com/tarsius/epkg and for
;; information about the Emacsmirror see https://emacsmirror.net.

;;; Code:

(require 'dash)
(require 'seq)
(require 'subr-x)

(require 'closql)

;;; Options

(defconst epkg-db-version 2)

(defconst epkg-origin-url "https://github.com/emacsmirror/epkgs.git"
  "The url of the remote Emacsmirror repository.")

(defgroup epkg nil
  "Browse the Emacsmirror package database."
  :group 'applications)

(defcustom epkg-repository (expand-file-name "epkgs/" user-emacs-directory)
  "The location of the local Emacsmirror repository.

This repository contains the Epkg SQLite database file and, if
they have been initialized, all package repositories from the
Emacsmirror and Emacsattic as submodules.

If you change the value of this option, then you should also
manually move the repository.  Otherwise it would be cloned
again."
  :group 'epkg
  :type 'directory)

;;; Database

(defclass epkg-database (closql-database)
  ((primary-table :initform packages)
   (primary-key   :initform name)
   (object-class  :initform epkg-package)))

(defvar epkg--db-connection nil
  "The EmacSQL database connection.")

(defun epkg-db ()
  "Return the connection to the Epkg database.

If the `epkg-repository', which contains the SQLite database
file, does not exist yet, then first ask the user to clone it."
  (unless (file-exists-p epkg-repository)
    (if (yes-or-no-p (format "Clone %s to %s? "
                             epkg-origin-url
                             epkg-repository))
        (let ((dir (file-name-directory (directory-file-name epkg-repository))))
          (make-directory dir t)
          (let ((default-directory dir))
            (epkg--call-git "clone"
                            epkg-origin-url
                            epkg-repository)))
      (user-error "Aborted.  Epkg requires the Epkgs repository")))
  (unless (and epkg--db-connection (emacsql-live-p epkg--db-connection))
    (closql-db 'epkg-database 'epkg--db-connection
               (expand-file-name "epkg.sqlite" epkg-repository))
    (let ((version (caar (emacsql epkg--db-connection "PRAGMA user_version"))))
      (cond
       ((> version epkg-db-version)
        (emacsql-close epkg--db-connection)
        (user-error
         (concat "Please update the `epkg' package.  The installed "
                 "version is to old for the current database scheme")))
       ((< version epkg-db-version)
        (emacsql-close epkg--db-connection)
        (if (yes-or-no-p (concat "The installed `epkg' version requires a new "
                                 "database scheme.  Update database now? "))
            (epkg-update)
          (user-error "Aborted.  A database update is required"))))))
  epkg--db-connection)

;;;###autoload
(defun epkg-update ()
  "Update the Epkg database.

In the `epkg-repository', pull the master branch and reload
the Epkg database.  Return a connection to the updated Epkg
database."
  (interactive)
  (when epkg--db-connection
    (emacsql-close epkg--db-connection))
  (let ((default-directory epkg-repository))
    (epkg--call-git "pull" "--recurse-submodules" "origin" "master"))
  (epkg-db))

(defvar magit-process-popup-time)
(declare-function magit-call-git "magit-process" (&rest args))

(defun epkg--call-git (&rest args)
  (if (require 'magit nil t)
      (let ((magit-process-popup-time 0))
        (magit-call-git args))
    (with-current-buffer (generate-new-buffer " *Epkg-Git*")
      (switch-to-buffer-other-window (current-buffer))
      (apply #'call-process "git" nil t t args))))

(cl-defmethod closql--class-to-sql ((_db epkg-database) value)
  (intern (substring (setq value (symbol-name value))
                     (if (string-prefix-p "eieio-class-tag--" value) 22 5)
                     -8)))

(cl-defmethod closql--sql-to-class ((_db epkg-database) value)
  (intern (format "eieio-class-tag--epkg-%s-package" value)))

;;; Superclass

(defclass epkg-package  (closql-object)
  ((repopage-format   :allocation :class        :initform nil)
   (homepage-format   :allocation :class        :initform nil)
   (mirrorpage-format :allocation :class        :initform nil)
   (mirror-url-format :allocation :class        :initform nil)
   (url-format        :allocation :class        :initform nil)
   (name              :initarg :name            :initform nil)
   (hash              :initarg :hash            :initform nil)
   (url               :initarg :url             :initform nil)
   (mirror-url        :initarg :mirror-url      :initform nil)
   (mirror-name       :initarg :mirror-name     :initform nil)
   (upstream-user     :initarg :upstream-user   :initform nil)
   (upstream-name     :initarg :upstream-name   :initform nil)
   (upstream-branch   :initarg :upstream-branch :initform nil)
   (upstream-tree     :initarg :upstream-tree   :initform nil)
   (library           :initarg :library         :initform nil)
   (repopage          :initarg :repopage        :initform nil)
   (homepage          :initarg :homepage        :initform nil)
   (mirrorpage        :initarg :mirrorpage      :initform nil)
   (wikipage          :initarg :wikipage        :initform nil)
   (license           :initarg :license         :initform nil)
   (created           :initarg :created         :initform nil)
   (updated           :initarg :updated         :initform nil)
   (summary           :initarg :summary         :initform nil)
   (commentary        :initarg :commentary      :initform nil)
   (libraries         :initarg :libraries
                      :columns [package library])
   (provided          :initarg :provided
                      :columns [package feature drop join])
   (required          :initarg :required
                      :columns [package feature hard ease drop])
   (keywords          :initarg :keywords
                      :columns [package keyword])
   (authors           :initarg :authors
                      :columns [package name email])
   (maintainers       :initarg :maintainers
                      :columns [package name email])
   (melpa-recipes     :initarg :melpa-recipes
                      :columns [closql-id name fetcher status
                                url repo repopage files
                                branch commit module
                                version-regexp old-names])
   (gelpa-recipes     :initarg :gelpa-recipes
                      :columns [closql-id name type status
                                method released url])
   (builtin-libraries :initarg :builtin-libraries
                      :columns [closql-id library feature name]))
  :abstract t)

;;; Subclasses

(defclass epkg-mirrored-package (epkg-package)
  ((mirrorpage-format :initform "https://github.com/emacsmirror/%m")
   (mirror-url-format :initform "git@github.com:emacsmirror/%m.git"))
  :abstract t)

(defclass epkg-file-package (epkg-mirrored-package) ())

(defclass epkg-minority-package (epkg-file-package) ())

(defclass epkg-gitish-package (epkg-mirrored-package) () :abstract t)

(defclass epkg-git-package (epkg-gitish-package) ())

(defclass epkg-github-package (epkg-git-package)
  ((url-format      :initform "git@github.com:%u/%n.git")
   (repopage-format :initform "https://github.com/%u/%n")))

(defclass epkg-orphaned-package (epkg-github-package)
  ((url-format      :initform "git@github.com:emacsorphanage/%n.git")
   (repopage-format :initform "https://github.com/emacsorphanage/%n")))

(defclass epkg-gitlab-package (epkg-git-package)
  ((url-format      :initform "git@gitlab.com:%u/%n.git")
   (repopage-format :initform "https://gitlab.com/%u/%n")))

(defclass epkg-subtree-package (epkg-git-package) ())

(defclass epkg-subset-package (epkg-gitish-package) () :abstract t)

(defclass epkg-wiki-package (epkg-subset-package)
  ((url-format      :initform "git@github:emacsmirror/emacswiki.org.git")
   (repopage-format :initform "https://github.com/emacsmirror/emacswiki.org")
   (homepage-format :initform "http://emacswiki.org/emacs/download/%n.el")))

(defclass epkg-elpa-package (epkg-subset-package)
  ((url-format      :initform "git://git.savannah.gnu.org/emacs/elpa.git")
   (repopage-format :initform "http://git.savannah.gnu.org/cgit/emacs/elpa.git/tree/packages/%n")
   (homepage-format :initform "https://elpa.gnu.org/packages/%n.html")))

(defclass epkg-elpa-branch-package (epkg-subset-package)
  ((url-format      :initform "git://git.savannah.gnu.org/emacs/elpa.git")
   (repopage-format :initform "http://git.savannah.gnu.org/cgit/emacs/elpa.git/log/?h=externals/%n")
   (homepage-format :initform "https://elpa.gnu.org/packages/%n.html")))

(defclass epkg-hg-package (epkg-gitish-package) ())

(defclass epkg-bitbucket-package (epkg-hg-package)
  ((url-format      :initform "hg::ssh://hg@bitbucket.org/%u/%n")
   (repopage-format :initform "https://bitbucket.org/%u/%n")))

(defclass epkg-mocking-package (epkg-package) () :abstract t)

(defclass epkg-builtin-package (epkg-mocking-package)
  ((url-format      :initform "git://git.savannah.gnu.org/emacs.git")
   (repopage-format :initform "http://git.savannah.gnu.org/cgit/emacs.git")
   (homepage-format :initform "http://www.gnu.org/software/emacs")))

(defclass epkg-shelved-package (epkg-mocking-package)
  ((mirrorpage-format :initform "https://github.com/emacsattic/%m")
   (mirror-url-format :initform "git@github.com:emacsattic/%m.git")))

;;; Interfaces

(defun epkg-sql (sql &rest args)
  "Send SQL s-expression to the Epkg database and return the result."
  (if (stringp sql)
      (emacsql (epkg-db) (apply #'format sql args))
    (apply #'emacsql (epkg-db) sql args)))

(defun epkgs (&optional select predicates)
  "Return a list of `epkg-package' objects or a list of rows.

The list is ordered by the package names in ascending order.

If optional SELECT is non-nil, then it has to be a list of
columns of the `packages' table.  In that case the returned
value is a list of database rows.

If optional PREDICATES is non-nil, then it has to be a list of
package class predicate functions, or a single such function.
Valid functions are named either `epkg-TYPE-package-p' or
`epkg-TYPE-package--eieio-childp'.  Only packages are returned
for which one of these predicates returns non-nil."
  (if select
      (let ((value (closql-select (epkg-db) select predicates)))
        (if (and select (symbolp select))
            (mapcar #'car value)
          value))
    (closql-entries (epkg-db) predicates)))

(cl-defmethod epkg ((name string))
  "Return an `epkg-package' object for the package named NAME.
NAME is the name of a package, a string."
  (closql-get (epkg-db) name))

(cl-defgeneric epkg-provided (package)
  "Return a list of features provided by PACKAGE.

Each element has the form (PACKAGE FEATURE...), where PACKAGE
is the name of a package, a string, and FEATURE is a feature
provided by that package.  If FEATURE is a symbol, then it is
a hard (mandatory) dependency; if it is a string, then it is
a soft (optional) dependency.")

(cl-defmethod  epkg-provided ((pkg epkg-package))
  "Return a list of features provided by the package PKG.
PKG is an `epkg-package' object."
  (epkg-provided (oref pkg name)))

(cl-defmethod  epkg-provided ((package string))
  "Return a list of features provided by PACKAGE.
PACKAGE is the name of a package, a string."
  (mapcar #'car (epkg-sql [:select feature :from provided
                           :where (= package $s1)
                           :order-by [(asc feature)]] package)))

(cl-defgeneric epkg-provided (package)
  "Return a list of packages and features required by PACKAGE.")

(cl-defmethod  epkg-required ((pkg epkg-package))
  "Return a list of packages and features required by the package PKG.
PKG is an `epkg-package' object."
  (epkg-required (oref pkg name)))

(cl-defmethod  epkg-required ((package string))
  "Return a list of packages and features required by PACKAGE.
PACKAGE is the name of a package, a string."
  (let (deps)
    (-each (epkg-sql [:select [feature hard] :from required
                      :where (= package $s1)
                      :order-by [(asc feature)]]
                     package)
      (-lambda ((feature hard))
        (let ((feature* (if hard feature (symbol-name feature))))
          (if-let ((package* (epkg--required package feature)))
              (unless (equal package* package)
                (if-let ((elt (assoc package* deps)))
                    (push feature* (cdr elt))
                  (push (list package* feature*) deps)))
            (push (list nil feature*) deps)))))
    (cl-sort (mapcar (-lambda ((package . features))
                       (cons package (sort features #'string<)))
                     deps)
             #'string< :key #'car)))

(cl-defmethod epkg--required ((package string) feature)
  (or (epkg--required (if (symbolp feature) feature (intern feature)))
      (let ((string (if (stringp feature) feature (symbol-name feature))))
        ;; Some packages require `NAME-autoloads' or `NAME-loaddefs',
        ;; others require `OTHER-autoloads' or `OTHER-loaddefs'.  Assume
        ;; that building the package which provides `NAME' or `OTHER'
        ;; also generates a file which provides the autoloads feature.
        (or (and (string-equal string (concat package "-autoloads")) package)
            (and (string-equal string (concat package "-loaddefs")) package)
            (and (string-suffix-p "-autoloads" string)
                 (epkg--required (intern (substring string 0 -10))))
            (and (string-suffix-p "-loaddefs" string)
                 (epkg--required (intern (substring string 0 -9))))
            ;; Some packages require `NAME-version'.  Assume that when
            ;; `NAME' is build, a file which provides `NAME-version' is
            ;; also generated.
            (and (string-equal string (concat package "-version")) package)
            ;; We ignore files ending with `-test' or `-tests' to avoid
            ;; dependencies that are only required to run the tests.  In
            ;; rare cases `NAME' does require `NAME-test', which might
            ;; then lead to unsatisfied, because unlisted, dependencies.
            ;; Nevertheless we assume that `NAME-test' is part of `NAME'.
            (and (string-equal string (concat package "-test"))  package)
            (and (string-equal string (concat package "-tests")) package)))))

(cl-defmethod epkg--required ((feature symbol))
  (let ((packages (mapcar #'car
                          (epkg-sql [:select package :from provided
                                     :where (= feature $s1)
                                     :order-by [(asc package)]] feature))))
    (if (= (length packages) 1)
        (car packages)
      (let ((alist (--map (cons it (epkg it)) packages)))
        (car (or (--first (cl-typep (cdr it) 'epkg-builtin-package) alist)
                 (--first (and (cl-typep (cdr it) 'epkg-mirrored-package)
                               (equal    (car it) (symbol-name feature)))
                          alist)
                 (--first (cl-typep (cdr it) 'epkg-mirrored-package) alist)
                 (--first (equal    (car it) (symbol-name feature)) alist)
                 (car alist)))))))

(cl-defgeneric epkg-reverse-dependencies (package)
  "Return a list of packages which depend on PACKAGE.

Each element has the form (PACKAGE FEATURE...), where PACKAGE
is the name of a package, a string, and FEATURE is a feature
required by that package.  If FEATURE is a symbol, then it is
a hard (mandatory) dependency; if it is a string, then it is
a soft (optional) dependency.")

(cl-defmethod  epkg-reverse-dependencies ((pkg epkg-package))
  "Return a list of packages which depend on PKG.
PKG is an `epkg-package' object."
  (epkg-reverse-dependencies (oref pkg name)))

(cl-defmethod  epkg-reverse-dependencies ((package string))
  "Return a list of packages which depend on PACKAGE.
PACKAGE is the name of a package, a string."
  (mapcar (-lambda ((package . required))
            (cons package (mapcar (-lambda ((_ feature hard))
                                    (if hard feature (symbol-name feature)))
                                  required)))
          (cl-sort
           (-group-by #'car
                      (epkg-sql [:select [package feature hard] :from required
                                 :where feature :in $v1]
                                (vconcat (epkg-provided package))))
           #'string< :key #'car)))

(cl-defmethod epkg-type ((pkg epkg-package))
  "Return the type of the Epkg object PKG."
  (epkg-type (eieio-object-class pkg)))

(cl-defmethod epkg-type ((class (subclass epkg-package)))
  "Return a short representation of CLASS."
  (if (eq class 'epkg-package)
      'all
    (setq class (symbol-name class))
    (when (string-match "\\`epkg-\\(.+\\)-package\\'" class)
      (intern (match-string 1 class)))))

(cl-defun epkg-package-types (&optional subtypes)
  "Return a list of all package types.

If optional SUBTYPES is non-nil, then also return symbols of
the form `TYPE*', which stands for `TYPE' and its subtypes."
  (cl-labels
      ((types (class)
              (let ((children (eieio--class-children (cl--find-class class)))
                    (type (epkg-type class)))
                (nconc (and (not (class-abstract-p class)) (list type))
                       (and subtypes children
                            (list (intern (format "%s*" type))))
                       (cl-mapcan #'types children)))))
    (sort (types 'epkg-package) #'string<)))

(defvar epkg-type-history nil)

(defun epkg-read-type (prompt &optional default subtypes)
  "Read an Epkg type and return it as a symbol.

If optional DEFAULT is non-nil, then that is offered as default
choice.  If optional CHILDP is non-nil, then entries of the form
`TYPE*', which stands for \"`TYPE' and its subtypes\", are also
offered as completion candidates."
  (intern (completing-read prompt (epkg-package-types subtypes)
                           nil t nil 'epkg-type-history default)))

(defvar epkg-package-history nil)

(defun epkg-read-package (prompt &optional default)
  "Read the name of an Epkg package and return it as a string.

Optional DEFAULT, if non-nil, is offered as default choice."
  (completing-read prompt (epkgs 'name) nil t nil
                   'epkg-package-history default))

(provide 'epkg)
(require 'epkg-desc)
(require 'epkg-list)
;;; epkg.el ends here
