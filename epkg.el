;;; epkg.el --- browse the Emacsmirror package database  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2021  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/epkg
;; Keywords: tools
;; Package-Requires: ((closql "20210927") (emacs "25.1"))
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

;;; Commentary:

;; This package provides access to a local copy of the Emacsmirror
;; package database.  It provides low-level functions for querying
;; the database and a `package.el'-like user interface for browsing
;; the database.  Epkg itself is not a package manager.

;; For more information see https://github.com/emacscollective/epkg
;; and https://emacsmirror.net.

;;; Code:

(require 'seq)
(require 'subr-x)

(require 'closql)

;;; Options

(defconst epkg-db-version 7)

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

(defcustom epkg-database-connector 'sqlite
  "The database connector used by Forge.
This must be set before `epkg' is loaded.  To use an alternative
connectors you must install the respective package explicitly."
  :package-version '(epkg . "3.4.0")
  :group 'epkg
  :type '(choice (const sqlite)
                 (const libsqlite3)
                 (symbol :tag "other")))

;;; Database

(declare-function epkg-database--eieio-childp "epkg.el" (obj) t)
(cl-case epkg-database-connector
  (sqlite
   (defclass epkg-database (emacsql-sqlite-connection closql-database)
     ((object-class :initform 'epkg-package))))
  (libsqlite3
   (require (quote emacsql-libsqlite3))
   (with-no-warnings
     (defclass epkg-database (emacsql-libsqlite3-connection closql-database)
       ((object-class :initform 'epkg-package))))))

(defvar epkg--db-connection nil
  "The EmacSQL database connection.")

(defvar epkg--db-prefer-binary nil
  "Whether to prefer the binary database over the dump.")

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
            (message "Cloning Epkgs repository...")
            (epkg--call-git "clone"
                            epkg-origin-url
                            epkg-repository)
            (message "Cloning Epkgs repository...done")))
      (user-error "Aborted.  Epkg requires the Epkgs repository")))
  (unless (and epkg--db-connection (emacsql-live-p epkg--db-connection))
    (let* ((default-directory epkg-repository)
           (bin-file (expand-file-name "epkg.sqlite"))
           (txt-file (expand-file-name "epkg.sql"))
           (rev-file (expand-file-name "epkg.rev"))
           (rev (car (process-lines "git" "rev-parse" "HEAD"))))
      (when (or (not (file-exists-p bin-file))
                (and (not epkg--db-prefer-binary)
                     (or (not (file-exists-p rev-file))
                         (not (equal (with-temp-buffer
                                       (insert-file-contents rev-file)
                                       (string-trim (buffer-string)))
                                     rev)))))
        (message "Initializing database from commit %s..." rev)
        (delete-file bin-file)
        (with-temp-buffer
          (unless (zerop (call-process "sqlite3" nil t nil
                                       bin-file
                                       (format ".read %s" txt-file)))
            (error "Failed to read %s: %s" txt-file (buffer-string))))
        (with-temp-file rev-file
          (insert rev ?\n))
        (message "Initializing database from commit %s...done" rev))
      (closql-db 'epkg-database 'epkg--db-connection bin-file))
    (let ((version (closql--db-get-version epkg--db-connection)))
      (cond
       ((> version epkg-db-version)
        (emacsql-close epkg--db-connection)
        (user-error
         (concat "Please update the `epkg' package.  The installed "
                 "version is too old for the current database scheme.")))
       ((and (> epkg-db-version version)
             (= epkg-db-version 5))
        (emacsql-close epkg--db-connection)
        (display-warning 'epkg "\
The database repository has been recreated from
scratch and you have to manually clone the new incarnation.
Please see https://github.com/emacscollective/borg/issues/91." :error))
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
  (prog1 (epkg-db)
    (let ((default-directory epkg-repository))
      (message "Pulling Epkg database...")
      (epkg--call-git "pull" "--no-recurse-submodules" "origin" "master")
      (message "Pulling Epkg database...done"))))

(defvar magit-process-popup-time)
(declare-function magit-call-git "magit-process" (&rest args))

(defun epkg--call-git (&rest args)
  (if (require 'magit nil t)
      (magit-call-git args)
    (with-current-buffer (generate-new-buffer " *Epkg-Git*")
      (switch-to-buffer-other-window (current-buffer))
      (apply #'call-process "git" nil t t args))))

;;; Superclass

(defclass epkg-package (closql-object)
  ((closql-class-prefix :initform "epkg-")
   (closql-class-suffix :initform "-package")
   (closql-table        :initform 'packages)
   (closql-primary-key  :initform 'name)
   (repopage-format     :initform nil :allocation :class)
   (homepage-format     :initform nil :allocation :class)
   (mirrorpage-format   :initform nil :allocation :class)
   (mirror-url-format   :initform nil :allocation :class)
   (url-format          :initform nil :allocation :class)
   (name                :initform nil :initarg :name)
   (hash                :initform nil)
   (url                 :initform nil :initarg :url)
   (mirror-url          :initform nil)
   (mirror-name         :initform nil)
   (upstream-user       :initform nil)
   (upstream-name       :initform nil)
   (upstream-branch     :initform nil :initarg :upstream-branch)
   (upstream-tree       :initform nil :initarg :upstream-tree)
   (library             :initform nil :initarg :library)
   (repopage            :initform nil)
   (homepage            :initform nil)
   (mirrorpage          :initform nil)
   (wikipage            :initform nil)
   (license             :initform nil)
   (created             :initform nil)
   (updated             :initform nil)
   (summary             :initform nil)
   (commentary          :initform nil)
   (libraries           :closql-table libraries)
   (provided            :closql-table provided)
   (required            :closql-table required)
   (keywords            :closql-table keywords)
   (authors             :closql-table authors)
   (maintainers         :closql-table maintainers)
   (melpa-recipes       :closql-class melpa-recipe)
   (gelpa-recipes       :closql-class gelpa-recipe)
   (builtin-libraries   :closql-table builtin_libraries)
   (patched             :initform nil :initarg :patched)
   (stars               :initform nil :initarg :stars)
   (downloads           :initform nil :initarg :downloads))
  :abstract t)

;;; Subclasses

(defclass epkg-mirrored-package (epkg-package)
  ((mirrorpage-format :initform "https://github.com/emacsmirror/%m")
   (mirror-url-format :initform "git@github.com:emacsmirror/%m.git"))
  :abstract t)

(defclass epkg-file-package (epkg-mirrored-package) ())

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

(defclass epkg-sourcehut-package (epkg-git-package)
  ((url-format      :initform "https://git.sr.ht/~%u/%n")
   (repopage-format :initform "https://git.sr.ht/~%u/%n")))

(defclass epkg-savannah-package (epkg-git-package) () :abstact t)

(defclass epkg-gnu-package (epkg-savannah-package)
  ((url-format      :initform "https://git.savannah.gnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/%n.git")
   (homepage-format :initform "https://savannah.gnu.org/projects/%n")))

(defclass epkg-nongnu-package (epkg-savannah-package)
  ((url-format      :initform "https://git.savannah.nongnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.nongnu.org/cgit/%n.git")
   (homepage-format :initform "https://savannah.nongnu.org/projects/%n")))

(defclass epkg-subtree-package (epkg-git-package) ())

(defclass epkg-subrepo-package (epkg-git-package) ())

(defclass epkg-minority-package (epkg-subrepo-package) ())

(defclass epkg-core-package (epkg-subrepo-package)
  ((url-format      :initform "https://git.savannah.gnu.org/git/emacs.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/emacs.git")
   (homepage-format :initform "https://elpa.gnu.org/packages/%n.html")))

(defclass epkg-wiki-package (epkg-git-package)
  ((url-format      :initform "git@github:emacsmirror/emacswiki.org.git")
   (repopage-format :initform "https://github.com/emacsmirror/emacswiki.org")
   (homepage-format :initform "https://emacswiki.org/emacs/download/%n.el")))

(defclass epkg-gnu-elpa-package (epkg-git-package)
  ((url-format      :initform "https://git.savannah.gnu.org/git/emacs/elpa.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/emacs/elpa.git/log/?h=externals/%n")
   (homepage-format :initform "https://elpa.gnu.org/packages/%n.html")))

(defclass epkg-hg-package (epkg-gitish-package) ())

(defclass epkg-mocking-package (epkg-package) () :abstract t)

(defclass epkg-builtin-package (epkg-mocking-package)
  ((url-format      :initform "https://git.savannah.gnu.org/git/emacs.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/emacs.git")
   (homepage-format :initform "https://www.gnu.org/software/emacs")))

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
  (closql-query (epkg-db) select predicates 'epkg-package))

(defun epkg (name)
  "Return an `epkg-package' object for the package named NAME.
NAME is the name of a package, a string."
  (closql-get (epkg-db) name))

(cl-defgeneric epkg-provided (package &optional include-bundled)
  "Return a list of features provided by PACKAGE.

Bundled features are excluded from the returned list unless
optional INCLUDE-BUNDLED is non-nil.

\(fn PACKAGE &optional include-bundled)")

(cl-defmethod  epkg-provided ((pkg epkg-package) &optional include-bundled)
  (epkg-provided (oref pkg name) include-bundled))

(cl-defmethod  epkg-provided ((package string) &optional include-bundled)
  (mapcar #'car (if include-bundled
                    (epkg-sql [:select feature :from provided
                               :where (= package $s1)
                               :order-by [(asc feature)]] package)
                  (epkg-sql [:select feature :from provided
                             :where (and (= package $s1)
                                         (isnull drop))
                             :order-by [(asc feature)]] package))))

(cl-defgeneric epkg-required (package)
  "Return a list of packages and features required by PACKAGE.

Each element has the form (DEPENDENCY FEATURES), where DEPENDENCY
is the name of a required package, a string, and FEATURES is a
list of features provided by DEPENDENCY and required by PACKAGE.

If a feature is represented using a symbol, then that indicates
that it is a mandatory dependency; if a string is used, then it
is an optional dependency.

There may be a single element (nil FEATURES), which means that
it is unknown which package or packages provide the feature or
features listed in FEATURES.")

(cl-defmethod  epkg-required ((pkg epkg-package))
  (epkg-required (oref pkg name)))

(cl-defmethod  epkg-required ((package string))
  (let (deps)
    (pcase-dolist (`(,feature ,hard)
                   (epkg-sql [:select [feature hard] :from required
                              :where (= package $s1)
                              :order-by [(asc feature)]]
                             package))
      (let ((feature* (if hard feature (symbol-name feature))))
        (if-let ((provider (epkg-provided-by feature)))
            (unless (equal provider package)
              (if-let ((elt (assoc provider deps)))
                  (push feature* (cdr elt))
                (push (list provider feature*) deps)))
          (push (list nil feature*) deps))))
    (cl-sort (mapcar (pcase-lambda (`(,package . ,features))
                       (cons package (sort features #'string<)))
                     deps)
             #'string< :key #'car)))

(cl-defgeneric epkg-provided-by (feature)
  "Return the name of the package providing FEATURE.
\n(fn FEATURE).")

(cl-defmethod  epkg-provided-by ((feature symbol))
  (let ((packages (mapcar #'car
                          (epkg-sql [:select package :from provided
                                     :where (= feature $s1)
                                     :order-by [(asc package)]] feature))))
    (if (= (length packages) 1)
        (car packages)
      (let ((alist (mapcar (lambda (name) (cons name (epkg name))) packages)))
        (car (or (cl-find-if (pcase-lambda (`(,_ . ,pkg))
                               (cl-typep pkg 'epkg-builtin-package))
                             alist)
                 (cl-find-if (pcase-lambda (`(,name . ,pkg))
                               (and (cl-typep pkg 'epkg-mirrored-package)
                                    (equal name (symbol-name feature))))
                             alist)
                 (cl-find-if (pcase-lambda (`(,_ . ,pkg))
                               (cl-typep pkg 'epkg-mirrored-package))
                             alist)
                 (cl-find-if (pcase-lambda (`(,name . ,_))
                               (equal name (symbol-name feature)))
                             alist)
                 (car alist)))))))

(cl-defgeneric epkg-reverse-dependencies (package)
  "Return a list of packages that depend on PACKAGE.

Each element has the form (DEPENDANT FEATURES), where DEPENDANT
is the name of a package that depends on PACKAGE, a string, and
FEATURES is a list of features provided by PACKAGE and required
by DEPENDANT.

If a feature is represented using a symbol, then that indicates
that it is a mandatory dependency; if a string is used, then it
is an optional dependency.

\(fn package)")

(cl-defmethod  epkg-reverse-dependencies ((pkg epkg-package))
  (epkg-reverse-dependencies (oref pkg name)))

(cl-defmethod  epkg-reverse-dependencies ((package string))
  (mapcar (pcase-lambda (`(,package . ,required))
            (cons package (mapcar (pcase-lambda (`(,_ ,feature ,hard))
                                    (if hard feature (symbol-name feature)))
                                  required)))
          (seq-group-by #'car
                        (epkg-sql [:select [package feature hard] :from required
                                   :where feature :in $v1
                                   :order-by [(asc package) (asc feature)]]
                                  (vconcat (epkg-provided package))))))

;;; Utilities

(defvar epkg-type-history nil)

(defun epkg-read-type (prompt &optional default childp)
  "Read an Epkg type and return it as a symbol.

If optional DEFAULT is non-nil, then that is offered as default
choice.  If optional CHILDP is non-nil, then entries of the form
`TYPE*', which stands for \"`TYPE' and its subtypes\", are also
offered as completion candidates."
  (intern (completing-read prompt
                           (closql--list-subabbrevs 'epkg-package childp)
                           nil t nil 'epkg-type-history default)))

(defvar epkg-package-history nil)

(defun epkg-read-package (prompt &optional default predicates)
  "Read the name of an Epkg package and return it as a string.

A reasonable default choice is offered.  Optional DEFAULT can
be used to provide an even better default choice, if possible.

If optional PREDICATES is non-nil, then it has to be a list of
package class predicate functions, or a single such function.
Valid functions are named either `epkg-TYPE-package-p' or
`epkg-TYPE-package--eieio-childp'.  Limit completion choices
to packages for which one of these predicates returns non-nil."
  (let ((choices (epkgs 'name predicates))
        (default
          (save-match-data
            (or default
                (and (derived-mode-p 'help-mode)
                     (boundp 'help-xref-stack-item)
                     (eq (car help-xref-stack-item) 'epkg-describe-package)
                     (cadr help-xref-stack-item))
                (and (derived-mode-p 'epkg-list-mode)
                     (tabulated-list-get-id))
                (and (derived-mode-p 'package-menu-mode)
                     (fboundp 'package-desc-name)
                     (symbol-name (package-desc-name (tabulated-list-get-id))))
                (and (derived-mode-p 'org-mode)
                     (looking-at "^[ \t]*| \\([^ ]+\\)")
                     (match-string 1))
                (when-let ((symbol (symbol-at-point)))
                  (thread-first (symbol-name symbol)
                    (string-trim-left  ".*/")
                    (string-trim-right "\\..*")))))))
    (completing-read prompt choices nil t nil 'epkg-package-history
                     (and default (member default choices) default))))

;;; _
(provide 'epkg)
(require 'epkg-desc)
(require 'epkg-list)
(require 'epkg-utils)
(require 'epkg-gelpa)
(require 'epkg-melpa)
;;; epkg.el ends here
