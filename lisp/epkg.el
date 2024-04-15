;;; epkg.el --- Browse the Emacsmirror package database  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.epkg@jonas.bernoulli.dev>
;; Homepage: https://github.com/emacscollective/epkg
;; Keywords: tools

;; Package-Requires: (
;;     (emacs "25.1")
;;     (compat "29.1.4.1")
;;     (closql "20230407")
;;     (emacsql "20230409")
;;     (llama "0.2.0"))

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

;; This package provides access to a local copy of the Emacsmirror
;; package database.  It provides low-level functions for querying
;; the database and a `package.el'-like user interface for browsing
;; the database.  Epkg itself is not a package manager.

;; For more information see https://github.com/emacscollective/epkg
;; and https://emacsmirror.net.

;;; Code:

(require 'compat)
(require 'llama)
(require 'seq)
(require 'subr-x)

(require 'closql)

;;; Options

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

(defvar epkg--db-prefer-binary nil
  "Whether to prefer the binary database over the dump.")

(defconst epkg-origin-url "https://github.com/emacsmirror/epkgs.git"
  "The url of the remote Emacsmirror repository.")

;;; Database

(defclass epkg-database (closql-database)
  ((name         :initform "Epkg")
   (object-class :initform 'epkg-package)
   (schemata     :initform 'epkg--db-table-schemata)
   (version      :initform 11)))

(defun epkg-db (&optional livep)
  "Return the Epkg database object.

If optional LIVEP is non-nil, then only return the object if
the connection to the database is live already.

If the `epkg-repository', which contains the SQLite database
file, does not exist yet, then first ask the user to clone it."
  (closql-db 'epkg-database livep))

(cl-defmethod closql--db-prepare-storage ((_class (subclass epkg-database)))
  (unless (file-exists-p epkg-repository)
    (if (y-or-n-p (format "Clone %s to %s? " epkg-origin-url epkg-repository))
        (let ((dir (file-name-directory (directory-file-name epkg-repository))))
          (make-directory dir t)
          (let ((default-directory dir))
            (message "Cloning Epkgs repository...")
            (epkg--call-git "clone" epkg-origin-url epkg-repository)
            (message "Cloning Epkgs repository...done")))
      (user-error "Aborted.  Epkg requires the Epkgs repository")))
  (let* ((default-directory epkg-repository)
         (bin-old  (expand-file-name "epkg.sqlite"))
         (bin-file (expand-file-name "epkg.db"))
         (txt-file (expand-file-name "epkg.sql"))
         (rev-file (expand-file-name "epkg.rev"))
         (rev (car (process-lines "git" "rev-parse" "HEAD"))))
    (when (file-exists-p bin-old)
      (if (file-exists-p bin-file)
          (delete-file bin-old t)
        (rename-file bin-old bin-file)))
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
    bin-file))

(cl-defmethod closql--db-update-schema ((db epkg-database))
  (when (< (closql--db-get-version db)
           (oref-default db version))
    (epkg-update))
  (cl-call-next-method))

;;;###autoload
(defun epkg-update ()
  "Update the Epkg database.

In the `epkg-repository', pull the master branch and reload
the Epkg database.  Return a connection to the updated Epkg
database."
  (interactive)
  (when-let ((db (epkg-db t)))
    (emacsql-close db))
  (let ((default-directory epkg-repository))
    (message "Pulling Epkg database...")
    (epkg--call-git "pull" "--no-recurse-submodules" "origin" "master")
    (message "Pulling Epkg database...done"))
  (epkg-db))

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
   (melpa-recipes       :closql-class epkg-melpa-recipe)
   (gnu-elpa-recipes    :closql-class epkg-gnu-elpa-recipe)
   (builtin-libraries   :closql-table builtin_libraries)
   (patched             :initform nil :initarg :patched)
   (stars               :initform nil :initarg :stars)
   (downloads           :initform nil :initarg :downloads)
   (upstream-state      :initform nil)
   (branch              :initform nil)
   (nongnu-elpa-recipes :closql-class epkg-nongnu-elpa-recipe)
   (mirrored            :initform t)
   )
  :abstract t)

;;; Subclasses

(defclass epkg-mirrored-package (epkg-package)
  ((mirrorpage-format :initform "https://github.com/emacsmirror/%m")
   (mirror-url-format :initform "https://github.com/emacsmirror/%m"))
  :abstract t)

(defclass epkg-file-package (epkg-mirrored-package) ())

(defclass epkg-gitish-package (epkg-mirrored-package) () :abstract t)

(defclass epkg-git-package (epkg-gitish-package) ())

(defclass epkg-github-package (epkg-git-package)
  ((url-format      :initform "https://github.com/%u/%n")
   (repopage-format :initform "https://github.com/%u/%n")))

(defclass epkg-orphaned-package (epkg-github-package)
  ((url-format      :initform "https://github.com/emacsorphanage/%n")
   (repopage-format :initform "https://github.com/emacsorphanage/%n")))

(defclass epkg-gitlab-package (epkg-git-package)
  ((url-format      :initform "https://gitlab.com/%u/%n")
   (repopage-format :initform "https://gitlab.com/%u/%n")))

(defclass epkg-codeberg-package (epkg-git-package)
  ((url-format      :initform "https://codeberg.org/%u/%n")
   (repopage-format :initform "https://codeberg.org/%u/%n")))

(defclass epkg-sourcehut-package (epkg-git-package)
  ((url-format      :initform "https://git.sr.ht/~%u/%n")
   (repopage-format :initform "https://git.sr.ht/~%u/%n")))

(defclass epkg-savannah-package (epkg-git-package) () :abstract t)

(defclass epkg-gnu-package (epkg-savannah-package)
  ((url-format      :initform "https://git.savannah.gnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/%n.git")
   (homepage-format :initform "https://savannah.gnu.org/projects/%n")))

(defclass epkg-nongnu-package (epkg-savannah-package)
  ((url-format      :initform "https://git.savannah.nongnu.org/git/%n.git")
   (repopage-format :initform "https://git.savannah.nongnu.org/cgit/%n.git")
   (homepage-format :initform "https://savannah.nongnu.org/projects/%n")))

(defclass epkg-subtree-package (epkg-git-package) ())

(defclass epkg-wiki-package (epkg-git-package)
  ((url-format      :initform "https://github.com/emacsmirror/emacswiki.org")
   (repopage-format :initform "https://github.com/emacsmirror/emacswiki.org")
   (homepage-format :initform "https://emacswiki.org/emacs/download/%n.el")))

(defclass epkg-nongnu-elpa-package (epkg-git-package)
  ((url-format      :initform "https://git.savannah.gnu.org/git/emacs/nongnu.git")
   (repopage-format :initform "https://git.savannah.gnu.org/cgit/emacs/nongnu.git/log/?h=elpa/%n")
   (homepage-format :initform "https://elpa.nongnu.org/nongnu/%n.html")))

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
   (mirror-url-format :initform "https://github.com/emacsattic/%m")))

;;; Interfaces

(defun epkg-sql (sql &rest args)
  "Send SQL s-expression to the Epkg database and return the result."
  (if (stringp sql)
      (emacsql (epkg-db) (apply #'format sql args))
    (apply #'emacsql (epkg-db) sql args)))

(defun epkgs (&optional select types)
  "Return a list of `epkg-package' objects, values or rows.

The list is ordered by the package names in ascending order.

If optional SELECT is non-nil, then it has to be symbol naming
a column in the `packages' table or a vector of such columns.
In those cases the returned value is a list of column values
or a list of database rows.  If SELECT is nil, return a list
of objects.

If optional TYPES is non-nil, then it has to be a vector of
package types, such as `github'.  To include subtypes, add an
asterisk to the symbol name, e.g., `mirrored*'.  For backward
compatibility, TYPES can also be a list of predicate functions
`epkg-TYPE-package-p' or `epkg-TYPE-package--eieio-childp', or
a single such function."
  (closql-query (epkg-db) select types 'epkg-package))

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
                               :order-by [(asc feature)]]
                              package)
                  (epkg-sql [:select feature :from provided
                             :where (and (= package $s1)
                                         (isnull drop))
                             :order-by [(asc feature)]]
                            package))))

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
                                     :order-by [(asc package)]]
                                    feature))))
    (if (length= packages 1)
        (car packages)
      (let ((alist (mapcar (##cons % (epkg %)) packages)))
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

(defvar epkg--package-parent-dirs
  (list (expand-file-name "mirror/" epkg-repository)
        (expand-file-name "attic/" epkg-repository)
        'borg-drones-directory))

(defun epkg-read-package (prompt &optional default predicates)
  "Read the name of an Epkg package and return it as a string.

A reasonable default choice is offered.  Optional DEFAULT can
be used to provide an even better default choice, if possible.

If optional PREDICATES is non-nil, then it has to be a list of
package class predicate functions, or a single such function.
Valid functions are named either `epkg-TYPE-package-p' or
`epkg-TYPE-package--eieio-childp'.  Limit completion choices
to packages for which one of these predicates returns non-nil."
  (let* ((choices (epkgs 'name predicates))
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
                (and (derived-mode-p 'magit-submodule-list-mode)
                     (car (member (file-name-nondirectory
                                   (tabulated-list-get-id))
                                  choices)))
                (and (derived-mode-p 'org-mode)
                     (looking-at "^[ \t]*| \\([^ ]+\\)")
                     (match-string 1))
                (and (derived-mode-p 'magit-status-mode)
                     (looking-at "^[ \t]*[-+]| \\([^ ]+\\)")
                     (match-string 1))
                (and-let* ((dir (seq-find
                                 (lambda (dir)
                                   (and (or (stringp dir)
                                            (and (boundp dir)
                                                 (setq dir (symbol-value dir))))
                                        (file-in-directory-p default-directory
                                                             dir)))
                                 epkg--package-parent-dirs)))
                  (car (epkg--file-name-split
                        (file-relative-name
                         default-directory
                         (if (stringp dir) dir (symbol-value dir))))))
                (and-let* ((symbol (symbol-at-point)))
                  (compat-call string-trim
                               (symbol-name symbol)
                               ".*/" "\\..*"))))))
    (completing-read prompt choices nil t nil 'epkg-package-history
                     (and default (member default choices) default))))

;;; Compatibility

(defalias 'epkg--file-name-split
  (if (fboundp 'file-name-split)
       'file-name-split
    (lambda (filename)
      (let ((components nil))
        (when (directory-name-p filename)
          (push "" components)
          (setq filename (directory-file-name filename)))
        (while (length> filename 0)
          (push (file-name-nondirectory filename) components)
          (let ((dir (file-name-directory filename)))
            (setq filename (and dir (directory-file-name dir)))
            (when (and dir (equal dir filename))
              (push (if (equal dir "") ""
                      (substring dir 0 -1))
                    components)
              (setq filename nil))))
        components))))

;;; _
(provide 'epkg)
(require 'epkg-desc)
(require 'epkg-list)
(require 'epkg-utils)
(require 'epkg-elpa)
;;; epkg.el ends here
