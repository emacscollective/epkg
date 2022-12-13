;;; epkg-schemata.el --- Table schemata  -*- lexical-binding:t -*-

;; Copyright (C) 2016-2022 Jonas Bernoulli

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

;; This file is only needed to bootstrap the Epkg database.  Since
;; that has already been done, it mainly serves as documentation.

;;; Code:

(require 'epkg)

(defconst epkg--db-table-schemata
  '((packages          [(class :not-null)
                        (name :not-null :primary-key)
                        hash
                        url
                        mirror-url
                        mirror-name
                        upstream-user
                        upstream-name
                        upstream-branch
                        upstream-tree
                        library
                        repopage
                        homepage
                        mirrorpage
                        wikipage
                        license
                        created
                        updated
                        summary
                        commentary
                        libraries
                        provided
                        required
                        keywords
                        authors
                        maintainers
                        ;; The schema dsl doesn't properly handle
                        ;; `:default', so `closql--db-init' adds
                        ;; these columns using `:alter-table'.
                        ;; (melpa-recipes :default eieio-unbound)
                        ;; (gnu-elpa-recipes :default eieio-unbound)
                        ;; (builtin-libraries :default eieio-unbound)
                        ;; patched
                        ;; stars
                        ;; downloads
                        ;; upstream-state
                        ;; branch
                        ;; (nongnu-elpa-recipes :default eieio-unbound)
                        ;; (mirrored :default t)
                        ])
    (libraries         [(package :not-null)
                        (library :not-null)]
                       (:primary-key [package library])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (provided          [(package :not-null)
                        (feature :not-null)
                        drop
                        join]
                       (:primary-key [package feature])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (required          [(package :not-null)
                        (feature :not-null)
                        hard
                        ease
                        drop]
                       (:primary-key [package feature])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (keywords          [(package :not-null)
                        (keyword :not-null)]
                       (:primary-key [package keyword])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (authors           [(package :not-null)
                        name
                        email]
                       (:primary-key [package name email])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (maintainers       [(package :not-null)
                        name
                        email]
                       (:primary-key [package name email])
                       (:foreign-key
                        [package] :references packages [name]
                        :on-delete :cascade))
    (melpa-recipes       [(class :not-null)
                          (name :not-null :primary-key)
                          url
                          repo
                          repopage
                          files
                          branch
                          commit
                          version-regexp
                          old-names
                          epkg-package]
                         (:foreign-key
                          [epkg-package] :references packages [name]
                          :on-delete :set-null))
    (nongnu-elpa-recipes [(class :not-null)
                          (name :not-null :primary-key)
                          released url
                          main-file lisp-dir ignored-files excludes
                          renames doc readme news shell-command make
                          texinfo auto-sync merge branch release-branch
                          rolling-release version-map
                          epkg-package]
                         (:foreign-key
                          [epkg-package] :references packages [name]
                          :on-delete :set-null))
    (gnu-elpa-recipes    [(class :not-null)
                          (name :not-null :primary-key)
                          released url core
                          main-file lisp-dir ignored-files excludes
                          renames doc readme news shell-command make
                          texinfo auto-sync merge branch release-branch
                          rolling-release version-map
                          epkg-package]
                         (:foreign-key
                          [epkg-package] :references packages [name]
                          :on-delete :set-null))
    (builtin-libraries   [(package :not-null)
                          (library :not-null)
                          feature]
                         (:foreign-key
                          [package] :references packages [name]
                          :on-delete :cascade))
    ))

(cl-defmethod closql--db-init ((db epkg-database))
  (emacsql-enable-debugging db)
  (emacsql-with-transaction db
    (emacsql db (format "PRAGMA user_version = %s" epkg-db-version))
    (pcase-dolist (`(,table . ,schema)
                   epkg--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (let ((add-column [:alter-table packages :add-column $i1 :default $s2]))
      (emacsql db add-column 'melpa-recipes     'eieio-unbound)
      (emacsql db add-column 'gnu-elpa-recipes  'eieio-unbound)
      (emacsql db add-column 'builtin-libraries 'eieio-unbound)
      (emacsql db add-column 'patched           nil)
      (emacsql db add-column 'stars             nil)
      (emacsql db add-column 'downloads         nil)
      (emacsql db add-column 'upstream-state    nil)
      (emacsql db add-column 'branch            nil)
      (emacsql db add-column 'nongnu-elpa-recipes 'eieio-unbound)
      (emacsql db add-column 'mirrored          t)
      )))

;;; _
(provide 'epkg-schemata)
;;; epkg-schemata.el ends here
