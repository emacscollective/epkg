;;; epkg-utils.el --- auxiliary commands and utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2017  Jonas Bernoulli

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

;;; Code:

(require 'epkg)

;;; Submodule Utilities

(defmacro with-epkg-repository (arg &rest body)
  (declare (indent defun))
  `(let ((default-directory
           ,(if (eq arg t)
                'epkg-repository
              `(or (epkg-repository ,arg)
                   (error "Need package or string")))))
     ,@body))

(cl-defmethod epkg-repository ((pkg epkg-mirrored-package))
  (expand-file-name (format "mirror/%s/" (oref pkg name)) epkg-repository))

(cl-defmethod epkg-repository ((pkg epkg-shelved-package))
  (expand-file-name (format "attic/%s/" (oref pkg name)) epkg-repository))

;;; Find-File Commands

;;;###autoload
(defun epkg-find-file (filename &optional wildcards)
  (interactive (epkg-find-file-read-args "Find file: "))
  (epkg-find-file-noselect filename #'switch-to-buffer wildcards))

;;;###autoload
(defun epkg-find-file-other-window (filename &optional wildcards)
  (interactive (epkg-find-file-read-args "Find file in other window: "))
  (epkg-find-file-noselect filename #'switch-to-buffer-other-window wildcards))

;;;###autoload
(defun epkg-find-file-other-frame (filename &optional wildcards)
  (interactive (epkg-find-file-read-args "Find file in other frame: "))
  (epkg-find-file-noselect filename #'switch-to-buffer-other-frame wildcards))

(defun epkg-find-file-read-args (prompt)
  (with-epkg-repository (epkg (epkg-read-package "Find file of package: "))
    (find-file-read-args prompt (confirm-nonexistent-file-or-buffer))))

(defun epkg-find-file-noselect (filename:s switch &optional wildcards)
  (let ((value (find-file-noselect filename:s nil nil wildcards)))
    (if (listp value)
	(let ((buffers (nreverse value)))
	  (funcall switch (car buffers))
	  (mapc #'switch-to-buffer (cdr buffers))
	  buffers)
      (funcall switch value))))

(provide 'epkg-utils)
;;; epkg-utils.el ends here

