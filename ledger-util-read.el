;;; ledger-util-read.el --- Emacs utilities for ledger-cli -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides utilities for reading ledger xacts from files.

;;; Code:

(require 'ledger-util-xact)

(defvar lu-xact-files nil
  "Files from which xacts should be loaded.")

(defvar lu-xact-files-dirty nil
  "Whether the xact files have been modified since the last time they were read.")

(defvar lu-xact-files-data nil
  "Transaction data loaded from xact files.")

(defun lu-xact-files-set-dirty ()
  (setq lu-xact-files-dirty t))

(defun lu-read-file (file)
  "Return a list of xacts in FILE."
  (read (shell-command-to-string (format "ledger lisp -f %s" file))))

(defun lu-read-all ()
  "Load xacts from each file in `lu-xact-files'."
  (apply 'append (mapcar #'lu-read-file lu-xact-files)))

(defun lu-get-xacts ()
  "Get a list of xact structs for all xacts in `lu-xact-files'."
  (when (or lu-xact-files-dirty (not lu-xact-files-data))
	(setq lu-xact-files-data (mapcar #'lu-xact-from-lisp (lu-read-all)))
	(setq lu-xact-files-dirty nil))
  lu-xact-files-data)

(defun lu-get-posts ()
  "Get a list of post structs for all xacts in `lu-xact-files'."
  (apply #'append (mapcar
				   #'lu-xact-posts
				   (lu-get-xacts))))

(defun lu-get-posts-by-id ()
  (mapcar
   (lambda (post) (cons (lu-post-id post) post))
   (lu-get-posts)))

(defun lu-get-post-by-id (id)
  (cdr (assoc id (lu-get-posts-by-id))))

(provide 'ledger-util-read)

;;; ledger-util-read.el ends here
