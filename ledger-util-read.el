;;; ledger-util-read.el --- Emacs utilities for ledger-cli -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides utilities for reading ledger xacts from files.

;;; Code:

(require 'ledger-util-xact)

(defvar lu-xact-files nil
  "Files from which xacts should be loaded.")

(defun lu-read-file (file)
  "Return a list of xacts in FILE."
  (read (shell-command-to-string (format "ledger lisp -f %s" file))))

(defun lu-read-all ()
  "Load xacts from each file in `lu-xact-files'."
  (apply 'append (mapcar #'lu-read-file lu-xact-files)))

(defun lu-get-xacts ()
  "Get a list of xact structs for all xacts in `lu-xact-files'."
  (mapcar #'lu-xact-from-lisp (lu-read-all)))

(provide 'ledger-util-read)

;;; ledger-util-read.el ends here
