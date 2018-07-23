;;; ledger-util.el --- Emacs utilities for ledger-cli -*- lexical-binding: t; -*-

;;; Commentary:
;; Most of the general ledger-util code is here.

;;; Code:

(require 'magit-popup)

(require 'ledger-util-xact)
(require 'ledger-util-post)
(require 'ledger-util-table)
(require 'ledger-util-read)

(defvar lu-table-buffer-name "*ledger-util-table*"
  "Name to use for xact matching buffer.")

(defun lu-show-table ()
  (interactive)
  (pop-to-buffer lu-table-buffer-name)
  (lu-table-mode))

(defun lu-match ()
  (interactive)
  (pop-to-buffer lu-table-buffer-name)
  (lu-table-match-mode))

(provide 'ledger-util)

;;; ledger-util.el ends here
