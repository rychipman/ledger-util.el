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

;;(setq lu-match-mode-highlights
;;	  '(("Liabilities\\|Assets" . font-lock-function-name-face)
;;		("..../../...+\\$-[0-9]+\.[0-9][0-9]" . font-lock-constant-face)
;;		("..../../...+\\$[0-9]+\.[0-9][0-9]" . font-lock-doc-face)
;;		))
;;
;;(magit-define-popup lu-match-help-popup
;;  "Help popup for lu-match."
;;  :actions '("Ledger Matching Help"
;;			 (?d "Delete")
;;			 (?a "Add")
;;			 (?m "Match")))

(provide 'ledger-util)

;;; ledger-util.el ends here
