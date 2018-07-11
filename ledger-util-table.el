;;; ledger-util-table.el --- Emacs utilities for ledger-cli -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides utilities for displaying ledger data in tabular format.

;;; Code:

(require 'ledger-util-xact)
(require 'ledger-util-post)
(require 'ledger-util-read)

(defun lu-table-post-to-entry (post)
  (list (lu-post-id post)
		(vector (lu-post-date-str post)
				(lu-post-amount post)
				(lu-post-payee post)
				(lu-post-account post))))

(defun lu-table-entries-from-xacts ()
  (mapcar
   #'lu-table-post-to-entry
   (apply #'append (mapcar
					#'lu-xact-posts
					(lu-get-xacts)))))

(define-derived-mode lu-table-mode
  tablist-mode "lu-table"
  "Major mode for displaying ledger data in a table."
  (setq tabulated-list-entries 'lu-table-entries-from-xacts
		tabulated-list-format  (vector
								'("Date" 12 t)
								'("Amount" 11)
								'("Payee" 30)
								'("Account" 40))
		tabulated-list-padding 2)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'ledger-util-table)

;;; ledger-util-table.el ends here
