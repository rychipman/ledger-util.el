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

(defun lu-table-entries-from-xacts (&optional mapfunc)
  (mapcar
   (or mapfunc #'lu-table-post-to-entry)
   (apply #'append (mapcar
					#'lu-xact-posts
					(lu-get-xacts)))))

(defun lu-table-col-from-name (name)
  (list (capitalize (downcase name)) 15 t))

(defun lu-table-get-entries-from-names (names)
  (lu-table-entries-from-xacts
   (lambda (post)
	 (list (lu-post-id post)
		   (cl-map 'vector
				   (lambda (name)
					 (let* ((fname (downcase name))
							(func (intern (concat "lu-post-" fname))))
					   (funcall func post)))
				   names)
		   )
	 ))
  )

(defun lu-table-use-columns (&rest cols)
  (unless (> (length cols) 0)
	(error "Must provide at least one column name"))
  (setq tabulated-list-format (cl-map 'vector #'lu-table-col-from-name cols))
  (setq tabulated-list-entries (lambda () (lu-table-get-entries-from-names cols)))
  (tabulated-list-init-header)
  (tablist-revert))

(define-derived-mode lu-table-mode
  tablist-mode "lu-table"
  "Major mode for displaying ledger data in a table."
  (lu-table-use-columns "date-str" "amount" "payee" "account"))

(provide 'ledger-util-table)

;;; ledger-util-table.el ends here
