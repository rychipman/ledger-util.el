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

(defun lu-table-use-filters (&rest filters)
  (setq tablist-current-filter (lu-table-get-filter-conjunction filters))
  (tablist-revert))

(defun lu-table-get-filter-conjunction (names)
  (let (conj)
	(dolist (name names)
	  (if conj
		  (setq conj `(and ,conj ,(lu-table-get-filter name)))
	  (setq conj (lu-table-get-filter name))))
  conj))

(defun lu-table-get-filter (name)
  (pcase name
	("unknown-expense" '(== "Account" "Expenses:Unknown"))
	("homegoods" '(== "Payee" "HomeGoods"))
	("nobudget" '(not (=~ "Account" "^Budget")))
	("noequity" '(not (=~ "Account" "^Equity")))
	("noexpense" '(not (=~ "Account" "^Expenses")))
	("account" (lu-table-get-filter-conjunction '("nobudget" "noequity" "noexpense")))
	("staged" '(=~ "File" "import.ledger"))
	("main" '(=~ "File" "test.ledger"))
	("cleared" '(== "Cleared-Str" "yes"))
	("uncleared" '(== "Cleared-Str" "no"))
	)
  )

(defun lu-table-configure ()
  (lu-table-use-columns "date-str" "amount" "payee" "account" "file" "cleared-str")
  (lu-table-use-filters "account" "staged"))

(defun lu-table-set-view (name)
  (pcase name
	("import"
	 (lu-table-use-columns "date-str" "amount" "payee" "account" "file")
	 (lu-table-use-filters "account" "staged"))
	("matchable"
	 (lu-table-use-columns "date-str" "amount" "payee" "account" "file" "cleared-str")
	 (lu-table-use-filters "account" "main" "uncleared"))
	)
  )

(define-derived-mode lu-table-mode
  tablist-mode "lu-table"
  "Major mode for displaying ledger data in a table."
  (lu-table-configure))

(provide 'ledger-util-table)

;;; ledger-util-table.el ends here
