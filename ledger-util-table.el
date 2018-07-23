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
  (setq tabulated-list-entries (lambda () (lu-table-get-entries-from-names cols))))

(defun lu-table-use-filters (&rest filters)
  (setq tablist-current-filter (lu-table-get-filter-conjunction filters)))

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

(defun lu-table-get-amount-filter (amt)
  `(== "Amount" ,amt))

(defun lu-table-configure ()
  (lu-table-use-columns "date-str" "amount" "payee" "account" "file" "cleared-str")
  (lu-table-use-filters "account" "staged")
  (tabulated-list-init-header)
  (tablist-revert))

(defvar lu-table-current-view nil
  "Name of the current view.")

(defun lu-table-set-view (name)
  (pcase name
	("all"
	 (lu-table-use-columns "date-str" "amount" "payee" "account" "file" "cleared-str")
	 (lu-table-use-filters))
	("import"
	 (lu-table-use-columns "date-str" "amount" "payee" "account" "file")
	 (lu-table-use-filters "account" "staged"))
	("matchable"
	 (lu-table-use-columns "date-str" "amount" "payee" "account" "file" "cleared-str")
	 (lu-table-use-filters "account" "main" "uncleared"))
	(_ (error "Invalid view name %s" name))
	)
  (setq lu-table-current-view name)
  (tabulated-list-init-header)
  (tablist-revert))

(define-derived-mode lu-table-mode
  tablist-mode "lu-table"
  "Major mode for displaying ledger data in a table."
  (setq lu-table-current-view-name nil)
  (linum-mode -1)
  (lu-table-configure))

(magit-define-popup lu-table-match-help-popup
  "Help popup for lu-table-match-mode."
  :actions '("Ledger xact matching help"
			 (?X "Exec" 'lu-table-match-marked)
			 (?M "Match" 'lu-table-match-select)
			 (?m "Mark")))

(defvar lu-table-match-mode-map nil
  "Keymap for `lu-table-match-mode'.")

(defun lu-table-match-select ()
  (interactive)
  (tablist-put-mark)
  (pcase lu-table-current-view
	("import" (lu-table-set-view "matchable"))
	("matchable" (lu-table-match-marked))
	(_ (error "In unexpected view state %s" lu-table-current-view))
	)
  )

(defun lu-table-match-marked ()
  (interactive)
  (lu-table-set-view "all")
  (let (marked)
	(setq marked (tablist-get-marked-items))
	(tablist-unmark-all-marks)
	(message "Marked XACTs:")
	(dolist (ent marked)
	  (message "  %s" ent)))
  (lu-table-set-view "import"))

(setq lu-table-match-mode-map (make-sparse-keymap))
(define-key lu-table-match-mode-map (kbd "?") 'lu-table-match-help-popup)
(define-key lu-table-match-mode-map (kbd "M") 'lu-table-match-select)

(define-derived-mode lu-table-match-mode
  lu-table-mode "lu-table-match"
  "Major mode for choosing a transaction to match."
  (lu-table-set-view "import"))

(provide 'ledger-util-table)

;;; ledger-util-table.el ends here
