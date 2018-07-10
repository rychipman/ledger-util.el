;;; ledger-util.el --- Helper code for use with ledger-mode.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Most of the general ledger-util code is here.

;;; Code:

(require 'seq)
(require 'ledger-util-lisp)

(defvar ledger-util-active-file
  "Active ledger file. ledger-util assumes that a single file is used.")

(defvar ledger-util-staging-file
  "File where externally-sourced transactions are stored while awaiting manual import/matching.")

(defun ledger-util-postings-in-xacts (xacts)
  "Get all the postings from the xacts in XACTS."
  (apply #'append (seq-map #'ledger-util-xact-postings xacts)))

(defun ledger-util-post-awaiting-match (post)
  "Return whether posting POST is a valid matching target."
  (not
   (or
    (ledger-util-post-cleared post)
	(string-prefix-p "Budget" (ledger-util-post-acct post))
	(string-prefix-p "Equity" (ledger-util-post-acct post))
	(string-prefix-p "Expenses" (ledger-util-post-acct post))
	(string-match-p (regexp-quote ":matched:") (or (ledger-util-post-ofxid-tag post) ""))
	(string-match-p (regexp-quote "ofxid:") (or (ledger-util-post-ofxid-tag post) ""))
	)))

(defun ledger-util-post-for-match (post)
  "Return whether POST is a valid posting for matching."
  (ledger-util-post-has-ofxid-tag post))

(defun ledger-util-get-postings-awaiting-match ()
  "Get all postings in 'ledger-util-active-file that are valid match targets."
  (let (posts)
	(setq posts (ledger-util-postings-in-xacts ledger-util-active-xacts))
	(seq-filter #'ledger-util-post-awaiting-match posts)))

(defun ledger-util-get-postings-for-match ()
  "Get all match-eligible postings from 'ledger-util-staging-file."
  (let (posts)
	(setq posts (ledger-util-postings-in-xacts ledger-util-staged-xacts))
	(seq-filter #'ledger-util-post-for-match posts)))

(defun ledger-util-postings-by-amount (posts)
  "Group the postings in POSTS into an alist by their amount."
  (seq-group-by #'ledger-util-post-amt posts))

(defun ledger-util-get-posts-for-match-with-candidates ()
  "Get an alist of match-eligible postings to candidate match targets based on posting amount."
  (let (awaiting-by-amount)
	(setq awaiting-by-amount (ledger-util-postings-by-amount (ledger-util-get-postings-awaiting-match)))
	(seq-map
	 (lambda (post)
	   (list post (cdr (assoc (ledger-util-post-amt post) awaiting-by-amount))))
	 (ledger-util-get-postings-for-match))))

(defun ledger-util-get-matchable-posts ()
  "Filter the output of 'ledger-util-get-posts-for-match-with-candidates to exclude postings with no candidate targets."
  (let (posts-with-candidates matchable)
	(setq posts-with-candidates (ledger-util-get-posts-for-match-with-candidates))
	(seq-filter (lambda (x) (cadr x)) posts-with-candidates)))

(defun ledger-util-match-first (&optional n)
  (interactive)
  (setq n (or n 0))
  (let (matchable first-matchable post candidates candidate)
	(setq matchable (ledger-util-get-matchable-posts))
	(setq first-matchable (car matchable))
	(setq post (car first-matchable))
	(setq candidates (cadr first-matchable))
	(setq candidate (nth n candidates))

	(delete-other-windows)

	(find-file ledger-util-staging-file)
	(goto-char (point-min))
	(forward-line (1- (ledger-util-post-line post)))

	(find-file-other-window ledger-util-active-file)
	(goto-char (point-min))
	(forward-line (1- (ledger-util-post-line candidate)))
	)
  )

(defun ledger-util-open-match-buffer ()
  (interactive)
  (ledger-util-load)
  (let ((mbuf (get-buffer-create ledger-util-match-buffer-name))
		(matchable (ledger-util-get-matchable-posts))
		(inhibit-read-only t))
    (setq mbuf (get-buffer-create ledger-util-match-buffer-name))
	(delete-other-windows)
	(set-window-buffer nil mbuf)
	(erase-buffer)
	(dolist (item matchable)
	  (let ((post (car item))
			(candidates (cadr item)))
		(insert (format "Matchable xact for account %s (%s)\n"
						(ledger-util-post-acct post)
						(ledger-util-post-amt post)))
		(dolist (target candidates)
		  (insert (format "    acct=%s, amt=%s\n"
						  (ledger-util-post-acct target)
						  (ledger-util-post-amt target)))
		  )
		)
	  )
	))

(defvar ledger-util-match-buffer-name "*Ledger-Util-Match*"
  "Name to use for xact matching buffer.")

(defvar ledger-util-match-mode-keymap
  "Keymap for 'ledger-util-match-mode.")

(setq ledger-util-match-mode-keymap (make-sparse-keymap))

(define-derived-mode ledger-util-match-mode
  special-mode "LdgMatch"
  "Major mode for interactively matching imported ledger transactions.")

(provide 'ledger-util)

;;; ledger-util.el ends here
