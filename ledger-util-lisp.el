;;; ledger-util-lisp.el --- Helper code for use with ledger-mode.el -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides helpers for working with the sexps output by ledger's "lisp" command.

;;; Code:

(require 'seq)

(defvar ledger-util-active-xacts
  "List of xacts in 'ledger-util-active-file.")

(defvar ledger-util-staged-xacts
  "List of xacts in 'ledger-util-staging-file.")

(defun ledger-util-load ()
  "Read ledger xacts as sexps and save in variables."
  (interactive)
  (setq ledger-util-active-xacts (read (shell-command-to-string (format "ledger lisp -f %s" ledger-util-active-file)))
        ledger-util-staged-xacts (read (shell-command-to-string (format "ledger lisp -f %s" ledger-util-staging-file)))))

(defun ledger-util-xact-file (xact)
  "Get the filename from XACT."
  (car xact))

(defun ledger-util-xact-line (xact)
  "Get the line number from XACT."
  (cadr xact))

(defun ledger-util-xact-date (xact)
  "Get the date string from XACT."
  (format-time-string "%Y/%m/%d" (nth 2 xact)))

(defun ledger-util-xact-payee (xact)
  "Get the payee from XACT."
  (nth 4 xact))

(defun ledger-util-xact-postings (xact)
  "Get the postings from XACT."
  (nthcdr 5 xact))

(defun ledger-util-xact-amts (xact)
  "Get the amounts from all the postings in XACT."
  (mapcar 'ledger-util-post-amt (ledger-util-xact-postings xact)))

(defun ledger-util-xact-cleared (xact)
  "Check whether all postings in XACT are cleared."
  (seq-every-p
   'ledger-util-post-cleared
   (ledger-util-xact-postings xact)))

(defun ledger-util-xact-uncleared (xact)
  "Check whether XACT has one or more uncleared postings."
  (not (ledger-util-xact-cleared xact)))

(defun ledger-util-xact-filter-postings (pred xact)
  "Filter the postings in XACT according to PRED."
  (seq-filter
   pred
   (ledger-util-xact-postings xact)))

(defun ledger-util-xact-map-postings (func xact)
  "Map FUNC over the postings in XACT."
  (seq-map
   func
   (ledger-util-xact-postings xact)))

(defun ledger-util-xact-has-amt (amt xact)
  "Check whether XACT contains a posting with an amount equal to AMT."
  (not (= 0
		 (seq-count
		  '(lambda (post) (string-equal amt (ledger-util-post-amt post)))
		 (ledger-util-xact-postings xact)))))

(defun ledger-util-post-line (post)
  "Get the line number from POST."
  (car post))

(defun ledger-util-post-acct (post)
  "Get the account from POST."
  (nth 1 post))

(defun ledger-util-post-amt (post)
  "Get the amount from POST."
  (nth 2 post))

(defun ledger-util-post-cleared (post)
  "Get the cleared status of POST."
  (nth 3 post))

(defun ledger-util-post-ofxid-tag (post)
  (nth 4 post))

(defun ledger-util-post-has-ofxid-tag (post)
  (ledger-util-post-ofxid-tag post)
  (nth 4 post))

(provide 'ledger-util-lisp)

;;; ledger-util-lisp.el ends here
