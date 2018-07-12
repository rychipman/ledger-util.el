;;; ledger-util-post.el --- Emacs utilities for ledger-cli -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a struct for ledger postings.

;;; Code:

(require 'cl-lib)

(cl-defstruct (lu-post (:constructor lu-post-create)
					   (:copier lu-post-copy))
  id file line date account amount payee cleared)

(defun lu-post--genid (post)
  "Set the id field of POST according to the values of its other fields."
  (setf (lu-post-id post)
		(format "__id__%s__%d__"
				(lu-post-file post)
				(lu-post-line post))))

(defun lu-post-from-lisp (post)
  "Build our in-memory representation of a POST in ledger's Lisp representation."
  (let* ((our-post nil)
		 (line (nth 0 post))
		 (account (nth 1 post))
		 (amount (nth 2 post))
		 (cleared (nth 3 post)))
	(setq our-post (lu-post-create :line line
								   :account account
								   :amount amount
								   :cleared cleared))
	(lu-post--genid our-post)
	our-post))

(defun lu-post-date-str (post)
  (format-time-string "%Y/%m/%d" (lu-post-date post)))

(defun lu-post-cleared-str (post)
  (if (lu-post-cleared post) "yes" "no"))

(provide 'ledger-util-post)

;;; ledger-util-post.el ends here
