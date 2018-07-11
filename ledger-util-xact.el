;;; ledger-util-xact.el --- Emacs utilities for ledger-cli -*- lexical-binding: t; -*-

;;; Commentary:
;; This file provides a struct for ledger xacts.

;;; Code:

(require 'cl-lib)
(require 'ledger-util-post)

(cl-defstruct (lu-xact (:constructor lu-xact-create)
					   (:copier lu-xact-copy))
  id file line date payee posts)

(defun lu-xact--genid (xact)
  "Set the id field of XACT according to the values of its other fields."
  (setf (lu-xact-id xact)
		(format "__id__%s__%d__"
				(lu-xact-file xact)
				(lu-xact-line xact))))

(defun lu-xact--propagate-post-data (xact)
  (mapcar
   (lambda (post)
	 (setf (lu-post-file post) (lu-xact-file xact))
	 (setf (lu-post-date post) (lu-xact-date xact))
	 (setf (lu-post-payee post) (lu-xact-payee xact))
	 (lu-post--genid post))
   (lu-xact-posts xact)))

(defun lu-xact-date-str (xact)
  (format-time-string "%Y/%m/%d" (lu-xact-date xact)))

(defun lu-xact-from-lisp (xact)
  "Build our in-memory representation of an XACT in ledger's Lisp representation."
  (let* ((our-xact nil)
		 (file (nth 0 xact))
		 (line (nth 1 xact))
		 (date (nth 2 xact))
		 (payee (nth 4 xact))
		 (posts (nthcdr 5 xact))
		 (our-posts (mapcar #'lu-post-from-lisp posts)))
	(setq our-xact (lu-xact-create :file file
								   :line line
								   :date date
								   :payee payee
								   :posts our-posts))
	(lu-xact--propagate-post-data our-xact)
	(lu-xact--genid our-xact)
	our-xact))

(provide 'ledger-util-xact)

;;; ledger-util-xact.el ends here
