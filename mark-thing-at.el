;;; mark-thing-at.el --- Mark a pattern at the current point  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Paul Landes

;; Version: 0.1
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: mark point lisp
;; URL: https://github.com/plandes/mark-thing-at
;; Package-Requires: ((emacs "26"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This is a really more of a convenience for `things' in thingatpt.el.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'thingatpt)
(require 'choice-program)

;; created dynamically by `mark-thing-at-make-functions'
(defvar mark-thing-at-thing-to-command-alist)

(defvar mark-thing-at-choices
  '(symbol number line-this line list sexp defun filename url word sentence
	   whitespace page java-expression)
  "A list of symbols one can use to query with `thing-at-point'.
It is also used for functions such as `bounds-of-thing-at-point'.")

;;;###autoload
(defun mark-thing-at (thing)
  "Mark THING \(symbol) at current point.
See function \`bounds-of-thing-at-point' for posibilities of
THING.  This function is also used to for the new region's
bounds.  Also see `thing-at-point' and \`mark-thing-at-choices'."
  (interactive (list (choice-program-complete "Mark thing: "
					     mark-thing-at-choices)))
  (if (not (memq thing mark-thing-at-choices))
      (error "Unknown thing to query: %S" thing))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if (null bounds) (error "No `%S' at point" thing))
    (goto-char (car bounds))
    (push-mark nil t t)
    (goto-char (cdr bounds))))

(defun mark-thing-at-attribs ()
  "Return attributes for the marked thing."
  (mapcar #'(lambda (choice)
	      `((name . ,choice)
		(mark-func . ,(intern (format "mark-%S" choice)))
		(binding-func . ,(intern (format "mark-%S-thing" choice)))))
	  mark-thing-at-choices))

;;;###autoload
(defun mark-thing-at-make-functions ()
  "Create function for each choice with names `mark-*' (i.e. mark-filename).
If this name is already that of a bound function, use `mark-*-thing'."
  (let (func-alist)
    (dolist (elt (mark-thing-at-attribs))
      (let ((name (cdr (assq 'name elt)))
	    (mark-func (cdr (assq 'mark-func elt)))
	    (binding-func (cdr (assq 'binding-func elt)))
	    fname)
	(setq fname (if (fboundp mark-func) binding-func mark-func))
	(eval `(defun ,fname ()
		 ,(format "Mark the current %S at point." name)
		 (interactive)
		 (mark-thing-at (quote ,name))
		 (message ,(format "Marked %S" name))))
	(setq func-alist (append func-alist (list (cons name fname)))))
      (defconst mark-thing-at-thing-to-command-alist func-alist
	"An alist of conses with the form `(THING . MARK-COMMAND)'.  Each thing
corresponds to its respective mark command."))))

(defun mark-thing-at-keybindings-help ()
  "Create a help buffer containing all mark-* functions with key bindings."
  (interactive)
  (with-current-buffer (get-buffer-create "*Mark Thing Help*")
    (save-excursion
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (elt (mark-thing-at-attribs))
	(let ((binding-func (cdr (assq 'binding-func elt))))
	  (where-is binding-func t)
	  (newline)))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun mark-thing-at-make-oplist (long-options)
  "Return an alist useful for making unique keys for options or key bindings.

An alist of \(LONG-OPTION . OPTION-CHAR) conses is returned.

LONG-OPTIONS are options to create character options from, which is a list of
strings."
  (let ((uhash (make-hash-table :test 'eq :size (length long-options)))
	ualist)
    (dolist (option long-options)
      (setq ualist
	    (append ualist
		    (list
		     (cl-block 'uas
		       (dolist (option-char (string-to-list option))
			 (when (not (gethash option-char uhash))
			   (puthash option-char t uhash)
			   (cl-return-from 'uas
			     (cons option option-char)))))))))
    ualist))

(defun mark-thing-at-make-keybindings (prefix)
  "Create keybindings for the mark-* functions.
PREFIX is the key used for the keybinding using `global-set-key'.
The functions are dynamically created with
`mark-thing-at-thing-to-command-alist'."
  (let ((option-alist (mark-thing-at-make-oplist
		       (mapcar #'symbol-name mark-thing-at-choices)))
	(mark-keymap (make-sparse-keymap))
	command)
    (global-set-key prefix mark-keymap)
    (dolist (elt option-alist)
      (setq command (cdr (assq (intern (car elt))
			       mark-thing-at-thing-to-command-alist)))
      (define-key mark-keymap (char-to-string (cdr elt)) command))
    (define-key mark-keymap "?" 'mark-thing-at-keybindings-help)))


;;; amended basic types
(defun mark-thing-at-beginning-of-number ()
  "Go to the position of beginning of the number at the cursor."
  (let ((regexp "[0-9\.-]"))
    (save-match-data
      (skip-chars-backward regexp)
      (if (or (looking-at regexp)
	      (re-search-backward regexp nil t))
	  (skip-chars-backward regexp)))))
(put 'number 'beginning-op 'mark-thing-at-beginning-of-number)

(defun mark-thing-at-end-of-number ()
  "Go to the position of end of the number at the cursor."
  (save-match-data (re-search-forward "[0-9.-]+" nil t)))
(put 'number 'end-op 'mark-thing-at-end-of-number)

;; don't span the next line like the default 'line thing
(put 'line-this 'beginning-op #'(lambda () (beginning-of-line)))
(put 'line-this 'end-op #'(lambda () (end-of-line)))

;; create the functions, let the user call `mark-thing-at-make-keybindings' if
;; they choose
(mark-thing-at-make-functions)

(provide 'mark-thing-at)

;;; mark-thing-at.el ends here
