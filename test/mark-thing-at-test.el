;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of mark-thing-at-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'mark-thing-at)

(ert-deftest test-load ()
  "Test successful evaluation of mark-thing-at"
  (mark-thing-at-make-functions)
  (->> (-map #'(lambda (elt)
		 (fboundp elt))
	     '(mark-symbol-thing
	       mark-number-thing
	       mark-url-thing))
       (equal '(t t t))
       should))

(provide 'mark-thing-at-test)

;;; mark-thing-at-test ends here
