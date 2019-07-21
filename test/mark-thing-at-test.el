;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of mark-thing-at-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'mark-thing-at)

(ert-deftest test-function-binding ()
  "Test successful function binding."
  (mark-thing-at-make-functions)
  (let ((funcs (-map #'(lambda (elt)
			 (intern (format "mark-%s-thing" elt)))
		     mark-thing-at-choices)))
    (->> funcs
	 (-map #'(lambda (elt)
		   (fboundp elt)))
	 (equal (make-list (length funcs) t))
	 should)))

(ert-deftest test-function-binding ()
  "Test excercise keybindings."
  (should (mark-thing-at-bind)))

(provide 'mark-thing-at-test)

;;; mark-thing-at-test ends here
