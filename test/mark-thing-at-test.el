;;; package --- Summary
;;; Commentary:
;;
;; Unit tests of mark-thing-at-test.el
;;
;;; Code:

(require 'ert)
(require 'dash)
(require 'mark-thing-at)

(ert-deftest test-functions ()
  "Test successful function binding."
  (mark-thing-at-make-functions)
  (let ((funcs (-map #'(lambda (elt)
			 (intern (format "mark-%s" elt)))
		     mark-thing-at-choices)))
    (->> funcs
	 (-map #'(lambda (elt)
		   (fboundp elt)))
	 (equal (make-list (length funcs) t))
	 should)))

(ert-deftest test-function-bindings ()
  "Test excercise keybindings."
  (dolist (elt (mark-thing-at-bind))
    (should (eq 'string (type-of (cdr (assq 'binding elt)))))
    (should (eq 'symbol (type-of (cdr (assq 'command elt)))))
    (should (eq 'string (type-of (cdr (assq 'desc elt)))))))

(provide 'mark-thing-at-test)

;;; mark-thing-at-test.el ends here
