# Mark a pattern at the current point.

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Build Status][build-badge]][build-link]

This package tracks and creates bindings for region marking functions found in
`thingatpt.el`.  The library adds on functionality that:

* Generates functions that marks *things* (per the parlance of `thingatpt.el`
  library).
* Creates a mode-map and bindings to make it easy to mark (create a region
  around) a word, URL, file, number, symbol etc (see the [usage](#usage)
  section).
* Adds a *number thing*, which allows for traversing and marking numbers.

The variable `mark-thing-at-choices` defines a list of symbols for which to
make mark functions.


## Installation

To install, add [melpa] if you have not already and a `use-package`
declaration in your `~/.emacs` file:

```emacs-lisp
;; configure add melpa as an archive source
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(use-package mark-thing-at
  :bind-keymap ("C-x m" . mark-thing-at-mode-map)
  :config
  (mark-thing-at-make-functions)
  (mark-thing-at-mode 1))
```

This binds all `mark-thing-at` functions to `C-x m`, which is the default
prefix.  If you do not like this prefix, you can change it by customizing the
prefix (`M-x customize-variable mark-thing-at-keymap-prefix`) and updating the
`:bind-keymap` declaration.  Once you save the customization, the change
immediately takes effect.


## Usage

To get the current set of available mark functions and their bindings, use
`mark-thing-at-keybindings-help`.  Assuming you followed the
[install](#install) section and bound the prefix key to `C-x m` you can use
`C-x m ?` to get the listing of functions and their binding.  Here's a list of
functions and what they mark:

| Binding  | Mark Function    | What's Marked                                 |
|----------|------------------|---------------------------------------------- |
| C-x m s  | mark-symbol      | Lisp symbol                                   |
| C-x m n  | mark-number      | Integer or float number                       |
| C-x m l  | mark-line-this   | Entire line (not including prompt in a shell) |
| C-x m i  | mark-line        | Entire line with the newline                  |
| C-x m t  | mark-list        | List for the given language mode              |
| C-x m e  | mark-sexp        | Lisp symbol expression                        |
| C-x m d  | mark-defun       | Function definition                           |
| C-x m f  | mark-filename    | File name                                     |
| C-x m u  | mark-url         | Universal resource locator                    |
| C-x m w  | mark-word        | Natural language word                         |
| C-x m c  | mark-sentence    | Natural language sentence                     |
| C-x m h  | mark-whitespace  | Tabs and spaces                               |
| C-x m p  | mark-page        | The page demarcated by ^L                     |
| C-x m a  | mark-latex-macro | Latex macro                                   |


## Adding New "Things" to Mark

You can add your own custom *markers* that work like those generated from the
predefined functions in `thingatpt`.  In this example we will show to do so
with LaTeX macros.  **Note** it is best to wrap this in a
`with-eval-after-load` so the calls to the library are defined.  First write
the marker function:

```emacs-lisp
;; the function that finds the extent of a LaTeX macro
(defun latex-macro-bounds ()
  "Return the bounds of the LaTeX macro around or at point.
This includes the macro name and its argument braces, if present."
  (save-excursion
    (let ((macro-backward-regex "\\\\[a-zA-Z]+\\*?")
	  (macro-forward-regex "[a-zA-Z]+")
          (brace-regex "{\\([^{}]*\\)}")
          start end)
      (when (re-search-backward macro-backward-regex nil t)
        (setq start (point))
	(re-search-forward macro-forward-regex nil t)
	(if (looking-at brace-regex)
	    (setq end (match-end 0))
	  (setq end (point))))
      (when (and start end)
        (cons start end)))))
```

Next, add it to the `mark-thing-at` library with the name, description, and the
new function that finds the bounds:
```emacs-lisp
;; add the marker, a new `mark-thing' function, and its binding
(mark-thing-at-add 'latex-macro "Latex macro" #'latex-macro-bounds t)
```


## License

Copyright (c) 2019 - 2025 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[melpa-link]: https://melpa.org/#/mark-thing-at
[melpa-stable-link]: https://stable.melpa.org/#/mark-thing-at
[melpa-badge]: https://melpa.org/packages/mark-thing-at-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/mark-thing-at-badge.svg
[build-badge]: https://github.com/plandes/mark-thing-at/workflows/CI/badge.svg
[build-link]: https://github.com/plandes/mark-thing-at/actions

[melpa]: https://melpa.org/
