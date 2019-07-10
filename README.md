# Mark a pattern at the current point.

[![MELPA badge][melpa-badge]][melpa-link]
[![MELPA stable badge][melpa-stable-badge]][melpa-stable-link]
[![Travis CI Build Status][travis-badge]][travis-link]

This is a really more of a convenience for *things* in `thingatpt.el`.  The
library adds on functionality that:

* Generates functions that marks *things* (per the parlance of `thingatpt.el`
  library.
* Creates a mode-map and bindings to make it easy to mark (create a region
  around) a word, URL, file, number, symbol etc (see the [usage](#usage)
  section).
* Adds a *number thing*, which allows for traversing and marking numbers.

The variable `mark-thing-at-choices` defines a list of symbols for which to
make mark functions.


## Installation

Add [melpa] to your package archive sources:

```emacs-lisp
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
```

To install: `M-x package-install mark-thing-at`.

Activate and enable it in your `~/.emacs` file:

```emacs-lisp
;; this also creates mark commands for each thing
(require 'mark-thing-at)
;; clobbers `compose-mail', makes the binding the prefix key
(mark-thing-at-make-keybindings "\C-xm")
```

This binds all `mark-thing-at` functions to `C-x m`.


## Usage

To get the current set of available mark functions and their bindings, use
`mark-thing-at-keybindings-help`.  Assuming you followed the
[install](#install) section and bound the prefix key to `C-x m` you can use
`C-x m ?` to get the listing of functions and their binding.  Here's a list of
functions and what they mark:

| Binding        | Mark Function   | What's Marked                                 |
|----------------|-----------------|-----------------------------------------------|
| C-x m s        | mark-symbol     | Lisp symbol                                   |
| C-x m n        | mark-number     | Integer or float number                       |
| C-x m l        | mark-line-this  | Entire line (not including prompt in a shell) |
| C-x l, C-x m i | mark-line       | Eentire line with the newline                 |
| C-x m t        | mark-list       | List for the given language mode              |
| C-M-@, C-M-SPC | mark-sexp       | Lisp symbol expression                        |
| C-M-h          | mark-defun      | Function definition                           |
| C-x m f        | mark-filename   | File name                                     |
| C-x m u        | mark-url        | Universal resource locator                    |
| M-@            | mark-word       | Natural language word                         |
| C-x m c        | mark-sentence   | Natural language sentence                     |
| C-x m h        | mark-whitespace | Tabs and spaces                               |


## License

Copyright (c) 2019 Paul Landes

GNU Lesser General Public License, Version 2.0


<!-- links -->
[melpa-link]: https://melpa.org/#/mark-thing-at
[melpa-stable-link]: https://stable.melpa.org/#/mark-thing-at
[melpa-badge]: https://melpa.org/packages/mark-thing-at-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/mark-thing-at-badge.svg
[travis-link]: https://travis-ci.org/plandes/mark-thing-at
[travis-badge]: https://travis-ci.org/plandes/mark-thing-at.svg?branch=master

[melpa]: https://melpa.org/
