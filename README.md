This is a work in progress.

I did not like any of the available static website generators, so I decided to write yet another one. This allows me to tailor it exactly to my needs, and use whatever tools I would like.

Implemented:

- transforming forms like `(:h1 [:attr1 "val1" ...] ...)` into equivalent html `<h1 [attr1="val1"]>...</h1>` with automatic escaping of problematic characters in both attribute and body. Values for attributes and body can be strings or lisp forms (evaluated using `eval`.)
  - `(:h1 "Title&")` => `<h1>Title&amp;</h1>`
  - `(:h1 :id "\"hello\"" "Title&")` => `<h1 id="&quot;hello&quot;">Title&amp;</h1>`
- evaluating lisp forms in text
- parsing of paragraphs
- escape character \

To be implemented:

- blocks like in org mode
  - src blocks fontified through emacs client and htmlize
- error handling on parsing
- line/column number tracking in the parser
- pre/post hooks on tags (e.g. pre-processing hyperlink's :href element)
  - pre would be run on a tag before it's converted to html
  - post would run after it has been converted to html
- site configuration
- per page configuration
- command line flags
- makefile for standalone binary
