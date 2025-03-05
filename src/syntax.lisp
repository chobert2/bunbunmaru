(defconstant +lisp-form-character+ #\$)
(defconstant +single-escape-character+ #\\)

(defconstant +void-tags+ '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "source" "track" "wbr"))

(defconstant +whitespace-vertical+ '(#\Space #\Tab))
(defconstant +whitespace-horizontal+ '(#\Newline))
(defconstant +whitespace+ (append +whitespace-vertical+ +whitespace-horizontal+))

(defconstant +sexpcode-starting-character+ #\{)
(defconstant +sexpcode-ending-character+ #\})
(defconstant +sexpcode-class-separating-character+ #\.)
(defconstant +sexpcode-id-separating-character+ #\#)
(defconstant +sexpcode-attribute-list-starting-character+ #\[)
(defconstant +sexpcode-attribute-list-ending-character+ #\])
(defconstant +sexpcode-tag-ending-character+ #\;)
(defconstant +sexpcode-tag-nesting-character+ #\,)

(defconstant +sexpcode-tag-terminating-characters+
  (list +sexpcode-tag-ending-character+
        +sexpcode-tag-nesting-character+))

(defconstant +sexpcode-name-ending-characters+
  (append (list +sexpcode-starting-character+
                +sexpcode-ending-character+
                +sexpcode-class-separating-character+
                +sexpcode-id-separating-character+
                +sexpcode-attribute-list-starting-character+)
          +sexpcode-tag-terminating-characters+
          +whitespace+))

(defconstant +sexpcode-class-ending-characters+
  +sexpcode-name-ending-characters+)

(defconstant +sexpcode-id-ending-characters+
  (remove +sexpcode-class-separating-character+
          +sexpcode-class-ending-characters+))

(defconstant +sexpcode-attribute-name-invalid-characters+
  (list #\/ #\> #\=))
(defconstant +sexpcode-attribute-name-ending-characters+
  (append (list #\= #\]) +whitespace+))
(defconstant +sexpcode-attribute-unquoted-ending-characters+
  (append +whitespace+ '(#\])))
