(defconstant +lisp-form-character+ #\$)
(defconstant +single-escape-character+ #\\)

(defconstant +whitespace-vertical+ '(#\Space #\Tab))
(defconstant +whitespace-horizontal+ '(#\Newline))
(defconstant +whitespace+ (append +whitespace-vertical+ +whitespace-horizontal+))

(defconstant +tag-open-char+ #\{)
(defconstant +tag-close-char+ #\})
(defconstant +tag-nest-char+ #\,)
(defconstant +tag-end-char+ #\;)
(defconstant +tag-class-char+ #\.)
(defconstant +tag-id-char+ #\#)
(defconstant +tag-attr-open-char+ #\[)
(defconstant +tag-attr-close-char+ #\])

(defconstant +amp-replace+ `(#\& ,(coerce (reverse "&#38;") 'list)))

(defconstant +tag-name-end-chars+ `(#\} #\. #\# #\[ #\; #\, ,@+whitespace+))
(defconstant +tag-name-ignored-chars+ '(#\/ #\>))
(defconstant +tag-name-unescapable-chars+ +whitespace+)

(defconstant +tag-class-end-chars+ `(#\} #\. #\# #\[ #\; #\, ,@+whitespace+))
(defconstant +tag-class-ignored-chars+ nil)
(defconstant +tag-class-unescapable-chars+ +whitespace+)
(defconstant +tag-class-replace-chars+ `((#\" ,(coerce (reverse "&quot;") 'list))
                                         ,+amp-replace+))

(defconstant +tag-id-end-chars+ `(#\} #\[ #\; #\, ,@+whitespace+))
(defconstant +tag-id-ignored-chars+ nil)
(defconstant +tag-id-unescapable-chars+ +whitespace+)
(defconstant +tag-id-replace-chars+ +tag-class-replace-chars+)

(defconstant +tag-attr-name-end-chars+ `(#\= #\] ,@+whitespace+))
(defconstant +tag-attr-name-ignored-chars+ `(#\/ #\>))
(defconstant +tag-attr-name-unescapable-chars+ +whitespace+)

(defconstant +tag-attr-sq-end-chars+ `(#\'))
(defconstant +tag-attr-sq-ignored-chars+ nil)
(defconstant +tag-attr-sq-unescapable-chars+ nil)
(defconstant +tag-attr-sq-replace-chars+ `((#\' ,(coerce (reverse "&#39;") 'list))
                                           ,+amp-replace+))

(defconstant +tag-attr-dq-end-chars+ `(#\"))
(defconstant +tag-attr-dq-ignored-chars+ nil)
(defconstant +tag-attr-dq-unescapable-chars+ nil)
(defconstant +tag-attr-dq-replace-chars+ `((#\" ,(coerce (reverse "&#34;") 'list))
                                           ,+amp-replace+))

(defconstant +tag-attr-uq-end-chars+ +whitespace+)
(defconstant +tag-attr-uq-ignored-chars+ nil)
(defconstant +tag-attr-uq-unescapable-chars+ nil)
(defconstant +tag-attr-uq-replace-chars+ +tag-attr-dq-replace-chars+)

(defconstant +tag-data-end-chars+ `(,+tag-open-char+ #\}))
(defconstant +tag-data-ignored-chars+ nil)
(defconstant +tag-data-unescapable-chars+ nil)
(defconstant +tag-data-replace-chars+ `((#\< ,(coerce (reverse "&#60;") 'list))
                                        ,+amp-replace+))

(defconstant +void-tags+ '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "source" "track" "wbr"))
