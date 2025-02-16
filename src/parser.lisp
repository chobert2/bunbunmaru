(defun skip-string-whitespace (string &optional (start 0) (end (1- (length string))))
  (loop for i from start to end
        if (not (whitespace-char-p (aref string i))) do (return i)
        finally (return end)))

(defun parse-sexpcode-tag (sexpcode &optional (index 1))
  (loop while t
        with start = (if (char= (aref sexpcode index) #\{) (1+ index) index)
        for end = (skip-string-whitespace sexpcode start) then (1+ end)
        with char = nil
        do
        (setf char (aref sexpcode end))
        (when (or (whitespace-char-p char) (char= char #\.) (char= char #\#) (char= char #\[))
          (return (values (subseq sexpcode start end) end)))))

(defun parse-sexpcode-class (sexpcode &optional (index 1))
  (loop while t
        with start = (1+ index)
        for end = start then (1+ end) 
        with char = nil
        with classes = nil
        do
        (setf char (aref sexpcode end))
        (when (or (whitespace-char-p char) (char= char #\#) (char= char #\[))
          (push (subseq sexpcode start end) classes)
          (return (values (nreverse classes) end)))
        (when (char= char #\.)
          (push (subseq sexpcode start end) classes)
          (setf start (1+ end)))))

(defun parse-sexpcode-id (sexpcode &optional (index 1))
  (loop while t
        with start = (if (char= (aref sexpcode index) #\#) (1+ index) index)
        for end = start then (1+ end)
        with char = nil
        do
        (setf char (aref sexpcode end))
        (when (or (whitespace-char-p char) (char= char #\[) (char= char #\}))
          (return (values (subseq sexpcode start end) end)))))

(defun html-space-p (code)
  ;; U+0020 SPACE U+0009 CHARACTER TABULATION (tab) U+000A LINE FEED (LF)
  ;; U+000C FORM FEED (FF) U+000D CARRIAGE RETURN (CR)
  (not (/= code 32 9 10 12 13)))

(defun html-skip-whitespace-in-string (string &optional (start 0) (end (length string) end-supplied-p))
  (loop for i from start to (if end-supplied-p end (1- end))
        if (not (html-space-p (char-code (aref string i)))) do (return i)
        finally (return end)))

(defun unicode-control-character-p (code)
  (or (< code 32) (and (>= code 128) (<= code 159))))

(defun html-attribute-name-char-p (code)
  ;; Characters other than space characters, NULL, ", ', >, /, = and the control characters.
  ;; The standard also specifies "characters not defined by unicode" are also invalid, but
  ;; I don't know how to check for that, or how to test it.
  ;; NULL, ", ', >, /, =, ] (for sexpcode syntax)
  (and
   (/= code 0 34 39 62 47 61 93)
   (not (or (html-space-p code) (unicode-control-character-p code)))))

(defun html-attribute-value-unquoted-char-p (code)
  (and
   ;; NULL, ", ', =, >, <, `
   (/= code 0 34 39 61 62 60 96)
   (not (or (html-space-p code) (unicode-control-character-p code)))))

(defun html-attribute-value-single-quoted-char-p (code)
  (and
   ;; ", ', =, >, <, `
   (/= code 34 39 61 62 60 96)
   (not (html-space-p code))))

(defun parse-sexpcode-attribute-name (sexpcode &optional (index 0))
  (loop with length = (length sexpcode)
        with end = (1- length)
        for i from index to end
        if (not (html-attribute-name-char-p (char-code (aref sexpcode i)))) do (loop-finish)
        finally (return (values (subseq sexpcode index i) i))))

(defun parse-sexpcode-attribute-value-unquoted (sexpcode &optional (index 0))
  (loop with length = (length sexpcode)
        with end = (1- length)
        for i from index to end
        if (not (html-attribute-value-unquoted-char-p (char-code (aref sexpcode i)))) do (loop-finish)
        finally (return (values (subseq sexpcode index i) i))))

(defun parse-sexpcode-attribute-value-double-quoted (sexpcode &optional (index 0))
  (loop with start = (if (char= (aref sexpcode index) #\") (1+ index) index)
        with length = (length sexpcode)
        for end = start then (1+ end)
        while (< end length)
        if (char= (aref sexpcode end) #\") do (return (values (subseq sexpcode start end) (1+ end)))
        finally (error "Double quote attribute not terminated with \"")))

(defun parse-sexpcode-attribute-value-single-quoted (sexpcode &optional (index 0))
  (loop with start = (if (char= (aref sexpcode index) #\') (1+ index) index)
        with length = (length sexpcode)
        for end = start then (1+ end)
        while (< end length)
        if (char= (aref sexpcode end) #\') do (return (values (subseq sexpcode start end) (1+ end)))
        finally (error "Single quote attribute not terminated with '")))

(defun parse-sexpcode-attributes (sexpcode &optional (index 1))
  (loop with start = (html-skip-whitespace-in-string sexpcode (if (char= (aref sexpcode index) #\[) (1+ index) index))
        with length = (length sexpcode)
        with char = nil
        with attribute-name = nil
        with attribute-value = nil
        with attributes = nil
        while (< start length)
        do
        (format t "~&start; length: ~A, start ~A~%" length start)
        (when (char= (aref sexpcode start) #\])
          (loop-finish))
        (multiple-value-bind (retval retindex)
            (parse-sexpcode-attribute-name sexpcode start)
          (setf attribute-name retval
                start (html-skip-whitespace-in-string sexpcode retindex)))
        (format t "~&hehe; length: ~A, start ~A~%" length start)
        (cond ((char/= (aref sexpcode start) #\=)
               ;; Empty attribute
               (push attribute-name attributes)
               (push nil attributes))
              (t
               (setf start (html-skip-whitespace-in-string sexpcode (1+ start))) ;; Skipping =
               (cond ((char= (aref sexpcode start) #\")
                      (multiple-value-bind (retval retindex)
                          (parse-sexpcode-attribute-value-double-quoted sexpcode start)
                        (setf attribute-value retval
                              start (html-skip-whitespace-in-string sexpcode retindex))))
                     ((char= (aref sexpcode start) #\')
                      (multiple-value-bind (retval retindex)
                          (parse-sexpcode-attribute-value-single-quoted sexpcode start)
                        (setf attribute-value retval
                              start (html-skip-whitespace-in-string sexpcode retindex))))
                     (t
                      ;; Unquoted attribute value
                      (multiple-value-bind (retval retindex)
                          (parse-sexpcode-attribute-value-unquoted sexpcode start)
                        (setf attribute-value retval
                              start (html-skip-whitespace-in-string sexpcode retindex)))))
               (push attribute-name attributes)
               (push attribute-value attributes)))
        (format t "~&end; length: ~A, start ~A~%" length start)
        (when (char= (aref sexpcode start) #\])
          (loop-finish))
        finally (return (values (nreverse attributes) start))))
                      
(defun parse-sexpcode (sexpcode)
  (let ((tag-name nil)
        (tag-classes nil)
        (tag-id nil)
        (tag-attributes nil)
        (index 0))
    (multiple-value-bind (retval retindex)
        (parse-sexpcode-tag sexpcode index)
      (setf tag-name retval
            index retindex))
    (when (char= (aref sexpcode index) #\.)
      (multiple-value-bind (retval retindex)
          (parse-sexpcode-class sexpcode index)
        (setf tag-classes retval
              index retindex)))
    (when (char= (aref sexpcode index) #\#)
      (multiple-value-bind (retval retindex)
          (parse-sexpcode-id sexpcode index)
        (setf tag-id retval
              index retindex)))
    (when (char= (aref sexpcode index) #\[)
      (multiple-value-bind (retval retindex)
          (parse-sexpcode-attributes sexpcode index)
        (setf tag-attributes retval
              index retindex)))
    (list :tag-name tag-name :tag-classes tag-classes :tag-id tag-id :tag-attributes tag-attributes)))

;; A C0 control is a code point in the range U+0000 NULL to U+001F INFORMATION SEPARATOR ONE, inclusive. (0-31)
;; A control is a C0 control or a code point in the range U+007F DELETE to U+009F APPLICATION PROGRAM COMMAND, inclusive. (0-31, 127-159)
;; A noncharacter is a code point that is in the range U+FDD0 to U+FDEF, inclusive, or U+FFFE, U+FFFF, U+1FFFE, U+1FFFF, U+2FFFE, U+2FFFF, U+3FFFE, U+3FFFF, U+4FFFE, U+4FFFF, U+5FFFE, U+5FFFF, U+6FFFE, U+6FFFF, U+7FFFE, U+7FFFF, U+8FFFE, U+8FFFF, U+9FFFE, U+9FFFF, U+AFFFE, U+AFFFF, U+BFFFE, U+BFFFF, U+CFFFE, U+CFFFF, U+DFFFE, U+DFFFF, U+EFFFE, U+EFFFF, U+FFFFE, U+FFFFF, U+10FFFE, or U+10FFFF. 
;; Attribute names must consist of one or more characters other than controls, U+0020 SPACE, U+0022 ("), U+0027 ('), U+003E (>), U+002F (/), U+003D (=), and noncharacters.
