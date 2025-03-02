(defstruct (buffer (:constructor %make-buffer))
  "Helper structure for easier parsing of strings into syntax."
  (string nil :type string)
  (char nil :type character)
  (position nil :type fixnum)
  (length nil :type fixnum))

(defun make-buffer (string)
  "Make a buffer structure out of string."
  (or (stringp string) (error "make-buffer was not passed a string"))
  (let ((length (length string)))
    (or (> length 0) (error "make-buffer was passed an empty string"))
    (%make-buffer :string string :position 0 :length length :char (char string 0))))

(declaim (inline buffer-char=))
(defun buffer-char= (buffer character)
  "Check if current character is CHAR= to CHARACTER."
  (and character (char= (buffer-char buffer) character)))

(declaim (inline buffer-char-member))
(defun buffer-char-member (buffer characters)
  "Check if current character is one of CHARACTERS."
  (not (apply #'char/= (buffer-char buffer) characters)))

(defun buffer-advance-when-char= (buffer character)
  "Advance if current character is CHAR= to CHARACTER."
  (or (and (buffer-char= buffer character) (buffer-advance buffer))
      (buffer-position buffer)))

(defun buffer-advance-when-char-member (buffer characters)
  "Advance if current character is one of CHARACTERS."
  (or (and (buffer-char-member buffer characters) (buffer-advance buffer))
      (buffer-position buffer)))

(defun buffer-advance-while-char-member (buffer characters)
  "Keep advancing until current character is not one of CHARACTERS."
  (loop while (buffer-char-member buffer characters)
        do (buffer-advance buffer)
        finally (return (buffer-position buffer))))

(defun buffer-trim-advance-when-char= (buffer character)
  "Advance until current character is not whitespace. Then, skip the
current character and whitespace following it, if it's CHAR= to CHARACTER."
  (buffer-advance-while-char-member buffer +whitespace+)
  (buffer-advance-when-char= buffer character)
  (buffer-advance-while-char-member buffer +whitespace+)
  (buffer-position buffer))

(defun buffer-advance (buffer &optional (n 1))
  "Advance buffer position by N characters (default 1)."
  (let ((position (+ (buffer-position buffer) n)))
    (or (< position (buffer-length buffer)) (error "buffer position overflow"))
    (setf (buffer-position buffer) position
          (buffer-char buffer) (char (buffer-string buffer) position))
    position))

(defun buffer-substring (buffer &optional (start 0) (end (buffer-position buffer)))
  "Return a string of characters in the buffer between START (inclusive) and END (exclusive)."
  (subseq (buffer-string buffer) start end))

(defun buffer-substring-on (buffer start characters)
  (loop while (not (buffer-char-member buffer characters))
        do
        (if (buffer-char= buffer #\\)
            ;; Skip escaped character.
            (buffer-advance buffer 2)
            (buffer-advance buffer))
        finally (return (buffer-substring buffer start))))

(defun buffer-delimited-substring (buffer character)
  "Return the text between two CHARACTERs.
Buffer should be positioned on the first CHARACTER, or directly after it."
  (let ((start (buffer-advance-when-char= buffer character)))
    (loop while (not (buffer-char= buffer character))
          do
          (if (buffer-char= buffer #\\)
              (buffer-advance buffer 2)
              (buffer-advance buffer))
          finally
          (return (prog1 (buffer-substring buffer start)
                    (buffer-advance buffer))))))

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

(defun sexpcode-name (buffer)
  "Parse sexpcode name out of the buffer and return it.
Buffer should be positioned at the beginning of the sexpcode, or on the first character of the sexpcode name."
  (let ((start (buffer-trim-advance-when-char= buffer +sexpcode-starting-character+)))
    (and (buffer-char-member buffer +sexpcode-name-ending-characters+)
         (error "Sexpcode name started with invalid character"))
    (and start (buffer-substring-on buffer start +sexpcode-name-ending-characters+))))

(defun sexpcode-class (buffer)
  "Parse list of sexpcode classes out of the buffer and return them.
Buffer should be positioned after sexpcode name, but before the class
name separator, or on the first character of the first class name."
  (loop while (buffer-char= buffer +sexpcode-class-separating-character+)
        collect (buffer-substring-on buffer
                                     (buffer-trim-advance-when-char= buffer +sexpcode-class-separating-character+)
                                     +sexpcode-class-ending-characters+)
        into classes
        do (buffer-advance-while-char-member buffer +whitespace+)
        finally (return classes)))

(defun sexpcode-id (buffer)
  "Parse sexpcode id out of the buffer and return it.
Buffer should be positioned on the id separating character, or any whitespace directly preceding said character."
  (let ((start (buffer-trim-advance-when-char= buffer +sexpcode-id-separating-character+)))
    (and (buffer-char-member buffer +sexpcode-id-ending-characters+)
         (error "Sexpcode id started with invalid character"))
    (and start (buffer-substring-on buffer start +sexpcode-id-ending-characters+))))

(defun sexpcode-attribute (buffer)
  "Parse sexpcode attributes out of the buffer and return them.
Buffer should be positioned on the attribute list starting character,
any whitespace preceding such character, or on the first character of
the first attribute name."
  (let ((attributes nil)
        (start (buffer-trim-advance-when-char= buffer +sexpcode-attribute-list-starting-character+))
        (attribute-name nil)
        (attribute-value nil))
    (tagbody
     :attribute-name
       ;; Buffer should be pointing to a non-whitespace character.
       (when (buffer-char-member buffer +sexpcode-attribute-name-invalid-characters+)
         ;; Ensure that the attribute has at least one valid character, so that
         ;; sanitizing for use in HTML later doesn't result in an empty string.
         (error "Attribute name starts with an invalid character"))
       (setf attribute-name (buffer-substring-on buffer start +sexpcode-attribute-name-ending-characters+))
       (buffer-advance-while-char-member buffer +whitespace+)
       (when (not (buffer-char= buffer #\=))
         (go :finish))
     :equal-sign
       ;; Tag not actually used, but included for clarity.
       (setf start (buffer-trim-advance-when-char= buffer #\=))
     :attribute-value
       ;; Tag not actually used, but included for clarity.
       (let* ((buffer-char (buffer-char buffer))
              (quote-type (when (buffer-char-member buffer '(#\" #\')) buffer-char)))
         (setf attribute-value
               (if quote-type
                   (buffer-delimited-substring buffer quote-type)
                   (buffer-substring-on buffer start +sexpcode-attribute-unquoted-ending-characters+))))
       (buffer-advance-while-char-member buffer +whitespace+)
     :finish
       (push attribute-name attributes)
       (push attribute-value attributes)
       ;; Buffer should be positioned past any trailing whitespace.
       (when (not (buffer-char= buffer +sexpcode-attribute-list-ending-character+))
         (setf start (buffer-position buffer)
               attribute-name nil
               attribute-value nil)
         (go :attribute-name)))
    (nreverse attributes)))

(defun sexpcode-content (buffer)
  "Parse sexpcode content out of the buffer and return it.
Buffer should be positioned on the tag ending character."
  (loop while (not (buffer-char= buffer +sexpcode-ending-character+))
        ;; Allow single whitespace character to separate the tag and its contents.
        with start = (buffer-advance-when-char-member buffer +whitespace+)
        with result = nil
        do
        (cond ((buffer-char= buffer #\\)
               (buffer-advance buffer 2))
              ((buffer-char= buffer +sexpcode-starting-character+)
               (push (buffer-substring buffer start) result)
               (push (parse-sexpcode buffer) result)
               (setf start (buffer-position buffer)))
              (t
               (buffer-advance buffer)))
        finally
        (push (buffer-substring buffer start) result)
        (when (< (1+ (buffer-position buffer)) (buffer-length buffer))
          (buffer-advance-when-char= buffer +sexpcode-ending-character+))
        (return (nreverse result))))

(defun parse-sexpcode (buffer)
  "Parse one, possibly nested sexpcode out of buffer and return it as a lisp object."
  (macrolet ((next-part (&optional expected-char &rest tags)
               "Jump to sexpcode part that should be processed next."
               (remove nil `(progn
                              (when (buffer-char= buffer +sexpcode-tag-nesting-character+)
                                (buffer-advance buffer)
                                (setf content (parse-sexpcode buffer))
                                (go :exit))
                              (when (buffer-char= buffer +sexpcode-tag-ending-character+)
                                (buffer-advance buffer)
                                (go :content))
                              ,(when tags
                                 '(buffer-advance-while-char-member buffer +whitespace+))
                              ,(when (member :id tags)
                                 `(when (buffer-char= buffer +sexpcode-id-separating-character+)
                                    (go :id)))
                              ,(when (member :attribute tags)
                                 `(when (buffer-char= buffer +sexpcode-attribute-list-starting-character+)
                                    (go :attribute)))
                              ,(if expected-char
                                   `(when (not (buffer-char= buffer ,expected-char))
                                      (error "Sexpcode not terminated with ;"))
                                   '(error "Sexpcode not terminated with ;"))))))
    (let ((name nil)
          (class nil)
          (id nil)
          (attribute nil)
          (content nil))
      (tagbody
       :name
         (setf name (sexpcode-name buffer))
         (next-part #\. :id :attribute)
     :class
       (setf class (sexpcode-class buffer))
       (next-part #\# :id :attribute)
     :id
       (setf id (sexpcode-id buffer))
       (next-part #\[)
     :attribute
       (setf attribute (sexpcode-attribute buffer))
       (buffer-advance-when-char= buffer #\])
       (next-part)
     :content
       (setf content (sexpcode-content buffer))
     :exit nil)
    (list :name name :class class :id id :attribute attribute :content content))))

(defun parse-sexpcodes (tokens)
  (loop for token in tokens
        for buffer = (make-buffer token)
        with result = nil
        do
        (loop while (< (buffer-position buffer) (buffer-length buffer))
              do (push (parse-sexpcode buffer) result))
        finally (return (nreverse result))))
