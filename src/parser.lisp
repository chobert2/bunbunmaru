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

(defun buffer-not-full-p (buffer &optional (n 1))
  "Check if changing buffer position by N (default 1) will result in an overflow."
  (< (+ (buffer-position buffer) n) (buffer-length buffer)))

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
  (symbol-macrolet ((position (buffer-position buffer)))
    (if (buffer-not-full-p buffer n)
        (setf position (+ position n)
              (buffer-char buffer) (char (buffer-string buffer) position))
        (error "buffer position overflow"))
    position))

(defun buffer-parse-raw-string (buffer ending unescapable ignored &optional replace wrap)
  (let ((result nil))
    (tagbody
     :start
       (when (buffer-char-member buffer ending)
         (go :end))
     :skip?
       (when (buffer-char-member buffer ignored)
         (go :skip))
       (when (buffer-char= buffer +single-escape-character+)
         (buffer-advance buffer)
         (when (buffer-char-member buffer unescapable)
           (go :skip))
         (when (buffer-char= buffer #\&)
           (go :append)))
       (if replace
           (go :replace)
           (go :append))
     :skip
       (buffer-advance buffer)
       (go :start)
     :replace
       (let ((replacement (cadr (assoc (buffer-char buffer) replace))))
         (if replacement
             (progn (setf result (append replacement result))
                    (go :advance))
             (go :append)))
     :append
       (push (buffer-char buffer) result)
       (go :advance)
     :advance
       (buffer-advance buffer)
       (go :start)
     :end)
    (if (and result wrap)
        (coerce (cons wrap (nreverse (cons wrap result))) 'string)
        (coerce (nreverse result) 'string))))

(defun sexpcode-name (buffer)
  "Parse sexpcode name out of the buffer and return it.
Buffer should be positioned at the beginning of the sexpcode, or on the first character of the sexpcode name."
  (buffer-trim-advance-when-char= buffer +tag-open-char+)
  (let ((name (buffer-parse-raw-string buffer
                                       +tag-name-end-chars+
                                       +tag-name-unescapable-chars+
                                       +tag-name-ignored-chars+)))
    (if (zerop (length name))
        "div"
        name)))

(defun sexpcode-class (buffer)
  "Parse list of sexpcode classes out of the buffer and return them.
Buffer should be positioned after sexpcode name, but before the class
name separator, or on the first character of the first class name."
  (loop while (buffer-char= buffer +tag-class-char+)
        with classes = nil
        do
        (buffer-trim-advance-when-char= buffer +tag-class-char+)
        (let ((class (buffer-parse-raw-string buffer
                                              +tag-class-end-chars+
                                              +tag-class-unescapable-chars+
                                              +tag-class-ignored-chars+
                                              +tag-class-replace-chars+)))
          (if (zerop (length class))
              (warn "Empty class name; skipping")
              (push class classes)))
        (buffer-trim-advance-when-char= buffer +tag-class-char+)
        finally (return (nreverse classes))))

(defun sexpcode-id (buffer)
  "Parse sexpcode id out of the buffer and return it.
Buffer should be positioned on the id separating character, or any whitespace directly preceding said character."
  (buffer-trim-advance-when-char= buffer +tag-id-char+)
  (let ((id (buffer-parse-raw-string buffer
                                     +tag-id-end-chars+
                                     +tag-id-unescapable-chars+
                                     +tag-id-ignored-chars+
                                     +tag-id-replace-chars+)))
    (if (zerop (length id))
        (warn "Empty id name; skipping.")
        id)))

(defun sexpcode-attribute (buffer)
  "Parse sexpcode attributes out of the buffer and return them.
Buffer should be positioned on the attribute list starting character,
any whitespace preceding such character, or on the first character of
the first attribute name."
  (let ((attributes nil)
        (attribute-name nil)
        (attribute-value nil))
    (buffer-trim-advance-when-char= buffer +tag-attr-open-char+)
    (tagbody
     :attribute-name
       ;; Buffer should be pointing to a non-whitespace character.
       (setf attribute-name (buffer-parse-raw-string buffer
                                                     +tag-attr-name-end-chars+
                                                     +tag-attr-name-unescapable-chars+
                                                     +tag-attr-name-ignored-chars+))
       (buffer-advance-while-char-member buffer +whitespace+)
       (when (not (buffer-char= buffer #\=))
         (go :finish))
     :equal-sign
       ;; Tag not actually used, but included for clarity.
       (buffer-trim-advance-when-char= buffer #\=)
     :attribute-value
       ;; Tag not actually used, but included for clarity.
       (cond ((buffer-char= buffer #\")
              (buffer-advance buffer)
              (setf attribute-value (buffer-parse-raw-string buffer
                                                             +tag-attr-dq-end-chars+
                                                             +tag-attr-dq-unescapable-chars+
                                                             +tag-attr-dq-ignored-chars+
                                                             +tag-attr-dq-replace-chars+
                                                             #\"))
              (buffer-advance buffer))
             ((buffer-char= buffer #\')
              (buffer-advance buffer)
              (setf attribute-value (buffer-parse-raw-string buffer
                                                             +tag-attr-sq-end-chars+
                                                             +tag-attr-sq-unescapable-chars+
                                                             +tag-attr-sq-ignored-chars+
                                                             +tag-attr-sq-replace-chars+
                                                             #\'))
              (buffer-advance buffer))
             (t
              (setf attribute-value (buffer-parse-raw-string buffer
                                                             +tag-attr-uq-end-chars+
                                                             +tag-attr-uq-ignored-chars+
                                                             +tag-attr-uq-unescapable-chars+
                                                             +tag-attr-uq-replace-chars+
                                                             #\"))))
       (buffer-advance-while-char-member buffer +whitespace+)
     :finish
       (when (not (zerop (length attribute-name)))
         (push attribute-name attributes)
         (if (and attribute-value (> (length attribute-value) 0))
             (push attribute-value attributes)
             (push nil attributes)))
       ;; Buffer should be positioned past any trailing whitespace.
       (when (not (buffer-char= buffer +tag-attr-close-char+))
         (setf attribute-name nil
               attribute-value nil)
         (go :attribute-name)))
    (nreverse attributes)))

(defun sexpcode-content (buffer)
  "Parse sexpcode content out of the buffer and return it.
Buffer should be positioned on the tag ending character."
  (loop while (not (buffer-char= buffer +tag-close-char+))
        with result = nil
        ;; Allow single whitespace character to separate the tag and its contents.
        initially (buffer-advance-when-char-member buffer +whitespace+)
        do
        (let ((data (buffer-parse-raw-string buffer
                                             +tag-data-end-chars+
                                             +tag-data-ignored-chars+
                                             +tag-data-unescapable-chars+
                                             +tag-data-replace-chars+)))
          (when (not (zerop (length data)))
            (push data result))
          (when (buffer-char= buffer +tag-open-char+)
            (push (parse-sexpcode buffer) result)))
        finally
        (when (buffer-not-full-p buffer)
          (buffer-advance-when-char= buffer +tag-close-char+))
        (return (nreverse result))))

(defun parse-sexpcode (buffer)
  "Parse one, possibly nested sexpcode out of buffer and return it as a lisp object."
  (macrolet ((next-part (&optional expected-char &rest tags)
               "Jump to sexpcode part that should be processed next."
               (remove nil `(progn
                              (when (buffer-char= buffer +tag-nest-char+)
                                (buffer-advance buffer)
                                (push (parse-sexpcode buffer) content)
                                (go :exit))
                              (when (buffer-char= buffer +tag-end-char+)
                                (buffer-advance buffer)
                                (go :content))
                              ,(when tags
                                 '(buffer-advance-while-char-member buffer +whitespace+))
                              ,(when (member :id tags)
                                 `(when (buffer-char= buffer +tag-id-char+)
                                    (go :id)))
                              ,(when (member :attribute tags)
                                 `(when (buffer-char= buffer +tag-attr-open-char+)
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
         (next-part +tag-class-char+ :id :attribute)
     :class
       (setf class (sexpcode-class buffer))
       (next-part +tag-id-char+ :id :attribute)
     :id
       (setf id (sexpcode-id buffer))
       (next-part +tag-attr-open-char+)
     :attribute
       (setf attribute (sexpcode-attribute buffer))
       (buffer-advance-when-char= buffer +tag-attr-close-char+)
       (next-part)
     :content
         (setf content (sexpcode-content buffer)
               ;; Inefficient way to ignore content for void tags.
               content (and (not (member name +void-tags+ :test #'string-equal)) content))
     :exit nil)
      (list :name name :class class :id id :attribute attribute :content content))))

(defun parse-sexpcodes (tokens)
  (loop for token in tokens
        for buffer = (make-buffer token)
        with result = nil
        do
        (loop while (buffer-not-full-p buffer)
              do (push (parse-sexpcode buffer) result))
        finally (return (nreverse result))))
