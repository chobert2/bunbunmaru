;;;; Code for preprocessing the page, i.e. for replacing all of the
;;;; lisp forms embedded in the page with their values, as
;;;; strings. This separate preprocessing step is needed so that
;;;; sexpcode can be returned from lisp code. Not only would this not
;;;; be possible if we processed sexpcode and lisp forms at the same
;;;; time, we would have to use EVAL on encountered lisp forms, losing
;;;; the lexical environment in the process.

;;;; TODO:
;;;; 1. Improve the error handling
;;;;    - report the position in stream, if available (FILE-POSITION)
;;;;    - report the filename, if available (PATHNAME, TRUENAME)
;;;; 2. Replace COND with CASE where possible.
;;;; 3. Add example usage.
;;;; 4. Add tests.
;;;; 5. Add support for parsing lambdaway style tags _h1 etc.
;;;; 6. See if it makes sense for paragraphs to start with just text instead of _p.
;;;; 7. Add support for blocks, including ones where preprocessing is not performed.
;;;; 8. See if it makes sense to support html-style (or equivalent) opening/close tags for wrapping large sections.
;;;; 9. Write the equivalent parser.lisp where the post-process text will actually get parsed.

(defconstant +lisp-form-char+ #\$)
(defconstant +sexpcode-beg-char+ #\{)
(defconstant +sexpcode-end-char+ #\})
(defconstant +escape-char+ #\\)

(defparameter *preprocessor-stream* *standard-output*
  "Stream to which all of the preprocessor output is written to.")

(defparameter *preprocessor-readtable* (copy-readtable)
  "Readtable used when parsing sexpcode that has lisp code embedded using +lisp-form-char+.")
(set-macro-character +sexpcode-beg-char+ 'read-sexpcode nil *preprocessor-readtable*)
(set-macro-character +lisp-form-char+ 'read-lisp-form nil *preprocessor-readtable*)

(defun mark-terminating (stream char)
  (declare (ignore stream))
  (error "Tried to read a dummy terminating character ~C" char))

(defparameter *lisp-form-readtable* (copy-readtable *preprocessor-readtable*)
  "Readtable used when parsing lisp forms delimited by +lisp-form-char+.")
(set-macro-character +lisp-form-char+ 'mark-terminating)

(define-condition bbmaru-unterminated-atom (error)
  ((filename :initarg :filename
             :initform nil
             :accessor bbmaru-unterminated-atom-filename)
   (position :initarg :position
             :initform nil
             :accessor bbmaru-unterminated-atom-position)
   (terminator :initarg :terminator
               :initform +lisp-form-char+
               :accessor bbmaru-unterminated-atom-terminator)
   (read-form :initarg :read-form
              :initform nil
              :accessor bbmaru-unterminated-atom-read-form))
  (:documentation "Error thrown inside READ-LISP-FORM when an atom was not terminated with +lisp-form-char+.")
  (:report (lambda (condition stream)
             (format stream
                     ;; [file:position] Atom...                  if file and position non-nil
                     ;; [?:position] Atom... or [file:?] Atom... if file or position nil
                     ;; Atom...                                  if file and position nil
                     "~:[~:[~;~:*[?:~A] ~]~;~:*[~A:~:[?~;~:*~A~]] ~]Atom not terminated with ~A: ~A~&"
                     (bbmaru-unterminated-atom-filename condition)
                     (bbmaru-unterminated-atom-position condition)
                     (bbmaru-unterminated-atom-terminator condition)
                     (bbmaru-unterminated-atom-read-form condition)))))

(define-condition bbmaru-unterminated-sexpcode (error)
  ((filename :initarg :filename
             :initform nil
             :accessor bbmaru-unterminated-sexpcode-filename)
   (position :initarg :position
             :initform nil
             :accessor bbmaru-unterminated-sexpcode-position)
   (terminator :initarg :terminator
               :initform +sexpcode-end-char+
               :accessor bbmaru-unterminated-sexpcode-terminator)
   (read-form :initarg :read-form
              :initform nil
              :accessor bbmaru-unterminated-sexpcode-read-form))
  (:documentation "Error thrown inside READ-SEXPCODE when a sexpcode was not terminated with +sexpcode-end-char+.")
  (:report (lambda (condition stream)
             (format stream
                     ;; [file:position] Sexpcode...                  if file and position non-nil
                     ;; [?:position] Sexpcode... or [file:?] Atom... if file or position nil
                     ;; Sexpcode...                                  if file and position nil
                     "~:[~:[~;~:*[?:~A] ~]~;~:*[~A:~:[?~;~:*~A~]] ~]Sexpcode not properly ended. Expected ~A, got ~A~&"
                     (bbmaru-unterminated-sexpcode-filename condition)
                     (bbmaru-unterminated-sexpcode-position condition)
                     (bbmaru-unterminated-sexpcode-terminator condition)
                     (bbmaru-unterminated-sexpcode-read-form condition)))))

(defun read-lisp-form (stream char)
  "Function started by the +lisp-form-char+ macro character. Reads a
single lisp form. Atoms need to be formatted as a single form and
terminated with +lisp-form-char+."
  (let* ((*readtable* *lisp-form-readtable*)
         ;; Unlike NAMESTRING this does not error on streams not associated with files.
         (position (file-position stream))
         ;; Since we are using READ and nothing else, escaping +lisp-form-char+ works automatically.
         (form (read stream nil nil t))
         (peek (peek-char nil stream nil nil t)))
    (cond ((not form)
           ;; EOF; treat +lisp-form-char+ as a normal character.
           (list 'princ char '*preprocessor-stream*))
          (t
           (when (atom form)
             (if (and peek (char/= peek +lisp-form-char+))
                 (error 'bbmaru-unterminated-atom
                        :filename (ignore-errors (namestring stream))
                        :position position
                        :terminator +lisp-form-char+
                        :read-form form)
                 (and peek (read-char stream nil nil t))))
           (list 'princ form '*preprocessor-stream*)))))

(defun read-escape-literally (stream &optional characters)
  "Reads all of the consecutive +escape-char+'s in STREAM, plus the
next character if the number of read escapes was odd. If CHARACTERS
was provided, it will be appendeded to the result."
  (loop for peek = (peek-char nil stream nil nil t)
        for count = 0 then (1+ count)
        with result = nil
        while (and peek (char= peek +escape-char+))
        do
        (push (read-char stream nil nil t) result)
        finally
        (when (and (oddp count) peek)
          (push (read-char stream nil nil t) result))
        (return (append result characters))))

(defun read-sexpcode (stream char)
  "Function started by the +sexpcode-beg-char+ macro character. Reads
a sexpcode between +sexpcode-beg-char+ and +sexpcode-end-char+
recursively."
  (loop for peek = (peek-char nil stream nil nil t)
        with characters = (cons char nil)
        with result = nil
        ;; Unlike NAMESTRING this does not error on streams not associated with files.
        with position = (file-position stream)
        while (and peek (char/= peek +sexpcode-end-char+))
        do
        (cond ((char= peek +escape-char+)
               (setf characters (read-escape-literally stream characters)))
               ((or (char= peek +lisp-form-char+) (char= peek +sexpcode-beg-char+))
               (and characters (push (list 'princ (coerce (nreverse characters) 'string) '*preprocessor-stream*) result))
               (setf characters nil)
               (push (read stream nil nil t) result))
              (t
               (push (read-char stream nil nil t) characters)))
        finally
        (when (or (not peek) (char/= peek +sexpcode-end-char+))
          (error 'bbmaru-unterminated-sexpcode
                 :filename (ignore-errors (namestring stream))
                 :position position
                 :terminator +sexpcode-end-char+
                 :read-form (or peek "EOF")))
        (push (read-char stream nil nil t) characters)
        (push (list 'princ (coerce (nreverse characters) 'string) '*preprocessor-stream*) result)
        (return
          ;; If there's more than one result, wrap in PROGN to combine
          ;; the forms into a single unit; otherwise return the single
          ;; form as is.
          (if (cdr result)
              (cons 'progn (nreverse result))
              (car result)))))
