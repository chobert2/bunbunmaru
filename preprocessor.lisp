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

(defun read-lisp-form (stream char)
  "Function started by the +lisp-form-char+ macro character. Reads a
single lisp form. Atoms need to be formatted as a single form and
terminated with +lisp-form-char+."
  (let* ((*readtable* *lisp-form-readtable*)
         ;; Since we are using READ and nothing else, escaping +lisp-form-char+ works automatically.
         (form (read stream nil nil t))
         (peek (peek-char nil stream nil nil t)))
    (cond ((not form)
           ;; EOF; treat +lisp-form-char+ as a normal character.
           (list 'princ char '*preprocessor-stream*))
          (t
           (when (atom form)
             (if (and peek (char/= peek +lisp-form-char+))
                 (error "Atom not terminated with ~C: ~S" +lisp-form-char+ form)
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
        (and peek (char/= peek +sexpcode-end-char+)
             (error "Sexpcode not properly ended. Expected ~C, got ~C~%" +sexpcode-end-char+ peek))
        (or peek
            (error "Sexpcode not properly ended. Expected ~C, got EOF~%" +sexpcode-end-char+))
        (push (read-char stream nil nil t) characters)
        (push (list 'princ (coerce (nreverse characters) 'string) '*preprocessor-stream*) result)
        (return
          ;; If there's more than one result, wrap in PROGN to combine
          ;; the forms into a single unit; otherwise return the single
          ;; form as is.
          (if (cdr result)
              (cons 'progn (nreverse result))
              (car result)))))
