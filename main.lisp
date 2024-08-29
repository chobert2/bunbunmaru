(defun tag-name (tag)
  (and tag (string-downcase (symbol-name (or (and (consp tag) (car tag)) tag)))))

(defun tagp (obj)
  (and (consp obj) (keywordp (car obj))))

(defstruct (token (:constructor make-token (type value position)))
  type value position)

(defconstant +escape-characters+ '(#\" "&quot;" #\& "&amp;" #\< "&lt;"))

(defun escape-html (html-string &optional (escapep t))
  (if escapep
      (with-output-to-string (result)
        (loop :for char :across html-string
              :do (princ (or (second (member char +escape-characters+ :test #'eql))
                             char)
                         result)))
      html-string))

(defun string-or-eval (form)
  (if (stringp form)
      form
      (let ((evaluated (eval form)))
        (if (stringp evaluated)
            evaluated
            ""))))

(defun process-tag (tag &key (escape-body t) (escape-attr t))
  (loop :with  tag-name                   = (or (tag-name tag) "")
        :with  tag-elems                  = (cdr tag)
        :with  tag-attr                   = (and tag-elems (make-string-output-stream))
        :with  tag-body                   = (and tag-elems (make-string-output-stream))
        :for   (tag-lval tag-rval . rest) = tag-elems
        :initially (when (keywordp (first tag-elems)) (write-char #\Space tag-attr))
        :while tag-elems
        :do
        (cond ((keywordp tag-lval)
               (format tag-attr
                       "~A=\"~A\"~A"
                       (tag-name tag-lval)
                       (escape-html (string-or-eval tag-rval) escape-attr)
                       (if (keywordp (first rest)) " " ""))
               (setq tag-elems (cddr tag-elems)))
              ((tagp tag-lval)
               (write-string (process-tag tag-lval) tag-body )
               (setq tag-elems (cdr tag-elems)))
              (t
               (and tag-lval (write-string (escape-html (string-or-eval tag-lval) escape-body)
                                           tag-body))
               (setq tag-elems (cdr tag-elems))))
        :finally (return (format nil
                                 "<~A~A>~A</~A>"
                                 tag-name
                                 (if tag-attr (get-output-stream-string tag-attr) "")
                                 (if tag-body (get-output-stream-string tag-body) "")
                                 tag-name))))

(define-condition scanner-eof (error)
  ((start :initarg :start :reader scanner-eof-start)
   (type  :initarg :type  :reader scanner-eof-type)
   (expected :initarg :expected :reader scanner-eof-expected))
  (:documentation "Thrown when EOF was found before one of the closing tokens.")
  (:report (lambda (condition stream)
             (format stream
                     "Reached end-of-file while scanning for token of type ~S at position ~D. Expected: '~A'."
                     (scanner-eof-type condition)
                     (scanner-eof-start condition)
                     (scanner-eof-expected condition)))))

(defun read-escape (stream)
  "Read either a single escape character, or an even number of escape characters.
In the former case returns a character, in the latter a string."
  (loop :with position = (file-position stream)
        :for n = 0 then (1+ n)
        :for char = (peek-char nil stream nil nil)
        :while (eql char #\\)
        :collect (read-char stream) into result
        :finally (return (and result (cond ((not (member char '(#\( #\)) :test #'eql))
                                            (make-token 'text result position))
                                           ((= n 1)
                                            (make-token 'escape nil position))
                                           (t
                                            (make-token 'text
                                                        (if (oddp n)
                                                            ;; We ate too much; give one back and
                                                            ;; adjust results accordingly.
                                                            (progn (unread-char #\\ stream)
                                                                   (cdr result))
                                                            result)
                                                        position)))))))

(defun read-paren (stream)
  (let ((position (file-position stream))
        (opening (cons (read-char stream) nil))
        (result nil))
    (loop :for char = (peek-char nil stream nil nil)
          :with type = (if (eql char #\:) 'tag 'form)
          :while (and char opening)
          :do
          (cond ((eql char #\\)
                 (let ((escape (read-escape stream)))
                   (if (and (eq (car escape) 'escape)
                            (eql (peek-char nil stream nil nil) #\)))
                       (push (read-char stream) result)
                       (and escape (setq result (append (cdr escape) result))))))
                ((eql char #\()
                 (push (car (push (read-char stream) opening)) result))
                ((eql char #\))
                 (push (read-char stream) result)
                 (pop opening))
                (t
                 (push (read-char stream) result)))
          :finally
          (and opening (error 'scanner-eof :start position :type type :expected ")"))
          (return (and result (make-token type (cons #\( (nreverse result)) position))))))

(defun read-until-char (stream &optional chars)
  (loop :with position = (file-position stream)
        :for char = (peek-char nil stream nil nil)
        :while (and char (not (member char chars :test #'eql)))
        :collect (read-char stream) :into result
        :finally (return (and result (make-token 'text result position)))))

(defun read-newline (stream)
  (loop :with position = (file-position stream)
        :for char = (peek-char nil stream nil nil)
        :for n = 0 then (1+ n)
        :while (eql char #\Newline)
        :collect (read-char stream) :into result
        :finally (return (when (> n 1) (make-token 'newline nil position)))))

(defun read-token (&optional stream eof-error-p eof-value)
  (let ((next-char (peek-char nil stream nil nil)))
    (cond ((not next-char)
           'eof)
          ((eql next-char #\Newline)
           (read-newline stream))
          ((eql next-char #\\)
           (read-escape stream))
          ((eql next-char #\()
           (read-paren stream))
          (t
           (read-until-char stream '(#\\ #\( #\Newline))))))

(defun tokenize (file)
  (with-open-file (stream file :direction :input)
    (loop :for token = (read-token stream)
          :while (not (eq token 'eof))
          :when token :collect token :into results
          :finally (return results))))

(defun parse (tokens)
  (loop :with parsed = nil
        :for token :in tokens
        :do
        (setf (token-value token) (coerce (token-value token) 'string))
        (unless (or (eq (token-type token) 'text) (eq (token-type token) 'newline))
          (setf (token-value token) (read-from-string (token-value token))))
        (push token parsed)
        :finally (return (nreverse parsed))))

(defun evaluate (parsed)
  (with-output-to-string (s)
    (loop with result = nil
          for token in parsed
          do
          (cond ((and (eq (token-type token) 'newline) result)
                 (princ (funcall #'process-tag `(:p ,@(nreverse result))) s)
                 (setq result nil))
                ((eq (token-type token) 'text)
                 (push (token-value token) result))
                (t
                 (push (token-value token) result)))
          finally (and result (princ (funcall #'process-tag `(:p ,@(nreverse result)))  s)))))

(defun process-page-body (body)
  (format nil
          "<html><head></head><body>~A</body></html>"
          body))

(defun process-page (in out)
  (with-open-file (f out :direction :output)
    (handler-case (progn
                    (princ (process-page-body (evaluate (parse (tokenize in)))) f)
                    (format t "~&[INFO] Successfully processed the file '~A'. Path: '~A'." in out))
      (scanner-eof (c)
        (format t "~&[FAIL] (~A): ~A~%" in c)))))

(defun process-pages (paths)
  (loop :for (in . out) :in paths
        :do
        (process-page in out)))
