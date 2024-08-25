(defun tag-name (tag)
  (and tag (string-downcase (symbol-name (or (and (consp tag) (car tag)) tag)))))

(defun tagp (obj)
  (and (consp obj) (keywordp (car obj))))

(defun whitespace-p (char)
  (member char '(#\Space #\Tab #\Linefeed #\Return #\Page) :test #'eql))

(defun escape-attribute (attribute)
  (with-output-to-string (result)
    (loop for char across attribute
          do
          ;; In HTML5 we only really need to escape " and & (https://mina86.com/2021/no-you-dont-need-to-escape-that/)
          (cond ((char= char #\")
                 (format result "&quot;"))
                ((char= char #\&)
                 (format result "&amp;"))
                (t
                 (format result "~C" char))))))

(defun escape-body (data)
  (with-output-to-string (result)
    (loop for char across data
          do
          ;; In HTML5 we only really need to escape < and & (https://mina86.com/2021/no-you-dont-need-to-escape-that/)
          (cond ((char= char #\<)
                 (format result "&lt;"))
                ((char= char #\&)
                 (format result "&amp;"))
                (t
                 (format result "~C" char))))))

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
                       (cond ((not tag-rval)
                              "")
                             (escape-attr
                              (escape-attribute tag-rval))
                             (t
                              tag-rval))
                       (if (keywordp (first rest)) " " ""))
               (setq tag-elems (cddr tag-elems)))
              ((tagp tag-lval)
               (write-string (process-tag tag-lval) tag-body )
               (setq tag-elems (cdr tag-elems)))
              (t
               (when tag-lval
                 (write-string (let ((tag-lval (if (stringp tag-lval)
                                                   tag-lval
                                                   (eval tag-lval))))
                                 (if escape-body
                                     (escape-body tag-lval)
                                     tag-lval))
                               tag-body))
               (setq tag-elems (cdr tag-elems))))
        :finally (return (format nil
                                 "<~A~A>~A</~A>"
                                 tag-name
                                 (if tag-attr (get-output-stream-string tag-attr) "")
                                 (if tag-body (get-output-stream-string tag-body) "")
                                 tag-name))))

(defun read-escape (stream)
  "Read either a single escape character, or an even number of escape characters.
In the former case returns a character, in the latter a string."
  (loop :for n = 0 then (1+ n)
        :for char = (peek-char nil stream nil nil)
        :while (eql char #\\)
        :collect (read-char stream) into result
        :finally (return (and result (cond ((not (member char '(#\( #\)) :test #'eql))
                                            (cons 'text result))
                                           ((= n 1)
                                            (cons 'escape nil))
                                           (t
                                            (cons 'text
                                                  (if (oddp n)
                                                      ;; We ate too much; give one back and
                                                      ;; adjust results accordingly.
                                                      (progn (unread-char #\\ stream)
                                                             (cdr result))
                                                      result))))))))

(defun read-form (stream)
  (let ((opening (cons #\( nil))
        (result nil))
    (loop :for char = (peek-char nil stream nil nil)
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
          :finally (return (and result
                                (cons 'form (cons #\( (nreverse result))))))))

(defun read-tag (stream)
  (let ((opening (cons #\( nil))
        (result nil))
    (loop :for char = (peek-char nil stream nil nil)
          :while (and char opening)
          :do
          (cond ((eql char #\()
                 (push (car (push (read-char stream) opening)) result))
                ((eql char #\))
                 (push (read-char stream) result)
                 (pop opening))
                (t
                 (push (read-char stream) result)))
          :finally (return (and result
                                (cons 'tag (cons #\( (nreverse result))))))))

(defun read-until-char (stream &optional chars)
  (loop :for char = (peek-char nil stream nil nil)
        :while (and char (not (member char chars :test #'eql)))
        :collect (read-char stream) :into result
        :finally (return (and result (cons 'text result)))))

(defun read-newline (stream)
  (loop :for char = (peek-char nil stream nil nil)
        :for n = 0 then (1+ n)
        :while (eql char #\Newline)
        :collect (read-char stream) :into result
        :finally (return (when (> n 1) (cons 'newline nil)))))

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
        (let ((type (car token))
              (value (cdr (rplacd token (coerce (cdr token) 'string)))))
          (cond ((eq type 'text)
                 (push token parsed))
                ((eq type 'newline)
                 (push token parsed))
                (t
                 (rplacd token (cons (read-from-string value) nil))
                 (push token parsed))))
        :finally (return (nreverse parsed))))

(defun process (parsed)
  (with-output-to-string (s)
    (loop with result = nil
          for token in parsed
          do
          (cond ((and (eq (car token) 'newline) result)
                 (princ (funcall #'process-tag `(:p ,@(nreverse result))) s)
                 (setq result nil))
                ((eq (car token) 'text)
                 (push (cdr token) result))
                (t
                 (push (cadr token) result)))
          finally (and result (princ (funcall #'process-tag `(:p ,@(nreverse result)))  s)))))

(defun process-page (out in)
  (with-open-file (f out :direction :output)
    (princ "<html><head></head><body>" f)
    (princ (process (parse (tokenize in))) f)
    (princ "</body></html>" f)))

(defun read-token (&optional stream eof-error-p eof-value)
  (let ((next-char (peek-char nil stream nil nil)))
    (cond ((not next-char)
           'eof)
          ((eql next-char #\Newline)
           (read-newline stream))
          ((eql next-char #\\)
           (read-escape stream))
          ((eql next-char #\()
           (read-char stream)
           (let ((peek (peek-char nil stream nil nil)))
             (cond ((not peek)
                    (error "error opening paren without closing paren"))
                   ((eql peek #\:)
                    (read-tag stream))
                   (t
                    (read-form stream)))))
          (t
           (read-until-char stream '(#\\ #\( #\Newline))))))
