(defun compile-sexpcode (sexpcode)
  (let ((name (getf sexpcode :name))
        (class (getf sexpcode :class))
        (id (getf sexpcode :id))
        (attribute (getf sexpcode :attribute))
        (content (getf sexpcode :content)))
    (with-output-to-string (s)
      (write-string "<" s)
      (write-string name s)
      (when class
        (write-string " class=\"" s)
        (loop for (c . rest) on class
              do
              (write-string c s)
              (when rest (write-string " " s))
              finally (write-string "\"" s)))
      (when id
        (write-string " id=\"" s)
        (write-string id s)
        (write-string "\"" s))
      (when attribute
        (loop for (name val . rest) on attribute by #'cddr
              initially (write-string " " s)
              do
              (write-string name s)
              (when val
                (write-string "=" s)
                (write-string val s))
              (when rest (write-string " " s))))
      (write-string ">" s)
      (when content
        (loop for c in content
              do (write-string (if (stringp c) c (compile-sexpcode c)) s)))
      (when (not (member name +void-tags+ :test #'string-equal))
        (write-string "</" s)
        (write-string name s)
        (write-string ">" s)))))
