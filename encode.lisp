(in-package :json)

(defconstant +whitespace+
  '(#\Space #\Newline #\Return #\Linefeed #\Tab)
  "List of whitespace characters")

(defconstant +string-escapes-alist+
  '((#\b . #\Backspace) (#\f . #\Page) (#\n . #\Newline) (#\r . #\Return) (#\t . #\Tab)
    (#\" . #\") (#\\ . #\\) (#\/ . #\/))
  "Alist of escaped string characters to our lisp equivalent")

(defgeneric json-slots (object)
  (:documentation "Return a list of slots to be output in our JSON object")
  (:method (object)
    (mapcar #'hcl:slot-definition-name (hcl:class-slots (class-of object)))))

(defun encode (json)
  "Turns the JSON parse tree back into a string"
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (with-output-to-string (stream nil :element-type 'base-char)
    (encode/to-stream json stream)))

(defun encode/to-stream (json stream)
  (declare (optimize (debug 0) (safety 0) (speed 3)))
  (cond ((stringp json) (write-char #\" stream) (encode/escape-string json stream) (write-char #\" stream))
        ((integerp json) (princ json stream))
        ((numberp json) (princ (float json 1.0) stream))
        ((eq json t) (write-string "true" stream))
        ((null json) (write-string "null" stream))
        ((symbolp json) (format stream "\"~(~A~)\"" (symbol-name json)))
        ((date:datep json) (write-char #\" stream)
         (write-string (date:format-date json date:+iso8601+) stream) (write-char #\" stream))

        ((and (consp json) (consp (car json)) (atom (caar json)))
         (write-char #\{ stream)
         (loop for j on json
               for key = (caar j)
               for value = (cdar j)
               do (encode/to-stream key stream) (write-char #\: stream) (encode/to-stream value stream)
               when (cdr j) do (write-char #\, stream))
         (write-char #\} stream))

        ((hash-table-p json)
         (write-char #\{ stream)
         (loop with size of-type fixnum = (hash-table-count json)
               for value being the hash-values of json using (hash-key key)
               for idx of-type fixnum from 1
               do (encode/to-stream key stream) (write-char #\: stream) (encode/to-stream value stream)
               when (< idx size) do (write-char #\, stream))
         (write-char #\} stream))

        ((arrayp json)
         (write-char #\[ stream)
         (loop with e of-type fixnum = (length json)
               for i of-type fixnum from 1
               for j across json
               do (encode/to-stream j stream)
               when (< i e) do (write-char #\, stream))
         (write-char #\] stream))

        ((consp json)
         (write-char #\[ stream)
         (loop for j on json
               do (encode/to-stream  (car j) stream)
               when (cdr j) do (write-char #\, stream))
         (write-char #\] stream))

        (t
         (write-char #\{ stream)
         (loop with first = nil
               for slot in (json-slots json)
               for value = (if (consp slot) (cdr slot) (and (slot-boundp json slot) (slot-value json slot)))
               for str = (symbol-name (if (consp slot) (car slot) slot))
               unless (find #\% str :test #'char=) do
                 (if first
                     (write-char #\, stream)
                     (setf first t))
                 (encode/to-stream (string-downcase str) stream)
                 (write-char #\: stream)
                 (encode/to-stream value stream))
         (write-char #\} stream))))

(defun encode/escape-string (string stream)
  (loop for c of-type character across string
        if (char= c #\Backspace)
          do (write-string "\\b" stream)
        else if (char= c #\Newline)
               do (write-string "\\n" stream)
        else if (char= c #\Return)
               do (write-string "\\r" stream)
        else if (char= c #\Page)
               do (write-string "\\f" stream)
        else if (char= c #\Tab)
               do (write-string "\\t" stream)
        else if (char= c #\")
               do (write-string "\\\"" stream)
        else if (char= c #\\)
               do (write-string "\\\\" stream)
        else
          do (let ((code (char-code c)))
               (if (or (< code 32) (> code 127))
                   (format stream "\\u~4,'0x" code)
                   (write-char c stream)))))


