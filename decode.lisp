(in-package :json)

(define-condition json-decode-error (error)
  ((what :initarg :what :reader what)
   (argument :initarg :argument :reader argument :initform nil))
  (:report (lambda (condition stream) (format stream "JSON parse error: ~A~@[: ~S~]" (what condition) (argument condition)))))

(defun decode (string)
  "Turns a JSON string into a lisp object.
{} objects are turned into EQUAL hash-tables :- {\"a\": 3} === #{equal \"a\" 3}
[] arrays are turned into vectors :- [1, 2, 3] == #(1 2 3)
true becomes T
false becomes NIL
null becomes NIL.

In multiple value strings, only the first value is returned:
[1, 2, 3], [4, 5, 6] === #(1 2 3) ; The #(4 5 6) is not read"
  (cond ((stringp string) (decode/parse (lex/string string)))
        ((pathnamep string) (decode/parse (lex/string (hcl:file-string string :external-format '(:utf-8 :eol-style :lf)))))
        (t nil)))

(defun lex/decode-string (string i eos)
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type string string) (type fixnum i eos))
  (cons (with-output-to-string (output)
          (loop with escape = nil
                for j from i below eos
                for c = (char string j)
                if (and escape (char= c #\u))
                  do (write-char (code-char (or (parse-integer string :start (1+ j) :end (+ j 5) :junk-allowed t :radix 16) 0)) output)
                     (setf escape nil j (+ j 4))
                else
                  if escape
                    do (write-char (or (cdr (assoc c +string-escapes-alist+ :test #'char=)) #\?) output) (setf escape nil)
                else
                  if (char= c #\\)
                    do (setf escape t)
                else
                  if (char= c #\")
                    do (loop-finish)
                else
                  do (write-char c output)
                finally (setf i j)))
        i))

(defun lex/string-decode-number-and-pos (string i end)
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type string string) (type fixnum i end))
  (multiple-value-bind (whole eow) (parse-integer string :start i :end end :junk-allowed t)
    (lw:when-let (char (and (/= eow end) (char string eow)))
      (when (char= char #\.)
        (multiple-value-bind (frac eof) (parse-integer string :start (1+ eow) :end end :junk-allowed t)
          (setf whole (* (if (< whole 0) -1 1) (+ (abs whole) (/ (or frac 0) (expt 10 (- eof eow 1))))) eow eof)
          (when (< eow end)
            (setf char (char string eow)))))
      (when (char-equal char #\e)
        (multiple-value-bind (exp eoe) (parse-integer string :start (1+ eow) :end end :junk-allowed t)
          (setf eow eoe whole (* whole (expt 10 exp))))))
    (cons whole (1- eow))))

(defun lex/string (string)
  (declare (optimize (speed 3) (safety 0) (debug 0)) (type string string))
  (loop with end of-type fixnum = (length string)
        for i of-type fixnum below end
        for char = (char string i)
        for eos = (and (char= char #\") (lex/decode-string string (1+ i) end))
        for eon = (and (or (digit-char-p char) (char= char #\-)) (lex/string-decode-number-and-pos string i end))
        if eos
          collect (car eos) and do (setf i (cdr eos))
        else if eon
               collect (car eon) and do (setf i (cdr eon))
        else if (find char #.(make-array 6 :element-type 'base-char :initial-contents "[]{},:") :test #'char=)
               collect char
        else if (string-equal string "true" :start1 i :end1 (min (+ i 4) end))
               collect t and do (incf i 3)
        else if (string-equal string "null" :start1 i :end1 (min (+ i 4) end))
               collect nil and do (incf i 3)
        else if (string-equal string "false" :start1 i :end1 (min (+ i 5) end))
               collect nil and do (incf i 4)))

(parsergen:defparser json-dom-parser
    ((<toplevel> <element>))
  (<array-element*>
   ((<element> #\, <array-element*>) (cons $1 $3))
   ((<element>) (cons $1 nil)))
  (<object-element*>
   ((:string #\: <element> #\, <object-element*>) (list* $1 $3 $5))
   ((:string #\: <element>) (list $1 $3)))
  (<element>
   ((:number) $1)
   ((:string) $1)
   ((:null) nil)
   ((t) $1)
   ((#\[ <array-element*> #\]) (coerce $2 'vector))
   ((#\[ #\]) (vector))
   ((#\{ <object-element*> #\}) (loop with hash = (make-hash-table :test #'equalp)
                                      for (key value) on $2 by #'cddr
                                      do (setf (gethash key hash) value)
                                      finally (return hash)))
   ((#\{ #\}) (make-hash-table :test #'equalp))))

(defun decode/parse (list)
  (let ((json list))
    (json-dom-parser (lambda () (let ((token (pop json)))
                                  (values (cond ((stringp token) :string)
                                                ((numberp token) :number)
                                                ((and json (null token)) :null)
                                                (t token))
                                          token))))))

