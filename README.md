# Cromulent JSON decoder and encoder
This is a dead-simple parser and generator, it doesn't try to coerce JSON 
object types into lisp types at all. Uses PARSERGEN:DEFPARSER under the 
hood, because reasons.

* JSON string => lisp string
* JSON number => lisp number
* JSON object => #'equalp hash table
* JSON array =>  lisp vector
* null => nil
* false => nil
* true => t

## API 

``` common-lisp
  (JSON:DECODE "string-or-pathname" &key EXTERNAL-FORMAT)
```

``` common-lisp
 (JSON:ENCODE lisp-object)
```

``` common-lisp
  JSON-DECODE-ERROR 
  ;; condition signaled on bad input
```

## Why 
Because the other JSON generators I looked at seemed a bit complicated.
Objects are just hash tables, man!
