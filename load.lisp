(in-package :cl-user)

(require "parsergen")

(define-lw-system json ()
  (:system "date")
  (:file "package")
  (:file "encode" :depends-on "package")
  (:file "decode" :depends-on "encode"))

