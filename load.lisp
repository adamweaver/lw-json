(in-package :cl-user)

(require "parsergen")

(define-lw-system lw-json ()
  (:system "lw-date")
  (:file "package")
  (:file "encode" :depends-on "package")
  (:file "decode" :depends-on "encode"))

