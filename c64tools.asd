(in-package :asdf-user)
(defsystem "c64tools"
  :description "c64 tools: A set of lisp tools for working with the commodore 64"
  :version "0.0.1"
  :license "Public Domain"
  :depends-on (:6502-lisp-cc-assembler
               :cl-ppcre)
  :components ((:file "main"
                      :depends-on ("d64tool"
                                   "file-tools"))
               (:file "d64tool")
               (:file "file-tools")))
