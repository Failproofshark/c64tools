(in-package :cl-user)
(defpackage :c64-file-tools
  (:use :cl)
  (:export :create-prg-file)
  (:import-from :6502-lisp-cc-assembler :assemble-source-to-object-code))
(in-package :c64-file-tools)

(defun create-prg-file (source-code-file prg-file-name load-address-high load-address-low)
  (with-open-file (source-code source-code-file :element-type 'unsigned-byte)
    (with-open-file (prg-file prg-file-name :direction :output :element-type 'unsigned-byte :if-does-not-exist :create)
      (write-byte load-address-low prg-file)
      (write-byte load-address-high prg-file)
      (let ((byte-count 2))
        (loop for byte = (read-byte source-code 'nil 'nil) while byte do
          (write-byte byte prg-file)
          (incf byte-count))
        byte-count))))
