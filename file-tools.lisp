(in-package :cl-user)
(defpackage :c64-file-tools
  (:use :cl))
(in-package :c64-file-tools)

(defun create-prg-file (object-code-file prg-file-name load-address-high load-address-low)
  (with-open-file (object-code object-code-file)
    (with-open-file (prg-file prg-file-name :direction :output :element-type 'unsigned-byte :if-does-not-exist :create)
      (write-byte load-address-low prg-file)
      (write-byte load-address-high prg-file)
      (let ((byte-count 2))
        (loop for byte = (read-byte object-code 'nil 'nil) while byte do
          (write-byte byte prg-file)
          (incf byte-count))
        byte-count))))
