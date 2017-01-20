(defpackage #:lsp.util
  (:use #:cl)
  (:export #:phlist))

(in-package #:lsp.util)

(defun phlist (&rest plist)
  (alexandria:plist-hash-table plist))
