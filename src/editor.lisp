(defpackage :cl-lsp/editor
  (:use :cl)
  (:export :open-file-contents))
(in-package :cl-lsp/editor)

(defclass editor () ())

(defclass lem (editor) ())

(defvar *editor* (make-instance 'lem))

(defmethod open-file-contents-using-editor ((editor lem) uri text)
  (let ((buffer
          (lem-base:make-buffer uri
                                :temporary t
                                :syntax-table lem-lisp-syntax:*syntax-table*)))
    (lem-base:insert-string buffer text)
    buffer))

(defun open-file-contents (uri text)
  (open-file-contents-using-editor *editor* uri text))
