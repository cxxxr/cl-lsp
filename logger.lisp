(defpackage :cl-lsp/logger
  (:use :cl)
  (:export :*logger-stream*
           :log-format
           :with-log-file
           :with-log-stream))
(in-package :cl-lsp/logger)

(defvar *logger-stream*)

(defun log-format (string &rest args)
  (when (boundp '*logger-stream*)
    (apply #'format *logger-stream* string args)
    (force-output *logger-stream*)))

(defmacro with-log-file ((file) &body body)
  (let ((_stream (gensym "STREAM")))
    `(let ((,_stream (open ,file
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists :append)))
       (setf *logger-stream* ,_stream)
       (unwind-protect (progn ,@body)
         (close ,_stream)))))

(defmacro with-log-stream ((stream) &body body)
  `(progn
     (setf *logger-stream* ,stream)
     ,@body))
