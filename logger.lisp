(defpackage :cl-lsp/logger
  (:use :cl)
  (:export :*logger-stream*
           :log-format
           :with-log-file
           :with-logger-stream))
(in-package :cl-lsp/logger)

(defvar *logger-stream*)

(defun log-format (string &rest args)
  (when (boundp '*logger-stream*)
    (apply #'format *logger-stream* string args)
    (force-output *logger-stream*)))

(defmacro with-log-file ((file) &body body)
  `(with-open-file (*logger-stream*
                    ,file
                    :direction :output
                    :if-does-not-exist :create
                    :if-exists :append)
     ,@body))

(defmacro with-logger-stream ((stream) &body body)
  `(let ((*logger-stream* ,stream))
     ,@body))
