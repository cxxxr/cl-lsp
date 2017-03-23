(defpackage :cl-lsp/logger
  (:use :cl)
  (:export :*enable-logger*
           :*logger-stream*
           :log-format
           :with-log-file
           :with-log-stream))
(in-package :cl-lsp/logger)

(defvar *enable-logger* t)
(defvar *logger-stream*)

(defun log-format (string &rest args)
  (when (boundp '*logger-stream*)
    (apply #'format *logger-stream* string args)
    (force-output *logger-stream*)))

(defun call-with-log-file (file function)
  (if *enable-logger*
      (let ((stream (open file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :append)))
        (setf *logger-stream* stream)
        (unwind-protect (funcall function)
          (close stream)))
      (funcall function)))

(defmacro with-log-file ((file) &body body)
  `(call-with-log-file ,file (lambda () ,@body)))

(defmacro with-log-stream ((stream) &body body)
  `(if *enable-logger*
       (let ((*logger-stream* ,stream))
         ,@body)
       (progn ,@body)))
