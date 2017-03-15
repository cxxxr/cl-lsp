(defpackage :cl-lsp/main
  (:use :cl
        :cl-lsp/server
        :cl-lsp/logger)
  (:export :run-tcp-mode
           :run-stdio-mode))
(in-package :cl-lsp/main)

(defun run-tcp-mode (&key (port 10003))
  (with-log-stream (*error-output*)
    (log-format "server-listen~%mode:tcp~%port:~D~%" port)
    (jsonrpc:server-listen *server* :port port :mode :tcp)))

(defun run-stdio-mode ()
  (with-log-file ("~/lsp-log")
    (log-format "server-listen~%mode:stdio~%")
    (jsonrpc:server-listen *server* :mode :stdio)))
