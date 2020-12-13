(cl-lsp/defpackage:defpackage :cl-lsp/main
  (:use :cl
        :alexandria
        :cl-lsp/methods
        :cl-lsp/logger
        :cl-lsp/eval)
  (:import-from :cl-lsp/config
                :with-environment
                :config)
  (:export :run-tcp-mode
           :run-stdio-mode)
  (:lock t))
(in-package :cl-lsp/main)

(defun start-swank-if-enabled ()
  (when-let ((port (config :swank :port)))
    (swank:create-server :dont-close t :port port)))

(defun run-tcp-mode (&key (port 10003))
  (with-environment :tcp
    (start-swank-if-enabled)
    (with-log-stream (*error-output*)
      (log-format "server-listen~%mode:tcp~%port:~D~%" port)
      (jsonrpc:server-listen cl-lsp/server:*server* :port port :mode :tcp))))

(defun run-stdio-mode ()
  (with-environment :stdio
    (start-swank-if-enabled)
    (with-log-file ((config :log-pathname))
      (log-format "server-listen~%mode:stdio~%")
      (jsonrpc:server-listen cl-lsp/server:*server* :mode :stdio))))
