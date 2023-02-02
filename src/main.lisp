(defpackage :cl-lsp/main
  (:use :cl)
  (:import-from :lem-language-server
                :start-tcp-server
                :start-stdio-server)
  (:export :main))
(in-package :cl-lsp/main)

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((mode (or (first args) "stdio")))
    (cond ((equal mode "tcp")
           (let ((port (second args)))
             (if port
                 (start-tcp-server (parse-integer port))
                 (start-tcp-server 10003))))
          ((equal mode "stdio")
           (log:config :off)
           (start-stdio-server))
          (t
           (uiop:println (format nil "unknown mode: ~A" mode))
           (uiop:quit 1)))))
