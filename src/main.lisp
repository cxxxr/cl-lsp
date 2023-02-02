(defpackage :cl-lsp/main
  (:use :cl)
  (:import-from :lem-lsp-server/main
                :run-tcp-mode
                :run-stdio-mode)
  (:export :run-tcp-mode
           :run-stdio-mode
           :main))

(in-package :cl-lsp/main)

(defun main (&optional (args (uiop:command-line-arguments)))
  (let ((mode (or (first args) "tcp")))
    (cond ((equal mode "tcp")
           (let ((port (second args)))
             (if port
                 (run-tcp-mode :port (parse-integer port))
                 (run-tcp-mode))))
          ((equal mode "stdio")
           (run-stdio-mode))
          (t
           (uiop:println (format nil "unknown mode: ~A" mode))
           (uiop:quit 1)))))

