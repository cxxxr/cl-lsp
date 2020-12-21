(cl-lsp/defpackage:defpackage :cl-lsp/test/initialized
  (:use :cl
        :rove
        :cl-lsp/server
        :cl-lsp/test/test-server)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json)))
(in-package :cl-lsp/test/initialized)

(deftest initialized
  (let ((server (make-instance 'test-server)))
    (server-listen server)
    (let ((response
            (call-lsp-method server
                             "initialized"
                             nil)))
      (ok (equal -32002 (json:json-get response "code")))
      (ok (equal "did not initialize" (json:json-get response "message"))))))
