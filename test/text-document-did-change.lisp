(cl-lsp/defpackage:defpackage :cl-lsp/test/text-document-did-change
  (:use :cl
        :rove
        :cl-lsp/server
        :cl-lsp/test/test-server
        :cl-lsp/text-document-controller)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json)))
(in-package :cl-lsp/test/text-document-did-change)

(deftest test
  (let ((server (make-instance 'test-server)))
    (server-listen server)
    (cl-lsp/test/initialize:initialize-request server)
    (let ((uri "file:///Users/user/a.lisp"))
      (cl-lsp/test/text-document-did-open::request server "hoge" uri)
      (let* ((controller (server-text-document-controller server))
             (text-document (find-text-document controller uri)))
        (call-lsp-method server
                         "textDocument/didChange"
                         (json:object-to-json
                          (make-instance
                           'protocol:did-change-text-document-params
                           :text-document (make-instance
                                           'protocol:versioned-text-document-identifier
                                           :uri uri
                                           :version 1)
                           :content-changes (vector))))
        text-document))))
