(cl-lsp/defpackage:defpackage :cl-lsp/test/text-document-did-open
  (:use :cl
        :rove
        :cl-lsp/server
        :cl-lsp/test/test-server
        :cl-lsp/text-document-controller)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json)))
(in-package :cl-lsp/test/text-document-did-open)

(defun request (server text uri)
  (call-lsp-method
   server
   "textDocument/didOpen"
   (json:object-to-json
    (make-instance
     'protocol:did-open-text-document-params
     :text-document (make-instance 'protocol:text-document-item
                                   :uri uri
                                   :language-id "lisp"
                                   :version 1
                                   :text text)))))

(deftest test
  (let ((server (make-instance 'test-server))
        (whole-text "(defun test (x) (cons x x))")
        (uri "file://Users/user/hoge.lisp"))
    (server-listen server)
    (cl-lsp/test/initialize:initialize-request server)
    (let ((response (request server whole-text uri)))
      (ok (null response))
      (let* ((controller (server-text-document-controller server))
             (text-document (find-text-document controller uri)))
        (ok text-document)
        (ok (equal whole-text
                   (lem-base:buffer-text (text-document-file-contents text-document))))))))
