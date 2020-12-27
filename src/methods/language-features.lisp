(cl-lsp/defpackage:defpackage :cl-lsp/methods/language-features
  (:use :cl
        :cl-lsp/text-document-controller
        :cl-lsp/swank)
  (:import-from :cl-lsp/server
                :this-server
                :server-text-document-controller
                :define-method)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json)
                    (:editor :cl-lsp/editor)))
(in-package :cl-lsp/methods/language-features)

(define-method "textDocument/hover" (params protocol:hover-params) ()
  (let* ((controller (server-text-document-controller (this-server)))
         (text-document (fetch-text-document controller params))
         (file-contents (text-document-file-contents text-document))
         (description (describe-symbol (editor:symbol-string-at-point file-contents)
                                       (editor:current-package file-contents))))
    (make-instance 'protocol:hover
                   :contents description)))
