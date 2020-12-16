(defpackage :cl-lsp/methods/text-document
  (:use :cl)
  (:import-from :cl-lsp/server
                :define-method)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)))
(in-package :cl-lsp/methods/text-document)

(defvar *text-documents* '())

(defstruct text-document
  uri
  language-id
  file-contents
  version)

(defun find-text-document (uri)
  (find uri *text-documents* :test #'string= :key #'text-document-uri))

(defun open-text-document (&key uri text language-id version)
  (when (find-text-document uri)
    (error "The file is already opened: ~S" uri))
  (let ((file-contents (cl-lsp/editor:open-file-contents uri text)))
    (push (make-text-document :uri uri
                              :language-id language-id
                              :file-contents file-contents
                              :version version)
          *text-documents*)
    file-contents))

(define-method "text-document/didOpen" (params protocol:did-open-text-document-params) ()
  (let* ((text-document (protocol:did-open-text-document-params-text-document params))
         (uri (protocol:text-document-item-uri text-document))
         (language-id (protocol:text-document-item-language-id text-document))
         (text (protocol:text-document-item-text text-document))
         (version (protocol:text-document-item-version text-document)))
    (open-text-document :uri uri
                        :text text
                        :language-id language-id
                        :version version)
    (values)))
