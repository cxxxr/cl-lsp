(defpackage #:lsp.server
  (:use #:cl
        #:lsp.protocol
        #:lsp.util))

(in-package #:lsp.server)

(defvar *mapper* (jsonrpc:make-mapper))

(defmacro define-method (name return (params) &body body)
  `(jsonrpc:register-method *mapper*
                            ,name
                            (lambda (,params)
                              (declare (ignorable ,params))
                              ,(if (string= name "initialize")
                                   `(progn ,@body)
                                   `(or (check-initialized)
                                        (progn ,@body))))
                            ,return))

(defstruct document
  buffer
  uri
  languageId
  version)

(defvar *documents* '())
(defvar *initialized* nil)
(defvar *shutdown* nil)

(defun check-initialized ()
  (unless *initialized*
    (phlist "code" -32002
            "message" "did not initialize")))

(define-method "initialize" t (params)
  (setq *initialized* t)
  t)

(define-method "shutdown" t (params)
  (setq *shutdown* t)
  t)

(define-method "exit" nil (params)
  (values))

(define-method "textDocument/didOpen" nil (params)
  (let* ((did-open-text-document-params
          (convert-from-hash-table '|DidOpenTextDocumentParams|
                                   params))
         (text-document
          (slot-value did-open-text-document-params
                      '|textDocument|)))
    (with-slots (|uri| |languageId| |version| |text|)
        text-document
      (let ((buffer (lem-base:get-buffer-create |uri|)))
        (lem-base:insert-string (lem-base:buffer-point buffer) |text|)
        (push (make-document :buffer buffer
                             :uri |uri|
                             :languageId |languageId|
                             :version |version|)
              *documents*))))
  (values))

(define-method "textDocument/didChange" nil (params)
  (let* ((did-change-text-document-params
          (convert-from-hash-table '|DidChangeTextDocumentParams|
                                   params))
         (text-document
          (slot-value did-change-text-document-params
                      '|textDocument|))
         (content-changes
          (slot-value did-change-text-document-params
                      '|contentChanges|)))
    (declare (ignore text-document content-changes))
    (values)))

(define-method "textDocument/didClose" nil (params)
  (let* ((did-close-text-document-params
          (convert-from-hash-table '|DidCloseTextDocumentParams|
                                   params))
         (text-document
          (slot-value did-close-text-document-params
                      '|textDocument|))
         (uri
          (slot-value text-document '|uri|))
         (document
          (find uri *documents* :key #'document-uri :test #'equal)))
    (lem-base:delete-buffer (document-buffer document))
    (setf *documents* (delete uri *documents* :key #'document-uri :test #'equal)))
  (values))
