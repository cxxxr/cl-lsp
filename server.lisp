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

(defvar *documents* '())
(defstruct document
  buffer
  uri
  languageId
  version)

(defun find-document (uri)
  (find uri *documents* :test #'equal :key #'document-uri))

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
    (let* ((document (find-document (slot-value text-document '|uri|)))
           (buffer (document-buffer document))
           (point (lem-base:buffer-point buffer)))
      (dolist (content-change content-changes)
        (with-slots (|range| |rangeLength| |text|)
            content-change
          (cond ((or (null |range|) (null |rangeLength|))
                 (lem-base:erase-buffer buffer)
                 (lem-base:insert-string point |text|))
                (t
                 (with-slots (|start|) |range|
                   (with-slots (|line| |character|) |start|
                     (lem-base:line-offset (lem-base:buffer-start point) |line|)
                     (lem-base:character-offset point |character|))
                   (lem-base:delete-character point |rangeLength|)
                   (lem-base:insert-string point |text|)))))))))

(define-method "textDocument/willSave" nil (params)
  )

(define-method "textDocument/willSaveWaitUntil" t (params)
  )

(define-method "textDocument/didSave" nil (params)
  (let* ((did-save-text-document-params
          (convert-from-hash-table '|DidSaveTextDocumentParams| params))
         (text
          (slot-value did-save-text-document-params '|text|))
         (text-document
          (slot-value did-save-text-document-params '|textDocument|))
         (uri
          (slot-value text-document '|uri|))
         (document
          (find-document uri))
         (buffer
          (document-buffer document)))
    (lem-base:erase-buffer buffer)
    (lem-base:insert-string (buffer-point buffer) text))
  (values))

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
          (find-document uri)))
    (lem-base:delete-buffer (document-buffer document))
    (setf *documents* (delete uri *documents* :key #'document-uri :test #'equal)))
  (values))
