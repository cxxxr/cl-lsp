(defpackage :lsp.server
  (:use :cl
        :lsp.protocol
        :lsp.util))

(in-package #:lsp.server)

(defvar *mapper* (jsonrpc:make-mapper))

(defmacro define-method (name (params) &body body)
  `(jsonrpc:register-method *mapper*
                            ,name
                            (lambda (,params)
                              (declare (ignorable ,params))
                              ,(if (string= name "initialize")
                                   `(progn ,@body)
                                   `(or (check-initialized)
                                        (progn ,@body))))))

(defvar *documents* '())
(defstruct document
  buffer
  uri
  languageId
  version)

(defun find-document (uri)
  (find uri *documents* :test #'equal :key #'document-uri))

(defun move-to-position (point position)
  (declare (type lem-base:point point)
           (type |Position| position))
  (with-slots (|line| |character|) position
    (lem-base:buffer-start point)
    (lem-base:line-offset point |line|)
    (lem-base:character-offset point |character|)
    point))

(defun buffer-package-name (buffer)
  (declare (ignore buffer))
  "CL-USER")

(defun call-with-text-document-position (params function)
  (let* ((text-document-position-params
          (convert-from-hash-table '|TextDocumentPositionParams| params))
         (position
          (slot-value text-document-position-params '|position|))
         (uri
          (slot-value (slot-value text-document-position-params '|textDocument|) '|uri|))
         (document
          (find-document uri))
         (buffer
          (document-buffer document))
         (point
          (lem-base:buffer-point buffer)))
    (move-to-position point position)
    (funcall function buffer point)))

(defmacro with-text-document-position ((buffer point) params &body body)
  `(call-with-text-document-position ,params (lambda (,buffer ,point) ,@body)))

(defvar *initialized* nil)
(defvar *shutdown* nil)

(defun check-initialized ()
  (unless *initialized*
    (phlist "code" -32002
            "message" "did not initialize")))

(define-method "initialize" (params)
  (setq *initialized* t)
  (let* ((initialize-params (convert-from-hash-table '|InitializeParams| params)))
    (declare (ignore initialize-params))
    (convert-to-hash-table
     (make-instance '|InitializeResult|
                    :|capabilities| (make-instance '|ServerCapabilities|
                                                   :|textDocumentSync| nil
                                                   :|hoverProvider| nil
                                                   :|completionProvider| nil
                                                   :|signatureHelpProvider| nil
                                                   :|definitionProvider| nil
                                                   :|referencesProvider| nil
                                                   :|documentHighlightProvider| nil
                                                   :|documentSymbolProvider| nil
                                                   :|workspaceSymbolProvider| nil
                                                   :|codeActionProvider| nil
                                                   :|codeLensProvider| nil
                                                   :|documentFormattingProvider| nil
                                                   :|documentRangeFormattingProvider| nil
                                                   :|documentOnTypeFormattingProvider| nil
                                                   :|renameProvider| nil
                                                   :|documentLinkProvider| nil
                                                   :|executeCommandProvider| nil
                                                   :|experimental| nil)))))

(define-method "shutdown" (params)
  (setq *shutdown* t)
  t)

(define-method "exit" (params)
  (values))

(define-method "textDocument/didOpen" (params)
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

(define-method "textDocument/didChange" (params)
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
                   (move-to-position point |start|)
                   (lem-base:delete-character point |rangeLength|)
                   (lem-base:insert-string point |text|)))))))))

(define-method "textDocument/willSave" (params)
  )

(define-method "textDocument/willSaveWaitUntil" (params)
  )

(define-method "textDocument/didSave" (params)
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
    (lem-base:insert-string (lem-base:buffer-point buffer) text))
  (values))

(define-method "textDocument/didClose" (params)
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

(define-method "textDocument/completion" (params)
  (with-text-document-position (buffer point) params
    (let ((result
           (swank:fuzzy-completions (lem-base:symbol-string-at-point point)
                                    (buffer-package-name buffer))))
      (when result
        (destructuring-bind (completions timeout) result
          (declare (ignore timeout))
          (convert-to-hash-table
           (make-instance
            '|CompletionList|
            :|isIncomplete| nil
            :|items| (loop :for completion :in completions
                           :collect (make-instance
                                     '|CompletionItem|
                                     :|label| (first completion)
                                     ;:|kind|
                                     ;:|detail|
                                     ;:|documentation|
                                     ;:|sortText|
                                     ;:|filterText|
                                     ;:|insertText|
                                     ;:|insertTextFormat|
                                     ;:|textEdit|
                                     ;:|additionalTextEdits|
                                     ;:|command|
                                     ;:|data|
                                     )))))))))

(define-method "textDocument/hover" (params)
  (with-text-document-position (buffer point) params
    (declare (ignore buffer))
    (let ((describe-string
           (ignore-errors
            (swank:describe-symbol
             (lem-base:symbol-string-at-point point)))))
      (when describe-string
        (convert-to-hash-table
         (make-instance '|Hover|
                        :contents describe-string))))))
