(defpackage :lsp.server
  (:use :cl
        :lsp.protocol
        :lsp.util
        :lsp.editor)
  (:export :run))

(in-package #:lsp.server)

(defvar *mapper* (jsonrpc:make-mapper))

(defun method-log (name params)
  (format t "name: ~A~%" name)
  (format t "params: ~A~%"
          (with-output-to-string (stream)
            (yason:encode params stream))))

(defun call-with-error-handle (function)
  (handler-bind ((error (lambda (c)
                          (format t "~A~%~%~A~%"
                                  c
                                  (with-output-to-string (stream)
                                    (uiop:print-backtrace :stream stream :condition c))))))
    (funcall function)))

(defmacro with-error-handle (&body body)
  `(call-with-error-handle (lambda () ,@body)))

(defmacro define-method (name (params) &body body)
  `(jsonrpc:register-method *mapper*
                            ,name
                            (lambda (,params)
                              (declare (ignorable ,params))
                              (with-error-handle
                                (method-log ',name ,params)
                                ,(if (string= name "initialize")
                                     `(progn ,@body)
                                     `(or (check-initialized)
                                          (progn ,@body)))))))

(defvar *documents* '())
(defstruct document
  buffer
  uri
  languageId
  version)

(defun find-document (uri)
  (dolist (document *documents*)
    (when (equal uri (document-uri document))
      (return document))))

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
          (find-document uri)))
    (when document
      (let* ((buffer
              (document-buffer document))
             (point
              (lem-base:buffer-point buffer)))
        (lsp.editor:move-to-lsp-position point position)
        (funcall function buffer point)))))

(defmacro with-text-document-position ((buffer point) params &body body)
  `(call-with-text-document-position ,params (lambda (,buffer ,point) ,@body)))

(defmacro with-swank ((&key (package (find-package "CL-USER"))
                            (readtable '*readtable*))
                      &body body)
  `(let ((swank::*buffer-package* ,package)
         (swank::*buffer-readtable* ,readtable))
     ,@body))

(defvar *initialize-params* nil)
(defvar *swank-fuzzy-completions* nil)

(defun check-initialized ()
  (when (null *initialize-params*)
    (phlist "code" -32002
            "message" "did not initialize")))

(define-method "initialize" (params)
  (swank:swank-require '("SWANK-TRACE-DIALOG"
                         "SWANK-PACKAGE-FU"
                         "SWANK-PRESENTATIONS"
                         "SWANK-FUZZY"
                         "SWANK-FANCY-INSPECTOR"
                         "SWANK-C-P-C"
                         "SWANK-ARGLISTS"
                         "SWANK-REPL"))
  (setf *swank-fuzzy-completions* (intern "FUZZY-COMPLETIONS" :SWANK))
  (setf *initialize-params* (convert-from-hash-table '|InitializeParams| params))
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
                                                 :|experimental| nil))))

(define-method "shutdown" (params)
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
      (let ((buffer (lem-base:make-buffer |uri|)))
        (setf (lem-base:buffer-syntax-table buffer) lsp.lisp-syntax:*syntax-table*)
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
                   (lsp.editor:move-to-lsp-position point |start|)
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
    (lem-base:with-point ((start point)
                          (end point))
      (lem-base:skip-chars-backward start #'lem-base:syntax-symbol-char-p)
      (lem-base:skip-chars-forward end #'lem-base:syntax-symbol-char-p)
      (let ((result
             (with-swank (:package (find-package (buffer-package-name buffer)))
               (funcall *swank-fuzzy-completions*
                        (lem-base:points-to-string start end)
                        (buffer-package-name buffer)))))
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
                                       :|detail| (fourth completion)
                                       ;:|documentation|
                                       ;:|sortText|
                                       ;:|filterText|
                                       ;:|insertText|
                                       ;:|insertTextFormat|
                                       :|textEdit| (make-instance
                                                    '|TextEdit|
                                                    :|range| (make-lsp-range start end)
                                                    :|newText| (first completion))
                                       ;:|additionalTextEdits|
                                       ;:|command|
                                       ;:|data|
                                       ))))))))))

(define-method "textDocument/hover" (params)
  (with-text-document-position (buffer point) params
    (let ((describe-string
           (ignore-errors
            (with-swank (:package (find-package (buffer-package-name buffer)))
              (swank:describe-symbol
               (lem-base:symbol-string-at-point point))))))
      (convert-to-hash-table
       (if describe-string
           (lem-base:with-point ((start point)
                                 (end point))
             (lem-base:skip-chars-backward start #'lem-base:syntax-symbol-char-p)
             (lem-base:skip-chars-forward end #'lem-base:syntax-symbol-char-p)
             (make-instance '|Hover|
                            :|contents| describe-string
                            :|range| (make-lsp-range start end)))
           (make-instance '|Hover|
                          :|contents| ""))))))

(defun run ()
  (format t "server-listen~%")
  (jsonrpc:server-listen *mapper* :port 10003))
