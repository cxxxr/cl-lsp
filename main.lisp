(defpackage :cl-lsp/main
  (:use :cl
        :cl-lsp/protocol
        :cl-lsp/protocol-util
        :cl-lsp/lisp-syntax
        :cl-lsp/logger
        :cl-lsp/slime
        :cl-lsp/swank)
  (:import-from :cl-ppcre)
  (:import-from :quri)
  (:import-from :jsonrpc)
  (:import-from :yason)
  (:import-from :uiop)
  (:import-from :alexandria)
  (:import-from :optima)
  (:import-from :lem-base)
  (:export :run-tcp-mode
           :run-stdio-mode))
(in-package :cl-lsp/main)

(defvar *server* (jsonrpc:make-server))

(defvar *request-log* nil)
(defvar *response-log* nil)

(defun request-log (name params)
  (when *request-log*
    (log-format "~%* from client~%")
    (log-format "name: ~A~%" name)
    (log-format "params: ~A~%"
                (with-output-to-string (stream)
                  (yason:encode params stream)))))

(defun response-log (hash)
  (when *response-log*
    (log-format "~%* to server~%~A~%"
                (with-output-to-string (out)
                  (yason:encode hash out)))))

(defun call-with-error-handle (function)
  (handler-bind ((error (lambda (c)
                          (log-format "~A~%~%~A~%"
                                      c
                                      (with-output-to-string (stream)
                                        (uiop:print-backtrace :stream stream
                                                              :condition c))))))
    (funcall function)))

(defmacro with-error-handle (&body body)
  `(call-with-error-handle (lambda () ,@body)))

(defmacro define-method (name (params &optional params-type) &body body)
  (let ((_val (gensym)))
    `(jsonrpc:expose *server*
                     ,name
                     (lambda (,params)
                       (with-error-handle
                         (request-log ',name ,params)
                         (let ((,_val
                                (let ((,params ,(if params-type
                                                    `(convert-from-hash-table ',params-type ,params)
                                                    params)))
                                  (declare (ignorable ,params))
                                  ,(if (string= name "initialize")
                                       `(progn ,@body)
                                       `(or (check-initialized)
                                            (progn ,@body))))))
                           (response-log ,_val)
                           ,_val))))))

(defun call-with-text-document-position (text-document-position-params function)
  (let* ((position (slot-value text-document-position-params '|position|))
         (uri (slot-value (slot-value text-document-position-params '|textDocument|) '|uri|))
         (buffer (lem-base:get-buffer uri)))
    (cond
      (buffer
       (let ((point (lem-base:buffer-point buffer)))
         (move-to-lsp-position point position)
         (funcall function point)))
      (t
       (vector)))))

(defmacro with-text-document-position ((point) params &body body)
  `(call-with-text-document-position ,params (lambda (,point) ,@body)))

(defvar *initialize-params* nil)

(defun check-initialized ()
  (when (null *initialize-params*)
    (alexandria:plist-hash-table
     (list "code" -32002
           "message" "did not initialize")
     :test 'equal)))

(define-method "initialize" (params |InitializeParams|)
  (setf *initialize-params* params)
  (swank-init)
  (convert-to-hash-table
   (make-instance
    '|InitializeResult|
    :|capabilities|
    (make-instance
     '|ServerCapabilities|
     :|textDocumentSync| (progn
                           #+(or)
                           (make-instance
                            '|TextDocumentSyncOptions|
                            :|openClose| t
                            :|change| |TextDocumentSyncKind.Incremental|
                            :|willSave| 'yason:false
                            :|willSaveWaitUntil| 'yason:false
                            :|save| (make-instance '|SaveOptions| :|includeText| t))
                           |TextDocumentSyncKind.Incremental|)
     :|hoverProvider| t
     :|completionProvider| (make-instance
                            '|CompletionOptions|
                            :|resolveProvider| nil
                            :|triggerCharacters| (loop :for code
                                                       :from (char-code #\a)
                                                       :below (char-code #\z)
                                                       :collect (string (code-char code))))
     :|signatureHelpProvider| (make-instance
                               '|SignatureHelpOptions|
                               :|triggerCharacters| (list " "))
     :|definitionProvider| t
     :|referencesProvider| t
     :|documentHighlightProvider| t
     :|documentSymbolProvider| t
     :|workspaceSymbolProvider| t
     :|codeActionProvider| nil
     :|codeLensProvider| nil
     :|documentFormattingProvider| nil
     :|documentRangeFormattingProvider| nil
     :|documentOnTypeFormattingProvider| nil
     :|renameProvider| nil
     :|documentLinkProvider| nil
     :|executeCommandProvider| nil
     :|experimental| nil))))

(define-method "initialized" (params)
  nil)

(define-method "shutdown" (params)
  t)

(define-method "exit" (params)
  (values))

(define-method "workspace/didChangeConfiguration" (params)
  nil)

(define-method "textDocument/didOpen" (params |DidOpenTextDocumentParams|)
  (let ((text-document
         (slot-value params
                     '|textDocument|)))
    (with-slots (|uri| |languageId| |version| |text|)
        text-document
      (let ((buffer (lem-base:make-buffer |uri|
                                          :filename (quri:uri-path (quri:uri |uri|))
                                          :enable-undo-p nil
                                          :syntax-table *syntax-table*)))
        (lem-base:insert-string (lem-base:buffer-point buffer) |text|)
        (setf (lem-base:buffer-value buffer 'document)
              (list :languageId |languageId|
                    :version |version|)))))
  (values))

(define-method "textDocument/didChange" (params |DidChangeTextDocumentParams|)
  (let ((text-document (slot-value params '|textDocument|))
        (content-changes (slot-value params '|contentChanges|)))
    (let* ((buffer (lem-base:get-buffer (slot-value text-document '|uri|)))
           (point (lem-base:buffer-point buffer)))
      (dolist (content-change content-changes)
        (with-slots (|range| |rangeLength| |text|)
            content-change
          (cond ((or (null |range|) (null |rangeLength|))
                 (lem-base:erase-buffer buffer)
                 (lem-base:insert-string point |text|))
                (t
                 (with-slots (|start|) |range|
                   (move-to-lsp-position point |start|)
                   (lem-base:delete-character point |rangeLength|)
                   (lem-base:insert-string point |text|)))))))))

(define-method "textDocument/willSave" (params)
  )

(define-method "textDocument/willSaveWaitUntil" (params)
  )

(define-method "textDocument/didSave" (params |DidSaveTextDocumentParams|)
  (let* ((text
          (slot-value params '|text|))
         (text-document
          (slot-value params '|textDocument|))
         (uri
          (slot-value text-document '|uri|))
         (buffer
          (lem-base:get-buffer uri)))
    (when text
      (lem-base:erase-buffer buffer)
      (lem-base:insert-string (lem-base:buffer-point buffer) text)))
  (values))

(define-method "textDocument/didClose" (params '|DidCloseTextDocumentParams|)
  (let* ((text-document
          (slot-value params '|textDocument|))
         (uri
          (slot-value text-document '|uri|))
         (buffer
          (lem-base:get-buffer uri)))
    (lem-base:delete-buffer buffer))
  (values))

(define-method "textDocument/completion" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (lem-base:with-point ((start point)
                          (end point))
      (lem-base:skip-symbol-backward start)
      (lem-base:skip-symbol-forward end)
      (let ((result
             (fuzzy-completions
              (lem-base:points-to-string start end)
              (search-buffer-package point))))
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

(define-method "textDocument/hover" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (let* ((symbol-string (symbol-string-at-point* point))
           (describe-string
            (describe-symbol symbol-string
                             (search-buffer-package point))))
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

(defun arglist (point)
  (loop :with start := (beginning-of-defun-point point)
        :while (lem-base:form-offset point -1)
        :do (when (lem-base:point< point start)
              (return-from arglist nil)))
  (lem-base:skip-whitespace-forward point)
  (let ((symbol-string (symbol-string-at-point* point)))
    (when symbol-string
      (operator-arglist symbol-string
                        (search-buffer-package point)))))

(define-method "textDocument/signatureHelp" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (let ((arglist (arglist point)))
      (convert-to-hash-table
       (make-instance
        '|SignatureHelp|
        :|signatures| (when arglist
                        (list (make-instance
                               '|SignatureInformation|
                               :|label| arglist))))))))

(defun xref-location (xref)
  (optima:match xref
    ((list _
           (list :location
                 (list :file file)
                 (list :position offset)
                 (list :snippet _)))
     (convert-to-hash-table (file-location file offset)))))

(defun xref-locations-from-definitions (defs)
  (loop :for xref :in defs
        :for location := (xref-location xref)
        :when location
        :collect location))

(define-method "textDocument/definition" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (alexandria:when-let ((name (symbol-string-at-point* point)))
      (alexandria:if-let ((p (search-local-definition point name)))
        (convert-to-hash-table (buffer-location p))
        (list-to-object-or-object[]
         (xref-locations-from-definitions
          (find-definitions name (search-buffer-package point))))))))

(define-method "textDocument/references" (params |ReferenceParams|)
  (with-text-document-position (point) params
    (let ((symbol-string (symbol-string-at-point* point)))
      (list-to-object-or-object[]
       (loop :for (type . definitions) :in (xrefs symbol-string
                                                  (search-buffer-package point))
             :nconc (xref-locations-from-definitions definitions))))))

(define-method "textDocument/documentHighlight" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (alexandria:when-let*
        ((string (symbol-string-at-point* point))
         (name (ignore-errors
                (symbol-name
                 (let ((*package* (search-buffer-package point)))
                   (read-from-string string))))))
      (let ((regex (ppcre:create-scanner `(:sequence
                                           (:alternation
                                            (:positive-lookbehind
                                             (:char-class #\( #\) #\space #\tab #\:))
                                            :start-anchor)
                                           ,name
                                           (:alternation
                                            (:positive-lookahead
                                             (:char-class #\( #\) #\space #\tab #\:))
                                            :end-anchor))
                                         :case-insensitive-mode t)))
        (lem-base:buffer-start point)
        (let ((response
               (loop :while (lem-base:search-forward-regexp point regex)
                     :collect (lem-base:with-point ((start point))
                                (lem-base:character-offset start (- (length name)))
                                (convert-to-hash-table
                                 (make-instance '|DocumentHighlight|
                                                :|range| (make-lsp-range start point)))))))
          (if (null response)
              (vector)
              response))))))

(defun type-to-symbol-kind (type)
  #+sbcl
  (case type
    (defvar |SymbolKind.Variable|)
    (defconstant |SymbolKind.Variable|)
    (deftype |SymbolKind.Class|)
    (define-symbol-macro |SymbolKind.Variable|)
    (defmacro |SymbolKind.Function|)
    (define-compiler-macro |SymbolKind.Function|)
    (defun |SymbolKind.Function|)
    (defgeneric |SymbolKind.Method|)
    (defmethod |SymbolKind.Method|)
    (define-setf-expander |SymbolKind.Function|)
    (defstruct |SymbolKind.Class|)
    (define-condition |SymbolKind.Class|)
    (defclass |SymbolKind.Class|)
    (define-method-combination |SymbolKind.Function|)
    (defpackage |SymbolKind.Namespace|)
    (:deftransform |SymbolKind.Function|)
    (:defoptimizer |SymbolKind.Function|)
    (:define-vop |SymbolKind.Function|)
    (:define-source-transform |SymbolKind.Function|)
    (:def-ir1-translator |SymbolKind.Function|)
    (declaim |SymbolKind.Function|)
    (:define-alien-type |SymbolKind.Function|)
    (otherwise
     |SymbolKind.Function|))
  #-sbcl
  |SymbolKind.Function|)

(defun xref-to-symbol-information (name xref buffer-file)
  (optima:match xref
    ((list (cons type _)
           (list :location
                 (list :file file)
                 (list :position position)
                 (list :snippet _)))
     (when (and (probe-file file)
                (or (null buffer-file)
                    (equal file buffer-file)))
       (make-instance '|SymbolInformation|
                      :|name| name
                      :|kind| (type-to-symbol-kind type)
                      :|location| (file-location file position))))))

(defun symbol-informations (name package buffer-file)
  (loop :for xref :in (find-definitions name package)
        :for info := (xref-to-symbol-information name xref buffer-file)
        :when info
        :collect info))

(defun document-symbol (buffer)
  (let ((symbol-informations '())
        (used (make-hash-table :test 'equal))
        (package (search-buffer-package (lem-base:buffers-start buffer)))
        (buffer-file (lem-base:buffer-filename buffer)))
    (map-buffer-symbols
     buffer
     (lambda (symbol-string)
       (unless (gethash symbol-string used)
         (setf (gethash symbol-string used) t)
         (dolist (si (symbol-informations symbol-string package buffer-file))
           (push si symbol-informations)))))
    (if (null symbol-informations)
        (vector)
        (mapcar #'convert-to-hash-table
                symbol-informations))))

(define-method "textDocument/documentSymbol" (params |DocumentSymbolParams|)
  (let* ((text-document (slot-value params '|textDocument|))
         (uri (slot-value text-document '|uri|))
         (buffer (lem-base:get-buffer uri)))
    (when buffer
      (document-symbol buffer))))

(define-method "workspace/symbol" (params |WorkspaceSymbolParams|)
  (let* ((query (slot-value params '|query|))
         (limit 42))
    (list-to-object[]
     (when (string/= query "")
       (mapcar #'convert-to-hash-table
               (loop :with package := (find-package "CL-USER")
                     :repeat limit
                     :for name :in (swank-apropos-list query package)
                     :append (symbol-informations name package nil)))))))

(define-method "textDocument/codeLens" (params)
  (vector))

(define-method "textDocument/documentLink" (params)
  (vector))

(defun run-tcp-mode (&key (port 10003))
  (with-log-stream (*error-output*)
    (log-format "server-listen~%mode:tcp~%port:~D~%" port)
    (jsonrpc:server-listen *server* :port port :mode :tcp)))

(defun run-stdio-mode ()
  (with-log-file ("~/lsp-log")
    (log-format "server-listen~%mode:stdio~%")
    (jsonrpc:server-listen *server* :mode :stdio)))
