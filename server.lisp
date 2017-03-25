(defpackage :cl-lsp/server
  (:use :cl
        :cl-lsp/protocol
        :cl-lsp/protocol-util
        :cl-lsp/lisp-syntax
        :cl-lsp/logger
        :cl-lsp/slime
        :cl-lsp/swank)
  (:import-from :cl-ppcre)
  (:import-from :jsonrpc)
  (:import-from :yason)
  (:import-from :uiop)
  (:import-from :alexandria)
  (:import-from :optima)
  (:import-from :lem-base)
  (:export :*server*))
(in-package :cl-lsp/server)

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

(defun call-with-document-position (uri position function)
  (let ((buffer (lem-base:get-buffer uri)))
    (assert (lem-base:bufferp buffer))
    (let ((point (lem-base:buffer-point buffer)))
      (move-to-lsp-position point position)
      (funcall function point))))

(defmacro with-document-position ((point uri position) &body body)
  `(call-with-document-position ,uri ,position (lambda (,point) ,@body)))

(defun call-with-text-document-position (text-document-position-params function)
  (let ((position (slot-value text-document-position-params '|position|))
        (uri (slot-value (slot-value text-document-position-params '|textDocument|) '|uri|)))
    (call-with-document-position uri position function)))

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
     :|renameProvider| t
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

(define-method "textDocument/didOpen" (params |DidOpenTextDocumentParams|)
  (let ((text-document
         (slot-value params
                     '|textDocument|)))
    (with-slots (|uri| |languageId| |version| |text|)
        text-document
      (let ((buffer (lem-base:make-buffer |uri|
                                          :filename (uri-to-filename |uri|)
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

(define-method "textDocument/didClose" (params |DidCloseTextDocumentParams|)
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
  (loop :with start := (beginning-of-defun-point point 1)
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

(defun collect-symbol-range (buffer name function)
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
    (lem-base:with-point ((point (lem-base:buffer-start-point buffer)))
      (loop :while (lem-base:search-forward-regexp point regex)
            :collect (lem-base:with-point ((start point))
                       (lem-base:character-offset start (- (length name)))
                       (funcall function (make-lsp-range start point)))))))

(defun symbol-name-at-point (point)
  (alexandria:when-let*
      ((string (symbol-string-at-point* point))
       (name (ignore-errors
              (symbol-name
               (let ((*package* (search-buffer-package point)))
                 (read-from-string string))))))
    name))

(define-method "textDocument/documentHighlight" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (list-to-object[]
     (alexandria:when-let (name (symbol-name-at-point point))
       (collect-symbol-range (lem-base:point-buffer point) name
                             (lambda (range)
                               (convert-to-hash-table
                                (make-instance '|DocumentHighlight|
                                               :|range| range))))))))

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
        (package (search-buffer-package (lem-base:buffer-start-point buffer)))
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

(define-method "textDocument/codeLens" (params)
  (vector))

(define-method "textDocument/documentLink" (params)
  (vector))

(define-method "textDocument/rename" (params |RenameParams|)
  (with-slots (|textDocument| |position| |newName|) params
    (with-document-position (point (slot-value |textDocument| '|uri|) |position|)
      (alexandria:when-let ((name (symbol-name-at-point point)))
        (let ((buffer (lem-base:point-buffer point)))
          (convert-to-hash-table
           (make-instance
            '|WorkspaceEdit|
            :|documentChanges|
            (list
             (make-instance '|TextDocumentEdit|
                            :|textDocument| (make-instance
                                             '|VersionedTextDocumentIdentifier|
                                             :|version| (lem-base:buffer-version buffer)
                                             :|uri| (uri-to-filename
                                                     (lem-base:buffer-filename buffer)))
                            :|edits| (collect-symbol-range
                                      (lem-base:point-buffer point)
                                      name
                                      (lambda (range)
                                        (make-instance '|TextEdit|
                                                       :|range| range
                                                       :|newText| |newName|))))))))))))


(defun notify-show-message (type message)
  (jsonrpc:notify-async *server*
                        "window/showMessage"
                        (convert-to-hash-table
                         (make-instance '|ShowMessageParams|
                                        :|type| type
                                        :|message| message))))

(defun notify-log-message (type message)
  (jsonrpc:notify-async *server*
                        "window/logMessage"
                        (convert-to-hash-table
                         (make-instance '|LogMessageParams|
                                        :|type| type
                                        :|message| message))))

(defun get-buffer-from-file (file)
  (dolist (buffer (lem-base:buffer-list))
    (when (uiop:pathname-equal file (lem-base:buffer-filename buffer))
      (return buffer))))

(defun eval-string-in-package (string package)
  (let ((*package* package))
    (eval (read-from-string string))))

(defun compilation-notes-to-diagnostics (notes)
  (let ((diagnostics '()))
    (dolist (note notes)
      (optima:match note
        ((and (optima:property :location
                               (or (list :location
                                         (list :buffer buffer-name)
                                         (list :offset pos _)
                                         _)
                                   (list :location
                                         (list :file file)
                                         (list :position pos)
                                         _)))
              (or (optima:property :message message) (and))
              (or (optima:property :severity severity) (and))
              (or (optima:property :source-context _source-context) (and)))
         (let* ((buffer (if buffer-name
                            (lem-base:get-buffer buffer-name)
                            (get-buffer-from-file file)))
                (point (lem-base:buffer-point buffer)))
           (lem-base:move-to-position point pos)
           (lem-base:skip-chars-backward point #'lem-base:syntax-symbol-char-p)
           (lem-base:with-point ((end point))
             (unless (lem-base:form-offset end 1)
               (when (eq severity :read-error)
                 (lem-base:buffer-start point))
               (lem-base:buffer-end end))
             (push (make-instance '|Diagnostic|
                                  :|range| (make-lsp-range point end)
                                  :|severity| (case severity
                                                ((:error :read-error)
                                                 |DiagnosticSeverity.Error|)
                                                ((:warning :style-warning)
                                                 |DiagnosticSeverity.Warning|)
                                                ((:note :redefinition)
                                                 |DiagnosticSeverity.Information|))
                                  ;; :|code|
                                  ;; :|source|
                                  :|message| message)
                   diagnostics))))))
    (list-to-object[] diagnostics)))

(defun compilation-message (notes secs successp)
  (with-output-to-string (out)
    (if successp
        (princ "Compilation finished" out)
        (princ "Compilation failed" out))
    (princ (if (null notes)
               ". (No warnings)"
               ". ")
           out)
    (when secs
      (format nil "[~,2f secs]" secs))))

(define-method "lisp/compileAndLoadFile" (params |TextDocumentIdentifier|)
  (let* ((uri (slot-value params '|uri|))
         (filename (uri-to-filename uri))
         (result))
    (handler-case (with-output-to-string (*standard-output*)
                    (setf result (swank-compile-file filename t)))
      (error (c)
        (notify-show-message |MessageType.Error|
                             (princ-to-string c))
        (setf result nil)))
    (when result
      (destructuring-bind (notes successp duration loadp fastfile)
          (rest result)
        (notify-show-message |MessageType.Info|
                             (compilation-message
                              notes duration successp))
        (let ((diagnostics (compilation-notes-to-diagnostics notes)))
          (let ((diagnostics-params
                 (convert-to-hash-table
                  (make-instance '|PublishDiagnosticsParams|
                                 :|uri| uri
                                 :|diagnostics| diagnostics))))
            (jsonrpc:notify-async *server*
                                  "textDocument/publishDiagnostics"
                                  diagnostics-params)
            (when (and loadp fastfile successp)
              (handler-case (let ((output-string
                                   (with-output-to-string (*standard-output*)
                                     (load fastfile))))
                              (notify-log-message |MessageType.Log| output-string))
                (error (condition)
                  (notify-show-message |MessageType.Error|
                                       (princ-to-string condition))))))))))
  nil)

(define-method "lisp/evalLastSexp" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (let ((string (last-form-string point))
          (package (search-buffer-package point))
          (result)
          (error))
      (when string
        (let ((output-string
               (with-output-to-string (*standard-output*)
                 (setf error
                       (nth-value
                        1
                        (ignore-errors
                         (handler-bind ((error (lambda (c)
                                                 (format t "~A~%~%" c)
                                                 (uiop:print-backtrace
                                                  :condition c
                                                  :stream *standard-output*))))
                           (setf result
                                 (format nil "~{~A~^, ~}"
                                         (multiple-value-list
                                          (eval-string-in-package string package)))))))))))
          (unless (string= output-string "")
            (notify-log-message |MessageType.Log| output-string))
          (if error
              (notify-show-message |MessageType.Error| (princ-to-string error))
              (notify-show-message |MessageType.Info| result))))
      nil)))
