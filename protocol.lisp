(defpackage #:lsp.protocol
  (:use #:cl)
  (:export #:convert-from-hash-table))

(in-package #:lsp.protocol)

(defvar *protocol-symbols* '())

(defmacro define-interface (name parent &body slots)
  `(progn
     (push ',name *protocol-symbols*)
     (export ',(cons name (mapcar #'first slots)))
     (defclass ,name ,parent
       ,(mapcar (lambda (slot)
                  (let ((slot-symbol (first slot))
                        (type (getf (rest slot) :type))
                        (optional (getf (rest slot) :optional))
                        (documentation (getf (rest slot) :documentation)))
                    `(,slot-symbol
                      :initarg ,(intern (string slot-symbol) :keyword)
                      ,@(if type
                            `(:type ,type))
                      ,@(if optional
                            `(:initform nil))
                      ,@(if documentation
                            `(:documentation ,documentation)))))
                slots))))

(define-interface |Position| ()
  (|line| :type number)
  (|character| :type number))

(define-interface |Range| ()
  (|start| :type |Position|)
  (|end| :type |Position|))

(define-interface |Location| ()
  (|uri| :type string)
  (|range| :type range))

(define-interface |Diagnostic| ()
  (|range| :type range)
  (|severity| :optional t :type (or null number))
  (|code| :optional t :type (or null number string))
  (|source| :optional t :type (or null string))
  (|message| :type string))

(define-interface |Command| ()
  (|title| :type string)
  (|command| :type string)
  (|arguments| :type list))

(define-interface |TextEdit| ()
  (|range| :type |Range|)
  (|newText| :type string))

(define-interface |WorkspaceEdit| ()
  (|changes| :type nil))

(define-interface |TextDocumentIdentifier| ()
  (|uri| :type string))

(define-interface |TextDocumentItem| ()
  (|uri| :type string)
  (|languageId| :type string)
  (|version| :type number)
  (|text| :type string))

(define-interface |VersionedTextDocumentIdentifier|
    (|TextDocumentIdentifier|)
  (|version| :type number))

(define-interface |TextDocumentPositionParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|position| :type |Position|))

(define-interface |InitializeParams| ()
  (|processId| :type (or number null))
  (|rootPath| :type (or string null))
  (|rootUri| :type (or string null))
  (|initializationOptions| :optional t)
  (|capabilities| :type |ClientCapabilities|)
  (|trace| :optional t))

(define-interface |WorkspaceClientCapabilites| ()
  (|applyEdit| :optional t :type boolean)
  (|didChangeConfiguration| :optional t)
  (|didChangeWatchedFiles| :optional t)
  (|symbol| :optional t)
  (|executeCommand| :optional t))

(define-interface |TextDocumentClientCapabilities| ()
  (|synchronization| :optional t)
  (|completion| :optional t)
  (|hover| :optional t)
  (|signatureHelp| :optional t)
  (|references| :optional t)
  (|documentHighlight| :optional t)
  (|documentSymbol| :optional t)
  (|formatting| :optional t)
  (|rangeFormatting| :optional t)
  (|onTypeFormatting| :optional t)
  (|definition| :optional t)
  (|codeAction| :optional t)
  (|codeLens| :optional t)
  (|documentLink| :optional t)
  (|rename| :optional t))

(define-interface |ClientCapabilities| ()
  (|workspace| :optional t :type |WorkspaceClientCapabilites|)
  (|textDocument| :optional t :type |TextDocumentClientCapabilities|)
  (|experimental| :optional t :type t))

(define-interface |InitializeResult| ()
  (|capabilities| :type |ServerCapabilities|))

(define-interface |InitializeError| ()
  (|retry| :type boolean))

(define-interface |CompletionOptions| ()
  (|resolveProvider| :optional t :type boolean)
  (|triggerCharacters| :optional t :type (trivial-types:proper-list string)))

(define-interface |SignatureHelpOptions| ()
  (|triggerCharacters| :optional t :type (trivial-types:proper-list string)))

(define-interface |CodeLensOptions| ()
  (|resolveProvider| :optional t :type boolean))

(define-interface |DocumentOnTypeFormattingOptions| ()
  (|firstTriggerCharacter| :type string)
  (|moreTriggerCharacter| :optional t :type (trivial-types:proper-list string)))

(define-interface |DocumentLinkOptions| ()
  (|resolveProvider| :optional t :type boolean))

(define-interface |ExecuteCommandOptions| ()
  (|commands| :type (trivial-types:proper-list string)))

(define-interface |SaveOptions| ()
  (|includeText| :optional t :type boolean))

(define-interface |TextDocumentSyncOptions| ()
  (|openClose| :optional t :type boolean)
  (|change| :optional t :type number)
  (|willSave| :optional t :type boolean)
  (|willSaveWaitUntil| :optional t :type boolean)
  (|save| :optional t :type |SaveOptions|))

(define-interface |ServerCapabilities| ()
  (|textDocumentSync| :optional t :type (or |TextDocumentSyncOptions| number))
  (|hoverProvider| :optional t :type boolean)
  (|completionProvider| :optional t :type |CompletionOptions|)
  (|signatureHelpProvider| :optional t :type |SignatureHelpOptions|)
  (|definitionProvider| :optional t :type boolean)
  (|referencesProvider| :optional t :type boolean)
  (|documentHighlightProvider| :optional t :type boolean)
  (|documentSymbolProvider| :optional t :type boolean)
  (|workspaceSymbolProvider| :optional t :type boolean)
  (|codeActionProvider| :optional t :type boolean)
  (|codeLensProvider| :optional t :type |CodeLensOptions|)
  (|documentFormattingProvider| :optional t :type boolean)
  (|documentRangeFormattingProvider| :optional t :type boolean)
  (|documentOnTypeFormattingProvider| :optional t :type |DocumentOnTypeFormattingOptions|)
  (|renameProvider| :optional t :type boolean)
  (|documentLinkProvider| :optional t :type |DocumentLinkOptions|)
  (|executeCommandProvider| :optional t :type |ExecuteCommandOptions|)
  (|experimental| :optional t :type t))

(define-interface |ShowMessageParams| ()
  (|type| :type number)
  (|message| :type string))

(define-interface |ShowMessageRequestParams| ()
  (|type| :type number)
  (|message| :type string)
  (|actions| :optional t :type (trivial-types:proper-list |MessageActionItem|)))

(define-interface |MessageActionItem| ()
  (|title| :type string))

(define-interface |LogMessageParams| ()
  (|type| :type number)
  (|message| :type string))

(define-interface |Registration| ()
  (|id| :type string)
  (|method| :type string)
  (|registerOptions| :optional t))

(define-interface |RegistrationParams| ()
 (|registrations| :type (trivial-types:proper-list |Registration|)))

(define-interface |TextDocumentRegistrationOptions| ()
  (|documentSelector| :type (or |DocumentSelector| |null|)))

(define-interface |Unregistration| ()
  (|id| :type string)
  (|method| :type string))

(define-interface |UnregistrationParams| ()
  (|unregisterations| :type (trivial-types:proper-list |Unregistration|)))

(define-interface |DidOpenTextDocumentParams| ()
  (|textDocument| :type |TextDocumentItem|))

(define-interface |DidChangeTextDocumentParams| ()
  (|textDocument| :type |VersionedTextDocumentIdentifier|)
  (|contentChanges| :type (trivial-types:proper-list |TextDocumentContentChangeEvent|)))

(define-interface |TextDocumentContentChangeEvent| ()
  (|range| :optional t :type |Range|)
  (|rangeLength| :optional t :type number)
  (|text| :type string))

(define-interface |DidSaveTextDocumentParams| ()
  (|textDocument| :type |TextDocumentIdentifier|)
  (|text| :optional t :type string))

(define-interface |DidCloseTextDocumentParams| ()
  (|textDocument| :type |TextDocumentIdentifier|))

(define-interface |CompletionList| ()
  (|isIncomplete| :type boolean)
  (|items| :type (trivial-types:proper-list |CompletionItem|)))

(define-interface |CompletionItem| ()
  (|label| :type string)
  (|kind| :optional t :type number)
  (|detail| :optional t :type string)
  (|documentation| :optional t :type string)
  (|sortText| :optional t :type string)
  (|filterText| :optional t :type string)
  (|insertText| :optional t :type string)
  (|insertTextFormat| :optional t :type |InsertTextFormat|)
  (|textEdit| :optional t :type |TextEdit|)
  (|additionalTextEdits| :optional t :type (trivial-types:proper-list |TextEdit|))
  (|command| :optional t :type |Command|)
  (|data| :optional t :type t))

(defun maybe-protocol-type (type hash-value)
  (cond ((and (symbolp type)
              (member type *protocol-symbols*)
              (hash-table-p hash-value))
         (convert-from-hash-table type hash-value))
        ((and (consp type)
              (eq 'trivial-types:proper-list (first type)))
         (let ((type (protocol-symbol-p (second type))))
           (when type
             (mapcar (lambda (hash-value-1)
                       (convert-from-hash-table type hash-value-1))
                     hash-value))))
        ((and (consp type)
              (eq 'or (first type)))
         (some (lambda (type-1)
                 (maybe-protocol-type type-1 hash-value))
               (rest type)))))

(defun convert-from-hash-table (name hash-table)
  (make-instance name)
  (let ((object (make-instance name)))
    (loop :for slot :in (c2mop:class-slots (find-class name))
          :for slot-name := (c2mop:slot-definition-name slot)
          :for slot-type := (c2mop:slot-definition-type slot)
          :for hash-key := (string slot-name)
          :do (setf (slot-value object slot-name)
                    (let ((hash-value (gethash hash-key hash-table)))
                      (or (maybe-protocol-type slot-type hash-value)
                          hash-value))))
    object))
