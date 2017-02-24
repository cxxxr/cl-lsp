(defpackage :cl-lsp/src/client
  (:use :cl
        :cl-lsp/src/general/protocol
        :cl-lsp/src/general/editor)
  (:import-from :jsonrpc)
  (:import-from :alexandria)
  (:import-from :lem))
(in-package :cl-lsp/src/client)

(defparameter +port+ 10003)

(defvar *client* nil)

(macrolet ((def (name key)
             (let ((_client (gensym "CLIENT"))
                   (_value (gensym "VALUE")))
               `(progn
                  (defun ,name (,_client)
                    (getf (jsonrpc:transport-data ,_client) ,key))
                  (defun (setf ,name) (,_value ,_client)
                    (setf (getf (jsonrpc:transport-data ,_client) ,key)
                          ,_value))))))
  (def client.server-capabilities :server-capabilities))

(defun initialize ()
  (unless *client*
    (setf *client* (jsonrpc:client-connect :host "127.0.0.1" :port +port+))
    (setf (client.server-capabilities *client*)
          (jsonrpc:call *client*
                        "initialize"
                        (list
                         (convert-to-hash-table
                          (make-instance '|InitializeParams|
                                         :|processId| nil
                                         :|rootPath| nil
                                         :|rootUri| nil
                                         #+(or):|initializationOptions|
                                         :|capabilities| (make-instance
                                                          '|ClientCapabilities|
                                                          :|workspace| (make-instance
                                                                        '|WorkspaceClientCapabilites|
                                                                        :|applyEdit| t
                                                                        #+(or):|didChangeConfiguration|
                                                                        #+(or):|didChangeWatchedFiles|
                                                                        #+(or):|symbol|
                                                                        #+(or):|executeCommand|)
                                                          :|textDocument| (make-instance
                                                                           '|TextDocumentClientCapabilities|
                                                                           :|synchronization| t
                                                                           :|completion| t
                                                                           :|hover| t
                                                                           :|signatureHelp| t
                                                                           :|references| t
                                                                           :|documentHighlight| t
                                                                           :|documentSymbol| t
                                                                           :|formatting| t
                                                                           :|rangeFormatting| t
                                                                           :|onTypeFormatting| t
                                                                           :|definition| t
                                                                           :|codeAction| t
                                                                           :|codeLens| t
                                                                           :|documentLink| t
                                                                           :|rename| t)
                                                          #+(or):|experimental|)
                                         #+(or):|trace|)))))))

(defun init-hook ()
  (lem:add-hook lem:*find-file-hook* 'text-document-did-open)
  (lem:add-hook lem:*after-save-hook* 'text-document-did-save)
  (lem:add-hook lem:*kill-buffer-hook* 'text-document-did-close))

(defmacro modify-events (point-or-buffer)
  `(lem:buffer-value ,point-or-buffer 'modify-events))

(defun reflect-modify-events (buffer)
  (let ((events (nreverse (modify-events buffer))))
    (setf (modify-events buffer) nil)
    (jsonrpc:notify *client*
                    "textDocument/didChange"
                    (list
                     (convert-to-hash-table
                      (make-instance
                       '|DidChangeTextDocumentParams|
                       :|textDocument| (make-instance '|VersionedTextDocumentIdentifier|
                                                      :|version| (lem:buffer-version buffer)
                                                      :|uri| (lem:buffer-filename buffer))
                       :|contentChanges| events))))))

(defun push-change-event (start end old-len)
  (push (make-instance
         '|TextDocumentContentChangeEvent|
         :|range| (make-lsp-range start end)
         :|rangeLength| old-len
         :|text| (if (zerop old-len)
                     (lem:points-to-string start end)
                     ""))
        (modify-events start)))

(defun text-document-did-open (buffer)
  (jsonrpc:notify *client*
                  "textDocument/didOpen"
                  (list
                   (convert-to-hash-table
                    (make-instance
                     '|DidOpenTextDocumentParams|
                     :|textDocument| (make-instance
                                      '|TextDocumentItem|
                                      :|uri| (lem:buffer-filename buffer)
                                      :|languageId| "common-lisp"
                                      :|version| 0
                                      :|text| (lem:points-to-string
                                               (lem:buffers-start buffer)
                                               (lem:buffers-end buffer))))))))

(defun text-document-did-save (buffer)
  (jsonrpc:notify *client*
                  "textDocument/didSave"
                  (list
                   (convert-to-hash-table
                    (make-instance
                     '|DidSaveTextDocumentParams|
                     :|textDocument| (make-instance '|TextDocumentIdentifier|
                                                    :|uri| (lem:buffer-filename buffer)))))))

(defun text-document-did-close (buffer)
  (jsonrpc:notify *client*
                  "textDocument/didClose"
                  (list
                   (convert-to-hash-table
                    (make-instance
                     '|DidCloseTextDocumentParams|
                     :|textDocument| (make-instance '|TextDocumentIdentifier|
                                                    :|uri| (lem:buffer-filename buffer)))))))

(defun completion (point)
  (let ((result
         (jsonrpc:call *client*
                       "textDocument/completion"
                       (list
                        (convert-to-hash-table
                         (make-text-document-position point))))))
    (if (listp result)
        (loop :for completion-item :in result
              :collect (convert-from-hash-table '|CompletionItem|
                                                completion-item))
        (slot-value (convert-from-hash-table '|CompletionList| result)
                    '|items|))))

(defun hover (point)
  (convert-from-hash-table
   '|Hover|
   (jsonrpc:call *client*
                 "textDocument/hover"
                 (list
                  (convert-to-hash-table
                   (make-text-document-position point))))))

(defun signature-help (point)
  (convert-from-hash-table
   '|SignatureHelp|
   (jsonrpc:call *client*
                 "textDocument/signatureHelp"
                 (list
                  (convert-to-hash-table
                   (make-text-document-position point))))))

(lem:define-command lsp-start () ()
  (initialize)
  (lem:add-hook (lem:variable-value 'lem:after-change-functions :buffer (lem:current-buffer))
                (lambda (start end old-len)
                  (push-change-event start end old-len))))

(defun marked-string-to-string (contents)
  (typecase contents
    (list
     (format nil "~{~A~%~}" (mapcar #'marked-string-to-string contents)))
    (hash-table
     (gethash "value" contents))
    (string
     contents)
    (otherwise "")))

(lem:define-command lsp-hover () ()
  (reflect-modify-events (lem:current-buffer))
  (let ((hover (hover (lem:current-point))))
    (with-slots (|contents| |range|) hover
      (declare (ignore |range|))
      (let ((string (marked-string-to-string |contents|)))
        (unless (string= string "")
          (lem:with-pop-up-typeout-window (output (lem:get-buffer-create "*hover*") :erase t :focus t)
            (princ string output)))))))

(lem:define-command lsp-completion () ()
  (reflect-modify-events (lem:current-buffer))
  (let ((items (completion (lem:current-point))))
    (lem:run-completion
     (loop :for item :in items
           :collect (with-slots (|label|
                                 |kind|
                                 |detail|
                                 |documentation|
                                 |sortText|
                                 |filterText|
                                 |insertText|
                                 |textEdit|
                                 |additionalTextEdits|
                                 |command|
                                 |data|)
                        item
                      (with-slots (|range| |newText|) |textEdit|
                        (with-slots (|start| |end|) |range|
                          (lem:with-point ((point-start (lem:current-point))
                                           (point-end (lem:current-point)))
                            (move-to-lsp-position point-start |start|)
                            (move-to-lsp-position point-end |end|)
                            (lem:make-completion-item :label |newText|
                                                      :detail |detail|
                                                      :start point-start
                                                      :end point-end)))))))))

(lem:define-command lsp-signature-help () ()
  (reflect-modify-events (lem:current-buffer))
  (let ((signature-help (signature-help (lem:current-point))))
    (let ((text
           (with-output-to-string (out)
             (with-slots (|signatures| |activeSignature| |activeParameter|) signature-help
               (declare (ignore |activeSignature| |activeParameter|))
               (dolist (info |signatures|)
                 (with-slots (|label| |documentation| |parameters|) info
                   (declare (ignore |documentation|))
                   (princ |label| out)
                   (princ " " out)
                   (dolist (parameter |parameters|)
                     (with-slots (|label|) parameter
                       (princ |label| out)
                       (princ " " out)))
                   (terpri out)))))))
      (lem:message "~A" text))))

(defvar *definition-locations* nil)
(defvar *definition-index* 0)

(defun goto-definition (location)
  (with-slots (|uri| |range|) location
    (lem:find-file |uri|)
    (with-slots (|start|) |range|
      (move-to-lsp-position (lem:current-point) |start|))))

(lem:define-command lsp-goto-definition () ()
  (reflect-modify-events (lem:current-buffer))
  (let ((locations
         (map 'vector
              (lambda (ht)
                (convert-from-hash-table '|Location| ht))
              (alexandria:ensure-list
               (jsonrpc:call *client*
                             "textDocument/definition"
                             (list
                              (convert-to-hash-table
                               (make-text-document-position (lem:current-point)))))))))
    (when (< 0 (length locations))
      (goto-definition (aref locations 0))
      (when (< 1 (length locations))
        (setf *definition-locations* locations)))))

(lem:define-command lsp-next-definition () ()
  (when (and *definition-locations*
             (< (1+ *definition-index*) (length *definition-locations*)))
    (goto-definition (aref *definition-locations* (incf *definition-index*)))))

(lem:define-command lsp-previous-definition () ()
  (when (and *definition-locations*
             (< 0 *definition-index*))
    (goto-definition (aref *definition-locations* (decf *definition-index*)))))

(lem:define-command test () ()
  (lem:find-file "~/tmp/test.lisp")
  (lsp-start)
  (text-document-did-open (lem:current-buffer)))
