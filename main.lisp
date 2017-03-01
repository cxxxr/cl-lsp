(defpackage :cl-lsp/main
  (:use :cl
        :cl-lsp/protocol
        :cl-lsp/editor
        :cl-lsp/lisp-syntax
        :cl-lsp/logger)
  (:import-from :quri)
  (:import-from :jsonrpc)
  (:import-from :yason)
  (:import-from :uiop)
  (:import-from :alexandria)
  (:import-from :swank)
  (:import-from :optima)
  (:import-from :lem-base)
  (:export :run-tcp-mode
           :run-stdio-mode))
(in-package :cl-lsp/main)

(defvar *server* (jsonrpc:make-server))

(defun method-log (name params)
  (log-format "name: ~A~%" name)
  (log-format "params: ~A~%"
              (with-output-to-string (stream)
                (yason:encode params stream))))

(defun call-with-error-handle (function)
  (handler-bind ((error (lambda (c)
                          (log-format "~A~%~%~A~%"
                                      c
                                      (with-output-to-string (stream)
                                        (uiop:print-backtrace :stream stream :condition c))))))
    (funcall function)))

(defmacro with-error-handle (&body body)
  `(call-with-error-handle (lambda () ,@body)))

(defmacro define-method (name (params) &body body)
  `(jsonrpc:register-method *server*
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

(defun call-with-text-document-position (text-document-position-params function)
  (let* ((position (slot-value text-document-position-params '|position|))
         (uri (slot-value (slot-value text-document-position-params '|textDocument|) '|uri|)))
    (multiple-value-bind (buffer)
        (lem-base:find-file-buffer (quri:uri-path (quri:uri uri)))
      (setf (lem-base:buffer-syntax-table buffer) *syntax-table*)
      (let ((point (lem-base:buffer-point buffer)))
        (move-to-lsp-position point position)
        (unwind-protect (funcall function point)
          (lem-base:delete-buffer buffer))))))

(defmacro with-text-document-position ((point) params &body body)
  `(call-with-text-document-position ,params (lambda (,point) ,@body)))

(defmacro with-swank ((&key (package (find-package "CL-USER"))
                            (readtable '*readtable*))
                      &body body)
  `(let ((swank::*buffer-package* ,package)
         (swank::*buffer-readtable* ,readtable))
     ,@body))

(defun search-buffer-package (point)
  (lem-base:with-point ((p point))
    (lem-base:line-start p)
    (or (loop
          (when (lem-base:looking-at p "^\\s*\\(in-package\\s")
            (return (and (lem-base:scan-lists p 1 -1 t)
                         (lem-base:form-offset p 1)
                         (lem-base:skip-whitespace-forward p)
                         (lem-base:with-point ((start p))
                           (when (lem-base:form-offset p 1)
                             (ignore-errors
                              (find-package
                               (read-from-string (lem-base:points-to-string start p)))))))))
          (unless (lem-base:line-offset p -1)
            (return)))
        (find-package "CL-USER"))))

(defvar *initialize-params* nil)
(defvar *swank-fuzzy-completions* nil)

(defun check-initialized ()
  (when (null *initialize-params*)
    (alexandria:plist-hash-table
     (list "code" -32002
           "message" "did not initialize")
     :test 'equal)))

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
   (make-instance
    '|InitializeResult|
    :|capabilities| (make-instance
                     '|ServerCapabilities|
                     :|textDocumentSync| (make-instance
                                          '|TextDocumentSyncOptions|
                                          :|openClose| t
                                          :|change| |TextDocumentSyncKind.Incremental|
                                          ;:|willSave| nil
                                          ;:|willSaveWaitUntil| nil
                                          ;:|save| ...
                                          )
                     :|hoverProvider| t
                     :|completionProvider| (make-instance
                                            '|CompletionOptions|
                                            :|resolveProvider| t
                                            :|triggerCharacters| (loop :for code
                                                                       :from (char-code #\a)
                                                                       :below (char-code #\z)
                                                                       :collect (string (code-char code))))
                     :|signatureHelpProvider| (make-instance
                                               '|SignatureHelpOptions|
                                               :|triggerCharacters| (list " "))
                     :|definitionProvider| t
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

(define-method "initialized" (params)
  nil)

(define-method "shutdown" (params)
  t)

(define-method "exit" (params)
  (values))

(define-method "workspace/didChangeConfiguration" (params)
  nil)

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
        (setf (lem-base:buffer-syntax-table buffer) *syntax-table*)
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
                   (move-to-lsp-position point |start|)
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
  (with-text-document-position (point)
      (convert-from-hash-table '|TextDocumentPositionParams| params)
    (lem-base:with-point ((start point)
                          (end point))
      (lem-base:skip-symbol-backward start)
      (lem-base:skip-symbol-forward end)
      (let ((result
             (with-swank ()
               (funcall *swank-fuzzy-completions*
                        (lem-base:points-to-string start end)
                        (search-buffer-package point)))))
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
  (with-text-document-position (point)
      (convert-from-hash-table '|TextDocumentPositionParams| params)
    (let ((symbol-string (lem-base:symbol-string-at-point point)))
      (setf symbol-string (ppcre:regex-replace "^#['.]" symbol-string ""))
      (let ((describe-string
             (ignore-errors
              (with-swank (:package (search-buffer-package point))
                (swank:describe-symbol symbol-string)))))
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
                            :|contents| "")))))))

#+(or)
(progn
  (defun autodoc-parsing-safe-p (point)
    (not (lem-base:in-string-or-comment-p point)))

  (defun autodoc-parse-context-1 (point suffix)
    )

  (defun autodoc-parse-context (point)
    (when (autodoc-parsing-safe-p point)
      (let ((suffix (list 'swank::%cursor-marker%)))
        (cond ((and (eql #\( (lem-base:character-at point))
                    (not (eql #\\ (lem-base:character-at point -1))))
               (lem-base:form-offset point 1)
               (push "" suffix))
              ((or (lem-base:start-line-p point)
                   (and (eql #\space (character-at point -1))
                        (not (eql #\\ (lem-base:character-at point -2)))))
               (push "" suffix))
              ((and (eql #\( (character-at point -1))
                    (not (eql #\\ (lem-base:character-at point -2))))
               (push "" suffix))
              (t
               (lem-base:skip-chars-forward point #'lem-base:syntax-symbol-char-p)))
        (autodoc-parse-context-1 point suffix))))
  )

(defun beginning-of-defun-point (point &optional limit-lines)
  (lem-base:with-point ((p point))
    (lem-base:line-start p)
    (loop
      (when (char= #\( (lem-base:character-at p))
        (return p))
      (unless (lem-base:line-offset p -1)
        (return (lem-base::line-start p)))
      (when limit-lines
        (when (>= 0 (decf limit-lines))
          (return p))))))

(defun parse-arglist-string (string)
  (labels ((f (start)
             (let ((list '()))
               (loop
                 (multiple-value-bind (ms me)
                     (ppcre:scan "[()]|[^() \\t\\n]+" string :start start)
                   (unless ms (return (nreverse list)))
                   (cond ((string= string "(" :start1 ms :end1 me)
                          (multiple-value-bind (elt pos) (f me)
                            (push elt list)
                            (setf start pos)))
                         ((string= string ")" :start1 ms :end1 me)
                          (return (values (nreverse list) me)))
                         (t
                          (setf start me)
                          (push (subseq string ms me) list))))))))
    (car (f 0))))

(defun arglist (point)
  (loop :with start := (beginning-of-defun-point point)
        :while (lem-base:form-offset point -1)
        :do (when (lem-base:point< point start)
              (return-from arglist nil)))
  (lem-base:skip-whitespace-forward point)
  (let ((symbol-string (lem-base:symbol-string-at-point point)))
    (when symbol-string
      (swank:operator-arglist symbol-string
                              (search-buffer-package point)))))

(define-method "textDocument/signatureHelp" (params)
  (with-text-document-position (point)
      (convert-from-hash-table '|TextDocumentPositionParams| params)
    (let ((arglist (arglist point)))
      (convert-to-hash-table
       (make-instance
        '|SignatureHelp|
        :|signatures| (when arglist
                        (list (make-instance
                               '|SignatureInformation|
                               :|label| arglist))))))))

(defun find-definitions (point name)
  (with-swank (:package (search-buffer-package point))
    (swank:find-definitions-for-emacs name)))

(define-method "textDocument/definition" (params)
  (with-text-document-position (point)
      (convert-from-hash-table '|TextDocumentPositionParams| params)
    (alexandria:when-let ((name (lem-base:symbol-string-at-point point)))
      (let ((locations '()))
        (dolist (def (find-definitions point name))
          (optima:match def
            ((list _
                   (list :location
                         (list :file file)
                         (list :position offset)
                         (list :snippet _)))
             (push (convert-to-hash-table
                    (file-location file offset))
                   locations))))
        (list-to-object[] locations)))))

(define-method "textDocument/references" (params)
  (with-text-document-position (point)
      (convert-from-hash-table '|ReferenceParams| params)
    (let ((symbol-string (lem-base:symbol-string-at-point point))
          (locations '()))
      (loop :for (type . definitions)
            :in (with-swank (:package (search-buffer-package point))
                  (swank:xrefs '(:calls :macroexpands :binds :references :sets :specializes)
                               symbol-string))
            :do (loop :for def :in definitions
                      :do (optima:match def
                            ((list _
                                   (list :location
                                         (list :file file)
                                         (list :position offset)
                                         (list :snippet _)))
                             (push (file-location file offset) locations)))))
      (list-to-object[] locations))))

(define-method "textDocument/codeLens" (params)
  #+(or)
  (let* ((code-lens-params
          (convert-from-hash-table '|CodeLensParams| params))
         (text-document (slot-value code-lens-params '|textDocument|))
         (uri (slot-value text-document '|uri|)))
    (declare (ignore uri)))
  (vector))

(define-method "textDocument/documentLink" (params)
  (let* ((document-link-params
          (convert-from-hash-table '|DocumentLinkParams| params))
         (text-document
          (slot-value document-link-params '|textDocument|))
         (uri
          (slot-value text-document '|uri|)))
    (declare (ignore uri))
    (list (convert-to-hash-table
           (make-instance
            '|DocumentLink|
            :|range| (make-instance
                      '|Range|
                      :|start| (make-instance '|Position| :|line| 0 :|character| 0)
                      :|end| (make-instance '|Position| :|line| 0 :|character| 0))
            :|target| nil)))))

(defun run-tcp-mode (&key (port 10003))
  (with-logger-stream (*error-output*)
    (log-format "server-listen~%mode:tcp~%port:~D~%" port)
    (jsonrpc:server-listen *server* :port port :mode :tcp)))

(defun run-stdio-mode ()
  (with-log-file ("~/lsp-log")
    (log-format "server-listen~%mode:stdio~%")
    (jsonrpc:server-listen *server* :mode :stdio)))
