(defpackage :lsp.server
  (:use :cl
        :lsp.protocol
        :lsp.editor)
  (:export :run-tcp-mode
           :run-stdio-mode))

(in-package #:lsp.server)

(defvar *server* (jsonrpc:make-server))

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

(defun buffer-package-name (buffer)
  (declare (ignore buffer))
  "CL-USER")

(defun get-point-from-text-document-position (text-document-position-params)
  (let* ((position (slot-value text-document-position-params '|position|))
         (uri (slot-value (slot-value text-document-position-params '|textDocument|) '|uri|))
         (document (find-document uri)))
    (when document
      (let* ((buffer
              (document-buffer document))
             (point
              (lem-base:buffer-point buffer)))
        (move-to-lsp-position point position)
        point))))

(defun call-with-text-document-position (params function)
  (let ((point (get-point-from-text-document-position
                (convert-from-hash-table '|TextDocumentPositionParams|
                                         params))))
    (funcall function point)))

(defmacro with-text-document-position ((point) params &body body)
  `(call-with-text-document-position ,params (lambda (,point) ,@body)))

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
   (make-instance '|InitializeResult|
                  :|capabilities| (make-instance '|ServerCapabilities|
                                                 :|textDocumentSync| t
                                                 :|hoverProvider| t
                                                 :|completionProvider| t
                                                 :|signatureHelpProvider| t
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
  (with-text-document-position (point) params
    (lem-base:with-point ((start point)
                          (end point))
      (lem-base:skip-symbol-backward start)
      (lem-base:skip-symbol-forward end)
      (let ((result
             (with-swank ()
               (funcall *swank-fuzzy-completions*
                        (lem-base:points-to-string start end)
                        (buffer-package-name (lem-base:point-buffer point))))))
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
  (with-text-document-position (point) params
    (let ((describe-string
           (ignore-errors
            (with-swank ()
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
      (parse-arglist-string
       (swank:operator-arglist symbol-string
                               (buffer-package-name
                                (lem-base:point-buffer point)))))))

(define-method "textDocument/signatureHelp" (params)
  (with-text-document-position (point) params
    (let ((arglist (arglist point)))
      (convert-to-hash-table
       (make-instance
        '|SignatureHelp|
        :|signatures| (when arglist
                      (list (make-instance
                             '|SignatureInformation|
                             :|label| (car arglist)
                             :|parameters| (mapcar (lambda (arg)
                                                     (make-instance
                                                      '|ParameterInformation|
                                                      :|label| arg))
                                                   (cdr arglist))))))))))

(defun find-definitions (name buffer)
  (with-swank (:package (find-package (buffer-package-name buffer)))
    (swank:find-definitions-for-emacs name)))

(define-method "textDocument/definition" (params)
  (with-text-document-position (point) params
    (alexandria:when-let ((name (lem-base:symbol-string-at-point point)))
      (let ((locations (make-array 0 :fill-pointer 0 :adjustable t)))
        (dolist (def (find-definitions name (lem-base:point-buffer point)))
          (optima:match def
            ((list _
                   (list :location
                         (list :file file)
                         (list :position offset)
                         (list :snippet _)))
             (multiple-value-bind (buffer new-buffer-p)
                 (lem-base:find-file-buffer file)
               (let ((point (lem-base:buffer-point buffer)))
                 (lem-base:move-to-position point offset)
                 (let ((start (make-lsp-position point))
                       (end (make-lsp-position (or (lem-base:form-offset point 1)
                                                   (lem-base:line-end point)))))
                   (vector-push-extend (make-instance '|Location|
                                                      :|uri| file
                                                      :|range| (make-instance
                                                                '|Range|
                                                                :|start| start
                                                                :|end| end))
                                       locations))
                 (when new-buffer-p
                   (lem-base:delete-buffer buffer)))))))
        (convert-to-hash-table
         (if (= 1 (length locations))
             (aref locations 0)
             locations))))))

(define-method "textDocument/references" (params)
  (let* ((reference-params (convert-from-hash-table '|ReferenceParams| params))
         (point (get-point-from-text-document-position reference-params)))
    ))

(defun run-tcp-mode (&key (port 10003))
  (format t "server-listen~%mode:tcp~%port:~D~%" port)
  (jsonrpc:server-listen *server* :port port :mode :tcp))

(defun run-stdio-mode ()
  (with-open-file (*standard-output* "~/LOG"
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :append)
    (format *standard-output* "server-listen~%mode:stdio~%")
    (jsonrpc:server-listen *server* :mode :stdio)))
