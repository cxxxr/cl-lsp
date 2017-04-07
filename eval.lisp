(defpackage :cl-lsp/eval
  (:use :cl
        :cl-lsp/logger
        :cl-lsp/server
        :cl-lsp/protocol
        :cl-lsp/protocol-util
        :cl-lsp/slime
        :cl-lsp/swank)
  (:import-from :bordeaux-threads)
  (:import-from :swank)
  (:import-from :optima)
  (:import-from :lem-base)
  (:import-from :jsonrpc))
(in-package :cl-lsp/eval)

(defvar *eval-thread* nil)

(let ((wait (bt:make-condition-variable))
      (lock (bt:make-lock))
      (queue (list)))

  (defun receive ()
    (bt:with-lock-held (lock)
      (bt:condition-wait wait lock)
      (pop queue)))

  (defun send (x)
    (bt:with-lock-held (lock)
      (setf queue (nconc queue (list x)))
      (bt:condition-notify wait))))

(defun ensure-package (package)
  (or (find-package package)
      (find-package "CL-USER")))

(defun eval-string (string package)
  (let ((*package* (ensure-package package))
        results)
    (with-input-from-string (*standard-input* "")
      (let ((output-string
             (with-output-to-string (out)
               (let ((*standard-output* out)
                     (*error-output* out))
                 (with-output-to-string (*error-output*)
                   (handler-bind
                       ((error (lambda (err)
                                 (bt:with-lock-held (*method-lock*)
                                   (notify-log-message |MessageType.Error|
                                                       (with-output-to-string (out)
                                                         (uiop:print-backtrace
                                                          :condition err
                                                          :stream out)))
                                   (notify-show-message |MessageType.Error|
                                                        (princ-to-string err))
                                   (return-from eval-string)))))
                     (setf results
                           (multiple-value-list
                            (eval (read-from-string string))))))))))
        (bt:with-lock-held (*method-lock*)
          (notify-log-message |MessageType.Log| output-string)
          (notify-show-message |MessageType.Info| (format nil "~{~A~^, ~}" results))
          )))))

(defun start-eval-thread ()
  (unless *eval-thread*
    (setf *eval-thread*
          (bt:make-thread
           (lambda ()
             (with-error-handle
               (loop :for event := (receive) :do
                 (destructuring-bind (string package) event
                   (eval-string string package)))))))))

(defun send-eval-string (string package)
  (start-eval-thread)
  (send (list string package)))


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
                            (lem-base:get-file-buffer file)))
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
          (package (search-buffer-package point)))
      (when string
        (send-eval-string string package))
      nil)))

(define-method "lisp/interrupt" (params)
  )
