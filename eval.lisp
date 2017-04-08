(defpackage :cl-lsp/eval
  (:use :cl
        :cl-lsp/logger
        :cl-lsp/server
        :cl-lsp/protocol
        :cl-lsp/protocol-util
        :cl-lsp/slime
        :cl-lsp/swank
        :cl-lsp/gray-streams)
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

(defun start-eval-thread ()
  (unless *eval-thread*
    (setf *eval-thread*
          (bt:make-thread
           (lambda ()
             (with-error-handle
               (loop :for event := (receive) :do
                 (funcall event))))
           :initial-bindings (acons 'jsonrpc/connection:*connection*
                                    jsonrpc/connection:*connection*
                                    nil)))))

(pushnew 'start-eval-thread *initialized-hooks*)


(defun compilation-notes-to-diagnostics (notes)
  (bt:with-lock-held (*method-lock*)
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
      (list-to-object[] diagnostics))))

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

(defun compile-and-load-file (uri)
  (let ((filename (uri-to-filename uri))
        result)
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
                                       (princ-to-string condition)))))))))))

(define-method "lisp/compileAndLoadFile" (params |TextDocumentIdentifier|)
  (let* ((uri (slot-value params '|uri|)))
    (send (lambda () (compile-and-load-file uri))))
  nil)

(defun lsp-output-fn (string)
  (notify-log-message |MessageType.Log| string))

(defun eval-string (string package)
  (let ((*package* (ensure-package package))
        results)
    (let ((_out (make-instance 'lsp-output-stream :output-fn #'lsp-output-fn)))
      (with-input-from-string (_in "")
        (with-open-stream (eval-stream (make-two-way-stream _in _out))
          (let ((*standard-output* eval-stream)
                (*error-output* eval-stream)
                (*standard-input* eval-stream)
                (*terminal-io* eval-stream)
                (*query-io* eval-stream)
                (*debug-io* eval-stream)
                (*trace-output* eval-stream))
            (handler-bind
                ((error (lambda (err)
                          (finish-output eval-stream)
                          (notify-log-message |MessageType.Error|
                                              (with-output-to-string (out)
                                                (format out "~%~A~%~%" err)
                                                (uiop:print-backtrace :stream out)))
                          (notify-show-message |MessageType.Error|
                                               (princ-to-string err))
                          (return-from eval-string))))
              (setf results
                    (multiple-value-list
                     (eval (read-from-string string))))))
          (finish-output eval-stream)
          (notify-show-message |MessageType.Info| (format nil "~{~A~^, ~}" results)))))))

(defun send-eval-string (string package)
  (send (lambda () (eval-string string package))))

(defun form-string (point)
  (if (and (lem-base:start-line-p point)
           (eql #\( (lem-base:character-at point)))
      (lem-base:with-point ((p point))
        (when (lem-base:form-offset p 1)
          (lem-base:points-to-string point p)))
      (lem-base:with-point ((p point))
        (when (lem-base:form-offset p -1)
          (lem-base:points-to-string p point)))))

(define-method "lisp/eval" (params |TextDocumentPositionParams|)
  (with-text-document-position (point) params
    (let ((string (form-string point)))
      (when string
        (let ((package (search-buffer-package point)))
          (send-eval-string string package)))
      nil)))

(define-method "lisp/rangeEval" (params)
  (let* ((uri (gethash "uri" (gethash "textDocument" params)))
         (range (convert-from-hash-table '|Range| (gethash "range" params))))
    (with-slots (|start| |end|) range
      (with-document-position (start uri |start|)
        (lem-base:with-point ((end start))
          (move-to-lsp-position end |end|)
          (send-eval-string (lem-base:points-to-string start end)
                            (search-buffer-package start)))))))

(define-method "lisp/interrupt" (params nil t)
  (when *eval-thread*
    (bt:interrupt-thread *eval-thread*
                         (lambda ()
                           (error "interrupt"))))
  nil)
