(cl-lsp/defpackage:defpackage :cl-lsp/server
  (:use :cl)
  (:import-from :cl-lsp/logger
                :log-format)
  (:import-from :cl-lsp/protocol
                :convert-from-hash-table
                :convert-to-hash-table)
  (:local-nicknames (:json-lsp-utils :lem-lsp-utils/json-lsp-utils))
  (:lock t)
  (:export
   :*server*
   :*method-lock*
   :with-error-handle
   :define-method
   :notify-show-message
   :notify-log-message
   :set-client-capabilities))
(in-package :cl-lsp/server)

(defvar *server* (jsonrpc:make-server))
(defvar *method-lock* (bt:make-lock))

(defun request-log (name params)
  (log-format "~%* from client~%")
  (log-format "name: ~A~%" name)
  (log-format "params: ~A~%"
              (with-output-to-string (stream)
                (yason:encode params stream))))

(defun response-log (hash)
  (log-format "~%* to server~%~A~%"
              (with-output-to-string (out)
                (yason:encode hash out))))

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

(defun convert-params (params params-type)
  (etypecase params-type
    (null params)
    (symbol
     (alexandria:switch ((symbol-package params-type))
       ((load-time-value (find-package :cl-lsp/protocol))
        (convert-from-hash-table params-type params))
       ((load-time-value (find-package :lem-lsp-utils/protocol))
        (json-lsp-utils:coerce-json params params-type))))))

(defun call-with-request-wrapper (name function
                                  &key params
                                       params-type
                                       without-lock
                                       without-initialized-check)
  (with-error-handle
    (request-log name params)
    (let* ((params (convert-params params params-type))
           (response
             (or (if without-initialized-check
                     nil
                     (error-response-if-already-initialized))
                 (if without-lock
                     (funcall function params)
                     (bt:with-lock-held (*method-lock*)
                       (funcall function params))))))
      (response-log response)
      response)))

(defmacro with-request-wrapper ((name params &optional params-type without-lock without-initialized-check)
                                &body body)
  `(call-with-request-wrapper ,name
                              (lambda (,params) (declare (ignorable ,params)) ,@body)
                              :params ,params
                              :params-type ',params-type
                              :without-lock ,without-lock
                              :without-initialized-check ,without-initialized-check))

(defmacro define-method (name
                         (&optional (params (gensym "PARAMS")) params-type)
                         (&key without-lock without-initialized-check)
                         &body body)
  `(jsonrpc:expose
    *server*
    ,name
    (lambda (,params)
      (with-request-wrapper (,name ,params ,params-type ,without-lock ,without-initialized-check)
        ,@body))))

(defun notify-show-message (type message)
  (log-format "window/showMessage: ~A ~A~%" type message)
  (jsonrpc:notify-async *server*
                        "window/showMessage"
                        (convert-to-hash-table
                         (make-instance '|ShowMessageParams|
                                        :|type| type
                                        :|message| message))))

(defun notify-log-message (type message)
  (log-format "window/logMessage: ~A ~A~%" type message)
  (jsonrpc:notify-async *server*
                        "window/logMessage"
                        (convert-to-hash-table
                         (make-instance '|LogMessageParams|
                                        :|type| type
                                        :|message| message))))

(defvar *initialize-params* nil)

(defun set-client-capabilities (initialize-params)
  (setf *initialize-params* initialize-params))

(defun error-response-if-already-initialized ()
  (unless *initialize-params*
    (alexandria:plist-hash-table
     (list "code" -32002
           "message" "did not initialize")
     :test 'equal)))
