(cl-lsp/defpackage:defpackage :cl-lsp/methods/text-document
  (:use :cl)
  (:import-from :cl-lsp/server
                :define-method)
  (:local-nicknames (:protocol :lem-lsp-utils/protocol)
                    (:json :lem-lsp-utils/json)
                    (:editor :cl-lsp/editor)))
(in-package :cl-lsp/methods/text-document)

(defvar *text-documents* '())

(defstruct text-document
  uri
  language-id
  file-contents)

(defun find-text-document (uri)
  (find uri *text-documents* :test #'string= :key #'text-document-uri))

(defun open-text-document (&key uri text language-id)
  (when (find-text-document uri)
    (error "The file is already opened: ~S" uri))
  (let ((file-contents (editor:open-file-contents uri text)))
    (push (make-text-document :uri uri
                              :language-id language-id
                              :file-contents file-contents)
          *text-documents*)
    file-contents))

(defun close-text-document (text-document)
  (setf *text-documents* (delete text-document *text-documents*))
  (values))

(defun convert-position (position)
  (editor:make-file-contents-position
   :line (protocol:position-line position)
   :character (protocol:position-character position)))

(defun convert-range (range)
  (editor:make-file-contents-range
   :start (convert-position (protocol:range-start range))
   :end (convert-position (protocol:range-end range))))

(defun apply-content-change (text-document content-change)
  (cond ((stringp content-change)
         (cl-lsp/editor:replace-file-contents text-document content-change))
        (t
         (let ((range (json:json-get content-change "range"))
               (text (json:json-get content-change "text")))
           (editor:edit-file-contents (text-document-file-contents text-document)
                                      (convert-range range)
                                      text)))))

(define-method "textDocument/didOpen" (params protocol:did-open-text-document-params) ()
  (let* ((text-document-item (protocol:did-open-text-document-params-text-document params))
         (uri (protocol:text-document-item-uri text-document-item))
         (language-id (protocol:text-document-item-language-id text-document-item))
         (text (protocol:text-document-item-text text-document-item)))
    (open-text-document :uri uri
                        :text text
                        :language-id language-id)
    (values)))

(define-method "textDocument/didChange" (params protocol:did-change-text-document-params) ()
  (let* ((text-document-identifier (protocol:did-change-text-document-params-text-document params))
         (content-changes (protocol:did-change-text-document-params-content-changes params))
         (text-document
           (find-text-document (protocol:text-document-identifier-uri text-document-identifier))))
    (assert text-document)
    (lem-utils:do-sequence (content-change content-changes)
      (apply-content-change text-document content-change))
    (values)))

(define-method "textDocument/willSave" () ()
  )

(define-method "textDocument/willSaveWaitUntil" () ()
  )

(define-method "textDocument/didSave" (params protocol:did-save-text-document-params) ()
  )

(define-method "textDocument/didClose" (params protocol:did-close-text-document-params) ()
  (let* ((text-document-identifier (protocol:did-close-text-document-params-text-document params))
         (text-document (find-text-document (protocol:text-document-identifier-uri text-document-identifier))))
    (assert text-document)
    (editor:close-file-contents (text-document-file-contents text-document))
    (values)))
