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

(define-interface |DidOpenTextDocumentParams| ()
  (|textDocument| :type |TextDocumentItem|))

(define-interface |DidChangeTextDocumentParams| ()
  (|textDocument| :type |VersionedTextDocumentIdentifier|)
  (|contentChanges| :type (trivial-types:proper-list |TextDocumentContentChangeEvent|)))

(define-interface |TextDocumentContentChangeEvent| ()
  (|range| :optional t :type |Range|)
  (|rangeLength| :optional t :type number)
  (|text| :type string))

(define-interface |DidCloseTextDocumentParams| ()
  (|textDocument| :type |TextDocumentIdentifier|))

(defun protocol-symbol-p (type)
  (if (symbolp type)
      (when (member type *protocol-symbols*)
        type)
      (and (consp type)
           (eq 'trivial-types:proper-list (first type))
           (protocol-symbol-p (second type)))))

(defun convert-from-hash-table (name hash-table)
  (make-instance name)
  (let ((object (make-instance name)))
    (loop :for slot :in (c2mop:class-slots (find-class name))
          :for slot-name := (c2mop:slot-definition-name slot)
          :for slot-type := (c2mop:slot-definition-type slot)
          :for hash-key := (string slot-name)
          :do (setf (slot-value object slot-name)
                    (if (setf slot-type (protocol-symbol-p slot-type))
                        (let ((hash-value (gethash hash-key hash-table)))
                          (if (listp hash-value)
                              (mapcar (lambda (hash-value-1)
                                        (convert-from-hash-table slot-type hash-value-1))
                                      hash-value)
                              (convert-from-hash-table slot-type hash-value)))
                        (gethash hash-key hash-table))))
    object))
