(defpackage :cl-lsp/slime
  (:use :cl
        :lem-base)
  (:import-from :cl-ppcre)
  (:export :symbol-string-at-point*
           :beginning-of-defun-point
           :search-local-definition))
(in-package :cl-lsp/slime)

(defun symbol-string-at-point* (point)
  (let ((string (lem-base:symbol-string-at-point point)))
    (when string
      (values (ppcre:regex-replace "^#\\." string "")))))

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

(defvar *variable-binding-ops*
  '(("let" &bindings 1 &body 2)
    ("let*" &bindings 1 &body 2)))

(defvar *function-binding-ops*
  '(("flet" &bindings 1 &body 2)
    ("labels" &bindings 1 &body 1)
    ("macrolet" &bindings 1 &body 2)))

(defun lookup-binding-op (op &optional binding-type)
  (labels ((lookup-in (list) (assoc op list :test #'string=)))
    (case binding-type
      ((:variable) (lookup-in *variable-binding-ops*))
      ((:function) (lookup-in *function-binding-ops*))
      (otherwise (or (lookup-in *variable-binding-ops*)
                     (lookup-in *function-binding-ops*))))))

(defun binding-op-p (op &optional binding-type)
  (when (lookup-binding-op op binding-type) t))

(defun binding-op-body-pos (op)
  (let ((specs (lookup-binding-op op)))
    (when specs
      (getf (cdr specs) '&body))))

(defun binding-op-bindings-pos (op)
  (let ((specs (lookup-binding-op op)))
    (when specs
      (getf (cdr specs) '&bindings))))

(defun traverse-list (point function)
  (with-point ((p point))
    (if (maybe-beginning-of-string-or-comment p)
        (form-offset p 1)
        (skip-symbol-forward p))
    (loop
      (let ((arg-index 0))
        (when (or (member (character-at p) '(#\( #\'))
                  (syntax-space-char-p (character-at p -1)))
          (incf arg-index))
        (form-offset p -1)
        (loop :while (form-offset p -1)
              :do (incf arg-index))
        (unless (scan-lists p -1 1 t)
          (return))
        (when (member (character-at p) '(#\( #\'))
          (character-offset p 1)
          (let ((name (symbol-string-at-point p)))
            (funcall function
                     (copy-point p :temporary)
                     name
                     arg-index))
          (scan-lists p -1 1)))
      (when (start-line-p p)
        (return)))))

(defun search-local-definition (point name)
  (traverse-list point
                 (lambda (p op index)
                   (when (and (binding-op-p op)
                              (>= index (binding-op-body-pos op)))
                     (form-offset p (binding-op-bindings-pos op))
                     (scan-lists p 1 -1)
                     (loop
                       (unless (scan-lists p 1 -1 t) (return))
                       (when (equal name (symbol-string-at-point p))
                         (return-from search-local-definition p))
                       (unless (scan-lists p 1 1 t) (return)))))))
