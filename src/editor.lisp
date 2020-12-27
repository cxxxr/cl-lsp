(cl-lsp/defpackage:defpackage :cl-lsp/editor
  (:use :cl)
  (:export :make-file-contents-position
           :make-file-contents-range
           :open-file-contents
           :close-file-contents
           :replace-file-contents
           :edit-file-contents
           :move-to-position
           :symbol-string-at-point
           :current-package))
(in-package :cl-lsp/editor)

(defclass editor () ())

(defclass lem (editor) ())

(defstruct file-contents-position
  line
  character)

(defstruct file-contents-range
  start
  end)

(defmethod open-file-contents-using-editor ((editor lem) uri text)
  (let ((buffer
          (lem-base:make-buffer uri
                                :temporary t
                                :syntax-table lem-lisp-syntax:*syntax-table*)))
    (lem-base:insert-string (lem-base:buffer-point buffer) text)
    buffer))

(defmethod close-file-contents-using-editor ((editor lem) file-contents)
  (check-type file-contents lem-base:buffer)
  (lem-base:delete-buffer file-contents))

(defun %move-to-position (point position)
  (lem-base:move-to-line point (1- (file-contents-position-line position)))
  (lem-base:line-offset point 0 (file-contents-position-character position)))

(defmethod replace-file-contents-using-editor ((editor lem) file-contents text)
  (let ((buffer file-contents))
    (lem-base:erase-buffer buffer)
    (lem-base:insert-string (lem-base:buffer-point buffer) text)))

(defmethod edit-file-contents-using-editor ((editor lem) file-contents range text)
  (let* ((buffer file-contents)
         (point (lem-base:buffer-point buffer)))
    (%move-to-position point (file-contents-range-start range))
    (lem-base:with-point ((end point))
      (%move-to-position end (file-contents-range-end range))
      (lem-base:delete-between-points point end))
    (lem-base:insert-string point text)))

(defmethod move-to-position-using-editor ((editor lem) file-contents position)
  (let* ((buffer file-contents)
         (point (lem-base:buffer-point buffer)))
    (%move-to-position point position)))

(defmethod symbol-string-at-point-using-editor ((editor lem) file-contents)
  (let* ((buffer file-contents)
         (point (lem-base:buffer-point buffer)))
    (lem-base:symbol-string-at-point point)))

(defmethod current-package-using-editor ((editor lem) file-contents)
  (let* ((buffer file-contents)
         (point (lem-base:buffer-point buffer))
         (regex (load-time-value
                 (ppcre:create-scanner "^\\s*\\(in-package\\s"
                                       :case-insensitive-mode t))))
    (loop
      (unless (lem-base:line-offset point -1)
        (return (find-package :cl-user)))
      (when (ppcre:scan regex (lem-base:line-string point))
        (lem-base:scan-lists point 1 -1 t)
        (lem-base:skip-whitespace-forward point)
        (return (ignore-errors (read-from-string (lem-base:symbol-string-at-point point))))))))


(defvar *editor* (make-instance 'lem))

(defun open-file-contents (uri text)
  (let ((file-contents (open-file-contents-using-editor *editor* uri text)))
    file-contents))

(defun close-file-contents (file-contents)
  (close-file-contents-using-editor *editor* file-contents)
  (values))

(defun replace-file-contents (file-contents text)
  (replace-file-contents-using-editor *editor* file-contents text)
  (values))

(defun edit-file-contents (file-contents range text)
  (edit-file-contents-using-editor *editor* file-contents range text)
  (values))

(defun move-to-position (file-contents position)
  (move-to-position-using-editor *editor* file-contents position)
  (values))

(defun symbol-string-at-point (file-contents)
  (symbol-string-at-point-using-editor *editor* file-contents))

(defun current-package (file-contents)
  (current-package-using-editor *editor* file-contents))
