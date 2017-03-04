(defpackage :cl-lsp/editor
  (:use :cl
        :lem-base
        :cl-lsp/protocol)
  (:export :move-to-lsp-position
           :make-lsp-position
           :make-lsp-range
           :make-text-document-position
           :file-location
           :buffer-location
           :map-buffer-symbols))
(in-package :cl-lsp/editor)

(defun move-to-lsp-position (point position)
  (declare (type point point)
           (type |Position| position))
  (with-slots (|line| |character|) position
    (move-to-line point (1+ |line|))
    (line-offset point 0 |character|)
    point))

(defun make-lsp-position (point)
  (make-instance '|Position|
                 :|line| (1- (line-number-at-point point))
                 :|character| (point-charpos point)))

(defun make-lsp-range (start end)
  (declare (type point start end))
  (let* ((start-line (1- (line-number-at-point start)))
         (start-character (point-charpos start))
         (end-line (+ start-line (count-lines start end)))
         (end-character (point-charpos end)))
    (make-instance '|Range|
                   :|start| (make-instance '|Position| :|line| start-line :|character| start-character)
                   :|end| (make-instance '|Position| :|line| end-line :|character| end-character))))

(defun make-text-document-position (point)
  (make-instance '|TextDocumentPositionParams|
                 :|textDocument| (make-instance
                                  '|TextDocumentIdentifier|
                                  :|uri| (buffer-filename (point-buffer point)))
                 :|position| (make-lsp-position point)))

(defun line-location (file line start-charpos end-charpos)
  (make-instance
   '|Location|
   :|uri| (format nil "file://~A" file)
   :|range| (make-instance
             '|Range|
             :|start| (make-instance
                       '|Position|
                       :|line| line
                       :|character| start-charpos)
             :|end| (make-instance
                     '|Position|
                     :|line| line
                     :|character| end-charpos))))

(defun file-location (file offset)
  (with-open-file (in file)
    (loop :for string := (read-line in)
          :for length := (1+ (length string))
          :for line :from 0
          :do (if (>= offset length)
                  (decf offset length)
                  (return (line-location file line 0 (length string)))))))

(defun buffer-location (point)
  (let ((line (1- (line-number-at-point point))))
    (line-location (buffer-filename (point-buffer point))
                   line
                   (point-charpos point)
                   (with-point ((p point))
                     (if (form-offset p 1)
                         (point-charpos p)
                         (length (line-string point)))))))
