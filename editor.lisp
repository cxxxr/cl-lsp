(defpackage :lsp.editor
  (:use :cl :lem-base :lsp.protocol)
  (:export :move-to-lsp-position
           :make-lsp-range))

(in-package :lsp.editor)

(defun move-to-lsp-position (point position)
  (declare (type point point)
           (type |Position| position))
  (with-slots (|line| |character|) position
    (buffer-start point)
    (line-offset point |line|)
    (character-offset point |character|)
    point))

(defun make-lsp-range (start end)
  (declare (type point start end))
  (let* ((start-line (line-number-at-point start))
         (start-character (point-charpos start))
         (end-line (+ start-line (count-lines start end)))
         (end-character (point-charpos end)))
    (make-instance '|Range|
                   :|start| (make-instance '|Position| :|line| start-line :|character| start-character)
                   :|end| (make-instance '|Position| :|line| end-line :|character| end-character))))