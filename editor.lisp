(defpackage :lsp.editor
  (:use :cl :lem-base :lsp.protocol)
  (:export :move-to-lsp-position
           :make-lsp-position
           :make-lsp-range))

(in-package :lsp.editor)

(defun move-to-lsp-position (point position)
  (declare (type point point)
           (type |Position| position))
  (with-slots (|line| |character|) position
    (move-to-line point (1+ |line|))
    (character-offset point |character|)
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
