(defpackage :cl-lsp/slime
  (:use :cl
        :lem-base)
  (:export :beginning-of-defun-point))
(in-package :cl-lsp/slime)

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

