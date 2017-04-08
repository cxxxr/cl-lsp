(defpackage :cl-lsp/slime
  (:use :cl
        :lem-base)
  (:import-from :cl-ppcre)
  (:export :symbol-string-at-point*
           :beginning-of-defun-point
           :beginning-of-defun
           :map-buffer-symbols
           :search-buffer-package))
(in-package :cl-lsp/slime)

(defun symbol-string-at-point* (point)
  (let ((string (symbol-string-at-point point)))
    (when string
      (values (ppcre:regex-replace "^(?:#\\.|,@)" string "")))))

(defun beginning-of-defun-point (point n)
  (with-point ((curr point))
    (if (minusp n)
        (dotimes (_ (- n) curr)
          (if (start-line-p curr)
              (line-offset curr -1)
              (line-start curr))
          (loop
            (when (char= #\( (character-at curr 0))
              (return))
            (unless (line-offset curr -1)
              (return-from beginning-of-defun-point curr))))
        (dotimes (_ n curr)
          (loop
            (unless (line-offset curr 1)
              (return-from beginning-of-defun-point curr))
            (when (char= #\( (character-at curr 0))
              (return)))))))

(defun beginning-of-defun (point n)
  (move-point point (beginning-of-defun-point point n)))

(defun map-buffer-symbols (buffer function)
  (with-point ((p (buffer-start-point buffer)))
    (loop
      (loop
        (when (= 0 (skip-chars-forward p
                                       (complement
                                        (lambda (c)
                                          (or (member c '(#\, #\' #\`))
                                              (syntax-symbol-char-p c))))))
          (return-from map-buffer-symbols))
        (alexandria:if-let ((str (looking-at p ",@|,|'|`|#\\.")))
          (character-offset p (length str))
          (return)))
      (cond
        ((maybe-beginning-of-string-or-comment p)
         (unless (form-offset p 1) (return)))
        (t
         (with-point ((start p))
           (form-offset p 1)
           (funcall function (points-to-string start p))))))))

(defun search-buffer-package (point)
  (with-point ((p point))
    (buffer-start p)
    (or (loop :while (search-forward-regexp p "^\\s*\\(in-package\\s")
              :do (with-point ((start p))
                    (when (form-offset p 1)
                      (handler-case (let ((name (symbol-name
                                                 (read-from-string
                                                  (points-to-string start p)))))
                                      (unless (equal name "CL-USER")
                                        (return (find-package name))))
                        (error ()
                          (find-package "CL-USER"))))))
        (find-package "CL-USER"))))
