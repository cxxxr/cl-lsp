(defpackage :cl-lsp/lisp-syntax
  (:use :cl :lem-base)
  (:export :*syntax-table*))
(in-package :cl-lsp/lisp-syntax)

(defvar *syntax-table*
  (make-syntax-table
   :space-chars '(#\space #\tab #\newline)
   :symbol-chars '(#\+ #\- #\< #\> #\/ #\* #\& #\= #\. #\? #\_ #\! #\$ #\% #\: #\@ #\[ #\]
                       #\^ #\{ #\} #\~ #\# #\|)
   :paren-alist '((#\( . #\))
                  (#\[ . #\])
                  (#\{ . #\}))
   :string-quote-chars '(#\")
   :escape-chars '(#\\)
   :fence-chars '(#\|)
   :expr-prefix-chars '(#\' #\, #\@ #\# #\`)
   :expr-prefix-forward-function 'skip-expr-prefix-forward
   :expr-prefix-backward-function 'skip-expr-prefix-backward
   :line-comment-string ";"
   :block-comment-pairs '(("#|" . "|#"))))

(defun %skip-expr-prefix (c1 c2 step-fn)
  (when c1
    (multiple-value-bind (unused-fn dispatch-char-p)
        (get-macro-character c1)
      (declare (ignore unused-fn))
      (when (and dispatch-char-p
                 (not (member c2 '(#\( #\\)))
                 (get-dispatch-macro-character c1 c2))
        (funcall step-fn c1 c2)))))

(defun skip-expr-prefix-forward (point)
  (%skip-expr-prefix
   (character-at point 0)
   (character-at point 1)
   (lambda (c1 c2)
     (declare (ignore c1 c2))
     (character-offset point 2))))

(defun skip-expr-prefix-backward (point)
  (%skip-expr-prefix
   (character-at point -2)
   (character-at point -1)
   (lambda (c1 c2)
     (declare (ignore c1 c2))
     (character-offset point -2))))
