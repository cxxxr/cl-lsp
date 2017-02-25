(defpackage :cl-lsp/lisp-syntax
  (:use :cl :lem-base)
  (:export :*syntax-table*))
(in-package :cl-lsp/lisp-syntax)

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

(defun featurep (form)
  (cond ((atom form)
         (find (find-symbol (princ-to-string form)
                            :keyword)
               *features*))
        ((string-equal 'and (car form))
         (every #'featurep (cdr form)))
        ((string-equal 'or (car form))
         (some #'featurep (cdr form)))
        (t)))

(defvar *feature-attribute* (copy-attribute *syntax-comment-attribute*))

(defparameter +symbol-package-prefix+
  '(:sequence
    (:greedy-repetition 1 nil (:inverted-char-class #\( #\) #\space #\tab)) #\:))

(defun word-length-sort (&rest words)
  (sort (copy-list words) #'> :key #'length))

(defvar *syntax-table*
  (let ((table
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
          :block-comment-pairs '(("#|" . "|#")))))
    (syntax-add-match table
                      (make-syntax-test ":[^()\" \\t]+"
                                        :word-p t)
                      :attribute *syntax-constant-attribute*)
    (syntax-add-match table
                      (make-syntax-test "\\(")
                      :matched-symbol :start-form
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test "[^() \\t]+")
                      :test-symbol :define-start
                      :attribute *syntax-function-name-attribute*)
    (syntax-add-match table
                      (make-syntax-test "[^() \\t]+")
                      :test-symbol :defpackage-start
                      :attribute *syntax-type-attribute*)
    (syntax-add-match table
                      (make-syntax-test "[^() \\t]+")
                      :test-symbol :defvar-start
                      :attribute *syntax-variable-attribute*)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "defun" "defclass" "defgeneric" "defsetf" "defmacro" "defmethod")
                          (:sequence "define-" (:greedy-repetition 0 nil
                                                (:inverted-char-class #\space #\tab #\( #\))))))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*
                      :matched-symbol :define-start
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "deftype" "defpackage" "defstruct")))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*
                      :matched-symbol :defpackage-start
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort "defvar" "defparameter" "defconstant")))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*
                      :matched-symbol :defvar-start
                      :symbol-lifetime 1)
    (syntax-add-match table
                      (make-syntax-test
                       `(:sequence
                         (:greedy-repetition 0 1 ,+symbol-package-prefix+)
                         (:alternation
                          ,@(word-length-sort
                             "block" "case" "ccase" "ecase" "typecase" "etypecase" "ctypecase" "catch"
                             "cond" "destructuring-bind" "do" "do*" "dolist" "dotimes"
                             "eval-when" "flet" "labels" "macrolet" "generic-flet" "generic-labels"
                             "handler-case" "restart-case" "if" "lambda" "let" "let*" "handler-bind"
                             "restart-bind" "locally" "multiple-value-bind" "multiple-value-call"
                             "multiple-value-prog1" "prog" "prog*" "prog1" "prog2" "progn" "progv" "return"
                             "return-from" "symbol-macrolet" "tagbody" "throw" "unless" "unwind-protect"
                             "when" "with-accessors" "with-condition-restarts" "with-open-file"
                             "with-output-to-string" "with-slots" "with-standard-io-syntax" "loop"
                             "declare" "declaim" "proclaim")))
                       :word-p t)
                      :test-symbol :start-form
                      :attribute *syntax-keyword-attribute*)
    (syntax-add-match table
                      (make-syntax-test "&[^() \\t]+"
                                        :word-p t)
                      :attribute *syntax-constant-attribute*)
    (syntax-add-match
     table
     (make-syntax-test "#[+-]")
     :move-action (lambda (cur-point)
                    (ignore-errors
                     (let ((positivep (eql #\+ (character-at cur-point 1))))
                       (character-offset cur-point 2)
                       (with-point ((prev cur-point))
                         (when (form-offset cur-point 1)
                           (cond
                             ((if (featurep (read-from-string
                                             (points-to-string
                                              prev cur-point)))
                                  positivep
                                  (not positivep))
                              nil)
                             (t
                              (form-offset cur-point 1))))))))
     :attribute *feature-attribute*)
    table))
