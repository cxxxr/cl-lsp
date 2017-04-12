(defsystem "cl-lsp.lem-lisp-syntax"
  :depends-on ("cl-lsp.lem-base" "cl-ppcre")
  :pathname "lem-lisp-syntax/"
  :serial t
  :components ((:file "indent")
               (:file "syntax-table")
               (:file "enclosing")))
