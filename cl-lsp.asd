(defsystem "cl-lsp"
  :class :package-inferred-system
  :depends-on ("cl-lsp/main"))

(register-system-packages "lem-lisp-syntax"
                          '(:lem-lisp-syntax.indent
                            :lem-lisp-syntax.enclosing
                            :lem-lisp-syntax.syntax-table))
