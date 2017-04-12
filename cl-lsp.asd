(load-asd (merge-pathnames "cl-lsp.lem-base.asd" *load-pathname*))
(load-asd (merge-pathnames "cl-lsp.lem-lisp-syntax.asd" *load-pathname*))

(defsystem "cl-lsp"
  :depends-on ("bordeaux-threads"
               "trivial-gray-streams"
               "swank"
               "cl-ppcre"
               "optima"
               "alexandria"
               "trivial-types"
               "closer-mop"
               "quri"
               "jsonrpc"
               "yason"
               "cl-lsp.lem-base"
               "cl-lsp.lem-lisp-syntax")
  :serial t
  :components ((:file "logger")
               (:file "gray-streams")
               (:file "swank")
               (:file "slime")
               (:file "protocol")
               (:file "protocol-util")
               (:file "formatting")
               (:file "server")
               (:file "eval")
               (:file "main")))
