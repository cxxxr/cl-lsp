(asdf:defsystem lsp-server
  :depends-on (:jsonrpc :lem-base :closer-mop :trivial-types :swank :optima)
  :serial t
  :components ((:file "protocol")
               (:file "editor")
               (:file "lisp-syntax")
               (:file "server")))
