(asdf:defsystem lsp-server
  :depends-on (:jsonrpc :lem-base :closer-mop :trivial-types :swank)
  :serial t
  :components ((:file "util")
               (:file "protocol")
               (:file "editor")
               (:file "lisp-syntax")
               (:file "server")))
