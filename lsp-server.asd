(asdf:defsystem lsp-server
  :depends-on (:jsonrpc :lem-base :closer-mop :trivial-types)
  :serial t
  :components ((:file "util")
               (:file "protocol")
               (:file "editor")
               (:file "server")))
