(defsystem "cl-lsp"
  :depends-on ("lem-lsp-server")
  :pathname "src/"
  :components ((:file "main"))
  :in-order-to ((test-op (test-op "cl-lsp/test"))))
