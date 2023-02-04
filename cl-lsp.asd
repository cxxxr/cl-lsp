(defsystem "cl-lsp"
  :depends-on ("lem-language-server/cli")
  :components ((:file "main"))
  :in-order-to ((test-op (test-op "cl-lsp/test"))))

(defsystem "cl-lsp/executable"
  :build-operation program-op
  :build-pathname "cl-lsp"
  :entry-point "cl-lsp/main:main"
  :depends-on ("cl-lsp"))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
