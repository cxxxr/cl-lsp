(defsystem "cl-lsp"
  :depends-on ("bordeaux-threads"
               "trivial-gray-streams"
               "swank"
               "cl-ppcre"
               "trivia"
               "alexandria"
               "trivial-types"
               "closer-mop"
               "quri"
               "jsonrpc"
               "yason"
               "jsonrpc/transport/stdio"
               "lem-base"
               "lem-lisp-syntax"
               "lem-lsp-utils"
               "cl-package-locks"
               "trivial-package-local-nicknames")
  :serial t
  :pathname "src/"
  :components ((:file "jsonrpc-patch")
               (:file "defpackage")
               (:file "editor")
               (:file "config")
               (:file "logger")
               (:file "gray-streams")
               (:file "swank")
               (:file "slime")
               (:file "protocol")
               (:file "protocol-util")
               (:file "formatting")
               (:file "server")
               (:file "text-document-controller")
               (:file "methods")
               (:module "lsp-methods"
                :pathname "methods"
                :serial t
                :components ((:file "lifetime")
                             (:file "workspace")
                             #+(or)(:file "text-document")))
               ;; (:file "eval")
               (:file "main"))
  :in-order-to ((test-op (test-op "cl-lsp/test"))))

(defsystem "cl-lsp/test"
  :depends-on ("cl-lsp" "rove")
  :pathname "test/"
  :components ((:file "test-server")
               (:file "initialize")
               (:file "initialized")
               (:file "text-document-did-open"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
