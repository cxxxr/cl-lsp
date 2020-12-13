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
               (:file "config")
               (:file "logger")
               (:file "gray-streams")
               (:file "swank")
               (:file "slime")
               (:file "protocol")
               (:file "protocol-util")
               (:file "formatting")
               (:file "server")
               (:file "methods")
               (:module "lsp-methods"
                :pathname "methods"
                :serial t
                :components ((:file "lifetime")
                             (:file "workspace")))
               (:file "eval")
               (:file "main")))
