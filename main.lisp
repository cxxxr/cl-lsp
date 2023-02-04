(defpackage :cl-lsp
  (:use :cl)
  (:import-from :lem-language-server/cli
                :main)
  (:export :main))
(in-package :cl-lsp)
