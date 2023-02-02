LISP ?= sbcl

cl-lsp: src/main.lisp
	$(LISP) --eval '(asdf:make :cl-lsp/executable)'

