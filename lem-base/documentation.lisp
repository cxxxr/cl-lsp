(in-package :cl-lsp.lem-base)

(defparameter *language* :jp)

(annot:defannotation lang (form)
    (:inline t)
  (let ((string (getf form *language*)))
    string))
