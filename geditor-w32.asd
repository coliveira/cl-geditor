(asdf:defsystem :geditor-w32
  :depends-on (:lisp-winapi)
  :serial t
  :components
  ((:file "geditor-main")))

;;; EOF
