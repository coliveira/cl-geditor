;;; -*- lisp -*-
;;;
;;; lisp-winapi.asd
;;;
;;; system definition for lisp-winapi
;;;

(asdf:defsystem :lisp-winapi
    :serial t
    :components
  ((:file "winapi-package")
   (:file "callback-hacking") ;; redefine bits of SBCL internals
   (:file "win32-types-and-constants")))

;;; EOF
