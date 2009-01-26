(require 'asdf)

(defun main ()
  (asdf:operate 'asdf:load-op 'geditor-w32))

;(main)

;; last time I tried, this didn't work (probably it is a compiler issue):
(sb-ext:save-lisp-and-die "geditor.exe" :toplevel 'main :executable t)
