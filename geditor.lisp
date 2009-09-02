(make-package :geditor-w32)

(defun ge-main ()
  (load "geditor-main")
  (in-package geditor-w32)
  (geditor-w32::main)
  (SB-EXT:QUIT))

;; last time I tried, this didn't work (probably it is a compiler issue):
(sb-ext:save-lisp-and-die "geditor.exe" :toplevel 'ge-main :executable t)
