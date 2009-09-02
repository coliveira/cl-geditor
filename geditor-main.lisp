;;; geditor-main.lisp
;;;
;;; graphical editor for windows in Lisp
;;;
; (setf *default-pathname-defaults* #P"c:/co/code/cl-gedit/")

(load "callback-hacking")
(load "winapi-package")
(load "win32-types-and-constants")
(cl:defpackage :geditor-w32
  (:use :common-lisp :winapi :sb-alien)
  (:export #:main))

(in-package :geditor-w32)

(defparameter *window-title* "GEditor in Lisp")
(defparameter *window-class-name* "GEditor")

(defparameter *lambda* (make-array 2 :element-type '(unsigned-byte 16)
                                   :initial-contents '(#x3bb 0)))

(defun draw-lambda (hdc rect-addr)
  "draw the lambda character"
  (alien-funcall
    (extern-alien "DrawTextW"
                  (function int hdc (* unsigned-short) int
                            (* (struct rect)) unsigned-int))
    hdc (sb-int::vector-sap *lambda*) -1
    rect-addr (logior dt_singleline dt_center dt_vcenter)))

(defun set-rect-size (rect)
  (setf (slot rect 'bottom) (+ (slot rect 'top)
                               (ash (- (slot rect 'bottom)
                                       (slot rect 'top)) -1))))
(defun do-paint (hdc rect rect-addr)
  (set-rect-size rect)
  (draw-lambda hdc rect-addr))

(defun paint-hello-lisp (hdc hwnd rect rect-addr)
  (getclientrect hwnd rect-addr)
  (drawtext hdc "Hello Lisp World!" -1 rect-addr
            (logior dt_singleline dt_center dt_vcenter))
  (do-paint hdc rect rect-addr))

(defstruct my-point
  x y)

(defvar *point1* (make-my-point :x 10 :y 10))
(defvar *point2* (make-my-point :x 100 :y 100))

(defun set-point-from-windows-msg (lparam point)
  "lparam is a 32 bit value encoding two 16 bit words.
  we create a point from lparam using these two words."
  (let ((s 16))
    (setf (my-point-x point) (mod lparam #x10000))
    (setf (my-point-y point) (ash lparam (- s)))))

(defun paint-line (hdc hwnd rect rect-addr)
  (moveto hdc (my-point-x *point1*) (my-point-y *point1*))
  (lineto hdc (my-point-x *point2*) (my-point-y *point2*)))

(defun on-window-paint (hwnd lparam)
  (with-alien ((ps (struct paintstruct)) (rect (struct rect)))
              (let ((hdc (beginpaint hwnd (addr ps))))
               ;(paint-hello-lisp hdc hwnd rect (addr rect))
                (paint-line hdc hwnd rect (addr rect))
                (endpaint hwnd (addr ps)))))

(defun on-mouse-down (hwnd lparam)
  ; save current point
  (setf (my-point-x *point1*) (my-point-x *point2*))
  (setf (my-point-y *point1*) (my-point-y *point2*))
  ; set second point
  (set-point-from-windows-msg lparam *point2*)
  (invalidaterect hwnd nil t))
  ;(updatewindow hwnd))

(defun on-window-create (hwnd)
  (let ((menu (createmenu)))
    (let ((sub-menu (createmenu)))
      (add-menu-item sub-menu "Open")
      (add-menu-item sub-menu "Save")
      (add-menu-item sub-menu "Save as")
      (add-menu-item sub-menu "Close")
      (add-menu-separator sub-menu)
      (add-menu-item sub-menu "Exit")
      (add-sub-menu menu "File" sub-menu))
    (let ((sub-menu (createmenu)))
      (add-menu-item sub-menu "Copy")
      (add-menu-item sub-menu "Cut")
      (add-menu-item sub-menu "Paste")
      (add-sub-menu menu "Edit" sub-menu))
    (let ((sub-menu (createmenu)))
      (add-menu-item sub-menu "Contents")
      (add-menu-item sub-menu "About")
      (add-sub-menu menu "Help" sub-menu))
    (setmenu hwnd menu)))

(defun window-procedure (hwnd imsg wparam lparam)
  (cond
    ((= imsg wm_create)  (on-window-create hwnd) 0)
    ((= imsg wm_paint)   (on-window-paint hwnd lparam) 0)
   ;((= imsg wm_keyup)   (postquitmessage 0) 0)
   ;((= imsg wm_lbuttondown) (messagebox hwnd "test" "gedit" 0) 0)
    ((= imsg wm_lbuttondown) (on-mouse-down hwnd lparam) 0)
    ((= imsg wm_destroy) (postquitmessage 0) 0)
    (t (defwindowproc hwnd imsg wparam lparam))))

(defparameter *window-procedure*
  (sb-alien::alien-callback
   (function unsigned-int hwnd unsigned-int unsigned-long unsigned-long)
   #'(lambda (x0 x1 x2 x3) (window-procedure x0 x1 x2 x3)) :stdcall))

;; Our hInstance is the load address of the runtime.
(defparameter *default-hinstance* #x400000)

(defun register-window-class ()
  (with-alien ((wndclass (struct wndclassex)))
    (setf (slot wndclass 'cbsize) 48)
    (setf (slot wndclass 'style) (logior cs_hredraw cs_vredraw))
    (setf (slot wndclass 'lpfnwndproc) (alien-sap *window-procedure*))
    (setf (slot wndclass 'cbclsextra) 0)
    (setf (slot wndclass 'cbwndextra) 0)
    (setf (slot wndclass 'hinstance) *default-hinstance*)
    (setf (slot wndclass 'hicon) (loadicon 0 idi_application))
    (setf (slot wndclass 'hcursor) (loadcursor 0 idc_arrow))
    (setf (slot wndclass 'hbrbackground) (getstockobject white_brush))
    (setf (slot wndclass 'lpszmenuname) nil)
    (setf (slot wndclass 'lpszclassname) *window-class-name*)
    (setf (slot wndclass 'hiconsm) (loadicon 0 idi_application))
    (registerclassex (addr wndclass))))

(defun unregister-window-class ()
  (unregisterclass *window-class-name* *default-hinstance*))

(defun create-window ()
  (let ((hwnd (createwindow *window-class-name* *window-title*
			    ws_overlappedwindow
			    cw_usedefault 0 500 500
			    nil nil *default-hinstance* nil)))
    (showwindow hwnd sw_shownormal)
    (format t "the window is ~a~%" hwnd)
    (updatewindow hwnd)))

(defun message-loop ()
  (with-alien ((msg (struct msg)))
              (loop until (zerop (getmessage (addr msg) nil 0 0))
                    do (progn
                         (translatemessage (addr msg))
                         (dispatchmessage (addr msg))))))

(defun main ()
  (register-window-class)
  (create-window)
  (message-loop)
  (unregister-window-class))
