;;; geditor-main.lisp
;;;
;;; graphical editor for windows in Lisp
;;;

(load "callback-hacking")
(load "winapi-package")
(load "win32-types-and-constants")
(cl:defpackage :geditor-w32
  (:use :common-lisp :winapi :sb-alien)
  (:export #:main))

(in-package :geditor-w32)

(defclass GraphicData ()
  (
   (points :initform (make-array 1 :fill-pointer 0 :adjustable t))
   (rectangles :initform (make-array 1 :fill-pointer 0 :adjustable t))
  ))

(defgeneric add-rectangle (this x y))
(defmethod add-rectangle ((this GraphicData) x y)
  (vector-push-extend (cons (cons x y) (cons (+ x 10) (+ y 10)))
(slot-value this 'rectangles))
  (format t "adding rectangle (~a ~a) ~%" x y))



(defgeneric add-point (this x y))
(defmethod add-point ((this GraphicData) x y)
  (vector-push-extend (cons x y) (slot-value this 'points))
  (format t "adding point (~a ~a) ~%" x y))

(defparameter *tool-line* 'line)
(defparameter *tool-rectangle* 'rectangle)
(defparameter *tool-oval* 'oval)

(defvar *current-tool* *tool-line* "current selected tool")

(defvar *graphic-data* (make-instance 'GraphicData) "graphic data")

(add-point *graphic-data* 10 10)
(add-point *graphic-data* 100 100)

(defparameter *window-title* "Simple Graphic Editor in Common Lisp")
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

(defun get-point-from-windows-msg (lparam)
  "lparam is a 32 bit value encoding two 16 bit words.
  we create a const from lparam and return it."
  (let ((s 16))
    (cons (mod lparam #x10000) (ash lparam (- s)))))

(defun paint-line-at (hdc x1 y1 x2 y2)
  (moveto hdc x1 y1)
  (lineto hdc x2 y2))

(defun paint-rectangle-at (hdc x1 y1 x2 y2)
  (rectangle hdc x1 y1 x2 y2))

(defun paint-line (hdc)
  (let* ((vec (slot-value *graphic-data* 'points))
         (n (fill-pointer vec)))
    (dotimes (i (- n 1))
      (let ((point1 (aref vec i))
            (point2 (aref vec (+ i 1))))
        (paint-line-at hdc (car point1) (cdr point1) (car point2) (cdr
point2)))))
  (let* ((vec (slot-value *graphic-data* 'rectangles))
         (n (fill-pointer vec)))
    (dotimes (i n)
      (let* ((pt1 (car (aref vec i)))
             (pt2 (cdr (aref vec i)))
             (x (car pt1))
             (y (cdr pt1))
             (x2 (car pt2))
             (y2 (cdr pt2)))
        (paint-rectangle-at hdc x y x2 y2)))))


(defun on-window-paint (hwnd)
  (with-alien ((ps (struct paintstruct)) (rect (struct rect)))
              (let ((hdc (beginpaint hwnd (addr ps))))
               ;(paint-hello-lisp hdc hwnd rect (addr rect))
                (paint-line hdc)
                (endpaint hwnd (addr ps)))))

(defun on-mouse-down (hwnd lparam)
  ; save current point
  (let ((pt (get-point-from-windows-msg lparam)))
    (cond
      ((eq *current-tool* *tool-line*)
          (add-point *graphic-data* (car pt) (cdr pt)))
      ((eq *current-tool* *tool-rectangle*)
         (add-rectangle *graphic-data* (car pt) (cdr pt)))))
  ; set second point
  (invalidaterect hwnd nil t))
  ;(updatewindow hwnd))

(defun on-window-create (hwnd)
  "create and attach a menu for the application"
  (let ((menu (createmenu)))
    (let ((sub-menu (createmenu)))
      (add-menu-item sub-menu 100 "Open")
      (add-menu-item sub-menu 102 "Save")
      (add-menu-item sub-menu 103 "Save as")
      (add-menu-item sub-menu 104 "Close")
      (add-menu-separator sub-menu)
      (add-menu-item sub-menu 105 "Exit")
      (add-sub-menu menu "File" sub-menu))
    (let ((sub-menu (createmenu)))
      (add-menu-item sub-menu 200 "Copy")
      (add-menu-item sub-menu 201 "Cut")
      (add-menu-item sub-menu 202 "Paste")
      (add-sub-menu menu "Edit" sub-menu))
    (let ((sub-menu (createmenu)))
      (add-menu-item sub-menu 300 "Line")
      (add-menu-item sub-menu 301 "Rectangle")
      (add-menu-item sub-menu 302 "Oval")
      (add-sub-menu menu "Tool" sub-menu))
    (let ((sub-menu (createmenu)))
      (add-menu-item sub-menu 300 "Contents")
      (add-menu-item sub-menu 401 "About")
      (add-sub-menu menu "Help" sub-menu))
    (setmenu hwnd menu)))

(defun get-hi-lo-word (wparam)
  "wparam is a 32 bit value encoding two 16 bit words.
  return these values as a result"
  (let ((s 16))
    (values (mod wparam #x10000) (ash wparam (- s)))))

(defun on-command (hwnd wparam lparam)
  ; we can also do: (multiple-value-bind (cmd-id event) (get-hi-lo-word wparam)
  (let ((cmd-id (get-hi-lo-word wparam)))
    (cond
      ((= cmd-id 105) (sendmessage hwnd wm_close 0 0) 0)
      ((= cmd-id 300) (setf *current-tool* *tool-line*) 0)
      ((= cmd-id 301) (setf *current-tool* *tool-rectangle*) 0)
      ((= cmd-id 302) (setf *current-tool* *tool-oval*) 0)
      ((= cmd-id 401) (messagebox hwnd "About Simple Editor" "Created
in Common Lisp" 0) 0)
      (t (defwindowproc hwnd wm_command wparam lparam)))))

(defun window-procedure (hwnd imsg wparam lparam)
  (cond
    ((= imsg wm_create)  (on-window-create hwnd) 0)
    ((= imsg wm_paint)   (on-window-paint hwnd) 0)
    ((= imsg wm_command) (on-command hwnd wparam lparam) 0)
   ;((= imsg wm_keyup)   (postquitmessage 0) 0)
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

(defvar *w*) ; handle for the window (useful for debugging)
(defun dbg-close () (sendmessage *w* wm_destroy 0 0)) ; (useful for debugging)

(defun create-window (width height)
  (let ((hwnd (createwindow *window-class-name* *window-title*
                            ws_overlappedwindow
                            cw_usedefault 0 width height
                            nil nil *default-hinstance* nil)))
    (showwindow hwnd sw_shownormal)
    (setf *w* hwnd)
    (updatewindow hwnd)))

(defun message-loop ()
  (with-alien ((msg (struct msg)))
              (loop until (zerop (getmessage (addr msg) nil 0 0))
                    do (progn
                         (translatemessage (addr msg))
                         (dispatchmessage (addr msg))))))

(defun main ()
  (register-window-class)
  (create-window 500 700)
  (message-loop)
  (unregister-window-class))

