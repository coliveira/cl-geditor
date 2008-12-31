;;;
;;; win32-types-and-constants.lisp
;;;
;;; And functions, oh my.
;;;

(in-package :winapi)

;; Types. Hacked, fairly badly in places.
(define-alien-type hgdiobj (* (struct nil)))
(define-alien-type atom long)
(define-alien-type hwnd (* char))
(define-alien-type hinstance long)
(define-alien-type hicon (* (struct nil)))
(define-alien-type hcursor (* (struct nil)))
(define-alien-type hbrush (* (struct nil)))
(define-alien-type hmenu (* (struct nil)))
(define-alien-type hdc (* (struct nil)))
(define-alien-type wndproc (* (function int int unsigned-int unsigned-int unsigned-int)))

(define-alien-type nil
    (struct wndclassex
	    (cbsize unsigned-int)
	    (style unsigned-int)
	    (lpfnwndproc wndproc)
	    (cbclsextra int)
	    (cbwndextra int)
	    (hinstance hinstance)
	    (hicon hicon)
	    (hcursor hcursor)
	    (hbrbackground hbrush)
	    (lpszmenuname c-string)
	    (lpszclassname c-string)
	    (hiconsm hicon)))

(define-alien-type nil
    (struct point
	    (x long)
	    (y long)))

(define-alien-type nil
    (struct rect
	    (left long)
	    (top long)
	    (right long)
	    (bottom long)))

(define-alien-type nil
    (struct paintstruct
	    (hdc hdc)
	    (ferase (boolean 32))
	    (rcpaint (struct rect))
	    (frestore (boolean 32))
	    (fincupdate (boolean 32))
	    (rgbreserved (array char 32))))

(define-alien-type nil
    (struct msg
	    (hwnd hwnd)
	    (msg unsigned-int)
	    (wparam unsigned-int)
	    (lparam unsigned-long)
	    (time unsigned-long)
	    (pt (struct point))))


;; Constants.
(defconstant wm_destroy 2)
(defconstant wm_paint 15)
(defconstant wm_keyup 257)
(defconstant wm_lbuttondown #x201)
(defconstant cs_hredraw 2)
(defconstant cs_vredraw 1)
(defconstant white_brush 0)
(defconstant idi_application 32512)
(defconstant idc_arrow 32512)
(defconstant ws_overlappedwindow #xcf0000)
(defconstant cw_usedefault #x80000000)
(defconstant sw_shownormal 1)
(defconstant dt_singleline 32)
(defconstant dt_center 1)
(defconstant dt_vcenter 4)

;; Functions. (should be in another file?)

(load-shared-object "USER32")

(declaim (inline postquitmessage))
(defun postquitmessage (nexitcode)
  (alien-funcall
   (extern-alien "PostQuitMessage" (function void int))
   nexitcode))

(declaim (inline messagebox))
(defun messagebox (hwnd text caption flags)
  (alien-funcall
    (extern-alien "MessageBoxA"
                  (function unsigned-int hwnd c-string c-string unsigned-int))
    hwnd text caption flags))

(declaim (inline defwindowproc))
(defun defwindowproc (hwnd msg wparam lparam)
  (alien-funcall
   (extern-alien "DefWindowProcA" (function unsigned-long hwnd unsigned-int unsigned-long unsigned-long))
   hwnd msg wparam lparam))

(declaim (inline registerclassex))
(defun registerclassex (wndclass)
  (alien-funcall
   (extern-alien "RegisterClassExA" (function atom (* (struct wndclassex))))
   wndclass))

(declaim (inline unregisterclass))
(defun unregisterclass (lpclassname hinstance)
  (alien-funcall
   (extern-alien "UnregisterClassA" (function boolean c-string hinstance))
   lpclassname hinstance))

;; Don't think I'm kidding about the function type. I just don't know how to express it properly.
(declaim (inline loadcursor))
(defun loadcursor (hinstance lpcursorname)
  (alien-funcall
   (extern-alien "LoadCursorA" (function hcursor hinstance #+(and) int #+(or) (or c-string unsigned-short)))
   hinstance lpcursorname))

(declaim (inline loadicon))
(defun loadicon (hinstance lpiconname)
  (alien-funcall
   (extern-alien "LoadIconA" (function hicon hinstance #+(and) int #+(or) (or c-string unsigned-short)))
   hinstance lpiconname))

;; Yes, CreateWindowExA. Backwards compatability.
(declaim (inline createwindow))
(defun createwindow (lpclassname lpwindowname dwstyle x y nwidth nheight hwndparent hmenu hinstance lpparam)
  (alien-funcall
   (extern-alien "CreateWindowExA" (function hwnd int c-string c-string unsigned-long unsigned-int unsigned-int unsigned-int unsigned-int hwnd hmenu hinstance (* char)))
   0 lpclassname lpwindowname dwstyle x y nwidth nheight hwndparent hmenu hinstance lpparam))

(declaim (inline showwindow))
(defun showwindow (hwnd ncmdshow)
  (alien-funcall
   (extern-alien "ShowWindow" (function boolean hwnd int))
   hwnd ncmdshow))

(declaim (inline updatewindow))
(defun updatewindow (hwnd)
  (alien-funcall
   (extern-alien "UpdateWindow" (function boolean hwnd))
   hwnd))

(declaim (inline invalidaterect))
(defun invalidaterect (hwnd lprect erase)
  (alien-funcall
    (extern-alien "InvalidateRect"
                  (function boolean hwnd (* (struct rect)) boolean))
    hwnd lprect erase))

;; win32.hlp defines GetMesage as returning BOOL, and then describes distinct zero, nonzero, and -1 return values.
(declaim (inline getmessage))
(defun getmessage (lpmsg hwnd wmsgfiltermin wmsgfiltermax)
  (alien-funcall
   (extern-alien "GetMessageA" (function int (* (struct msg)) hwnd unsigned-int unsigned-int))
   lpmsg hwnd wmsgfiltermin wmsgfiltermax))

(declaim (inline translatemessage))
(defun translatemessage (lpmsg)
  (alien-funcall
   (extern-alien "TranslateMessage" (function boolean (* (struct msg))))
   lpmsg))

(declaim (inline dispatchmessage))
(defun dispatchmessage (lpmsg)
  (alien-funcall
   (extern-alien "DispatchMessageA" (function long (* (struct msg))))
   lpmsg))

(declaim (inline beginpaint))
(defun beginpaint (hwnd lppaint)
  (alien-funcall
   (extern-alien "BeginPaint" (function hdc hwnd (* (struct paintstruct))))
   hwnd lppaint))

(declaim (inline endpaint))
(defun endpaint (hwnd lppaint)
  (alien-funcall
   (extern-alien "EndPaint" (function boolean hwnd (* (struct paintstruct))))
   hwnd lppaint))

(declaim (inline getclientrect))
(defun getclientrect (hwnd lprect)
  (alien-funcall
   (extern-alien "GetClientRect" (function boolean hwnd (* (struct rect))))
   hwnd lprect))

(load-shared-object "GDI32")

(declaim (inline getstockobject))
(defun getstockobject (fnobject)
  (alien-funcall
   (extern-alien "GetStockObject" (function hgdiobj int))
   fnobject))

(declaim (inline drawtext))
(defun drawtext (hdc lpstring ncount lprect uformat)
  (alien-funcall
   (extern-alien "DrawTextA" (function int hdc c-string int (* (struct rect)) unsigned-int))
   hdc lpstring ncount lprect uformat))

(declaim (inline lineto))
(defun lineto (hdc x y)
  (alien-funcall
    (extern-alien "LineTo" (function int hdc int int))
    hdc x y))

(declaim (inline moveto))
(defun moveto (hdc x y)
  (alien-funcall
    (extern-alien "MoveToEx" (function int hdc int int int))
    hdc x y 0))

;;; EOF
