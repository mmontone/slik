;;;
;;; frames
;;;
;;; This file describes the basic SLIK class, the frame
;;;
;;; 05-Apr-1992 I. Kalet started
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 27-May-1992 I. Kalet add mapped keyword parameter to make-frame
;;; 29-May-1992 I. Kalet set default font initarg here
;;;  8-Oct-1992 I. Kalet change defsetf to defmethod (setf ...
;;; 27-Oct-1992 I. Kalet enhance erase function, fix draw-border,
;;; remove pixmap attribute - only in pictures, fix refresh fn.
;;; 29-Nov-1992 I. Kalet put exposure event here not just in picture,
;;; and announce it as well as calling refresh. Also delete ulc-x and
;;; ulc-y slots, since they are not needed.
;;; 15-Feb-1993 I. Kalet change fg-color and bg-color to have
;;; accessors, not just readers.  Add setf after methods that call
;;; refresh.
;;;  3-Jan-1995 I. Kalet fix up some setf methods
;;; 18-Feb-1996 I. Kalet use new global *screen-root-depth* instead of
;;; querying every time.
;;;  4-May-1997 I. Kalet fix error of omission in (setf bg-color)
;;;  method - need to update the xlib window, not just set the attribute.
;;;  1-Apr-1999 C. Wilcox added look-ahead slot.
;;; 22-Apr-1999 I. Kalet add support for each frame to have its own
;;; visual and/or colormap instead of the inherited ones.
;;; 28-May-2000 I. Kalet add support for shaded 3-d borders.
;;; 26-Nov-2000 I. Kalet change default border-style to :raised and
;;; default background color to gray.  Changes the overall look and
;;; feel of user interfaces by default.
;;; 11-Mar-2001 I. Kalet make default foreground and background colors
;;; and default border style initializable parameters instead of hardcoded.
;;;  2-Feb-2003 I. Kalet use :around methods for setf fg-color etc. to
;;; insure that the stuff that has to be done last is done last.
;;; 27-Aug-2003 I. Kalet when creating a window for a frame, add
;;; WM_DELETE_WINDOW to the WM_PROTOCOLS property for the window, so
;;; that a window manager destroy operation can be intercepted.  By
;;; default, the destroy operation is ignored.
;;; 19-Mar-2007 I. Kalet change initialize-instance method to insure
;;; that the default visual parameter is a card29, not the keyword :copy.
;;;

(in-package :slik)

;;;-------------------------------------------

(defclass frame ()

  ((title :type string
	  :accessor title
	  :initarg :title)

   (width :type xlib:card16
	  :reader width
	  :initarg :width)

   (height :type xlib:card16
	   :reader height
	   :initarg :height)

   (bg-color :type symbol
	     :accessor bg-color
	     :initarg :bg-color
	     :documentation "A symbol in the SLIK package naming a color")

   (fg-color :type symbol
	     :accessor fg-color
	     :initarg :fg-color
	     :documentation "A symbol in the SLIK package naming a color")

   (font :type xlib:font
	 :accessor font
	 :initarg :font)

   (border-width :type xlib:card8
		 :accessor border-width
		 :initarg :border-width)

   (border-color :type symbol
		 :accessor border-color
		 :initarg :border-color
		 :documentation "A symbol in the SLIK package naming a
color")

   (border-style :accessor border-style
		 :initarg :border-style
		 :documentation "Border-style is a keyword, :flat for
the original widget border style, :raised for a sort of raised button
look, or :lowered for an indented look.")

   (border-gc :accessor border-gc
	      :initform (make-duplicate-gc)
	      :documentation "Set for border width and color
initially, much faster than using the xlib:with-gcontext macro on a
standard gcontext.")

   (window :type xlib:window
	   :accessor window)

   (colormap :type xlib:colormap
	     :accessor colormap
	     :initarg :colormap
	     :documentation "The colormap associated with the window
of the frame.  It is usually just a copy of the parent's.")

   (exposure :type ev:event
	     :accessor exposure
	     :initform (ev:make-event)
	     :documentation "Announced when a part of the frame window
is exposed.")

   (wm-close :type ev:event
	     :accessor wm-close
	     :initform (ev:make-event)
	     :documentation "Announced when the window manager
	     attempts to close a window, usually because the user
	     clicked on the window manager provided close-window icon.")

   (look-ahead :accessor look-ahead
	       :initarg :look-ahead
	       :documentation "When this slot's value is not nil, the
event handler will look ahead in the event queue to remove duplicate
events of the specified types.")

   )

  (:default-initargs :title "SLIK frame" :bg-color 'default-bg
		     :fg-color 'default-fg :border-width 1
		     :border-color 'white  :font *default-font*
		     :colormap nil :look-ahead nil
		     :border-style *default-border-style*)

  (:documentation "The basic SLIK entity which includes all the CLX
stuff and of which all other SLIK classes are subclasses.")

  )

;;;----------------------------------------

(defun erase (f)

  "erase f

erases the contents of frame f by setting the entire window of the
frame to the background color."

  (xlib:clear-area (window f))
  (flush-output))

;;;----------------------------------------

(defun draw-border (f)

  "draw-border f

Draws the border of frame f in border-color, border-width wide.  If
border-width is 0, skip it."

  (when (> (border-width f) 0)
    (case (border-style f)
      (:flat (let ((b2 (truncate (/ (border-width f) 2))))
	       (xlib:draw-rectangle (window f) (border-gc f)
				   b2 b2
				   (- (width f) (1+ b2))
				   (- (height f) (1+ b2)))))
      (:raised (xlib:draw-lines (window f) (border-gc f)
			       (list 0 (height f) 0 0 (width f) 0))
	       (xlib:draw-lines (window f) (color-gc 'black2 (colormap f))
			       (list (- (width f) 1) 0
				     (- (width f) 1) (- (height f) 1)
				     0 (- (height f) 1))))
      (:lowered (xlib:draw-lines (window f) (color-gc 'black2 (colormap f))
				(list 1 (height f) 1 1 (width f) 1))
		(xlib:draw-lines (window f) (border-gc f)
				(list (- (width f) 1) 0
				      (- (width f) 1) (- (height f) 1)
				      0 (- (height f) 1)))))))

;;;---------------------------------------

(defmethod initialize-instance :after ((f frame)
				       &key parent (mapped t)
					    (ulc-x 0) (ulc-y 0)
					    visual
				       &allow-other-keys)

  "Method for creating the CLX window and pixmap for any SLIK object."

  (unless (colormap f)
    (setf (colormap f) ;(xlib:window-colormap (or parent *screen-root*))
	  *screen-default-colormap*))
  (setf (window f)
    (xlib:create-window :parent (or parent *screen-root*)
		       :x ulc-x :y ulc-y
		       :width (width f) :height (height f)
		       :depth *screen-root-depth*
		       :visual (or visual
				   (xlib:window-visual (or parent
							  *screen-root*)))
		       :colormap (colormap f)
		       :background (xlib:gcontext-foreground
				    (color-gc (bg-color f)
					      (colormap f)))
		       :event-mask
		       '(:key-press :button-press :button-release
			 :button-motion :enter-window :leave-window
			 :exposure)
		       ))
  (setf (xlib:wm-protocols (window f))
    (cons 'WM_DELETE_WINDOW (xlib:wm-protocols (window f))))
  (xlib:copy-gcontext (color-gc (border-color f) (colormap f))
		     (border-gc f))
  (setf (xlib:gcontext-line-width (border-gc f)) (border-width f))
  (setf (xlib:wm-name (window f)) (title f))
  (erase f) ; erase everything initially
  (draw-border f)
  (if mapped (xlib:map-window (window f)))
  (flush-output)
  (register f)
  f)

;;;---------------------------------------

(defun make-frame (width height &rest other-initargs)

  "make-frame width height &rest other-initargs

Returns a new instance of class frame.  Width and height are required.
The rest of the argument list specifies the initial values for the
attributes of a frame.  If parent is nil, the frame's window is a top
level window.  Otherwise, parent is a CLX window that should be the
parent of the new frame's window."

  (apply 'make-instance 'frame
	 :width width :height height other-initargs))

;;;---------------------------------------

(defmethod refresh :around ((f frame))

  "refresh f

Calls all the other applicable methods, then draws the border and
flushes the output queue."

  (call-next-method)
  (draw-border f)
  (flush-output))

;;;---------------------------------------

(defmethod refresh ((f frame))

  "refresh f

The primary method for a frame is just a stub."

  nil)

;;;---------------------------------------

(defmethod destroy ((obj frame))
  
  "destroy obj

Does the CLX calls to unmap the object's window w and free storage
used.  Should do other stuff too."

  (unregister obj)
  (xlib:destroy-window (window obj))
  (flush-output)
  (xlib:free-gcontext (border-gc obj)))

;;;----------------------------------------

(defmethod (setf title) :before (new-title (f frame))

  "The update function for the title attribute sets the window title
also."

  (setf (xlib:wm-name (window f)) new-title))

;;;----------------------------------------

(defmethod (setf bg-color) :around (new-color (f frame))

  (call-next-method)
  (setf (xlib:window-background (window f))
    (xlib:gcontext-foreground (color-gc new-color (colormap f))))
  (erase f)
  (refresh f))

;;;----------------------------------------

(defmethod (setf fg-color) :around (new-color (f frame))

  (declare (ignore new-color))
  (call-next-method)
  (refresh f))

;;;----------------------------------------

(defmethod (setf border-color) :around (new-color (f frame))

  "Updates border-gc and redraws the border."

  (call-next-method)
  (xlib:copy-gcontext (color-gc new-color (colormap f)) (border-gc f))
  (setf (xlib:gcontext-line-width (border-gc f)) (border-width f))
  (draw-border f)
  (flush-output))

;;;----------------------------------------

(defmethod (setf border-width) :around (new-width (f frame))

  "Updates border-gc and redraws the border."

  (call-next-method)
  (setf (xlib:gcontext-line-width (border-gc f)) new-width)
  (erase f)
  (refresh f))

;;;----------------------------------------

(defmethod process-exposure ((f frame) x y width height count)

  "The usual method for handling exposure events for any frame is to
call the refresh generic function, which calls flush-output too.  The
exposure event is also announced so application code can fill in
picture data or labels or other."

  (ev:announce f (exposure f) x y width height count)
  (refresh f)
  nil)

;;;----------------------------------------

(defmethod process-client-message ((f frame) type format data)

  (ev:announce f (wm-close f) type format data)
  nil)

;;;----------------------------------------
;;; End.
