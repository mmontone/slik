;;;
;;; buttons
;;;
;;; This file defines the various types of buttons in SLIK
;;;
;;; 13-Apr-1992 I. Kalet started
;;; 27-Apr-1992 I. Kalet add Exit button
;;; 29-May-1992 I. Kalet font defaults in frame class
;;;  9-Jun-1992 I. Kalet take out process-enter-notify and
;;;  process-leave-notify - they did not behave as expected.
;;;  6-Jul-1992 I. Kalet add :justify parameter to make-button, change
;;;  behavior to event and be: to ev:
;;;  8-Oct-1992 I. Kalet change defsetf to defmethod (setf ... also
;;;  add (setf label)
;;; 25-Oct-1992 I. Kalet eliminate pixmap
;;; 29-Nov-1992 I. Kalet make default border color white not gray
;;; 27-Feb-1993 I. Kalet reposition label when label or font is
;;; changed
;;;  3-Aug-1993 I. Kalet provide button-2-on event for middle mouse
;;;  button press.  Used in scrolling list code.  Not yet for export.
;;; 26-May-1994 I. Kalet implement the active attribute, also the
;;; confirm attribute for exit buttons.
;;;  5-Jun-1994 I. Kalet modify process-button-press for exit button,
;;;  to check for on, so that accidental exit does not occur
;;;  3-Jan-1995 I. Kalet move exit button stuff to dialogboxes to
;;;  remove circular module dependency, also remove proclaim form.
;;;  7-Jun-1997 I. Kalet add icon-button, which provides a filled or
;;;  outline contour in the foreground color, also make-arrow-button,
;;;  which returns an icon-button with a filled arrow polygon.
;;; 23-Apr-1999 I. Kalet changes to support multiple colormaps
;;; 30-May-2000 I. Kalet add support for 3-d border style.
;;; 13-Mar-2001 I. Kalet allow button-2 active even if button-1 not.
;;;

(in-package :slik)

;;;------------------------------------------------

(defclass button (frame)

  ((active :accessor active
	   :initarg :active
	   :documentation "True if the button responds to X events,
otherwise nil.")

   (allow-button-2 :accessor allow-button-2
		   :initarg :allow-button-2
		   :documentation "Normally button-2 is disabled if
active is nil, but this being non-nil overrides that.")

   (on :accessor on
       :initarg :on
       :documentation "A flag that holds the state of the button.")

   (button-type :type (member :momentary :hold)
		:reader button-type
		:initarg :button-type)

   (label :type string
	  :accessor label
	  :initarg :label)

   (justify :type (member :left :center :right)
	    :accessor justify
	    :initarg :justify)

   (label-x :type xlib:card16
	    :accessor label-x)

   (label-y :type xlib:card16
	    :accessor label-y)

   (button-on :type ev:event
	      :accessor button-on
	      :initform (ev:make-event))

   (button-off :type ev:event
	       :accessor button-off
	       :initform (ev:make-event))

   (button-2-on :type ev:event
		:accessor button-2-on
		:initform (ev:make-event))
   )

  (:default-initargs :title "SLIK button"
    :active t :allow-button-2 nil :on nil 
    :label "" :justify :center :button-type :hold)

  (:documentation "A button is a frame which can be clicked on or off.
It has a color that changes when it is on, and might have a text label.")

  )

;;;------------------------------------------------

(defun make-button (width height &rest other-initargs)

  "make-button width height &rest other-initargs

Returns a button with the specified parameters.  If a label is
provided it is positioned accordingly."

  (apply 'make-instance 'button
	 :width width :height height other-initargs))

;;;------------------------------------------------

(defmethod refresh :before ((b button))

  (let* ((text (label b))
	 (win (window b))
	 (gc-fg (color-gc (fg-color b) (colormap b)))
	 (gc-bg (color-gc (bg-color b) (colormap b)))
	 (flood (if (and (eql (border-style b) :flat) (on b))
		    gc-fg gc-bg))
	 (text-col (if (and (eql (border-style b) :flat) (on b))
		       gc-bg gc-fg)))
    ;; first color the button
    (xlib:draw-rectangle win flood 0 0 (width b) (height b) t)
    ;; then add label if there is one
    (unless (equal text "")
      (xlib:with-gcontext (text-col :font (font b))
	(xlib:draw-glyphs win text-col (label-x b) (label-y b) text)))))

;;;------------------------------------------------

(defun set-label-xy (b)

  "set-label-xy b

updates the label-x and label-y attributes according to the current
contents of the other button attributes."

  (let* ((w (width b))
	 (h (height b))
	 (f (font b))
	 (font-descent (xlib:max-char-descent f))
	 (label-width (xlib:text-width f (label b)))
	 )
    (setf (label-x b) (case (justify b)
			    (:left 5)
			    (:center (round (/ (- w label-width) 2)))
			    (:right (- w label-width)))
	  (label-y b) (- h (round (/ (- h (font-height f)) 2))
			 font-descent))))

;;;------------------------------------------------

(defmethod initialize-instance :after ((b button) &rest initargs)

  "Used also by exit-button."

  (declare (ignore initargs))
  (set-label-xy b)
  (refresh b))

;;;------------------------------------------------

(defmethod (setf on) :after (turned-on (b button))

  "Used to change the on-off state of the button.  The turned-on
parameter is t or nil."

  (unless (eql (border-style b) :flat)
    (setf (border-style b) (if turned-on :lowered :raised)))
  (refresh b)
  (ev:announce b (if (on b) (button-on b) (button-off b))))

;;;------------------------------------------------

(defmethod (setf label) :after (new-label (b button))

  (declare (ignore new-label))
  (set-label-xy b)
  (refresh b))

;;;------------------------------------------------

(defmethod (setf font) :after (new-font (b button))

  (declare (ignore new-font))
  (set-label-xy b)
  (refresh b))

;;;------------------------------------------------

(defmethod process-button-press ((b button) code x y)

  (declare (ignore x y))
  (case code 
    (1 (when (active b)			; left mouse button
	 (if (eql (button-type b) :hold) ; i.e., click on, click off
	     (setf (on b) (not (on b)))	; if off, turn on and vice versa
	   (setf (on b) t))))		; for momentary, just turn on
    (2 (when (or (active b) (allow-button-2 b)) ; middle mouse button
	 (ev:announce b (button-2-on b))))) ; just announce
  nil)

;;;------------------------------------------------

(defmethod process-button-release ((b button) code x y)

  (declare (ignore x y))

  (when (and (active b)
	     (= code 1) ;; left button
	     (eql (button-type b) :momentary)) ;; release turns it off
    (setf (on b) nil)) ;; but for :hold type, do nothing
  nil)

;;;------------------------------------------------

(defclass icon-button (button)

  ((icon :type list
	 :accessor icon
	 :initarg :icon
	 :documentation "The pixel coordinates of the icon outline, in
a form suitable for input to xlib:draw-lines, i.e., a simple list of
alternating x and y values for the vertices.")

   (filled :accessor filled
	   :initarg :filled
	   :documentation "A boolean, specifies whether to fill the
icon.")

   )

  (:default-initargs :button-type :momentary :icon nil :filled t)

  (:documentation "An icon button has a polygon drawn on it, like an
arrow shape, in the foreground color, usually instead of text, but if
not filled, could be in combination with some text.")

  )

;;;------------------------------------------------

(defmethod refresh :after ((b icon-button))

  "Just adds the polygon."

  (xlib:draw-lines (window b)
		  (color-gc (if (on b) (bg-color b) (fg-color b))
			    (colormap b))
		  (icon b)
		  :fill-p (filled b)))

;;;------------------------------------------------

(defun make-icon-button (width height icon &rest initargs)

  "make-icon-button width height icon &rest initargs

Returns an icon button with the specified parameters."

  (apply 'make-instance 'icon-button
	 :width width :height height
	 :icon icon
	 initargs))

;;;------------------------------------------------

(defun make-arrow-button (width height direction &rest initargs)

  "make-arrow-button width height direction &rest initargs

Returns an arrow button in the specified direction, one of the
keywords, :left :right :up or :down."

  (apply #'make-icon-button width height
	 (let* ((hx (round (/ width 2)))
		(hy (round (/ height 2)))
		(x13 (round (/ width 3)))
		(x23 (* 2 x13))
		(y13 (round (/ height 3)))
		(y23 (* 2 y13)))
	   (case direction ;; pass in the correct arrow polygon
	     (:left (list 0 hy hx 0 hx y13 width y13
			  width y23 hx y23 hx height 0 hy))
	     (:right (list width hy hx 0 hx y13 0 y13
			   0 y23 hx y23 hx height width hy))
	     (:up (list hx 0 width hy x23 hy x23 height
			x13 height x13 hy 0 hy hx 0))
	     (:down (list hx height width hy x23 hy x23 0
			  x13 0 x13 hy 0 hy hx height))))
	 initargs))

;;;------------------------------------------------
;;; End.
