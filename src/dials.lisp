;;;
;;; dials
;;;
;;; A dial is a widget for setting/adjusting an angular value
;;;
;;; 07-Apr-1992 I. Kalet written
;;; 14-Apr-1992 I. Kalet clean up and add X event processing
;;; 01-May-1992 I. Kalet take out optional force-output-p in draw-pointer
;;; 06-May-1992 I. Kalet don't export radius - can't be changed
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 27-May-1992 I. Kalet fix up type declarations
;;;  6-Jul-1992 I. Kalet change be: to ev: and behavior to event
;;;  6-Oct-1992 I. Kalet change defsetf angle to defmethod (setf
;;;  angle)
;;; 25-Oct-1992 I. Kalet change refresh and drawing - no more pixmap
;;; 29-Dec-1992 I. Kalet change angle attribute to degrees, not
;;; radians
;;; 22-Mar-1993 I. Kalet delete type declaration of variable index -
;;; no such variable exists.
;;;  3-Jan-1995 I. Kalet remove proclaim form and a few style changes
;;;  3-Sep-1995 I. Kalet enforce range of 0 to 360 and single-float
;;;  3-Apr-1999 C. Wilcox enabled event look-ahead for :motion-notify
;;; 23-Apr-1999 I. Kalet changes for multiple colormaps.
;;;

(in-package :slik)

(defconstant *rad-to-deg* (coerce (/ 180.0 pi) 'single-float))
(defconstant *deg-to-rad* (coerce (/ pi 180.0) 'single-float))
(defconstant *pi-over-2* (coerce (/ pi 2.0) 'single-float))
(defconstant *two-pi* (coerce (* 2.0 pi) 'single-float))
(defconstant *5-pi-over-2* (coerce (* 5.0 *pi-over-2*) 'single-float))

;;;----------------------------------------

(defclass dial (frame)

  ((angle :type single-float
	  :reader angle ;; setf method provided below
	  :initarg :angle
	  :documentation "The pointer angle in degrees")

   (radius :type xlib:card16
	   :reader radius
	   :initarg :radius
	   :documentation "The radius of the dial circle in pixels")

   (pointer :type vector
	    :accessor pointer
	    :initform (make-sequence '(vector xlib:card16)
				     12 :initial-element 0)
	    :documentation "The polygon describing the pointer, in pixel
coords.")

   (button-down :accessor button-down
		:initform nil
		:documentation "True if a mouse button is down while
the window pointer is inside the dial window.")

   (value-changed :type ev:event
		  :accessor value-changed
		  :initform (ev:make-event)
		  :documentation "Other objects interested in being
notified when the dial's value has changed should call add-notify
for this event.")

   )

  (:default-initargs :title "SLIK dial" :angle 0.0 :radius 50
		     :width 120 :height 120)

  (:documentation "A dial as currently configured is meant to display
and manipulate angular values.  angle is the angle the needle should
point in.  Actual value is straight up for 0.0, increasing clockwise,
but the computations are done in the standard mathematical coordinate
system, with zero degrees pointing to the right, and increasing
counter-clockwise.")
  )

;;;---------------------------------------------

(defun dial-erase-pointer (d)

  "dial-erase-pointer d

Erase dial pointer from window."

  (xlib:draw-lines (window d)
		  (color-gc (bg-color d) (colormap d))
		  (pointer d)
		  :relative-p nil :fill-p t :shape :convex))

;;;---------------------------------------------

(defun dial-draw-pointer (d)

  "dial-draw-pointer d

Computes new pointer polygon, draws it in window."

  (let* (;; convert angle to radians first
	 (angle (- *pi-over-2* (* *deg-to-rad* (angle d))))
	 (r (float (radius d)))
	 (wp (+ (/ r 30.0) 2.0)) ;; pointer half-width
	 (sin-a (sin angle))
	 (cos-a (cos angle))
	 (base-x (round (* wp sin-a)))
	 (base-y (round (* wp cos-a)))
	 (xlen (round (* (- r 2.0) cos-a)))
	 (ylen (round (* (- r 2.0) sin-a)))
	 (center (/ (width d) 2))
	 (point-x (+ xlen center))
	 (point-y (- center ylen))
	 (shaft-top-x (+ center (round (* 0.80 xlen))))
	 (shaft-top-y (- center (round (* 0.80 ylen))))
	 (pointer-vector (pointer d)))
    (declare
     (type single-float angle wp sin-a cos-a r)
     (type xlib:card16 center base-x base-y xlen ylen point-x point-y
	   shaft-top-x shaft-top-y)
     (type array pointer-vector))
    (setf
	;; shaft base left
	(aref pointer-vector 0) (- center base-x)
	(aref pointer-vector 1) (- center base-y)
	;; shaft top left
	(aref pointer-vector 2) (- shaft-top-x base-x)
	(aref pointer-vector 3) (- shaft-top-y base-y)
	;; arrow tip
	(aref pointer-vector 4) point-x
	(aref pointer-vector 5) point-y
	;; shaft top right
	(aref pointer-vector 6) (+ shaft-top-x base-x)
	(aref pointer-vector 7) (+ shaft-top-y base-y)
	;; shaft base right
	(aref pointer-vector 8) (+ center base-x)
	(aref pointer-vector 9) (+ center base-y)
	;; shaft base left - again
	(aref pointer-vector 10) (aref pointer-vector 0)
	(aref pointer-vector 11) (aref pointer-vector 1))
    ;; draw arrow
    (xlib:draw-lines (window d)
		    (color-gc (fg-color d) (colormap d))
		    (pointer d)
		    :relative-p nil :fill-p t :shape :convex)
    (flush-output)))

;;;---------------------------------------

(defmethod refresh :before ((d dial))

  "Fills in the circle and the pointer."

  (let ((w (width d)))
    (xlib:draw-arc (window d)
		  (color-gc (border-color d) (colormap d))
		  0 0 w w 0.0 *two-pi*)
    (dial-draw-pointer d)))

;;;---------------------------------------

(defun make-dial (radius &rest other-initargs)

  "make-dial radius &rest other-initargs

Makes a dial with the specified radius and other parameters, or
default values."

  (let* ((w (* 2 (+ radius 5)))
	 (d (apply 'make-instance 'dial :radius radius
		   :width w :height w ;; dials are square
		   other-initargs))) ;; supplied width and height are ignored
    (push :motion-notify (look-ahead d))
    (refresh d)
    d))

;;;---------------------------------------

(defmethod (setf angle) (new-angle (d dial))

  "This is always used by outsiders or X event handlers to set a new
angle value.  New-angle is in degrees."

  (setq new-angle (mod (coerce new-angle 'single-float) 360.0))
  (setf (slot-value d 'angle) new-angle)
  (dial-erase-pointer d) ;; uses cached polygon
  (dial-draw-pointer d)
  (ev:announce d (value-changed d) new-angle)
  new-angle)

;;;---------------------------------------

(defun dial-pointer-angle (d x y)

  "dial-pointer-angle d x y

Computes the angle in radians corresponding to the endpoint x,y and
the dial center, then converts to degrees and returns that."

  (let* ((c (/ (width d) 2))
	 (dx (- x c))
	 (dy (- y c))
	 (len (sqrt (+ (* dx dx) (* dy dy)))))
    (* *rad-to-deg*
       (if (zerop len) 0.0
	 (let ((raw (coerce (acos (/ (float dx) len)) 'single-float)))
	   (if (<= dy 0)
	       (if (<= dx 0)
		   (- *5-pi-over-2* raw)
		 (- *pi-over-2* raw))
	     (+ *pi-over-2* raw)))))))

;;;---------------------------------------

(defmethod process-motion-notify ((d dial) x y state)

  (declare (ignore state))
  (when (button-down d)
    (setf (angle d) (dial-pointer-angle d x y)))
  nil)

;;;---------------------------------------

(defmethod process-button-press ((d dial) code x y)

  (when (= code 1) ;; left button
    (setf (button-down d) t)
    (setf (angle d) (dial-pointer-angle d x y)))
  nil)

;;;---------------------------------------

(defmethod process-button-release ((d dial) code x y)

  (declare (ignore x y))
  (when (= code 1) ;; left button
    (setf (button-down d) nil))
  nil)

;;;----------------------------------------
;;; End.
