;;;
;;; readouts
;;;
;;; not much to these - just a box displaying some text or a number
;;;
;;; 21-Apr-1992 I. Kalet created
;;; 01-May-1992 I. Kalet use erase, add a destroy method
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 29-May-1992 I. Kalet default font in frame not here
;;;  7-Jul-1992 I. Kalet make set-info a generic function so textline
;;;  can produce the announcement described in the SLIK Programmer's
;;;  Guide
;;;  8-Oct-1992 I. Kalet take out :initarg for info-x, replace defsetf
;;;  info with defmethod (setf info), replace xlib:create-gcontext with
;;;  SLIK function make-duplicate-gc
;;; 25-Oct-1992 I. Kalet eliminate pixmap and fix up refresh
;;;  3-Jan-1995 I. Kalet remove proclaim form and add setf method for
;;;  changing fg-color.
;;; 23-Apr-1999 I. Kalet changes to support multiple colormaps.
;;; 26-Nov-2000 I. Kalet explicitly make default border-style flat
;;; since for frames in general it is now raised.
;;;  2-Feb-2003 I. Kalet make setf fg-color an :after method now that
;;; the method for frames is an :around.
;;;

(in-package :slik)

;;;------------------------------------------

(defclass readout (frame)

  ((info :type string
	 :reader info ; a primary setf method is provided below...
	 :initarg :info
	 :documentation "The value stored here is always a string, but
the setf method accepts any data input and converts it to a string.")

   (label :type string
	  :accessor label
	  :initarg :label)

   (info-x :type xlib:card16
	   :accessor info-x)

   (info-y :type xlib:card16
	   :accessor info-y)

   (gc-with-font :accessor gc-with-font
		 :initform (make-duplicate-gc)
		 :documentation "A cached graphic context for drawing
in the font for this readout instead of the default font.  Much faster
than using the with-gcontext macro.")

   )

  (:default-initargs :title "SLIK Readout" :info "" :label ""
		     :border-style :flat)

  (:documentation "A readout is a passive box that displays whatever
data is written to it.  By default the text is vertically centered and
starts 10 pixels in from the left.")
  )

;;;----------------------------------------

(defmethod initialize-instance :after ((r readout) &rest initargs)

  "Much setup done here so it can also be used by the textline class."

  (declare (ignore initargs))
  (let* ((w (width r))
	 (h (height r))
	 (f (font r))
	 (font-descent (xlib:max-char-descent f))
	 (info-width (xlib:text-width f (info r)))
	 (label-width (xlib:text-width f (label r))))
    (setf (info-x r) (if (= info-width 0) (+ label-width 10)
		       (+ (round (/ (- w info-width label-width) 2))
			  label-width))
	  (info-y r) (- h (round (/ (- h (font-height f)) 2)) font-descent))
    (xlib:copy-gcontext (color-gc (fg-color r) (colormap r))
		       (gc-with-font r))
    (setf (xlib:gcontext-font (gc-with-font r)) f)
    (when (> label-width 0)
      (xlib:draw-glyphs (window r) (gc-with-font r) ; draw the label
		       (- (info-x r) label-width) (info-y r)
		       (label r)))))

;;;--------------------------------------

(defun update-info (r)

  "Erase and rewrite only the info region, leave the label."

  (let* ((start-x (info-x r))
	 (erase-width (- (width r) start-x))
	 (w (window r)))
    (xlib:draw-rectangle w (color-gc (bg-color r) (colormap r))
			start-x 0 erase-width (height r) t)
    (xlib:draw-glyphs w (gc-with-font r)
		     start-x (info-y r) (info r))
    (draw-border r)
    (flush-output)))

;;;--------------------------------------

(defmethod (setf fg-color) :after (new-col (r readout))

  (xlib:copy-gcontext (color-gc new-col (colormap r))
		     (gc-with-font r))
  (setf (xlib:gcontext-font (gc-with-font r)) (font r)))

;;;--------------------------------------

(defmethod refresh :after ((r readout))

  "Draw the label and the info."

  (let* ((lab (label r))
	 (lw (xlib:text-width (font r) lab))
	 (ix (info-x r))
	 (iy (info-y r))
	 (w (window r))
	 (gc (gc-with-font r)))
    (if (> lw 0) ;; draw the label
	(xlib:draw-glyphs w gc (- ix lw) iy (label r)))
    (xlib:draw-glyphs w gc ix iy (info r))))

;;;----------------------------------------

(defun make-readout (width height &rest other-initargs)

  "make-readout width height &rest other-initargs

Returns a readout with the specified parameters.  If the info
parameter is provided it is centered as well as possible."

  (let ((r (apply 'make-instance 'readout
		  :width width :height height other-initargs)))
    (refresh r)
    r))

;;;--------------------------------------

(defmethod (setf info) (new-info (r readout))

  "This setf method takes any input and creates a string that is the
LISP printed representation of the input, and stores that string."

  (setf (slot-value r 'info) (format nil "~A" new-info))
  (update-info r))

;;;--------------------------------------

(defmethod destroy :before ((r readout))

  (xlib:free-gcontext (gc-with-font r)))

;;;--------------------------------------
;;; End.
