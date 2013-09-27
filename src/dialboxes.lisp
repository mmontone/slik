;;;
;;; dialboxes
;;;
;;; The dialbox is a combination of a dial and a textline constrained
;;; so that the textline displays the value set on the dial.
;;;
;;; 12-May-1992 I. Kalet created
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 27-May-1992 I. Kalet fix up type declarations
;;;  6-Jul-1992 I. Kalet change behavior to event and be: to ev:
;;;  9-Jul-1992 I. Kalet fix dial-update so blank string sets 0
;;;  degrees
;;;  8-Oct-1992 I. Kalet add destroy :before method, add missing
;;;  (setf angle) method
;;; 28-Oct-1992 I. Kalet eliminate pixmap
;;; 29-Nov-1992 I. Kalet fix minor errors - better positioning of
;;; title text, provide refresh method so text is redrawn.
;;; 29-Dec-1992 I. Kalet change angle to degrees instead of radians
;;; 12-Feb-1993 I. Kalet squeeze and parametrize margins.
;;; 13-May-1994 I. Kalet use error checking in textline
;;;  3-Jan-1995 I. Kalet remove proclaim form
;;;  3-Sep-1995 I. Kalet move announce to correct place.  Eliminate
;;;  the busy flag - not needed since textlines don't announce when
;;;  their infos are set, only on RETURN.
;;;

(in-package :slik)

;;;-----------------------------------------

(defclass dialbox (frame)

  ((the-dial :type dial
	     :accessor the-dial)

   (the-text :type textline
	     :accessor the-text)

   (title-x :type xlib:card16
	    :accessor title-x) ; computed and cached

   (title-y :type xlib:card16
	    :accessor title-y) ; computed and cached

   (value-changed :type ev:event
		  :accessor value-changed
		  :initform (ev:make-event))

   )

  (:default-initargs :title "")

  (:documentation "A dialbox contains a dial and a textline, and
constrains the textline to display the value on the dial, and set the
dial pointer at the value of the textline.")

  )

;;;-----------------------------------------

(defmethod angle ((db dialbox))

  "Returns the angle of the dial in the dialbox"

  (angle (the-dial db))
  )

;;;-----------------------------------------

(defmethod (setf angle) (new-angle (db dialbox))

  "Sets the angle of the dial in the dialbox"

  (setf (angle (the-dial db)) new-angle)
  )

;;;-----------------------------------------

(defmethod refresh ((db dialbox))

  "draws the title text."

  (xlib:draw-glyphs (window db) (gc-with-font (the-text db))
		   (title-x db) (title-y db) (title db))
  )

;;;-----------------------------------------

(defun make-dialbox (radius &rest other-initargs
			    &key (font *default-font*)
			    &allow-other-keys)

  "MAKE-DIALBOX radius &rest other-initargs

returns a dialbox with a dial of the specified radius, a textline with
the size needed for angle values in the specified or default font, and
with all the other specified parameters, e.g., foreground and
background colors, etc."

  (let* ((dx 5) ; margin sizes
	 (dy 5)
	 (ds (* 2 (+ radius 5))) ; dial size - dependent on dial specs
	 (width (+ ds (* 2 dx)))
	 (th (+ (font-height font) 10)) ; this 10 is arbitrary
	 (height (+ (* 2 dy) ds (* 2 th))) ; top, bottom, dial, title
					   ; and textline
	 (db (apply #'make-instance 'dialbox
		    :width width :height height other-initargs))
	 )
    (setf (the-dial db)
	  (apply #'make-dial radius :parent (window db)
		 :ulc-x dx :ulc-y dy other-initargs)
	  (the-text db)
	  (apply #'make-textline ds th :info "  0.0"
		 :parent (window db)
		 :ulc-x dx
		 :ulc-y (+ dy ds th)
		 :numeric t :lower-limit 0.0 :upper-limit 360.0
		 other-initargs)
	  (title-x db) (round (/ (- width
				    (xlib:text-width font (title db)))
				 2))
	  (title-y db) (+ dy ds th -8)) ; arbitrary - needs work
    (refresh db)

    ;; following is needed so angle can be provided as initpar
    (setf (info (the-text db)) (format nil "~5,1F" (angle db)))

    ;; when the dial changes, the text updates and the outer event is
    ;; announced
    (ev:add-notify db (value-changed (the-dial db))
		   #'(lambda (box d val)
		       (declare (ignore d))
		       (setf (info (the-text box))
			 (format nil "~5,1F" val))
		       (ev:announce box (value-changed box) val)))
    ;; when the user presses RETURN, setting the angle also causes the
    ;; previous action, since the dial announces value-changed.  No
    ;; circularity here since setting the textline does not trigger an
    ;; event, only pressing RETURN does.
    (ev:add-notify db (new-info (the-text db))
		   #'(lambda (box tl info)
		       (declare (ignore tl))
		       (setf (angle box)
			 (read-from-string info nil 0.0)))) ;; blank = 0
    db)
  )

;;;-----------------------------------------

(defmethod destroy :before ((db dialbox))

  "Destroys the dial and the textline first."

  (destroy (the-dial db))
  (destroy (the-text db))
  )

;;;-----------------------------------------
