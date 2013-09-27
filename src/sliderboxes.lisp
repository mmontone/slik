;;;
;;; sliderboxes
;;;
;;; A sliderbox has a slider and a textline in it like a dialbox.
;;;
;;; 13-May-1992 I. Kalet created
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 27-May-1992 I. Kalet fix up type declarations
;;;  6-Jul-1992 I. Kalet change be: to ev: and behavior to event
;;; 29-Nov-1992 I. Kalet finish
;;; 12-Feb-1993 I. Kalet squeeze and parametrize margins
;;; 13-May-1994 I. Kalet add range checking with new textline code
;;;  3-Jan-1995 I. Kalet remove proclaim form, take range checking out
;;;  of slider-update.
;;;  3-Sep-1995 I. Kalet rearrange announce, etc. since textlines
;;;  don't announce when info is set - no need for busy flag.  Also,
;;;  move most initialization to initialize-instance method so
;;;  subclasses don't duplicate it.  This requires caching some
;;;  initialization parameters as local attributes.
;;;  4-May-1997 I. Kalet don't overload the title attribute - add the
;;;  label attribute to be used in the textline.  Don't use it in the
;;;  limit textlines in the adjustable sliderbox.
;;; 15-Mar-1999 I. Kalet add display-limits attribute, default to t,
;;; so backward compatible.  Also, explicitly set slider border width,
;;; so can make a sliderbox without border, but slider will still look ok.
;;; 11-Mar-2001 I. Kalet explicitly set textline border style - it
;;; does not default correctly.
;;; 16-Aug-2002 J. Sager add label-slider-box class
;;; 20-Sep-2002 I. Kalet a little cosmetic cleanup
;;;

(in-package :slik)

;;;---------------------------------------------

(defparameter *sx* 5 "Sliderbox left and right margins in pixels")

(defparameter *sy* 5 "Sliderbox top margin in pixels")

;;;---------------------------------------------

(defclass sliderbox (frame)

  ((sl-width :type xlib:card16
	     :initarg :sl-width
	     :accessor sl-width
	     :documentation "Slider width - initialization parameter
captured in call to make-instance.")

   (sl-height :type xlib:card16
	      :initarg :sl-height
	      :accessor sl-height
	      :documentation "Slider height - initialization parameter
captured in call to make-instance.")

   (sl-min :type single-float
	   :initarg :sl-min
	   :accessor sl-min
	   :documentation "Minimum value allowed - initialization
parameter captured in call to make-instance.")

   (sl-max :type single-float
	   :initarg :sl-max
	   :accessor sl-max
	   :documentation "Maximum value allowed - initialization
parameter captured in call to make-instance.")

   (display-limits :type (member t nil)
		   :initarg :display-limits
		   :accessor display-limits
		   :documentation "Flag to indicate whether to show
the upper and lower limits, sl-max and sl-min.")

   (digits :type single-float
	   :initarg :digits
	   :accessor digits
	   :documentation "The widest number that will appear in the
textline, for sizing the textline - initialization parameter captured
in call to make-instance.")

   (the-slider :type slider
	       :accessor the-slider)

   (the-text :type textline
	     :accessor the-text)

   (label :type string
	  :initarg :label
	  :accessor label
	  :documentation "The label that appears in the textline.")

   (min-x :type xlib:card16
	  :accessor min-x
	  :documentation "Specifies location of text showing minimum
value in slider - computed and cached")

   (min-y :type xlib:card16
	  :accessor min-y
	  :documentation "See min-x")

   (max-x :type xlib:card16
	  :accessor max-x
	  :documentation "Specifies location of text showing maximum
value in slider - computed and cached")

   (max-y :type xlib:card16
	  :accessor max-y
	  :documentation "See max-x")

   (value-changed :type ev:event
		  :accessor value-changed
		  :initform (ev:make-event)
		  :documentation "This event is for the sliderbox
as a whole, not the individual components.")

   )

  (:default-initargs :label "" :display-limits t)

  (:documentation "A sliderbox contains a slider and a textline,
constrained so the textline displays the value on the slider, and the
slider is set to the value typed in on the textline.")

  )

;;;------------------------------------------

(defmethod minimum ((sb sliderbox))

  (minimum (the-slider sb)))

;;;------------------------------------------

(defmethod maximum ((sb sliderbox))

  (maximum (the-slider sb)))

;;;------------------------------------------

(defmethod refresh ((sb sliderbox))

  "Draws the min and max labels if required."

  (when (display-limits sb)
    (xlib:draw-glyphs (window sb) (gc-with-font (the-text sb))
		     (min-x sb) (min-y sb)
		     (format nil "~A" (minimum sb)))
    (xlib:draw-glyphs (window sb) (gc-with-font (the-text sb))
		     (max-x sb) (max-y sb)
		     (format nil "~A" (maximum sb)))))

;;;------------------------------------------

(defmethod setting ((sb sliderbox))

  "Returns the current setting of the slider in the sliderbox."

  (setting (the-slider sb)))

;;;------------------------------------------

(defmethod (setf setting) (new-setting (sb sliderbox))

  "Sets the setting of the slider in the sliderbox."

  (setf (setting (the-slider sb)) new-setting))

;;;------------------------------------------

(defun make-sliderbox (sl-width sl-height min max digits
				&rest other-initargs
				&key (font *default-font*)
				&allow-other-keys)

  "make-sliderbox sl-width sl-height min max digits
                  &rest other-initargs

Returns an instance of a sliderbox with the specified parameters.  The
digits parameter is a number that is used to determine how big to make
the textline, to accomodate the setting values to whatever significant
digits are needed by the application."

  (apply #'make-instance 'sliderbox
	 :sl-width sl-width :sl-height sl-height
	 :sl-min min :sl-max max :digits digits
	 :width (+ sl-width (* 2 *sx*))
	 ;; allow 5 pixels above and below textline, and same inside
	 ;; textline above and below the text, for total of 20
	 :height (+ *sy* sl-height (font-height font) 20)
	 other-initargs))

;;;------------------------------------------

(defmethod initialize-instance :after ((sb sliderbox)
				       &rest other-initargs)

  (let* ((sl-height (sl-height sb))
	 (sl-width (sl-width sb))
	 (min (sl-min sb))
	 (max (sl-max sb))
	 (digits (digits sb))
	 (width (width sb))
	 (font (font sb))
	 (fh (font-height font))
	 (th (+ fh 10)) ;; textline height
	 (tw (+ (xlib:text-width font (format nil "~A" digits))
		(xlib:text-width font (label sb))
		20)) ;; 10 pixels margin on each side
	 (win (window sb)))

    (setf (the-slider sb) (apply #'make-slider
				 sl-width sl-height min max
				 :parent win
				 :ulc-x *sx* :ulc-y *sy*
				 :border-width 1
				 other-initargs)
	  (the-text sb) (apply #'make-textline tw th
			       :parent win
			       :ulc-x (round (/ (- width tw) 2))
			       :ulc-y (+ *sy* sl-height 5)
			       :border-width 1
			       :border-style
			       (if (eql *default-border-style* :flat)
				   :flat :lowered)
			       :numeric t :lower-limit min :upper-limit max
			       other-initargs) ;; includes label
	  (min-x sb) *sx*
	  (min-y sb) (+ (* 2 *sy*) sl-height fh)
	  (max-x sb) (- width *sx*
			(if (typep sb 'label-sliderbox)
			    (xlib:text-width font (format nil "~A" 
							 (max-label sb)))
			  (xlib:text-width font (format nil "~A" max))))
	  (max-y sb) (min-y sb))
    (refresh sb)
    (setf (info (the-text sb)) (setting sb))
    (ev:add-notify sb (value-changed (the-slider sb))
		   #'(lambda (box sl val)
		       (declare (ignore sl))
		       (setf (info (the-text box)) val)
		       (ev:announce box (value-changed box) val)))
    (ev:add-notify sb (new-info (the-text sb))
		   #'(lambda (box tl info)
		       (declare (ignore tl))
		       (setf (setting box) (read-from-string info))))))

;;;------------------------------------------

(defmethod destroy :before ((sb sliderbox))

  "Destroys the slider and the textline first."

  (destroy (the-slider sb))
  (destroy (the-text sb)))

;;;---------------------------------------------

(defclass label-sliderbox (sliderbox)

  ((min-label :type string
	      :initarg :min-label
	      :reader min-label
	      :documentation "The label that appears under the minimum. 
Not resetable.")
   (max-label :type string
	      :initarg :max-label
	      :reader max-label
	      :documentation "The label that appears under the maximum. 
Not resettable.")

  )

 (:default-initargs :min-label "" :max-label "")

  (:documentation "A label-sliderbox is just like a sliderbox, but it
  displays a min-label and a max-label. Display limits is disabled.")

)

;;;---------------------------------------------

(defun make-label-sliderbox (sl-width sl-height min max digits
				      &rest other-initargs
				      &key (font *default-font*)
				      &allow-other-keys)

  "make-label-sliderbox sl-width sl-height min max digits min-digits
                  max-digits &rest other-initargs

Returns an instance of a label-sliderbox with the specified parameters.  
The digits, min-digits, and max-digits parameters is a number that is 
used to determine how big to make the textline and labels, to accomodate the 
setting values to whatever significant digits are needed by the application."

 (apply #'make-instance 'label-sliderbox
	 :sl-width sl-width :sl-height sl-height
	 :sl-min min :sl-max max :digits digits
	 :width (+ sl-width (* 2 *sx*))
	 ;; allow 5 pixels above and below textline, and same inside
	 ;; textline above and below the text, for total of 20
	 :height (+ *sy* sl-height (font-height font) 20)
	 other-initargs))

;;;------------------------------------------

(defmethod refresh ((sb label-sliderbox))

  "Draws the min and max labels always."

  (xlib:draw-glyphs (window sb) (gc-with-font (the-text sb))
		   (min-x sb) (min-y sb)
		   (format nil "~A" (min-label sb)))
  (xlib:draw-glyphs (window sb) (gc-with-font (the-text sb))
		   (max-x sb) (max-y sb)
		   (format nil "~A" (max-label sb))))

;;;------------------------------------------
;;; End.
