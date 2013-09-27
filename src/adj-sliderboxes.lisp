;;;
;;; adj-sliderboxes
;;;
;;; An adjustable sliderbox is a sliderbox in which you can edit the
;;; minimum and maximum values.
;;;
;;; 20-Apr-1994 J. Unger created
;;; 23-Sep-1994 J. Unger make textlines numeric.
;;;  3-Jan-1995 I. Kalet remove proclaim form.
;;;  8-Sep-1995 I. Kalet make consistent with fixes in sliderboxes,
;;;  including use of initialize-instance, finally.
;;;  4-May-1997 I. Kalet don't use the label (formerly title) to
;;;  determine the width of the limit textlines.
;;;  3-Nov-1998 I. Kalet track changes in sliders module.
;;; 11-Mar-2001 I. Kalet explicitly set border style in textlines -
;;; does not default correctly.
;;;

(in-package :slik)

;;;---------------------------------------------

(defclass adjustable-sliderbox (sliderbox)

  ((the-minimum :type textline
		:accessor the-minimum
		:documentation "The minimum value textline in the 
lower left corner of the sliderbox.")

   (the-maximum :type textline
		:accessor the-maximum
		:documentation "The maximum value textline in the 
lower right corner of the sliderbox.")
   
   (smallest-range :type single-float
		   :accessor smallest-range
		   :initform 1.0
		   :initarg :smallest-range
		   :documentation "The smallest distance between the
minimum and maximum values of the sliderbox.")

   (minimum-changed :type ev:event
                    :accessor minimum-changed
                    :initform (ev:make-event)
                    :documentation "Announced when the minimum value
of the sliderbox changes.")

   (maximum-changed :type ev:event
                    :accessor maximum-changed
                    :initform (ev:make-event)
                    :documentation "Announced when the maximum value
of the sliderbox changes.")

   )

  (:documentation "A sliderbox with editable min and max values.")
  )

;;;---------------------------------------------

(defmethod (setf minimum) (new-min (asb adjustable-sliderbox))

  "Sets the minimum value of the sliderbox and announces minimum-changed."

  (erase-knob (the-slider asb))
  (setf (minimum (the-slider asb))
    (min new-min (- (maximum asb) (smallest-range asb))))
  (unless (= new-min (minimum (the-slider asb)))
    (setf (info (the-minimum asb)) (minimum asb)))
  (scale-knob (the-slider asb))
  (refresh (the-slider asb))
  (ev:announce asb (minimum-changed asb) (minimum (the-slider asb))))

;;;---------------------------------------------

(defmethod (setf maximum) (new-max (asb adjustable-sliderbox))

  "Sets the maximum value of the sliderbox and announces maximum-changed."

  (erase-knob (the-slider asb))
  (setf (maximum (the-slider asb)) 
    (max new-max (+ (minimum asb) (smallest-range asb))))
  (unless (= new-max (maximum (the-slider asb)))
    (setf (info (the-maximum asb)) (maximum asb)))
  (scale-knob (the-slider asb))
  (refresh (the-slider asb))
  (ev:announce asb (maximum-changed asb) (maximum (the-slider asb))))

;;;---------------------------------------------

(defmethod refresh ((asb adjustable-sliderbox))

  "Supercedes the sliderbox refresh method, since everything here
refreshes itself."

  nil)

;;;---------------------------------------------

(defun make-adjustable-sliderbox (sl-width sl-height min max digits
                                  &rest other-initargs 
                                  &key (font *default-font*)
                                  &allow-other-keys)

  "make-adjustable-sliderbox sl-width sl-height min max digits
                             &rest other-initargs
                             &key (font *default-font*)
                             &allow-other-keys

Returns an instance of an adjustable sliderbox with the specified
parameters.  The digits parameter is a number that is used to
determine how big to make the textline, to accomodate the setting
values to whatever significant digits are needed by the application."

  (apply #'make-instance 'adjustable-sliderbox
	 :sl-width sl-width :sl-height sl-height
	 :sl-min min :sl-max max :digits digits
	 :width (+ sl-width (* 2 *sx*)) ;; *sx* in sliderboxes module.
	 ;; allow 5 pixels above and below textline, and same inside
	 ;; textline above and below the text, for total of 20
	 :height (+ *sy* sl-height (font-height font) 20)
	 other-initargs))

;;;---------------------------------------------

(defmethod initialize-instance :after ((sb adjustable-sliderbox)
				       &rest other-initargs
				       &key lower-limit upper-limit
				       &allow-other-keys)

  (let* ((sl-width (sl-width sb))
	 (sl-height (sl-height sb))
	 (max (sl-max sb))
	 (min (sl-min sb))
	 (font (font sb))
	 (width (+ sl-width (* 2 *sx*))) ;; different for vert. *******
	 (fh (font-height font))
	 (th (+ fh 10)) ; textline height
	 (tw (+ (xlib:text-width font (format nil "~A" (digits sb)))
		20))			; 10 pixels margin on each side
	 (win (window sb)))
    (setf (the-minimum sb) (apply #'make-textline tw th 
  			          :parent win
			          :ulc-x *sx*
			          :ulc-y (+ *sy* sl-height 5)
				  :border-style
				  (if (eql *default-border-style* :flat)
				      :flat :lowered)
				  :numeric t
				  :upper-limit (or upper-limit max)
				  :lower-limit (or lower-limit min)
			          other-initargs)
          (the-maximum sb) (apply #'make-textline tw th 
    			          :parent win
			          :ulc-x (- width *sx* tw)
			          :ulc-y (+ *sy* sl-height 5)
				  :border-style
				  (if (eql *default-border-style* :flat)
				      :flat :lowered)
				  :numeric t
				  :upper-limit (or upper-limit max)
				  :lower-limit (or lower-limit min)
			          other-initargs))
    (setf (info (the-minimum sb)) (minimum sb))
    (setf (info (the-maximum sb)) (maximum sb))
    (ev:add-notify sb (new-info (the-minimum sb))
		   #'(lambda (asb ann val)
		       (declare (ignore ann))
		       (setf (minimum asb) (read-from-string val))
		       (when (< (setting asb) (minimum asb))
			 (setf (setting asb) (minimum asb)))))
    (ev:add-notify sb (new-info (the-maximum sb))
		   #'(lambda (asb ann val)
		       (declare (ignore ann))
		       (setf (maximum asb) (read-from-string val))
		       (when (> (setting asb) (maximum asb))
			 (setf (setting asb) (maximum asb)))))))

;;;---------------------------------------------

(defmethod destroy :before ((asb adjustable-sliderbox))

  "destroy the extra textlines first"

  (destroy (the-minimum asb))
  (destroy (the-maximum asb)))

;;;---------------------------------------------
;;; End.
