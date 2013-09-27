;;;
;;; scrollbars
;;;
;;; 12-Aug-1998 M. Lease written.  Support for scrolling via holding 
;;; down the increment or decrement button is not yet added.
;;; 23-Nov-1998 I. Kalet include a real event, and forward slider
;;; announcement, rather than depend on event implementation details.
;;; Also add a destroy method.
;;;

(in-package :slik)

;;;------------------------------------------

(defclass scrollbar (frame)

  ((slider :type slider
	   :accessor slider
	   :initarg :slider
	   :documentation "The knob and bar of the scrollbar.")

   (btn-incr :type icon-button
	     :accessor btn-incr
	     :initarg :btn-incr    
	     :documentation "The button to increment the current setting.")

   (btn-decr :type icon-button
	     :accessor btn-decr
	     :initarg :btn-decr    
	     :documentation "The button to decrement the current setting.")

   (scroll-size :type single-float
		:accessor scroll-size
		:initarg :scroll-size
		:initform 0.0
		:documentation "Amount by which the setting is
incremented or decremented when the appropriate button is pressed.")

   (value-changed :type ev:event
		  :accessor value-changed
		  :initform (ev:make-event)
		  :documentation "Announced when the scrollbar knob
moves, whether it is by the slider moving or the arrow buttons.")

   )

  (:default-initargs :title "SLIK scrollbar" :orient :vertical)

  (:documentation "A scrollbar is a compound SLIK widget composed of a
slider and two arrow buttons.  Just as the sliderbox complements the
slider by allowing the setting to be changed via a textbox, the
scrollbar complements the slider by allowing the setting to be
incremented or decremented a fixed amount via arrow buttons.")
  )

;;;------------------------------------------

(defun make-scrollbar (width height min max &rest other-initargs)

  (apply #'make-instance 'scrollbar :width width :height height
	 :minimum min :maximum max other-initargs))

;;;------------------------------------------

(defmethod initialize-instance :after ((s scrollbar)
				       &rest other-initargs
				       &key orient minimum maximum
				       &allow-other-keys)

  (let ((width (width s))
	(height (height s))
	(win (window s))
	(btn-side) (slider-width) (slider-height) (btn-decr-dir)
	(btn-incr-dir) (btn-incr-x) (btn-decr-y) (slider-x)
	(slider-y) (setting))
    (if (eq orient :vertical)
	(progn
	  (setq btn-side width)
	  (setq slider-width width)
	  (setq slider-height (- height (* 2 btn-side)))
	  (setq btn-decr-dir :down)
	  (setq btn-incr-dir :up)
	  (setq btn-incr-x 0)
	  (setq btn-decr-y (+ btn-side slider-height))
	  (setq slider-x 0)
	  (setq slider-y btn-side)
	  (setq setting maximum))
      (progn
	(setq btn-side height)
	(setq slider-width (- width (* 2 btn-side)))
	(setq slider-height height)
	(setq btn-decr-dir :left)
	(setq btn-incr-dir :right)
	(setq btn-incr-x (+ btn-side slider-width))
	(setq btn-decr-y 0)
	(setq slider-x btn-side)
	(setq slider-y 0)
	(setq setting minimum)))
    (setf (btn-decr s) (apply 'make-arrow-button btn-side btn-side 
			      btn-decr-dir 
			      :ulc-x 0 :ulc-y btn-decr-y 
			      :parent win 
			      other-initargs)) 
    (setf (btn-incr s) (apply 'make-arrow-button btn-side btn-side
			      btn-incr-dir  
			      :ulc-x btn-incr-x :ulc-y 0
			      :parent win 
			      other-initargs))
    (setf (slider s) (apply 'make-slider slider-width 
			    slider-height minimum maximum
			    :setting setting
			    :ulc-x slider-x :ulc-y slider-y 
			    :parent win 
			    other-initargs)))
  (ev:add-notify s (button-on (btn-decr s)) 
		 #'(lambda (sbar b)
		     (declare (ignore b))
		     (setf (setting sbar)  
		       (max (- (setting sbar) (scroll-size sbar)) 
			    (minimum (slider sbar))))))
  (ev:add-notify s (button-on (btn-incr s)) 
		 #'(lambda (sbar b)
		     (declare (ignore b))
		     (setf (setting sbar)  
		       (min (+ (setting sbar) (scroll-size sbar)) 
			    (maximum (slider sbar))))))
  (ev:add-notify s (value-changed (slider s))
		 #'(lambda (sbar sl newval)
		     (declare (ignore sl))
		     (ev:announce sbar (value-changed sbar) newval))))

;;;------------------------------------------

(defmethod setting ((s scrollbar))

  (setting (slider s)))

;;;------------------------------------------

(defmethod (setf setting) (val (s scrollbar))

  (setf (setting (slider s)) val))

;;;------------------------------------------

(defmethod maximum ((s scrollbar))

  (maximum (slider s)))

;;;------------------------------------------

(defmethod (setf maximum) (val (s scrollbar))

  (setf (maximum (slider s)) val))

;;;------------------------------------------

(defmethod knob-scale ((s scrollbar))

  (knob-scale (slider s)))

;;;------------------------------------------

(defmethod (setf knob-scale) (val (s scrollbar))

  (setf (knob-scale (slider s)) val))

;;;------------------------------------------

(defmethod destroy :before ((s scrollbar))

  (destroy (slider s))
  (destroy (btn-incr s))
  (destroy (btn-decr s)))

;;;------------------------------------------
;;; End.
