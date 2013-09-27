;;;
;;; sliders
;;;
;;; A slider has a rectangular knob that moves along a track, and
;;; adjusts a real value (float) similarly to the dial, but with
;;; range specified by two parameters, for the upper and lower end.
;;;
;;; 26-Apr-1992 I. Kalet written
;;; 27-Apr-1992 I. Kalet minor adjustments
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 27-May-1992 I. Kalet fix up type declarations
;;;  6-Jul-1992 I. Kalet change be: to ev: and behavior to event
;;;  8-Oct-1992 I. Kalet change defsetf setting to
;;;  defmethod (setf setting)
;;; 25-Oct-1992 I. Kalet eliminate pixmap, fix up refresh
;;; 29-Nov-1992 I. Kalet fix up method for setf setting, fix knob initargs
;;; 21-Apr-1994 J. Unger put scale-factor and slot-zero initialization code
;;;  in its own function, since it's called elsewhere (adj-sliderboxes).
;;; 25-Apr-1994 J. Unger omit some unused variables in init-inst.
;;;  3-Jan-1995 I. Kalet remove proclaim form
;;;  3-Sep-1995 I. Kalet enforce single-float for setting
;;; 26-Aug-1998 M. Lease revamped while adding support for scrollbars.
;;;  3-Nov-1998 I. Kalet make maximum and minimum accessors instead of
;;;  readers.
;;; 12-Jan-1999 I. Kalet always coerce new setting value in setf
;;; method, and continue to announce value-changed even when value is
;;; the same.
;;;  2-Apr-1999 C. Wilcox enabled event look-ahead for slider drags.
;;; 23-Apr-1999 I. Kalet changes for multiple colormaps.
;;;

(in-package :slik)

;;;------------------------------------------

(defconstant *knob-thickness* (/ 2 3))
(defconstant *slot-offset* 5)
(defconstant *slot-thickness* (/ 1 6))
(defconstant *default-knob-scale* 0.03)

;;;------------------------------------------

(defclass slider (frame)

  ((setting :type single-float
	    :accessor setting
	    :initarg :setting
	    :documentation "The slider's current setting")

   (minimum :type single-float
	    :accessor minimum
	    :initarg :minimum)

   (maximum :type single-float
	    :accessor maximum
	    :initarg :maximum)

   (orient :type (member :horizontal :vertical)
	   :reader orient
	   :initarg :orient
	   :initform :horizontal
	   :documentation "Values increase left-to-right for
horizontal sliders, bottom-to-top for vertical sliders.")

   (knob-scale :type single-float
	       :accessor knob-scale
	       :initarg :knob-scale
	       :initform *default-knob-scale*
	       :documentation "Positive float <= 1.0 describing ratio of
knob size to slot size.")		

   (knob-width :type xlib:card16
	       :accessor knob-width)

   (knob-height :type xlib:card16
		:accessor knob-height)

   (slot-ulc-x :type xlib:card16
	       :accessor slot-ulc-x)

   (slot-ulc-y :type xlib:card16
	       :accessor slot-ulc-y)

   (slot-width :type xlib:card16
	       :accessor slot-width)

   (slot-height :type xlib:card16
		:accessor slot-height)

   (drag-offset :type fixnum
		:accessor drag-offset)

   (dragging-knob :type (member t nil)
		  :accessor dragging-knob
		  :initform nil
		  :documentation "Flag indicating whether the user is 
currently dragging the knob.")

   (value-changed :type ev:event
		  :accessor value-changed
		  :initform (ev:make-event))

   )

  (:default-initargs :title "SLIK slider")

  (:documentation "A slider provides a control for manipulating a real
or integer value.")
  )

;;;------------------------------------------

(defun make-slider (width height min max &rest other-initargs)

  (let ((s  (apply 'make-instance 'slider :width width :height height
		   :minimum min :maximum max other-initargs)))
    (push :motion-notify (look-ahead s))
    (refresh s)
    s))

;;;---------------------------------------------

(defmethod initialize-instance :after ((s slider) &rest other-initargs) 

  (declare (ignore other-initargs))
  (if (eq (orient s) :vertical)
      (progn
	(setf (knob-width s) (round (* (width s) *knob-thickness*)))
	(setf (slot-width s) (round (* (width s) *slot-thickness*)))
	(setf (slot-height s) (- (height s) (* *slot-offset* 2))))
    (progn
      (setf (knob-height s) (round (* (height s) *knob-thickness*)))
      (setf (slot-width s) (- (width s) (* *slot-offset* 2)))
      (setf (slot-height s) (round (* (height s) *slot-thickness*)))))
  (setf (slot-ulc-x s) (round (/ (- (width s) (slot-width s)) 2)))
  (setf (slot-ulc-y s) (round (/ (- (height s) (slot-height s)) 2)))
  (unless (slot-boundp s 'setting)
    (setf (slot-value s 'setting) (/ (+ (maximum s) (minimum s)) 2.0)))
  (scale-knob s))

;;;------------------------------------------

(defmethod (setf setting) :around (new-setting (s slider))

  (setq new-setting (coerce new-setting 'single-float))
  (unless (= new-setting (setting s))
    (erase-knob s)
    (if (> new-setting (maximum s)) (setq new-setting (maximum s)))
    (if (< new-setting (minimum s)) (setq new-setting (minimum s)))
    (setf (slot-value s 'setting) new-setting)
    (slider-draw s))
  (ev:announce s (value-changed s) new-setting)
  new-setting)

;;;------------------------------------------

(defun knob-ulc-x (s)

  (if (eq (orient s) :vertical)
      (round (/ (- (width s) (knob-width s)) 2))
    (+ (slot-ulc-x s) (knob-offset s))))

;;;------------------------------------------

(defun knob-ulc-y (s)

  (if (eq (orient s) :vertical)
      (- (height s) *slot-offset* (knob-offset s) (knob-height s))
    (round (/ (- (height s) (knob-height s)) 2))))

;;;------------------------------------------

(defun knob-offset (s)

  (round (* (/ (- (setting s) (minimum s))
	       (- (maximum s) (minimum s)))
	    (knob-range s))))

;;;------------------------------------------

(defun knob-range (s)

  (if (eq (orient s) :vertical)
      (- (slot-height s) (knob-height s))
    (- (slot-width s) (knob-width s))))

;;;------------------------------------------

(defmethod (setf knob-scale) :around (new-scale (s slider))

  (erase-knob s)
  (call-next-method)
  (scale-knob s)
  (slider-draw s)
  new-scale)

;;;------------------------------------------

(defun scale-knob (s)

  (if (eq (orient s) :vertical)
      (setf (knob-height s) (round (* (slot-height s) (knob-scale s))))
    (setf (knob-width s) (round (* (slot-width s) (knob-scale s))))))

;;;------------------------------------------

(defmethod refresh :before ((s slider))

  (slider-draw s))

;;;------------------------------------------

(defun erase-knob (s)

  "erase-knob s

replaces the knob with the background color.  This function used instead
of 'erase' in order to avoid flickering."

  (xlib:draw-rectangle (window s)
		      (color-gc (bg-color s) (colormap s))
		      (knob-ulc-x s) (knob-ulc-y s)
		      (knob-width s) (knob-height s) 
		      t))

;;;------------------------------------------

(defun slider-draw (s)

  (xlib:draw-rectangle (window s)
		      (color-gc (fg-color s) (colormap s))
		      (slot-ulc-x s) (slot-ulc-y s) 
		      (slot-width s) (slot-height s) 
		      nil)
  (xlib:draw-rectangle (window s)
		      (color-gc (fg-color s) (colormap s))
		      (knob-ulc-x s) (knob-ulc-y s)
		      (knob-width s) (knob-height s) 
		      t)
  (flush-output))

;;;------------------------------------------

(defmethod process-button-press ((s slider) button-id x y)

  (when (and (= button-id *button-1*) (plusp (knob-range s)))
    (setf (dragging-knob s) t)
    (if (is-pt-in-rect x y (knob-ulc-x s) (knob-ulc-y s)
		       (knob-width s) (knob-height s))
	(setf (drag-offset s) (if (eq (orient s) :vertical)
				  (- y (knob-ulc-y s))
				(- x (knob-ulc-x s))))
      (progn
	(setf (drag-offset s) 0)
	(update-setting s x y))))
  nil)

;;;------------------------------------------

(defmethod process-motion-notify ((s slider) x y state)

  (declare (ignore state))
  (when (dragging-knob s)
    (update-setting s x y))
  nil)

;;;------------------------------------------

(defmethod process-button-release ((s slider) button-id x y)

  (declare (ignore x y))
  (when (= button-id *button-1*)
    (setf (dragging-knob s) nil))
  nil)

;;;------------------------------------------

(defun update-setting (s x y)

  (let ((knob-offset (restrict-range
		      (if (eq (orient s) :vertical)
			  (+ (- (height s) *slot-offset* y 
				(knob-height s)) (drag-offset s))
			(- x *slot-offset* (drag-offset s))) 
		      0 (knob-range s))))  
    (setf (setting s)
      (+ (minimum s) (* (/ knob-offset (knob-range s)) 
			(- (maximum s) (minimum s)))))))

;;;------------------------------------------

;;; ### this is much too general to be here

(defun is-pt-in-rect (x y ulc-x ulc-y width height)

  (and (>= x ulc-x) (>= y ulc-y) 
       (<= x (+ ulc-x width)) (<= y (+ ulc-y height))))

;;;------------------------------------------

;;; ### this is much too general to be here

(defun restrict-range (val minimum maximum)
  (max (min val maximum) minimum))

;;;------------------------------------------
;;; End.
