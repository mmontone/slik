;;;
;;; pictures
;;;
;;; A picture is a SLIK frame with some process- methods that forward
;;; X events to interested parties using the announcement of events.
;;;
;;;  6-Jul-1992 I. Kalet created
;;;  8-Oct-1992 I. Kalet add forwarding of exposure events
;;; 25-Oct-1992 I. Kalet only pictures now have pixmaps so must add it
;;; here.  Also, pixmap is set to window background.
;;; 12-Nov-1992 I. Kalet move exposure event to frame
;;; 28-Jan-1994 I. Kalet add pickable objects rectangle, circle, segment
;;; 17-Apr-1994 I. Kalet add square pickable object, other enhancements
;;; 25-Apr-1994 I. Kalet change color attribute to gcontext, not symbol
;;; 22-May-1994 I. Kalet don't update pickable object location on
;;; pointer motion, just announce - provide update-pickable-object
;;; generic function so application can do it if desired.
;;; 24-May-1994 J. Unger finish implementation of segment pickable obj.
;;; 20-Jun-1994 J. Unger factor out point-near-segment code from picked
;;; method for segment pickable obj (so can be called elsewhere).
;;; 25-Jul-1994 J. Unger make enabled attrib of pickable-obj an
;;; initarg
;;;  3-Jan-1995 I. Kalet delete unnecessary draw method for square
;;;  7-May-1997 BobGian changed (EXPT (some-form) 2) to inline squaring
;;;    with LET to avoid multiple evaluation of (some-form).
;;; 23-Apr-1999 I. Kalet changes for multiple colormaps.
;;; 21-Jul-2000 I. Kalet enable look-ahead for motion-notify events.
;;; 26-Nov-2000 I. Kalet make default bg-color black here since it is
;;; gray in the general case.
;;;

(in-package :slik)

;;;--------------------------------

(defclass picture (frame)

  ((pixmap :accessor pixmap
	   :initarg :pixmap
	   :documentation "The pixmap is set to the window background,
and can be used for foreground/background applications like line
graphics over images.")

   (pick-list :type list
	      :accessor pick-list
	      :initform nil
	      :documentation "The list of pickable objects to be
checked before announcing button down/up and pointer move events.")

   (enter-notify :type ev:event
		 :accessor enter-notify
		 :initform (ev:make-event)
		 :documentation "Announced when the picture window
receives an X enter-notify event.")

   (leave-notify :type ev:event
		 :accessor leave-notify
		 :initform (ev:make-event)
		 :documentation "Announced when the picture window
receives an X leave-notify event.")

   (button-press :type ev:event
		 :accessor button-press
		 :initform (ev:make-event)
		 :documentation "Announced when the picture window
receives an X button-press event.")

   (button-release :type ev:event
		   :accessor button-release
		   :initform (ev:make-event)
		   :documentation "Announced when the picture window
receives an X button-release event.")

   (motion-notify :type ev:event
		  :accessor motion-notify
		  :initform (ev:make-event)
		  :documentation "Announced when the picture window
receives an X motion-notify event.")

   (key-press :type ev:event
	      :accessor key-press
	      :initform (ev:make-event)
	      :documentation "Announced when the picture window
receives an X key-press event.")

   )

  (:default-initargs :bg-color 'black :border-style :flat)

  (:documentation "A picture is simply a SLIK frame that passes on
announcements of X events so application code can register with it to
handle them in any way it wishes without interfering with or knowing
about SLIK X event processing or the internal details of other SLIK
objects.")

  )

;;;--------------------------------

(defun erase-bg (pic)

  "erase-bg pic

erases both the pixmap and the window of the picture pic."

  (xlib:draw-rectangle (pixmap pic)
		      (color-gc (bg-color pic) (colormap pic))
		      0 0
		      (width pic) (height pic)
		      t)
  (xlib:clear-area (window pic))
  (flush-output))

;;;--------------------------------

(defun make-picture (width height &rest other-initargs)

  "make-picture width height &rest other-initargs

returns an instance of a picture with blank pixmap and window."

  (apply #'make-instance 'picture :width width :height height
	 other-initargs))

;;;--------------------------------

(defmethod initialize-instance :after ((pic picture) &rest initargs)

  "adds the extra initialization for pictures to that of frames."

  (declare (ignore initargs))
  (let* ((w (window pic))
	 (px (xlib:create-pixmap :width (width pic)
				:height (height pic)
				:depth (xlib:drawable-depth w)
				:drawable w)))
    (setf (pixmap pic) px
	  (xlib:window-background w) px)
    (push :motion-notify (look-ahead pic))
    (erase-bg pic)
    (draw-border pic)
    (flush-output)
    pic))

;;;--------------------------------

(defmethod destroy :after ((obj picture))

  (xlib:free-pixmap (pixmap obj)))

;;;--------------------------------

(defmethod process-enter-notify ((p picture) x y state)

  "Forwards an announcement to registered parties."

  (ev:announce p (enter-notify p) x y state))

;;;--------------------------------

(defmethod process-leave-notify ((p picture) x y state)

  "Forwards an announcement to registered parties."

  (ev:announce p (leave-notify p) x y state))

;;;--------------------------------

(defmethod process-key-press ((p picture) code state)

  "Forwards an announcement to registered parties."

  (ev:announce p (key-press p) code state))

;;;--------------------------------

(defun display-picture (pic)

  "display-picture pic

copies the background pixmap to the window and draws the pickable
objects and border in the window."

  (xlib:clear-area (window pic))
  (refresh pic))

;;;--------------------------------
;;; pickable objects begin here
;;;--------------------------------

(defclass pickable-object ()

  ((object :accessor object
	   :initarg :object
	   :documentation "The object associated with this pickable
object.")

   (color :type xlib:gcontext
	  :accessor color
	  :initarg :color
	  :documentation "The xlib gcontext specifying the color in
which to draw the pickable object.")

   (enabled :accessor enabled
            :initarg :enabled
	    :documentation "Enabled indicates that this pickable
object can receive and process selection events.  If nil it ignores
them and is not drawn in refresh operations.")

   (active :accessor active
	   :initform nil
	   :documentation "Active indicates whether this region is
picked, i.e., it got selected and the mouse button is still down.")

   (selected :type ev:event
	     :accessor selected
	     :initform (ev:make-event)
	     :documentation "Announced when the mouse button is
pressed while the pointer is within the pick region.")

   (deselected :type ev:event
	       :accessor deselected
	       :initform (ev:make-event)
	       :documentation "Announced when the mouse button is
released while the pointer is within the pick region and the region is
active.")

   (motion :type ev:event
	   :accessor motion
	   :initform (ev:make-event)
	   :documentation "Announced when the pointer moves while this
pickable object is active.")

   )

  (:default-initargs :color (color-gc 'white) :enabled t)

  (:documentation "A pickable object defines a region in a picture
which is responsive to button press, i.e., selection operations.")

  )

;;;--------------------------------

(defun add-pickable-obj (po pic)

  "add-pickable-obj po pic

adds the pickable object po to the pick list of picture pic.  The
parameter po can also be a list of pickable objects."

  (if (listp po)
      (dolist (ob po) (push ob (pick-list pic)))
    (push po (pick-list pic))))

;;;--------------------------------

(defun find-pickable-objs (obj pic)

  "find-pickable-objs obj pic

returns a list of all pickable objects in the pick list of picture
pic, that correspond to object obj."

  ;; returns just the first one for now
  (list (find obj (pick-list pic) :key #'object)))

;;;--------------------------------

(defun remove-pickable-objs (obj pic)

  "remove-pickable-objs obj pic

replaces the pick list in pic with a new list in which all pickable
objects corresponding to obj are omitted.  Returns the new list."

  (setf (pick-list pic)
    (remove obj (pick-list pic) :key #'object)))

;;;--------------------------------

(defmethod picked ((obj pickable-object) code x y)

  "default method - should use defgeneric instead, for these."

  (declare (ignore code x y))
  nil)

;;;--------------------------------

(defmethod draw ((obj pickable-object) pic)

  "default method - renders obj into the window of the picture pic."

  (declare (ignore pic))
  nil)

;;;--------------------------------

(defmethod refresh ((pic picture))

  (dolist (obj (pick-list pic))
    (if (enabled obj) (draw obj pic))))

;;;--------------------------------

(defmethod process-button-press ((p picture) code x y)

  "Forwards an announcement to registered parties or announces a
pick."

  (unless (dolist (obj (pick-list p))
	    (when (and (enabled obj) (picked obj code x y))
	      (setf (active obj) t)
	      (ev:announce obj (selected obj) code x y)
	      (return t)))
    (ev:announce p (button-press p) code x y)))

;;;--------------------------------

(defmethod process-button-release ((p picture) code x y)

  "Forwards an announcement to registered parties or announces a
pick."

  (unless (dolist (obj (pick-list p))
	    (when (active obj)
	      (setf (active obj) nil)
	      (ev:announce obj (deselected obj))
	      (return t)))
    (ev:announce p (button-release p) code x y)))

;;;--------------------------------

(defmethod process-motion-notify ((p picture) x y state)

  "Forwards an announcement to registered parties or, if a pickable
object is active, announces a pickable object motion event."

  (unless (dolist (obj (pick-list p))
	    (when (active obj)
	      (ev:announce obj (motion obj) x y state)
	      (return t)))
    (ev:announce p (motion-notify p) x y state)))

;;;--------------------------------
;;; the pickable objects themselves
;;;--------------------------------

(defclass rectangle (pickable-object)

  ((ulc-x :type fixnum
	  :accessor ulc-x
	  :initarg :ulc-x
	  :documentation "The x coordinate, window relative, of the
upper left corner of the rectangular sensitive region.")

   (ulc-y :type fixnum
	  :accessor ulc-y
	  :initarg :ulc-y
	  :documentation "The y coordinate, window relative, of the
upper left corner of the rectangular sensitive region.")

   (width :type fixnum
	  :accessor width
	  :initarg :width
	  :documentation "The width in pixels, of the rectangular
sensitive region.")

   (height :type fixnum
	   :accessor height
	   :initarg :height
	   :documentation "The y coordinate, window relative, of the
lower right corner of the rectangular sensitive region.")

   (filled :type (member t nil)
	   :accessor filled
	   :initarg :filled
	   :documentation "A boolean specifying whether the rectangle
is drawn filled or open.")

   (last-x :type fixnum
	   :accessor last-x
	   :documentation "A cache for doing translations.")

   (last-y :type fixnum
	   :accessor last-y
	   :documentation "A cache for doing translations.")

   )

  (:default-initargs :filled nil)

  (:documentation "A rectangular sensitive region, for example, a grab
box.")

  )

;;;--------------------------------

(defmethod draw ((obj rectangle) pic)

  (xlib:draw-rectangle (window pic) (color obj)
		      (ulc-x obj) (ulc-y obj)
		      (width obj) (height obj)
		      (filled obj)))

;;;--------------------------------

(defmethod update-pickable-object ((obj rectangle) x y)

  "translate from last position to the new position"

  (setf (ulc-x obj) (+ (ulc-x obj) (- x (last-x obj)))
	(ulc-y obj) (+ (ulc-y obj) (- y (last-y obj)))
	(last-x obj) x
	(last-y obj) y))

;;;--------------------------------

(defmethod initialize-instance :after ((obj rectangle) &rest initargs)

  (declare (ignore initargs))
  (setf (last-x obj) (+ (ulc-x obj) (truncate (/ (width obj) 2)))
	(last-y obj) (+ (ulc-y obj) (truncate (/ (height obj) 2)))))

;;;--------------------------------

(defun make-rectangle (obj ulc-x ulc-y width height &rest keyargs)

  "make-rectangle obj ulc-x ulc-y width height &rest keyargs

returns a rectangle pickable object at the specified place, associated
with object obj."

  (apply #'make-instance 'rectangle
	 :object obj
	 :ulc-x ulc-x :ulc-y ulc-y
	 :width width :height height
	 keyargs))

;;;--------------------------------

(defmethod picked ((obj rectangle) code x y)

  "checks if x y is in the rectangle"

  (declare (ignore code))
  (let ((xu (ulc-x obj))
	(yu (ulc-y obj)))
    (and (>= x xu)
	 (>= y yu)
	 (<= x (+ xu (width obj)))
	 (<= y (+ yu (height obj))))))

;;;--------------------------------

(defclass square (rectangle)

  ((x-center :type fixnum
	     :accessor x-center
	     :initarg :x-center
	     :documentation "The x coordinate of the square center.")

   (y-center :type fixnum
	     :accessor y-center
	     :initarg :y-center
	     :documentation "The y coordinate of the square center.")

   )

  ;; ulc-x, ulc-y, height need to be bound, but the initial values
  ;; don't matter because they are reset after creation
  (:default-initargs :ulc-x 0 :ulc-y 0 :width 6 :height 6)

  (:documentation "A square sensitive area.")

  )

;;;--------------------------------

(defun set-square-corners (s)

  "set-square-corners s

sets the rectangle slots from the center and width slots."

  (let ((hw (round (/ (width s) 2))))
    (setf (ulc-x s) (- (x-center s) hw)
	  (ulc-y s) (- (y-center s) hw))))

;;;--------------------------------

(defmethod initialize-instance :after ((s square) &rest initargs)

  (declare (ignore initargs))
  (set-square-corners s))

;;;--------------------------------

(defmethod (setf x-center) :after (new-x (s square))

  (declare (ignore new-x))
  (set-square-corners s))

;;;--------------------------------

(defmethod (setf y-center) :after (new-y (s square))

  (declare (ignore new-y))
  (set-square-corners s))

;;;--------------------------------

(defmethod (setf width) :after (new-w (s square))

  (setf (height s) new-w) ;; for draw method and picked method
  (set-square-corners s))

;;;--------------------------------

(defmethod update-pickable-object ((obj square) x y)

  "just put in the new position"

  (setf (x-center obj) x
	(y-center obj) y))

;;;--------------------------------

(defun make-square (obj x y &rest keyargs)

  "make-square obj x y &rest keyargs

returns a square pickable object at the specified place, associated
with object obj."

  (apply #'make-instance 'square
	 :object obj
	 :x-center x :y-center y
	 keyargs))

;;;--------------------------------

(defclass circle (pickable-object)

  ((x-center :type fixnum
	     :accessor x-center
	     :initarg :x-center
	     :documentation "The x coordinate of the circle center.")

   (y-center :type fixnum
	     :accessor y-center
	     :initarg :y-center
	     :documentation "The y coordinate of the circle center.")

   (radius :type fixnum
	   :accessor radius
	   :initarg :radius
	   :documentation "The radius in pixels of the circle.")

   (filled :type (member t nil)
	   :accessor filled
	   :initarg :filled
	   :documentation "A boolean specifying whether the circle is
drawn filled or open.")

   )

  (:default-initargs :radius 4 :filled nil)

  (:documentation "A circular sensitive area.")

  )

;;;--------------------------------

(defmethod draw ((obj circle) pic)

  (let* ((r (radius obj))
	 (width (* 2 r)))
    (xlib:draw-arc (window pic) (color obj)
		  (- (x-center obj) r)
		  (- (y-center obj) r)
		  width width
		  0.0 *two-pi* ;; constant from dials module
		  (filled obj))))

;;;--------------------------------

(defmethod update-pickable-object ((obj circle) x y)

  "just put in the new position"

  (setf (x-center obj) x
	(y-center obj) y))

;;;--------------------------------

(defun make-circle (obj x y &rest keyargs)

  "make-circle obj x y &rest keyargs

returns a circle pickable object at the specified place, associated
with object obj."

  (apply #'make-instance 'circle
	 :object obj
	 :x-center x :y-center y
	 keyargs))

;;;--------------------------------

(defmethod picked ((obj circle) code x y)

  "check for within circle"

  (declare (ignore code))
  (let ((x-val (- x (x-center obj)))
	(y-val (- y (y-center obj))))
    (<= (sqrt (+ (* x-val x-val)
		 (* y-val y-val)))
	(radius obj))))

;;;--------------------------------

(defclass segment (pickable-object)

  ((x1 :type fixnum
       :accessor x1
       :initarg :x1
       :documentation "The x coordinate of end 1.")

   (y1 :type fixnum
       :accessor y1
       :initarg :y1
       :documentation "The y coordinate of end 1.")

   (x2 :type fixnum
       :accessor x2
       :initarg :x2
       :documentation "The x coordinate of end 2.")

   (y2 :type fixnum
       :accessor y2
       :initarg :y2
       :documentation "The y coordinate of end 2.")

   (last-x :type fixnum
	   :accessor last-x
	   :documentation "A cache for doing translations.")

   (last-y :type fixnum
	   :accessor last-y
	   :documentation "A cache for doing translations.")

   (thickness :type fixnum
	      :accessor thickness
	      :initarg :thickness
	      :documentation "The number of pixels thick the
line segment should be drawn.")

   (tolerance :type fixnum
	      :accessor tolerance
	      :initarg :tolerance
	      :documentation "The number of pixels away from the line
segment the pointer can be and still be considered on the segment.")

   )

  (:default-initargs :thickness 1 :tolerance 1)

  (:documentation "A line segment sensitive region, like a tube, that
can be selected and dragged.")

  )

;;;--------------------------------

(defmethod draw ((obj segment) pic)

  (unless (zerop (thickness obj))
    (xlib:draw-line 
      (window pic) (color obj) (x1 obj) (y1 obj) (x2 obj) (y2 obj))))

;;;--------------------------------

(defmethod update-pickable-object ((obj segment) x y)

  "translate from last position to the new position"

  (setf (x1 obj) (+ (x1 obj) (- x (last-x obj)))
	(x2 obj) (+ (x2 obj) (- x (last-x obj)))
	(y1 obj) (+ (y1 obj) (- y (last-y obj)))
	(y2 obj) (+ (y2 obj) (- y (last-y obj)))
        (last-x obj) x
	(last-y obj) y))

;;;--------------------------------

(defmethod initialize-instance :after ((obj segment) &rest initargs)

  (declare (ignore initargs))
  (setf (last-x obj) (truncate (+ (x1 obj) (x2 obj)) 2)
        (last-y obj) (truncate (+ (y1 obj) (y2 obj)) 2))
  (when (< 1 (thickness obj))
    (let ((gc (sl:make-duplicate-gc (color obj))))
      (setf (xlib:gcontext-line-width gc) (thickness obj))
      (setf (color obj) gc))))

;;;--------------------------------

(defun make-segment (obj x1 y1 x2 y2 &rest keyargs)

  "make-segment obj x1 y1 x2 y2 &rest keyargs

returns an instance of a segment with specified endpoints."

  (apply #'make-instance 'segment
	 :object obj
	 :x1 x1 :y1 y1 :x2 x2 :y2 y2
	 keyargs))

;;;--------------------------------

(defun point-near-segment (x y x1 y1 x2 y2 tolerance)

  "point-near-segment x y x1 y1 x2 y2 tolerance

Returns t iff the point (x y) is within tolerance pixels of the segment
with endpoints (x1 y1) and (x2 y2)."

  ;; Translate and rotate the segment so that it sits at (0,0) (x,0)
  ;; on the x axis, then apply the same transformation to the point.
  ;; The point will be near the segment if its y value is smaller than
  ;; the threshold, and its x value lies between 0 and that of the
  ;; other end of the segment.

  (let* ((xt (- x x1))
         (yt (- y y1))
         (x2t (- x2 x1))
         (y2t (- y2 y1))
         (theta (atan y2t x2t))
         (sin-theta (sin theta))
         (cos-theta (cos theta))
         (xr (+ (* xt cos-theta) (* yt sin-theta)))
         (yr (- (* yt cos-theta) (* xt sin-theta)))
         (x2r (+ (* x2t cos-theta) (* y2t sin-theta))))
    (and (or (<= 0.0 xr x2r)
	     (<= x2r xr 0.0))
	 (or (<= 0.0 yr tolerance)
	     (<= (- tolerance) yr 0.0)))))

;;;-----------------------------------

(defmethod picked ((obj segment) code x y)

  "check if x y is within tolerance pixels of segment"

  (declare (ignore code))
  (point-near-segment x y (x1 obj) (y1 obj) (x2 obj) (y2 obj)
		      (tolerance obj)))

;;;-----------------------------------

(defmethod destroy :after ((obj segment))

  (when (< 1 (thickness obj))
    (xlib:free-gcontext (color obj))))

;;;-----------------------------------
;;; End.
