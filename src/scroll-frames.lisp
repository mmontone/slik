;;;
;;; scroll-frames
;;;
;;; Provides a horizontal scroll window for sliding through a series
;;; of pictures (provided by the caller), including the capability to
;;; page through subsets if the entire lists is too big to fit in the X
;;; window system address space.
;;;
;;;  8-Sep-2003 I. Kalet created with ideas from popup-scroll-menu and
;;; the Prism filmstrip.
;;;

(in-package :slik)

;;;--------------------------------------

(defconstant *scrollbar-height* 20)

;;;----------------------------------------

(defclass scroll-frame (frame)

  ((width :type fixnum
	  :accessor width
	  :initarg :width
	  :documentation "The width of the overall scroll-frame,
	  specified by the caller.")

   (pictures :type list
	     :accessor pictures
	     :initarg :pictures
	     :documentation "The list of SLIK pictures that can be
	     displayed in the scroll-frame.  They should be supplied
	     unmapped.")

   (index :type fixnum
	  :accessor index
	  :initarg :index
	  :documentation "The index of the selected picture in the
	  list of pictures.")

   (new-index :type ev:event
	      :accessor new-index
	      :initform (ev:make-event)
	      :documentation "Announced when the user selects a
	      picture by clicking on it.")

   (offset :type fixnum
	   :accessor offset
	   :initform 0
	   :documentation "The index of the first picture in the
	   currently scrollable subset of the picture list.")

   (scroll-window :accessor scroll-window
		  :documentation "The parent window of all the
		  pictures currently in the displayable list, fits
		  within the scroll-frame, and only those pictures
		  whose x coordinate puts them in the displayable part
		  are visible.")

   (scrollbar :accessor scrollbar
	      :documentation "Used to move through the currently
	       displayable pictures.")

   (page-button :accessor page-button
		:documentation "Used to move to the next subset of
		pictures, when there are too many to map all at once
		in the X address space.")

   )

  (:default-initargs :pictures nil :width 768 :index 0)

  (:documentation "The scroll-frame provides a display of a linear
  sequence of pictures that can be scrolled horizontally in a fixed
  viewport.")

  )

;;;----------------------------------------

(defmethod initialize-instance :after ((scr scroll-frame)
				       &rest initargs)

  "parallels the scrolling list...most constants defined elsewhere in
  SLIK."

  (let* ((background-color (color-gc (bg-color scr) (colormap scr)))
	 (background (xlib:gcontext-foreground background-color))
	 (pixmap-height (xlib:drawable-height
			 (pixmap (first (pictures scr)))))
	 )
    (setf (scroll-window scr)
      (xlib:create-window :parent (window scr)
			 :x 0 :y 0
			 :width (width scr)
			 :height (+ *scrollbar-height* pixmap-height)
			 :depth *screen-root-depth*
			 :background background))
    (xlib:map-window (scroll-window scr))
    (setf (scrollbar scr)
      (make-scrollbar (width s) *scrollbar-height*
		      *scroll-minimum* *scroll-maximum*
		      :parent (window scr) 
		      :ulc-x 0 :ulc-y pixmap-height))
    ;; as scrollbar moves right, scroll-window moves left (and vice-versa)
    (ev:add-notify scr (value-changed (scrollbar scr)) 
		   #'(lambda (sl sb setting)
		       (declare (ignore sb))
		       (setf (xlib:drawable-x (scroll-window sl)) 
			 (round (* (- (width sl) 
				      (xlib:drawable-width (scroll-window sl))) 
				   (- *scroll-maximum* setting))))
		       (xlib:display-finish-output *display*)))
    ))

;;;----------------------------------------

(defun make-scroll-frame (width picture-list &rest initargs)

  "make-scroll-frame width picture-list &rest initargs

returns a scroll-frame of the specified width in pixels, with the
picture-list positioned to show the first picture in the list at the
left-most position in the frame, unless otherwise specified in the
initargs."

  (apply #'make-instance 'scroll-frame
	 :width width :pictures picture-list initargs))

;;;----------------------------------------

(defmethod (setf index) :after (new (scr scroll-frame))

  (ev:announce scr (new-index scr) new))

;;;----------------------------------------

(defun add-picture (pic scr place)

  "Provides a way for a client to add a new picture in the list scr at
  a place indexed by place."

  ;; use insert from misc.cl, like in filmstrip and insert-button in
  ;; scrolling list
  
  )

;;;----------------------------------------

(defun delete-picture (pic scr)

  "Provides a way for a client to remove a picture from the list scr."
  
  
  )

;;;----------------------------------------

(defun display-scroll-frame (scr)

  "Updates the display for scroll-frame scr, usually after some change
  is made to one or more of the pixmaps in the picture list."

  )

;;;----------------------------------------

(defmethod destroy :before ((scr scroll-frame))

  "dsetroys the scroll-frame and its components, but does not do
  anything to the picture list, except unmap it."

  (destroy (scrollbar scr))
  (aif (page-button scr) (destroy it))
  (xlib:destroy-window (scroll-window scr)))

;;;----------------------------------------
;;; End.
