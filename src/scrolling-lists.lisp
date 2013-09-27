;;;
;;; scrolling-lists
;;;
;;; A scrolling list contains a list of items like a menu, along with 
;;; a scrollbar, in case the list is too long to display all of it in
;;; the available area.  These lists only scroll vertically.
;;;
;;; 25-May-1992 I. Kalet created
;;; 26-May-1992 I. Kalet don't use menus - use unmapped buttons.
;;;  4-Jun-1992 I. Kalet delete-button also deselects
;;;  6-Jul-1992 I. Kalet make scroll-bar half the width of the
;;;  available space, add make-radio-scrolling-list and
;;;  make-list-button, change behavior to event and be: to ev:
;;;  8-Oct-1992 I. Kalet change select-button, deselect-button to
;;;  generic functions instead of ordinary functions.  Add optional
;;;  button-type parameter to make-list-button.
;;; 25-Oct-1992 I. Kalet eliminate pixmap, make delete-button generic
;;;  5-Nov-1992 I. Kalet change make-list-button parameters from
;;;  optional to keyword (justify and button-type)
;;; 29-Nov-1992 I. Kalet take out reference to ulc-x and ulc-y
;;;  6-Aug-1993 I. Kalet finally implement delete with middle mouse
;;;  button, include keyword parameter enable-delete.
;;; 10-Jan-1995 I. Kalet insure that in a radio-scrolling-list, user
;;;  cannot deselect the selected button, also put popup-scroll-menu
;;;  here to remove circularity with dialogboxes.  Also change destroy
;;;  method to destroy the buttons instead of deleting them.  This
;;;  should be faster.
;;; 19-Jul-1995 I. Kalet change scrollbar behavior to simply move top
;;; of bar to pointer location, and track (slowly...) with motion.
;;; 23-Jun-1997 I. Kalet fix insert-button for radio-scrolling-list to
;;; just do that button, and when a button is turned off, reactivate
;;; it.
;;;  4-Jun-1998 I. Kalet fix place-button to be more judicious.
;;; 16-Jun-1998 I. Kalet make popup scroll more efficient.
;;; 25-Aug-1998 M. Lease now uses slik scrollbar, more efficient, maps 
;;; all items and so limits the maximum number of items to be (max
;;; 16-bit signed int / button-height) since xlib uses a 16-bit int to
;;; hold drawable-y value.  Deleting buttons not tested; Prism should be
;;; be built using the revised scrolling-lists for testing.
;;; make-list-button now inserts buttons; popup-scroll-menu not tested 
;;; with this change.
;;; 29-Nov-1998 I. Kalet change defmethod to defun in some places,
;;; change make-list-button back to previous API and define
;;; make-and-insert-list-button to do both create and insert.  Fix
;;; button delete, other stuff.
;;; 16-Dec-1998 I. Kalet if more items in popup-scroll-menu than fit
;;; in X address space, add a page button and segment the items into
;;; pages.
;;; 22-Mar-1999 I. Kalet add a reorder function that accepts a
;;; reordered list of the existing buttons, replaces the old list, and
;;; resets the button y coordinates to correspond to the new order.
;;; 23-Apr-1999 I. Kalet changes to support multiple colormaps.
;;;

;; testing cvs

(in-package :slik)

;;;--------------------------------------

(defconstant *scrollbar-width* 20)

;;; use these limits on the scrollbar to simplify the translation
;;; of scrollbar setting to drawable-y pos of btn-win

(defconstant *scroll-minimum* 0.0)
(defconstant *scroll-maximum* 1.0)

(defconstant *scrollwindow-maxsize* 32767
  "limit of size of scrollable window")

;;;--------------------------------------

(defclass scrolling-list (frame)

  ((buttons :type list 
	    :accessor buttons 
	    :initform nil 
	    :documentation "List of buttons in the scrolling list.")

   (enable-delete :accessor enable-delete
		  :initarg :enable-delete
		  :initform nil
		  :documentation "Boolean variable, if t, allows
delete of button with middle mouse button.  If nil, ignores middle
mouse button clicks.")

   (button-height :type xlib:card16
		  :accessor button-height
		  :documentation "The computed height that buttons in
this scrolling-list should be, based on the specified font.")

   (button-width :type xlib:card16
		 :accessor button-width
		 :documentation "The computed width that buttons in
this scrolling-list should be, based on the width of the
scrolling-list and the width of the scroll-bar.")

   (btn-win :type xlib:window
            :accessor btn-win
            :documentation "Parent window of buttons.")

   (scrollbar :type scrollbar
	      :accessor scrollbar)

   (inserted :type ev:event
	     :accessor inserted
	     :initform (ev:make-event)
	     :documentation "Announced when an item is inserted into
the list.")

   (deleted :type ev:event
	    :accessor deleted
	    :initform (ev:make-event)
	    :documentation "Announced when an item is deleted from the
list.")

   (selected :type ev:event
	     :accessor selected
	     :initform (ev:make-event)
	     :documentation "Announced when an item in the list is
selected.")

   (deselected :type ev:event
	       :accessor deselected
	       :initform (ev:make-event)
	       :documentation "Announced when an item in the list is
deselected.")

   )

  (:default-initargs :title "SLIK Scrolling List")

  (:documentation "The scrolling-list contains a list of buttons and a
scroll bar.  In case only part of the list of buttons is visible, the
scroll bar enables the user to change the portion that appears in the
window.")

  )

;;;--------------------------------------

(defun make-list-button (s label &key (justify :left)
			   (button-type :hold) (ulc-y 0))

  "make-list-button s label &key justify button-type ulc-y

Returns an instance of a SLIK button with width and height sized to
fit scrolling-list s, and with the specified label, positioning and
button type.  The default for justify is :left, for button-type is
hold, and for ulc-y is 0.  The button gets the same graphic
characteristics as the scrolling-list, i.e., foreground color,
background color, border color, etc."

  (make-button (button-width s) (button-height s)
	       :parent (btn-win s)
	       :ulc-y ulc-y :mapped nil :font (font s)
	       :bg-color (bg-color s)
	       :fg-color (fg-color s)
	       :border-width (border-width s)
	       :border-color (border-color s)
	       :label label
	       :justify justify
	       :button-type button-type))

;;;--------------------------------------

(defun init-button (b s)
  
  "init-button b s

sets up event notification for button b."
  
  (ev:add-notify s (button-on b)
		 #'(lambda (sc bt)
		     (ev:announce sc (selected sc) bt)))
  (ev:add-notify s (button-off b)
		 #'(lambda (sc bt)
		     (ev:announce sc (deselected sc) bt)))
  (ev:add-notify s (button-2-on b)
		 #'(lambda (scr btn)
		     (if (and (enable-delete scr)
			      (confirm (concatenate 'string
					 "Delete " (label btn))))
			 (delete-button btn scr)))))

;;;--------------------------------------

(defun update-scrollbar (s)

  (let ((s-ht (height s))
	(bw-ht (xlib:drawable-height (btn-win s))))
    (setf (knob-scale (scrollbar s)) 
	  (float (min 1 (/ s-ht bw-ht))))
    (setf (scroll-size (scrollbar s)) 
	  (if (<= bw-ht s-ht) 0
	    (/ (button-height s) (- bw-ht s-ht))))))

;;;--------------------------------------

(defmethod (setf items) (items (s scrolling-list))

  "removes any buttons in scrolling list s and makes new buttons with
labels from items, a list of strings."

  (mapc #'destroy (buttons s))
  (let ((button-y 0))
    (setf (buttons s)
      (mapcar #'(lambda (item)
		  (prog1
		      (make-list-button s item :ulc-y button-y)   
		    (incf button-y (button-height s))))
	      items)))
  (dolist (b (buttons s)) (init-button b s))                
  (xlib:map-subwindows (btn-win s))
  (update-scrollbar s)
  (setf (setting (scrollbar s)) *scroll-maximum*))

;;;--------------------------------------

(defmethod initialize-instance :after ((s scrolling-list)
				       &rest other-initargs 
				       &key items
				       &allow-other-keys)
  
  #| frames cannot be of height 0, so in the case that there are no
  initial items we'll set the height to 1 pixel and just let
  everthing be one pixel off (it won't be noticable) |#

  (let* ((background-color (color-gc (bg-color s) (colormap s)))
	 (background (xlib:gcontext-foreground background-color))
	 (btn-win-height))
    (setf (button-height s) (+ (font-height (font s)) 10))
    (setf (button-width s) (- (width s) *scrollbar-width*))
    (setq btn-win-height (max 1 (* (button-height s) (length items)))) 
    (setf (btn-win s) (xlib:create-window :parent (window s)
					 :x *scrollbar-width* 
					 :y 0
					 :width (button-width s)
					 :height btn-win-height
					 :depth *screen-root-depth*
					 :background background))
    (xlib:map-window (btn-win s))
    (setf (scrollbar s) (make-scrollbar *scrollbar-width* (height s)
					*scroll-minimum* *scroll-maximum*
					:parent (window s) 
					:ulc-x 0 :ulc-y 0))
    ;; as scrollbar moves down, btn-win moves up (and vice-versa)
    (ev:add-notify s (value-changed (scrollbar s)) 
		   #'(lambda (sl sb setting)
		       (declare (ignore sb))
		       (setf (xlib:drawable-y (btn-win sl)) 
			 (round (* (- (height sl) 
				      (xlib:drawable-height (btn-win sl))) 
				   (- *scroll-maximum* setting))))
		       (xlib:display-finish-output *display*)))
    (when items (setf (items s) items))))

;;;-------------------------------------

(defun make-scrolling-list (width height &rest other-initargs)

  "make-scrolling-list width height &rest other-initargs

returns an instance of a scrolling list with the specified
parameters."

  (apply #'make-instance 'scrolling-list
	 :width width :height height
	 other-initargs))

;;;--------------------------------------

(defmethod destroy :before ((s scrolling-list))

  "Destroys the buttons and scrollbar.  It is up to the caller to take
care of removing event notifications if necessary or turning buttons
off first."

  (mapc #'destroy (buttons s))
  (destroy (scrollbar s))
  (xlib:destroy-window (btn-win s)))

;;;--------------------------------------

(defun make-and-insert-list-button (s label &rest other-args)

  (let ((b (apply 'make-list-button s label other-args)))
    (insert-button b s)
    b))

;;;--------------------------------------

(defmethod insert-button ((b button) (s scrolling-list))

  "insert-button b s

inserts the button b, into the scrolling list s, at the end."

  (init-button b s)
  (setf (buttons s) (append (buttons s) (list b)))
  (setf (xlib:drawable-y (window b)) (xlib:drawable-height (btn-win s)))
  (xlib:map-window (window b))
  (incf (xlib:drawable-height (btn-win s)) (button-height s))
  (update-scrollbar s)
  (ev:announce s (inserted s) b))

;;;--------------------------------------

(defmethod delete-button ((b button) (s scrolling-list))

  "delete-button b s

deletes the button b from the scrolling list s"

  (let ((y-removed-at (xlib:drawable-y (window b))))
    (deselect-button b s)
    (setf (buttons s) (remove b (buttons s)))
    (destroy b)
    (dolist (btn (buttons s))
      (when (> (xlib:drawable-y (window btn)) y-removed-at)
	(decf (xlib:drawable-y (window btn)) (button-height s))))
    (decf (xlib:drawable-height (btn-win s)) (button-height s))
    (update-scrollbar s)
    (ev:announce s (deleted s) b)))

;;;--------------------------------------

(defmethod select-button (b (s scrolling-list))

  "select-button b s

selects button b in scrolling-list s, i.e., adds the button to the
selected button set, if not already selected."

  (if (and (member b (buttons s))
	   (not (on b)))
      (setf (on b) t)))

;;;--------------------------------------

(defmethod deselect-button (b (s scrolling-list))

  "deselect-button b s

deselects button b in scrolling-list s, i.e., removes the button from
the selected button set, if it is on, i.e., selected."

  (if (and (member b (buttons s))
	   (on b))
      (setf (on b) nil)))

;;;--------------------------------------

(defun reorder-buttons (scr btn-list)

  "reorder-buttons scr btn-list

replaces the buttons in scr with btn-list, a reordered list of the
SAME buttons, and updates the y coordinates of their windows to
reflect the new order."

  (let* ((bthgt (height (first (buttons scr))))
	 (bt-y (- bthgt)))
    (setf (buttons scr) btn-list)
    (mapc #'(lambda (bt)
	      (setf (xlib:drawable-y (window bt)) (incf bt-y bthgt)))
	  (buttons scr))))

;;;--------------------------------------

(defclass radio-scrolling-list (scrolling-list)
  
  () ;; no additional slots, just different actions for events

  (:documentation "A radio-scrolling-list is a scrolling-list with the
constraint that no more than one item can be selected at any time.")

  )

;;;------------------------------------

(defun set-radio-button (b s)

  "This function provides an action function for button-on that turns
off any others when it is turned on."

  (ev:add-notify s (button-on b)
		 #'(lambda (scr bt)
		     (setf (active bt) nil)
		     (mapc #'(lambda (other-b)
			       (when (and (on other-b)
					  (not (eq bt other-b)))
				 (setf (on other-b) nil)
				 (setf (active other-b) t)))
			   (buttons scr))
		     (ev:announce scr (selected scr) bt)))
  (ev:add-notify s (button-off b)
		 #'(lambda (scr bt)
		     (setf (active bt) t)
		     (ev:announce scr (deselected scr) bt))))

;;;--------------------------------------

(defmethod (setf items) :after (items (r radio-scrolling-list))

  (declare (ignore items))
  (mapc #'(lambda (b) (set-radio-button b r)) (buttons r)))

;;;--------------------------------------

(defun make-radio-scrolling-list (width height &rest other-initargs)

  "make-radio-scrolling-list width height &rest other-initargs

Returns an instance of a scrolling-list that is constrained to have no
more than one item selected at any time.  When an item is selected, it
deselects any other item that is selected."

  (apply #'make-instance 'radio-scrolling-list
	 :width width :height height other-initargs))

;;;------------------------------------

(defmethod insert-button :after ((b button) (s radio-scrolling-list))

  (set-radio-button b s))

;;;--------------------------------------

(defun popup-scroll-menu (items width height &rest initargs
				&key multiple font &allow-other-keys)

  "popup-scroll-menu items width height &rest initargs &key multiple

displays a scrolling list of the items, a list of strings, at a nested
event level so the user may choose one or more menu items.  If
multiple is nil, the default, then only one item can be selected and
the function returns the item number.  If multiple is not nil, then
multiple selections are allowed and the function returns a list of
item numbers.  Since a scrolling list is limited by the X window
address space, if the size of the items list is too large, a page
button is included and the list is displayed a page at a time.  The
initargs are the usual SLIK frame parameters."

  (push-event-level)
  (let* ((ft (or font *default-font*)) ;; default for frames
	 (button-height (+ (font-height ft) *linespace*))
	 (maxitems (round (/ *scrollwindow-maxsize* button-height)))
	 (listsize (length items))
	 (offset 0)
	 ;; use only a page at a time from items if necessary
	 (current-page (if (< listsize maxitems) items
			 (subseq items 0 maxitems)))
	 (scrmenu (apply (if multiple #'make-scrolling-list
			   #'make-radio-scrolling-list)
			 width height :mapped nil
			 :items current-page
			 initargs))
	 (scrmenu-win (window scrmenu))
	 (button-width (+ 10 (xlib:text-width ft "Accept")))
	 ;; compute menubox size from menu size and accept/cancel
	 ;; button sizes, and page button if needed
	 (boxwidth (max width (+ (* 2 button-width)
				 (if (< listsize maxitems) 20
				   (+ 30 button-width)))))
	 (boxheight (+ height button-height 10))
	 (menubox (apply #'make-frame boxwidth boxheight initargs))
	 (win (window menubox))
	 (left-x (round (/ (- boxwidth (* 2 button-width)
			      (if (< listsize maxitems) 10
				(+ 20 button-width)))
			   2)))
	 (ok-b (apply #'make-exit-button button-width button-height
		      :label "Accept" :parent win
		      :ulc-x left-x
		      :ulc-y (- boxheight button-height 5)
		      :bg-color 'green
		      initargs))
	 (can-b (apply #'make-exit-button button-width button-height
		       :label "Cancel" :parent win
		       :ulc-x (+ left-x button-width 10)
		       :ulc-y (- boxheight button-height 5)
		       initargs))
	 (page-b (unless (< listsize maxitems)
		   (apply #'make-button button-width button-height
			  :label "Page" :parent win
			  :ulc-x (+ left-x (* 2 (+ button-width 10)))
			  :ulc-y (- boxheight button-height 5)
			  :bg-color 'yellow
			  :button-type :momentary
			  initargs)))
	 (return-value nil))
    (ev:add-notify menubox (button-on can-b)
		   #'(lambda (box btn)
		       (declare (ignore box btn))
		       (setq return-value nil)))
    (ev:add-notify menubox (selected scrmenu)
		   #'(lambda (box scr btn)
		       (declare (ignore box))
		       ;; find out where in the list the selected
		       ;; button occurs and use that index
		       (let ((itemno (+ offset
					(position btn (buttons scr)))))
			 (if multiple (push itemno return-value)
			   (setq return-value itemno)))))
    (ev:add-notify menubox (deselected scrmenu)
		   #'(lambda (box scr btn)
		       (declare (ignore box))
		       (if multiple
			   (setq return-value
			     (remove (+ offset
					(position btn (buttons scr)))
				     return-value)))))
    (if page-b (ev:add-notify scrmenu (button-on page-b)
			      #'(lambda (s btn)
				  (declare (ignore btn))
				  ;; go to next page, or beginning of list
				  (setf offset (+ offset maxitems))
				  (if (> offset listsize)
				      (setq offset 0))
				  (setf current-page
				    (subseq items offset (min listsize
							      (+ offset
								 maxitems))))
				  ;; update scrolling list with new buttons
				  (setf (items s) current-page))))
    (xlib:reparent-window scrmenu-win win ;; center in x, at top for y
			 (round (/ (- boxwidth width) 2)) 0)
    (refresh scrmenu)
    (xlib:map-window scrmenu-win)
    (xlib:map-subwindows scrmenu-win)
    (xlib:map-window (window (scrollbar scrmenu)))
    (xlib:map-subwindows (window (scrollbar scrmenu)))
    (flush-output)
    (process-events)
    ;; don't neet remove-notify - we are destroying all the controls anyway    
    (destroy scrmenu)
    (destroy ok-b)
    (destroy can-b)
    (if page-b (destroy page-b))
    (destroy menubox)
    (pop-event-level)
    (if (listp return-value) (sort return-value #'<)
      return-value)))

;;;--------------------------------------
;;; End.
