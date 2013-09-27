;;;
;;; dialogboxes
;;;
;;; This module implements some simple dialog boxes using the SLIK
;;; facility for nested event processing loops.  Thus the dialog box
;;; waits for user input, and events that happen in all the other
;;; application windows are ignored and discarded.  Note that this
;;; applies only to windows created by the current SLIK application,
;;; not to other processes that have their own connection to the
;;; display, i.e. other terminal windows, other applications running
;;; on the same display...
;;;
;;; 28-Oct-1992 I. Kalet created
;;; 15-Feb-1993 I. Kalet add popup-color-menu
;;;  3-Aug-1993 I. Kalet add invisible to popup-color-menu
;;; 16-May-1994 J. Unger add popup-textbox function.
;;;  3-Jan-1995 I. Kalet move exit-button here because it uses
;;;  confirm, move popup-scroll-menu to scrolling-lists to undo
;;;  circularity there.
;;; 25-Apr-1997 I. Kalet cosmetics, also add popup-textline to
;;; textlines, not here, to avoid circularity in dependency graph
;;; 10-Apr-1999 C. Wilcox changed exit-button to work with the
;;;  new event-handling code.
;;; 23-Apr-1999 I. Kalet changes to support multiple colormaps.
;;; 26-Nov-2000 I. Kalet some cosmetic changes to match default gray
;;; backgrounds of other widgets and default border styles.
;;; 21-Jun-2004 I. Kalet add default selection input to popup-menu
;;;

(in-package :slik)

;;;--------------------------------------

(defparameter *ack-label* "Acknowledge")
(defparameter *proc-label* "Proceed")
(defparameter *can-label* "Cancel")

;;;--------------------------------------

(defclass exit-button (button)

  ((confirm-exit :accessor confirm-exit
		 :initarg :confirm-exit
		 :documentation "If not nil, should be a string or
list of strings that will be used as a confirmation message.  This
specifies that the exit button should confirm before terminating. ")

   )

  (:default-initargs :title "Exit button" :button-type :momentary
		     :label "EXIT" :bg-color 'red :confirm-exit nil)

  (:documentation "A pre-made button that returns t instead of nil.")

  )

;;;--------------------------------------

(defun make-exit-button (width height &rest other-initargs)

  (apply 'make-instance 'exit-button
	 :width width :height height other-initargs))

;;;--------------------------------------

(defmethod process-button-release ((b exit-button) code x y)

  (declare (ignore x y))
  (when (and (active b)
	     (on b)
	     (= code 1)) ;; left button
    (setf (on b) nil)

    (when (not (and (confirm-exit b) (not (confirm (confirm-exit b)))))
      (decf *current-event-level* 1))
    t))

;;;--------------------------------------

(defclass message-box (frame)

  ((message :reader message
	    :initarg :message
	    :documentation "The list of strings, one string per line,
that is the contents of the message box.")

   (ack-button :accessor ack-button
	       :documentation "The exit button that says Acknowledge
and returns from the event loop when pressed.")

   )

  (:default-initargs :title "Message")
  )

;;;--------------------------------------

(defmethod refresh ((mb message-box))

  "redraws the message in the message box - the button takes care of
itself."

  (let* ((item-height (+ (font-height (font mb)) 5))
	 (y 0))
    (dolist (line (message mb))
      (setq y (+ y item-height))
      (xlib:draw-glyphs (window mb)
		       (color-gc (fg-color mb) (colormap mb))
		       5 y line))))

;;;--------------------------------------

(defmethod initialize-instance :after ((mb message-box)
				       &rest initargs)

  (let* ((win (window mb))
	 (width (width mb))
	 (height (height mb))
	 (ft (font mb))
	 (button-width (+ 10 (xlib:text-width ft *ack-label*)))
	 (button-height (+ (font-height ft) *linespace*)))
    (setf (ack-button mb)
      (apply #'make-exit-button button-width button-height
	     :label *ack-label* :parent win
	     :ulc-x (round (/ (- width button-width) 2))
	     :ulc-y (- height button-height 5)
	     initargs))
    (refresh mb)))

;;;--------------------------------------

(defun acknowledge (message &rest initargs &key font &allow-other-keys)

  "acknowledge message &rest initargs

creates a message box for message, a string or list of strings,
together with an Acknowledge button.  Waits for the user to press the
Acknowledge button, then returns nil.  Any other events for windows in
the same display connection are discarded."

  (push-event-level)
  (unless (listp message) (setq message (list message)))
  (let* ((ft (or font *default-font*))
	 (width (apply 'max
		       (+ 10 (xlib:text-width ft *ack-label*))
		       (mapcar #'(lambda (item)
				   (xlib:text-width ft item))
			       message)))
	 (item-height (+ (font-height ft) *linespace*))
	 (mbox (apply #'make-instance 'message-box
		      :width (+ width 10)
		      :height (+ (* (length message) item-height)
				 item-height ; for button
				 10)	; space between text and button
		      :message message
		      initargs)))
    (process-events)
    (destroy (ack-button mbox))
    (destroy mbox))
  (pop-event-level))

;;;--------------------------------------

(defclass confirm-box (frame)

  ((message :reader message
	    :initarg :message
	    :documentation "The list of strings, one string per line,
that is the contents of the message box.")

   (proc-button :accessor proc-button
		:documentation "The exit button that says Proceed and
returns t from the function when pressed.")

   (can-button :accessor can-button
	       :documentation "The exit button that says Cancel and
returns nil from the function when pressed.")

   (return-value :accessor return-value
		 :documentation "Set by whichever button is pressed.")

   )

  (:default-initargs :title "Confirmation")
  )

;;;--------------------------------------

(defmethod refresh ((mb confirm-box))

  "exactly like the message box, redraws the message.  The buttons
take care of themselves."

  (let* ((item-height (+ (font-height (font mb)) 5))
	 (y 0))
    (dolist (line (message mb))
	    (setq y (+ y item-height))
	    (xlib:draw-glyphs (window mb)
			     (color-gc (fg-color mb) (colormap mb))
			     5 y line))))

;;;--------------------------------------

(defmethod initialize-instance :after ((mb confirm-box)
				       &rest initargs)

  (let* ((win (window mb))
	 (width (width mb))
	 (height (height mb))
	 (ft (font mb))
	 (button-width (+ 10 (xlib:text-width ft *proc-label*)))
	 (button-height (+ (font-height ft) *linespace*))
	 (left-x (round (/ (- width (* 2 button-width) 10) 2))))
    (setf (proc-button mb)
      (apply #'make-exit-button button-width button-height
	     :label *proc-label* :parent win
	     :ulc-x left-x
	     :ulc-y (- height button-height 5)
	     :bg-color 'green
	     initargs))
    (setf (can-button mb)
      (apply #'make-exit-button button-width button-height
	     :label *can-label* :parent win
	     :ulc-x (- width button-width left-x)
	     :ulc-y (- height button-height 5)
	     initargs))
    (ev:add-notify mb (button-on (proc-button mb))
		   #'(lambda (box btn)
		       (declare (ignore btn))
		       (setf (return-value box) t)))
    (ev:add-notify mb (button-on (can-button mb))
		   #'(lambda (box btn)
		       (declare (ignore btn))
		       (setf (return-value box) nil)))
    (refresh mb)))

;;;--------------------------------------

(defun confirm (message &rest initargs
			&key font &allow-other-keys)

  "confirm message &rest initargs

creates a confirm box for message, a string or list of strings,
together with a proceed button and a cancel button.  Waits for the
user to press either button, then returns t if proceed was pressed, or
nil if cancel was pressed.  Any other events for windows in the same
display connection are discarded."

  (push-event-level)
  (unless (listp message) (setq message (list message)))
  (let* ((ft (or font *default-font*))
	 (width (apply 'max
		       (+ (* 2 (xlib:text-width ft *proc-label*))
			  30) ;; 10 for each button, and 10 between
		       (mapcar #'(lambda (item)
				   (xlib:text-width ft item))
			       message)))
	 (item-height (+ (font-height ft) *linespace*))
	 (mbox (apply #'make-instance 'confirm-box
		      :width (+ width 10)
		      :height (+ (* (length message) item-height)
				 item-height ; for buttons
				 10)	; space between text and buttons
		      :message message
		      initargs))
	 (result nil))
    (process-events)
    (destroy (proc-button mbox))
    (destroy (can-button mbox))
    (setq result (return-value mbox))
    (destroy mbox)
    (pop-event-level)
    result))

;;;--------------------------------------

(defun popup-menu (items &rest initargs
			 &key multiple default &allow-other-keys)

  "popup-menu items &rest initargs &key multiple

displays a menu of the items, a list of strings, at a nested event
level so the user must choose one or more menu items.  If multiple is
nil, the default, then only one item can be selected and the function
returns the item number.  If multiple is not nil, then multiple
selections are allowed and the function returns a list of item
numbers.  The initargs are the usual SLIK frame parameters."

  (push-event-level)
  (let* ((pmenu (apply (if multiple #'make-menu #'make-radio-menu)
		       items :mapped nil initargs))
	 (pmenu-width (width pmenu))
	 (pmenu-win (window pmenu))
	 (ft (font pmenu)) ;; Should buttons be same font as menu???
	 (button-width (+ 10 (xlib:text-width ft "Accept")))
	 (button-height (+ (font-height ft) *linespace*))
	 ;; compute menubox size from menu size and accept/cancel
	 ;; button sizes
	 (width (max pmenu-width (+ (* 2 button-width) 20)))
	 (height (+ (height pmenu) button-height 10))
	 (menubox (apply #'make-frame width height initargs))
	 (win (window menubox))
	 (left-x (round (/ (- width (* 2 button-width) 10) 2)))
	 (ok-b (apply #'make-exit-button button-width button-height
		      :label "Accept" :parent win
		      :ulc-x left-x
		      :ulc-y (- height button-height 5)
		      :bg-color 'green
		      initargs))
	 (can-b (apply #'make-exit-button button-width button-height
		       :label *can-label* :parent win
		       :ulc-x (- width button-width left-x)
		       :ulc-y (- height button-height 5)
		       initargs))
	 (return-value nil))
    (ev:add-notify menubox (button-on can-b)
		   #'(lambda (box btn)
		       (declare (ignore box btn))
		       (setq return-value nil)))
    (ev:add-notify menubox (selected pmenu)
		   #'(lambda (l a item)
		       (declare (ignore l a))
		       (if multiple (push item return-value)
			 (setq return-value item))))
    (ev:add-notify menubox (deselected pmenu)
		   #'(lambda (l a item)
		       (declare (ignore l a))
		       (if multiple (setq return-value
				      (remove item return-value)))))
    (xlib:reparent-window pmenu-win win
			 (round (/ (- width pmenu-width) 2))
			 0)		; center in x, at top for y
    (xlib:map-window pmenu-win)
    (xlib:map-subwindows pmenu-win)
    (when default (select-button default pmenu))
    (flush-output)
    (process-events)
    ;; don't neet to ev:remove-notify since we are
    ;; destroying all the controls anyway
    (destroy pmenu)
    (destroy ok-b)
    (destroy can-b)
    (destroy menubox)
    (pop-event-level)
    return-value))

;;;--------------------------------------

(defun popup-color-menu (&rest initargs)

  "popup-color-menu

displays a menu of SLIK named colors, at a nested event level so the
user must choose one of the colors.  No more than one color can be
selected and the function returns the symbol in the SLIK package for
that color.  If the cancel button is pressed, the function returns
NIL."

  (let* ((color-list '(red green blue yellow magenta cyan white black
		       gray invisible))
	 (menu-list (mapcar #'symbol-name color-list))
	 (selection (apply #'popup-menu menu-list initargs)))
    (if selection (nth selection color-list)))) ;; otherwise nil

;;;--------------------------------------

(defun popup-textbox (info width height &rest initargs)

  "popup-textbox info width height &rest initargs

Pops up a textbox, of the specified width and height, at a nested
event level.  The info parameter is a list of strings to initially
appear in the textbox.  When the Accept button is pressed, returns a
list of strings representing the edited text.  If the Cancel button is
pressed, returns nil."

  (push-event-level)
  (let* ((frm (apply #'make-frame width height initargs))
         (frm-win (window frm))
         (tb (apply #'make-textbox width (- height 40)
		    :parent frm-win :info info initargs))
         (ft (font tb))
	 (button-width (+ 10 (xlib:text-width ft "Accept")))
	 (button-height 30)
	 (left-x (round (/ (- width (* 2 button-width) 10) 2)))
         (acc-b (apply #'make-exit-button button-width button-height
		       :label "Accept" :parent frm-win
		       :ulc-x left-x
		       :ulc-y (- height button-height 5)
		       :bg-color 'green
		       initargs))
	 (can-b (apply #'make-exit-button button-width button-height
		       :label "Cancel" :parent frm-win
		       :ulc-x (- width button-width left-x)
		       :ulc-y (- height button-height 5)
		       initargs))
	 (return-value nil))
    (ev:add-notify frm (button-on can-b)
		   #'(lambda (box btn)
		       (declare (ignore box btn))
		       (setq return-value nil)))
    (ev:add-notify tb (button-on acc-b)
		   #'(lambda (box btn)
		       (declare (ignore box btn))
		       (setq return-value (info tb))))
    (xlib:map-window frm-win)
    (xlib:map-subwindows frm-win)
    (flush-output)
    (process-events)
    (destroy tb)
    (destroy acc-b)
    (destroy can-b)
    (destroy frm)
    (pop-event-level)
    return-value))

;;;--------------------------------------
;;; End.
