;;;
;;; event-loop
;;;
;;; This module contains the functions and variables for the main
;;; event loop for an application that uses the SLIK toolkit.
;;;
;;; 13-Jan-1992 I. Kalet started from Mark Niehaus' controls module
;;; 05-Apr-1992 I. Kalet change process-x-event to specific event type
;;; 13-Apr-1992 I. Kalet add default methods for event processing
;;; generic functions
;;; 19-May-1992 I. Kalet add terminate function
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 22-Jun-1992 I. Kalet reorder so can compile without loading first
;;; 27-Oct-1992 I. Kalet add recursive event loop capapility by
;;; providing a window table stack and push and pop functions.
;;; 02-Mar-1993 J. Unger comment out part of make-hash-table calls.  CMU
;;; lisp seems to barf on this.
;;;  3-Jan-1995 I. Kalet take out proclaim form, increase size of
;;;  window table, but keep terminate function here.
;;;  1-Apr-1999 C. Wilcox add support for background events,
;;;  event look-ahead, and active exposure handling.
;;; 15-Jun-2000 I. Kalet cosmetic changes.
;;; 27-Aug-2003 I. Kalet add processing for :client-message X events,
;;; so that window manager destroy can be intercepted.
;;;

(in-package :slik)

;;;----------------------------------------------------------------

(defvar *window-table* (make-hash-table :test #'eq :size 1024)
  "The global variable relating objects to their windows")

(defvar *window-table-stack* nil
  "The window table stack for creating multiple levels of event
processing.")

(defvar *current-event-level* 0
  "This is the slik global variable to define the current event
level.  A level of 0 implies no event processing.  We want to
ensure the constraint that the *current-event-level* is equal to
the recursion depth of process-events.")

(defvar *background-event-queue* nil
  "This is a FIFO queue for background events.")

(defvar *active-exposure-enabled* t
  "When this is true, windows accept exposure events regardless
of whether they are in the *window-table* or buried within the
*window-table-stack*")

;;;----------------------------------------------------------------
;;; Provide default methods for all the generic functions called in
;;; the event loop below. They just return nil to continue event
;;; processing.
;;;----------------------------------------------------------------

(defmethod process-enter-notify ((obj t) x y state)

  (declare (ignore x y state))

  nil)

(defmethod process-leave-notify ((obj t) x y state)

  (declare (ignore x y state))

  nil)

(defmethod process-exposure ((obj t) x y width height count)

  (declare (ignore x y width height count))

  nil)

(defmethod process-button-press ((obj t) code x y)

  (declare (ignore code x y))

  nil)

(defmethod process-button-release ((obj t) code x y)

  (declare (ignore code x y))

  nil)

(defmethod process-motion-notify ((obj t) x y state)

  (declare (ignore x y state))

  nil)

(defmethod process-key-press ((obj t) code state)

  (declare (ignore code state))

  nil)

(defmethod process-client-message ((obj t) type format data)

  (declare (ignore type format data))

  nil)

;;;---------------------------------------------------
;;; background event support
;;;---------------------------------------------------

(defun enqueue-bg-event (event)

  "enqueue-bg-event event

adds event to the background processing queue."

  (setf *background-event-queue*
    (append *background-event-queue* (list event)))
  nil)

(defun dequeue-bg-event (compare-func)

  "dequeue-bg-event compare-func

removes event from the background processing queue."

  (setf *background-event-queue*
    (remove-if compare-func *background-event-queue*))
  nil)

;;;---------------------------------------------------

(defun process-events ()

  "process-events

Handles X events, notifying windows when need be."

  (incf *current-event-level* 1)
  (let ((my-event-level *current-event-level*))
    (loop until (< *current-event-level* my-event-level)
	do
	  (if (not (xlib:event-listen *display*))
	      (if *background-event-queue*
		  (let ((ev (pop *background-event-queue*)))
		    (eval ev))
		;; this will block until a new event arrives
		(handle-event-case))
	    (progn (look-ahead-handler)
		   (handle-event-case))))))

;;;---------------------------------------------------
;;; look-ahead-handler will check peek at the event queue to
;;; see if look-ahead is enabled for the top event and discard 
;;; all consecutive occurences of the same event except for 
;;; the last one
;;;---------------------------------------------------

(defun look-ahead-handler ()

  (let ((num-discard 0))
    (xlib:process-event *display* :discard-p nil :peek-p t :timeout 0
		       :handler
		       #'(lambda (&rest args &key event-key window
				  &allow-other-keys)
			   (let ((win (gethash window *window-table*))
				 (queue-length (xlib:event-listen *display*)))
			     ;; check to see if win is nil before
			     ;; calling look-ahead
			     (when (and queue-length win
					(find event-key (look-ahead win)))
			       (setf num-discard
				 (look-ahead-helper 0 event-key window))
			       ))
			   t))

    ;; this loop will throw away num-discard events 
    ;;  from the event queue
    (dotimes (i num-discard)
      (xlib:process-event *display* :discard-p nil :peek-p nil :timeout 0
			 :handler #'(lambda (&rest args) t)))))

;;;---------------------------------------------------
;;; look-ahead-helper returns the number of consecutive 
;;; occurrences of events on the event queue which have an event 
;;; type equal to event-symbol and a window id equal to window-id
;;;---------------------------------------------------

(defun look-ahead-helper (iter event-symbol window-id)

  (xlib:process-event 
   *display* :discard-p nil :peek-p t :timeout 0
   :handler 
   #'(lambda (&rest args &key event-key window &allow-other-keys)
       (if (and (xlib:event-listen *display*) 
		(eq event-symbol event-key)  
		(eq window-id window))
	   (look-ahead-helper (+ iter 1) event-key window)
	 iter))))

;;;-------------------------------------
;;; This function does the actual dispatching of events to be executed.
;;; If there are no events on the event queue, it will block and wait
;;; for a new event to arrive

(defun handle-event-case ()

  (xlib:event-case (*display* :discard-p nil :force-output-p nil)
		  (:enter-notify (event-window x y state)
				 (process-enter-notify 
				  (gethash event-window *window-table*)
				  x y state) t)
		  (:leave-notify (event-window x y state)
				 (process-leave-notify
				  (gethash event-window *window-table*)
				  x y state) t)
		  (:exposure (event-window x y width height count)
			     (let ((win (gethash event-window *window-table*)))
			       (when (and *active-exposure-enabled*
					  *window-table-stack* (not win))
				 (dolist (win-table *window-table-stack*)
				   (if (not win)
				       (setf win (gethash event-window
							  win-table)))))
			       (process-exposure win x y
						 width height count) t))
		  (:button-press (event-window code x y)
				 (process-button-press
				  (gethash event-window *window-table*)
				  code x y) t)
		  (:button-release (event-window code x y)
				   (process-button-release
				    (gethash event-window *window-table*)
				    code x y) t)
		  (:motion-notify (event-window x y state)
				  (process-motion-notify
				   (gethash event-window *window-table*)
				   x y state) t)
		  (:key-press (event-window code state)
			      (process-key-press
			       (gethash event-window *window-table*)
			       code state) t)
		  (:client-message (event-window type format data)
				   (process-client-message
				    (gethash event-window *window-table*)
				    type format data) t)
		  (otherwise () t)))	; just keep processing

;;;-------------------------------------

(defclass object ()

  ((window :type xlib:window
	   :accessor window))

  (:documentation "A stub class that defines an accessor function
named window.") ;; could also accomplish this with defgeneric

  )

;;;-------------------------------------

(defun register (obj)

  "register obj

Adds the object obj to the table of known objects and associated
windows, so that its process-event method will be called when an X
event occurs in its window.  The object must have a CLX window
accessible by a call to an accessor function named window."

  (unless (gethash (window obj) *window-table*)
	  (setf (gethash (window obj) *window-table*) obj)))

;;;-------------------------------------

(defun unregister (obj)

  "unregister obj

Removes obj from the table of known objects associated with X events."

  (remhash (window obj) *window-table*))

;;;-------------------------------------

(defun terminate ()

  "terminate

closes the connection to the display opened by initialize and resets
internal data structures in the SLIK package."

  (xlib:close-display *display*)
  (clrhash *window-table*)
  "SLIK display connection closed.")

;;;-------------------------------------

(defun push-event-level ()

  "push-event-level

puts the current window table on the stack and creates a new one for
an inner event processing loop."

  (push *window-table* *window-table-stack*)
  (setq *window-table* (make-hash-table :test #'eq :size 256)))

;;;-------------------------------------

(defun pop-event-level ()

  "pop-event-level

disposes of the current window table and restores the last one from
the top of the stack."

  (clrhash *window-table*)
  (setq *window-table* (pop *window-table-stack*))
  nil)

;;;-------------------------------------
;;; End.
