;;;
;;; textlines
;;;
;;; A textline is a readout in which text can be edited, like a
;;; typical command line editor - i.e., you can insert text at the
;;; cursor position, delete the character before the cursor and move
;;; the  cursor left and right on the line.  Only one line is present.
;;;
;;; 27-Apr-1992 I. Kalet started
;;; 12-May-1992 I. Kalet compute cursor x position in refresh, don't
;;; store it.  Also, don't bother keeping track of cursor position -
;;; it is always at the end of the string, so just use the length of
;;; the string.
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;;  6-Jul-1992 I. Kalet change be: to ev: and behavior to event
;;;  9-Jul-1992 I. Kalet add set-info :after method to produce
;;;  announcement specified in SLIK Programmer's Guide, but not on
;;;  every new character input.
;;; 27-Oct-1992 I. Kalet eliminate pixmap, add flush-output to setf
;;; info
;;; 12-Feb-1993 I. Kalet accept #\backspace as well as #\rubout,
;;; discard other control characters, accept only graphic characters.
;;; 13-May-1994 I. Kalet add facility for input error checking if
;;; restricted to numbers.
;;; 28-Jun-1994 I. Kalet check for empty string also.
;;; 11-Sep-1994 J. Unger add facility for border color change to indicate
;;; volatile information in textlines.  Also add 'ENTER' as synonym for
;;; 'RETURN' key.
;;; 18-Oct-1994 J. Unger trap blank string input into numeric textline
;;; - would cause an error, as would backslash at end of input.
;;; 27-Dec-1994 J. Unger trap colon typed into numeric textline.
;;;  3-Jan-1995 I. Kalet make kp-enter-keysym a global in xlib-support
;;;  2-Oct-1995 I. Kalet use ignore-error to trap more input hash in
;;;  numeric textlines.
;;; 25-Apr-1997 I. Kalet add popup-textline here to avoid circularity
;;; with dialogboxes (added more documentation 3-May-1997).
;;; 22-Jun-1997 I. Kalet add button-2 clear function in textline.
;;; 31-May-1998 I. Kalet take out multiple-value-bind in
;;; process-key-press.  The xlib functions do NOT return multiple
;;; values except in VAXLISP.
;;; 26-Nov-2000 I. Kalet make default border-style lowered, and fix
;;; some exit buttons to better match the other defaults.
;;; 11-Mar-2001 I. Kalet make default border-style adaptive to general
;;; default, if flat, make textlines flat too, otherwise lowered.
;;; 15-Feb-2003 I. Kalet make popup-textline correctly handle the case
;;; where the specified width is too small - just enlarge it.
;;;

(in-package :slik)

;;;-------------------------------------

(defclass textline (readout)

  ((cursor-y1 :type xlib:card16
	      :accessor cursor-y1
	      :documentation "Cursor upper y coordinate - computed
only initially, when font is chosen.")

   (cursor-y2 :type xlib:card16
	      :accessor cursor-y2
	      :documentation "Cursor lower y coordinate.")

   (new-info :type ev:event
	     :accessor new-info
	     :initform (ev:make-event)
	     :documentation "Announced when the user presses the
RETURN key on the keyboard while the textline has the input focus.")

   (numeric :accessor numeric
	    :initarg :numeric
	    :documentation "True if input is restricted to form a
valid number")

   (lower-limit :accessor lower-limit
		:initarg :lower-limit
		:documentation "The lowest numeric value accepted if
numeric input is required.")

   (upper-limit :accessor upper-limit
		:initarg :upper-limit
		:documentation "The highest numeric value accepted if
numeric input is required.")

   (volatile-color :type symbol
                   :accessor volatile-color
                   :initarg :volatile-color
                   :documentation "The color to turn the border when
the textline is volatile.")

   (volatile-width :type fixnum
                   :accessor volatile-width
                   :initarg :volatile-width
                   :documentation "The width to make the border when
the textline is volatile.")

   (border-color-cache :type symbol
                       :accessor border-color-cache
                       :documentation "The saved border color while the 
textline is volatile.")

   (border-width-cache :type fixnum
                       :accessor border-width-cache
                       :documentation "The saved border width while the
textline is volatile.")

   )

  (:default-initargs :title "Text input" :cursor 0 :numeric nil
                     :volatile-color 'red :volatile-width 2
		     :border-style (if (eql *default-border-style* :flat)
				       :flat :lowered))

  (:documentation "A textline displays and allows the user to edit a
line of text in the window.  By default the text is vertically
centered and starts 10 pixels in from the left.  If numeric is true,
when the user presses the RETURN or NEWLINE key the text is checked
for validity and cleared, with an acknowledge message, if not valid.")

  )

;;;----------------------------------------

(defmethod initialize-instance :after ((tl textline) &rest initargs)

  (declare (ignore initargs))

  (let ((fh (font-height (font tl))))
    (setf (cursor-y1 tl) (- (info-y tl) fh)
	  (cursor-y2 tl) (+ (cursor-y1 tl) fh 4)))
  (setf (border-color-cache tl) (border-color tl)
	(border-width-cache tl) (border-width tl))
  (unless (volatile-color tl) ;; if nil, no change to bdr when keys pressed
    (setf (volatile-color tl) (border-color tl)
	  (volatile-width tl) (border-width tl))))

;;;----------------------------------------

(defun make-textline (width height &rest other-initargs)

  "MAKE-TEXTLINE width height &rest other-initargs

Returns a textline with the specified parameters.  If the info
parameter is provided it is centered as well as possible.  This
function relies on the initialization mechanisms of the readout."

  (let ((tl (apply 'make-instance 'textline
		   :width width :height height other-initargs)))
    (refresh tl)
    tl))

;;;----------------------------------------

(defun draw-text-cursor (tl)

  "This function draws the cursor.  The cursor x position is computed
here, from the current info value."

  (let ((x1 (+ (info-x tl) (xlib:text-width (font tl) (info tl)) 1)))
    (xlib:draw-line (window tl) (gc-with-font tl)
		   x1 (cursor-y1 tl) x1 (cursor-y2 tl))))

;;;----------------------------------------

(defmethod (setf info) :after (new-info (tl textline))

  "This method adds the cursor.  The readout method writes the
text and background so this has to happen afterward."

  (declare (ignore new-info))
  (draw-text-cursor tl)
  (flush-output))

;;;----------------------------------------

(defmethod refresh :after ((tl textline))

  "This method adds the cursor.  The readout method writes the
text and background so this has to happen afterward."

  (draw-text-cursor tl))

;;;----------------------------------------

(defmethod process-button-press ((tl textline) code x y)

  "clears the contents of tl if button number 2 pressed"

  (declare (ignore x y))
  (when (= code 2)
    (setf (info tl) "")
    (setf (border-color tl) (volatile-color tl))
    (setf (border-width tl) (volatile-width tl)))
  nil) ;; needed to continue processing

;;;----------------------------------------

(defmethod process-key-press ((tl textline) code state)

  "This method finds out which key was pressed and updates the info
slot.  Characters can only be added or deleted at the end of the
string for now.  Only graphic characters are accepted, and control
characters are discarded, except for Newline, Return, Rubout and
Backspace."

  (let* ((text (info tl))
	 (count (length text))
	 (chr (xlib:keycode->character *display* code state)))
    ;; The ENTER key is not a standard Common Lisp character but we
    ;; would like to recognize it as a synonym for #\return.  So
    ;; just set the resulting chr to #\return if the keypad ENTER
    ;; key was pressed.
    (when (= *kp-enter-keysym* (xlib:keycode->keysym *display* code 0))
      (setq chr #\return))
    (case chr
      ((#\newline #\return) ;; check input if needed, update border
       (if (numeric tl)
	   (let ((result
		  ;; trap anything unreadable - in that case
		  ;; ignore-errors returns nil
		  (ignore-errors (read-from-string (info tl))))
		 (ll (lower-limit tl))
		 (ul (upper-limit tl)))
	     (if (and (numberp result)
		      (<= result ul)
		      (>= result ll))
		 (progn
		   (erase tl) ;; border width otherwise not reset right
		   (setf (border-width tl) (border-width-cache tl))
		   (setf (border-color tl) (border-color-cache tl))
		   (ev:announce tl (new-info tl) (info tl)))
	       (progn (acknowledge
		       (list "Please enter a number"
			     (format nil "between ~A and ~A" ll ul)))
		      (setf (info tl) ""))))
	 (progn
	   (erase tl) ;; border width otherwise not reset right
	   (setf (border-width tl) (border-width-cache tl))
	   (setf (border-color tl) (border-color-cache tl))
	   (ev:announce tl (new-info tl) (info tl)))))
      ((#\rubout #\backspace) ;; erase last character
       (when (> count 0)
	 (setf (border-color tl) (volatile-color tl))
	 (setf (border-width tl) (volatile-width tl))
	 (setq count (1- count))
	 (setf (info tl)
	   (if (> count 0) (subseq text 0 count)
	     ""))))
      (otherwise
       (if (and (characterp chr)
		(graphic-char-p chr))
	   (setf
	       (border-color tl) (volatile-color tl)
	       (border-width tl) (volatile-width tl)
	       (info tl) (concatenate 'string text (string chr)))))))
  nil)

;;;--------------------------------------

(defun popup-textline (info width &rest initargs
		       &key font &allow-other-keys)

  "popup-textline info width &rest initargs &key font &allow-other-keys

Pops up a textline, of the specified width, at a nested event level.
The info parameter is a string to initially appear in the textline as
a default.  It can be an empty string.  The initargs are the other
parameters suitable to the textline, and the height is determined from
the font.  The text and the label if supplied always start 10 pixels
from the left, even if info is supplied.  When the Accept button is
pressed, returns the string representing the edited text.  If the
Cancel button is pressed, returns nil."

  (push-event-level)
  (let* ((ft (or font *default-font*))
	 (height (+ (font-height ft) 10))
	 (button-width (+ 10 (xlib:text-width ft "Accept")))
	 (padded-width (max width (+ (* 2 button-width) 10)))
	 (frm (apply #'make-frame
		     padded-width (+ (* 2 height) 5)
		     initargs))
         (frm-win (window frm))
         (tl (apply #'make-textline padded-width height
		    :parent frm-win initargs))
	 (left-x (round (/ (- padded-width (* 2 button-width) 10) 2)))
         (acc-b (apply #'make-exit-button button-width height
		       :label "Accept" :parent frm-win
		       :ulc-x left-x
		       :ulc-y (+ height 5)
		       :bg-color 'green
		       initargs))
	 (can-b (apply #'make-exit-button button-width height
		       :label "Cancel" :parent frm-win
		       :ulc-x (- padded-width button-width left-x)
		       :ulc-y (+ height 5)
		       initargs))
	 (return-value nil))
    (setf (info tl) info)
    (ev:add-notify frm (button-on can-b)
		   #'(lambda (fr btn)
		       (declare (ignore fr btn))
		       (setq return-value nil)))
    (ev:add-notify tl (button-on acc-b)
		   #'(lambda (tln btn)
		       (declare (ignore btn))
		       (setq return-value (info tln))))
    (xlib:map-window frm-win)
    (xlib:map-subwindows frm-win)
    (flush-output)
    (process-events)
    (destroy tl)
    (destroy acc-b)
    (destroy can-b)
    (destroy frm)
    (pop-event-level)
    return-value))

;;;--------------------------------------
;;; End.
