;;;
;;; textboxes
;;;
;;; A textbox is a very simple screen-oriented editing facility, for
;;; editing multiple lines of text.
;;;
;;; 13-May-1994 J. Unger implemented
;;; 18-May-1994 I. Kalet fix default initarg for info, provide an
;;; event to notify when text changes.
;;; 19-May-1994 J. Unger add setf method for info attribute.
;;; 26-Jul-1994 J. Unger fix calls to flush-output (take no parameters).
;;; 03-Oct-1994 J. Unger add 'ENTER' as synonym for 'RETURN' key.
;;; 29-Jan-1995 I. Kalet definitions of keysym constants moved to
;;;  xlib-support, correct omission of default for info keyword
;;; 31-Jan-1996 I. Kalet delete extra values allegedly returned by
;;; keycode->character in process-hey-press.
;;; 23-Apr-1999 I. Kalet changes to support multiple colormaps.
;;; 27-May-2000 I. Kalet compute cursor x position from current text
;;; using xlib:text-width, not max, to accomodate proportional fonts.
;;; 26-Nov-2000 I. Kalet change default border-style to :lowered
;;; 11-Mar-2001 I. Kalet make default border-style adaptive to general
;;; default, if flat, make textboxes flat too, otherwise lowered.
;;; 14-Mar-2002 I. Kalet add another slot, scroll, that when nil does
;;; not allow additional lines that would be outside the visible
;;; region of the textbox.
;;;  2-Jul-2004 I. Kalet fix error in insert-line that allows new
;;; lines in the middle exceeding the space even when scroll is nil.
;;;

(in-package :slik)

;;;--------------------------------------

(defclass textbox (frame)

  ((info :type list
         :accessor info
         :initarg :info
         :documentation "A list of strings, the data being edited.")

   (new-info :type ev:event
	     :accessor new-info
	     :initform (ev:make-event)
	     :documentation "Announced whenever any text is changed,
i.e., new characters, delete characters, new line, delete line, but
not cursor motion.")

   (row-height :type fixnum
               :accessor row-height
               :documentation "The height of a line of text.")

   (row-offset :type fixnum
               :accessor row-offset
               :initarg row-offset
               :documentation "The row number of the first visible row.")

   (gc-with-font :accessor gc-with-font
                 :initform (make-duplicate-gc)
		 :documentation "A cached graphic context for drawing
in the font for this textbox instead of the default font.  Much faster
than using the with-gcontext macro.")                

   (cursor-row :type fixnum
               :accessor cursor-row
               :initarg :cursor-row
               :documentation "Cursor row number.  Updated when cursor
is moved up or down.")

   (scroll :accessor scroll
	   :initarg :scroll
	   :documentation "If nil, the text lines do not move up or
	   down and no additional lines are accepted beyond those visible.")

   )
  (:default-initargs :title "Text input" :info '("")
                     :cursor-row 0 :row-offset 0 :scroll t
		     :border-style (if (eql *default-border-style* :flat)
				       :flat :lowered))

  (:documentation "A textbox is a simple screen-oriented text editor.")
  )

;;;--------------------------------------

(defmethod (setf info) :after (new-info (tb textbox))

  "Resets the textbox to an initial state if new info is submitted to
it.  Adjusts the cursor column so that it is always at the end of the
first row.  Also erases & refreshes tb to display new info."

  (declare (ignore new-info))
  (setf (row-offset tb) 0)
  (setf (cursor-row tb) 0)
  (erase tb)
  (refresh tb))

;;;--------------------------------------

(defmethod initialize-instance :after ((tb textbox) &rest initargs)

  (declare (ignore initargs))
  (setf (row-height tb) (+ (font-height (font tb)) 6))
  (xlib:copy-gcontext (color-gc (fg-color tb) (colormap tb))
		     (gc-with-font tb))
  (setf (xlib:gcontext-font (gc-with-font tb)) (font tb))
  (setf (row-offset tb) 0)
  (setf (cursor-row tb) 0))

;;;--------------------------------------

(defun draw-textbox-cursor (tb color)

  (let* ((h (row-height tb))
         (fac (truncate h 4))
         (x (+ 11 (xlib:text-width (font tb)
				  (nth (+ (row-offset tb) (cursor-row tb))
				       (info tb)))))
	 (y (* (cursor-row tb) (row-height tb))))
    (xlib:draw-line (window tb) color x (+ y fac) x (+ y h fac))))

;;;--------------------------------------

(defmethod refresh :after ((tb textbox))

  "Draw the lines of info and the cursor."

  (let* ((win (window tb))
         (gc (gc-with-font tb))
         (h (row-height tb))
         (y h))
    (dolist (line (subseq (info tb) (row-offset tb)))
      (xlib:draw-glyphs win gc 10 y line)
      (incf y h))
    (draw-textbox-cursor tb gc)))

;;;--------------------------------------

(defun move-textbox-cursor (tb direction)

  "move-textbox-cursor tb direction

Moves textbox tb's cursor either in the specified direction, one of
:up or :down.  The cursor is placed at the end of the line to which it
is moved."

  (draw-textbox-cursor tb (color-gc (bg-color tb) (colormap tb)))
  (case direction
    (:up (if (plusp (cursor-row tb))
	     (decf (cursor-row tb))
	   (unless (zerop (row-offset tb))
	     (decf (row-offset tb))
	     (erase tb)
	     (refresh tb))))
    (:down (when (< (cursor-row tb)
		    (- (length (info tb)) (row-offset tb) 1))
	     (if (< (cursor-row tb) 
		    (- (round (/ (height tb) (row-height tb))) 2))
		 (incf (cursor-row tb))
	       (when (scroll tb)
		 (incf (row-offset tb))
		 (erase tb)
		 (refresh tb))))))
  (draw-textbox-cursor tb (color-gc (fg-color tb) (colormap tb)))
  (flush-output))

;;;--------------------------------------

(defmacro insert-at (loc item list)

  "insert-at loc item list

Inserts item into position loc in list - loc should be between 0 and
(length list) inclusive."

  `(if (zerop ,loc)
     (setf ,list (cons ,item ,list))
     (let ((end (nthcdr (1- ,loc) ,list)))
       (setf (rest end) (cons ,item (rest end))))))

;;;--------------------------------------

(defmacro delete-at (loc list)

  "delete-at loc list

Deletes the item at location loc from list."

  `(if (zerop ,loc)
     (setf ,list (rest ,list))
     (let ((end (nthcdr (1- ,loc) ,list)))
       (setf (rest end) (rest (rest end))))))

;;;--------------------------------------

(defun insert-line (tb)

  "insert-line tb

Puts a newline into textbox tb beneath the current cursor location, if
allowed, i.e., the number of lines is not limited.  If we're at the
bottom of the textbox, move all the text up a line."

  (let ((vert-line-limit (truncate (/ (height tb) (row-height tb)))))
    (when (or (scroll tb)
	      (< (length (info tb)) vert-line-limit))
      (insert-at (+ 1 (cursor-row tb) (row-offset tb)) "" (info tb))
      (if (< (cursor-row tb) (- vert-line-limit 1))
	  (incf (cursor-row tb))
	(incf (row-offset tb)))
      (erase tb)
      (refresh tb)
      (ev:announce tb (new-info tb)))))

;;;--------------------------------------

(defun delete-line (tb)

  "delete-line tb

Removes the line in textbox tb at the current cursor row; moves the
cursor to the previous row.  Does nothing if the cursor is already on
the top row."

  (when (plusp (+ (cursor-row tb) (row-offset tb)))
    (delete-at (+ (cursor-row tb) (row-offset tb)) (info tb))
    (if (plusp (cursor-row tb))
	(decf (cursor-row tb))
      (decf (row-offset tb)))
    (erase tb)
    (refresh tb)
    (ev:announce tb (new-info tb))))

;;;--------------------------------------

(defun insert-character (tb chr)

  "insert-character tb chr

Inserts a character after the cursor."

  (draw-textbox-cursor tb (color-gc (bg-color tb) (colormap tb)))
  (let ((n (+ (cursor-row tb) (row-offset tb))))
    (setf (nth n (info tb)) 
      (concatenate 'string (nth n (info tb)) (string chr))))
  (refresh tb) ;; refreshing is quick enough to do here
  (ev:announce tb (new-info tb)))

;;;--------------------------------------

(defun delete-character (tb)

  "delete-character tb

Deletes the character immediately before the cursor."

  (let ((n (+ (cursor-row tb) (row-offset tb))))
    (draw-textbox-cursor tb (color-gc (bg-color tb) (colormap tb)))
    (setf (nth n (info tb))
      (subseq (nth n (info tb)) 0 (- (length (nth n (info tb))) 1)))
    (xlib:draw-rectangle (window tb)
			(color-gc (bg-color tb) (colormap tb))
			(+ 10 (xlib:text-width (font tb) (nth n (info tb))))
			(+ (* (cursor-row tb) (row-height tb)) 6)
			(xlib:max-char-width (font tb))
			(row-height tb)
			t)
    (draw-textbox-cursor tb (color-gc (fg-color tb) (colormap tb)))
    (flush-output))
  (ev:announce tb (new-info tb)))

;;;--------------------------------------

(defmethod process-key-press ((tb textbox) code state)

  "This method finds out which key was pressed and updates the
textbox.  Graphic characters, the up/down arrows, Newline, Return,
Rubout, and Backspace are accepted."

  (let* ((keysym (xlib:keycode->keysym *display* code 0))
	 (chr (xlib:keycode->character *display* code state)))
    (cond
     ((= keysym *up-arrow-keysym*) (move-textbox-cursor tb :up))
     ((= keysym *down-arrow-keysym*) (move-textbox-cursor tb :down))
     ((or (member chr '(#\Newline #\Return))
	  (= keysym *kp-enter-keysym*))
      (insert-line tb))
     ((member chr '(#\Rubout #\Backspace))
      (if (string-equal "" (nth (+ (row-offset tb) (cursor-row tb))
				(info tb)))
	  (delete-line tb)
	(delete-character tb)))
     ((and (characterp chr) (graphic-char-p chr))
      (insert-character tb chr))))
  nil)

;;;--------------------------------------

(defun make-textbox (width height &rest other-args
		     &key (info '("")) &allow-other-keys)

  "make-textbox width height &rest other-args &key info &allow-other-keys)

Creates and returns a textbox, with initial text appearing in the textbox
window."

  (apply #'make-instance 'textbox 
	 :width width :height height :info (copy-list info)
	 other-args))

;;;--------------------------------------

(defmethod destroy :before ((tb textbox))

  (xlib:free-gcontext (gc-with-font tb)))

;;;--------------------------------------
;;; End.
