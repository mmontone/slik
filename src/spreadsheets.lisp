;;;
;;; spreadsheets - a first cut at a general spreadsheet facility for
;;; use in the Prism system.
;;;
;;;  1-Sep-1997 I. Kalet started from point dose panels.
;;; 24-Sep-1997 I. Kalet continuing design.
;;; 10-Dec-1997 I. Kalet move to the SLIK package.
;;; 23-Dec-1997 I. Kalet simplify and build.
;;; 27-Feb-1998 I. Kalet add 5 pixel borders and make arrow buttons a
;;; little smaller than their cell size.  Add some convenience
;;; functions.
;;; 19-Dec-1999 I. Kalet pass on initargs of spreadsheet to individual
;;; cell widgets, with cell specs superceding any duplicate initargs.
;;; 25-Apr-2000 I. Kalet add cell-object function, to access the
;;; widget of a cell, e.g., to change the fg or bg color.
;;;  4-Feb-2001 I. Kalet enforce border style :flat for readouts, and
;;; adaptive for textlines, otherwise otherargs makes it :raised
;;;  5-May-2002 I. Kalet add an announcement for button-off as well as
;;; button-on for the various button types.
;;;

(in-package :slik)

;;;---------------------------------------------

(defclass spreadsheet (frame)

  ((cells :accessor cells
	  :documentation "An array of the widgets that appear on the
spreadsheet panel, to display and modify some or all of the values.
There can be more or less or the same number of cells as values to be
controlled, but the cells are fixed in position on the panel, and the
assignment of values to cells may change during use.")

   (row-heights :accessor row-heights
		:initarg :row-heights
		:documentation "A list of row heights in pixels.")

   (col-widths :accessor col-widths
	       :initarg :col-widths
	       :documentation "A list of column widths in pixels.")

   (cell-specs :accessor cell-specs
	       :initarg :cell-specs
	       :documentation "An array of cell specifications, each
of which may be nil for an empty cell, or a list of information to be
used to create the cell at that position in the spreadsheet.  This
list contains in order, the keyword identifying the cell type, the
initial contents, and if the cell type is numeric, there should be two
additional values, the lower limit and the upper limit.")

   (user-input :accessor user-input
	       :initform (ev:make-event)
	       :documentation "Announced when any widget that can
accept user input actually receives some user input, i.e., the user
presses a button or enters a new value in a textline and presses the
RETURN key.")

   )

  (:default-initargs :title "SLIK spreadsheet")

  (:documentation "A general purpose spreadsheet facility.")

  )

;;;---------------------------------------------

(defun make-spreadsheet (row-hgts col-wds cell-specs &rest pars)

  (apply #'make-instance 'spreadsheet
	 :width (apply #'+ 10 col-wds)
	 :height (apply #'+ 10 row-hgts)
	 :row-heights row-hgts
	 :col-widths col-wds
	 :cell-specs cell-specs
	 pars))

;;;---------------------------------------------

(defmethod initialize-instance :after ((pan spreadsheet)
				       &rest initargs)

  (let* ((win (window pan))
	 (hgts (row-heights pan))
	 (wids (col-widths pan))
	 (rows (length hgts))
	 (cols (length wids))
	 (specs (cell-specs pan))
	 (cells (make-array (list rows cols) :initial-element nil))
	 (x 5)
	 (y 5))
    (setf (cells pan) cells)
    ;; go through the lists and make all the widgets
    (dotimes (i rows)
      (let ((hgt (nth i hgts))
	    (local-i i))
	(dotimes (j cols)
	  (let ((wid (nth j wids))
		(cell-spec (aref specs i j)))
	    (when cell-spec
	      (let ((cell-type (first cell-spec))
		    (init-info (second cell-spec))
		    (ll (third cell-spec))
		    (ul (fourth cell-spec))
		    (otherargs (append (nthcdr 4 cell-spec) initargs))
		    (local-j j))
		(setf (aref cells i j)
		  (case cell-type
		    (:label (apply #'make-readout wid hgt
				   :ulc-x x :ulc-y y :parent win
				   :info init-info
				   :border-width 0
				   otherargs))
		    (:readout (apply #'make-readout wid hgt
				     :ulc-x x :ulc-y y :parent win
				     :border-style :flat
				     otherargs))
		    (:text (apply #'make-textline wid hgt
				  :ulc-x x :ulc-y y :parent win
				  :border-style
				  (if (eql *default-border-style* :flat)
				      :flat :lowered)
				  otherargs))
		    (:number (apply #'make-textline wid hgt
				    :ulc-x x :ulc-y y :parent win
				    :numeric t
				    :lower-limit ll :upper-limit ul
				    :border-style
				    (if (eql *default-border-style* :flat)
					:flat :lowered)
				    otherargs))
		    (:button (apply #'make-button wid hgt
				    :label init-info
				    :ulc-x x :ulc-y y :parent win
				    otherargs))
		    (:left-arrow (apply #'make-arrow-button
					(- wid 10) (- hgt 10)
					:left
					:ulc-x (+ x 5) :ulc-y (+ y 5)
					:parent win
					otherargs))
		    (:right-arrow (apply #'make-arrow-button
					 (- wid 10) (- hgt 10)
					 :right
					 :ulc-x (+ x 5) :ulc-y (+ y 5)
					 :parent win
					 otherargs))
		    (:up-arrow (apply #'make-arrow-button
				      (- wid 10) (- hgt 10)
				      :up
				      :ulc-x (+ x 5) :ulc-y (+ y 5)
				      :parent win
				      otherargs))
		    (:down-arrow (apply #'make-arrow-button
					(- wid 10) (- hgt 10)
					:down
					:ulc-x (+ x 5) :ulc-y (+ y 5)
					:parent win
					otherargs))))
		;; the following was deferred so that init-info will
		;; not be centered in these cases
		(if (and (member cell-type '(:readout :text :number))
			 init-info)
		    (setf (info (aref cells i j)) init-info))
		(case cell-type
		  (:text
		   (ev:add-notify pan (new-info (aref cells i j))
				  #'(lambda (pnl wdgt newstuff)
				      (declare (ignore wdgt))
				      (ev:announce pnl (user-input pnl)
						   local-i local-j
						   newstuff))))
		  (:number
		   (ev:add-notify pan (new-info (aref cells i j))
				  #'(lambda (pnl wdgt newstuff)
				      (declare (ignore wdgt))
				      (ev:announce pnl (user-input pnl)
						   local-i local-j
						   (read-from-string
						    newstuff)))))
		  ((:button :left-arrow :right-arrow
		    :up-arrow :down-arrow)
		   (ev:add-notify pan (button-off (aref cells i j))
				  #'(lambda (pnl wdgt)
				      (declare (ignore wdgt))
				      (ev:announce pnl (user-input pnl)
						   local-i local-j 0)))
		   (ev:add-notify pan (button-on (aref cells i j))
				  #'(lambda (pnl wdgt)
				      (declare (ignore wdgt))
				      (ev:announce pnl (user-input pnl)
						   local-i local-j 1)))
		   (ev:add-notify pan (button-2-on (aref cells i j))
				  #'(lambda (pnl wdgt)
				      (declare (ignore wdgt))
				      (ev:announce pnl (user-input pnl)
						   local-i local-j 2)))
		   ))))
	    (incf x wid))) ;; for next widget in row
	(incf y hgt) ;; for next row
	(setf x 5) ;; start at beginning of row
	))))

;;;---------------------------------------------

(defmethod destroy :before ((pan spreadsheet))

  "Releases X resources used by this panel."

  (let ((cell-array (cells pan)))
    (dotimes (i (length (row-heights pan)))
      (dotimes (j (length (col-widths pan)))
	(let ((cell (aref cell-array i j)))
	  (if cell (destroy cell)))))))

;;;---------------------------------------------

(defun contents (sheet row col)

  "contents sheet row col

returns the contents of the widget in spreadsheet sheet at place row,
col.  If the widget is a button, the label is returned, if it is a
textline or readout, the info is returned.  Other widget types are
ignored."

  (let ((widget (aref (cells sheet) row col)))
    (cond ((typep widget 'readout) (info widget))
	  ((typep widget 'button) (label widget))
	  (t nil))))

;;;---------------------------------------------

(defun set-contents (sheet row col newval)

  "set-contents sheet row col newval

updates the contents of the widget in spreadsheet sheet at place row,
col.  The newval parameter should be a string.  If the widget is a
button, the label is updated, if it is a textline or readout, the info
is updated.  Other widget types are ignored."

  (let ((widget (aref (cells sheet) row col)))
    (cond ((typep widget 'readout)
	   (setf (info widget) newval))
	  ((typep widget 'button)
	   (setf (label widget) newval))
	  (t nil))))

;;;---------------------------------------------

(defun erase-contents (sheet row col)

  "erase-contents sheet row col

erases the readout or textline in position row, col to blank, and
resets the border color if a textline."

  (let ((tl (aref (cells sheet) row col)))
    (setf (info tl) "")
    (erase tl) ;; border width otherwise not reset right
    (when (typep tl 'textline)
      (setf (border-width tl) (border-width-cache tl))
      (setf (border-color tl) (border-color-cache tl)))))

;;;---------------------------------------------

(defun set-button (sheet row col newval)

  "set-button sheet row col newval

sets the button at row, col to on or off according as newval is
non-nil or nil."

  (setf (on (aref (cells sheet) row col)) newval))

;;;---------------------------------------------

(defun cell-object (sheet i j)

  "cell-object sheet i j

returns the actual SLIK widget in spreadsheet sheet at position i,j"

  (aref (cells sheet) i j))

;;;---------------------------------------------
;;; End.
