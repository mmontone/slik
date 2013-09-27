;;;
;;; 2d-plot
;;;
;;; A 2d-plot is a SLIK frame which displays a 2d-plot of data
;;;
;;; 19-Aug-1998 C. Wilcox created
;;; 14-Apr-1999 I. Kalet add labels for tick spacing boxes
;;; 23-Apr-1999 I. Kalet changes for multiple colormaps
;;;    Jun-1999 J. Zeman implement print to postscript
;;; 24=Oct-1999 I. Kalet some code format cleanup
;;; 28-May-2000 I. Kalet use Helvetica medium as small font, instead
;;; of Courier bold.
;;;

(in-package :slik)

;;;--------------------------------

(defclass 2d-plot (frame)

  ((bottom-label :type string
		 :reader bottom-label
		 :initarg :bottom-label
		 :documentation "The axis label below the plot")

   (top-label :type string
	      :reader top-label
	      :initarg :top-label
	      :documentation "The axis label above the plot")

   (left-label :type string
	       :reader left-label
	       :initarg :left-label
	       :documentation "The axis label to the left of the plot")

   (right-label :type string
		:reader right-label
		:initarg :right-label
		:documentation "The axis label to the right of the plot")

   (pad :type xlib:card16
	:reader pad
	:initarg :pad
	:documentation "The amount of space around the plot.")

   ;; define ranges to be displayed from the dataset
   (max-x-value :type number
		:accessor max-x-value
		:initarg :max-x-value
		:documentation "The maximum value plotted on the x axis.")

   (min-x-value :type number
		:accessor min-x-value
		:initarg :min-x-value
		:documentation "The minimum value plotted on the x axis.")

   (max-y-value :type number
		:accessor max-y-value
		:initarg :max-y-value
		:documentation "The maximum value plotted on the y axis.")

   (min-y-value :type number
		:accessor min-y-value
		:initarg :min-y-value
		:documentation "The minimum value plotted on the y axis.")

   (epsilon :reader epsilon
	    :initarg :epsilon
	    :documentation "The minimum allowable difference between 
corresponding max and min values.")

   ;; define distance between tick marks
   (x-units-per-tick :type number
		     :accessor x-units-per-tick
		     :initarg :x-units-per-tick
		     :documentation "The distance in x-coorinates between
tick marks.")

   (y-units-per-tick :type number
		     :accessor y-units-per-tick
		     :initarg :y-units-per-tick
		     :documentation "The distance in y-coorinates between
tick marks.")

   (tick-style  :type (member :tick :grid :none)
		:reader tick-style
		:initarg :tick-style
		:documentation "Define the way that ticks are defined.")

   ;; define the positions for slider bars in the graph
   (x-slider-val :type number
		:accessor x-slider-val
		:initarg :x-slider-val
		:documentation "This is the position of the x-coordinate
slider bar.")

   (y-slider-val :type number
		 :accessor y-slider-val
		 :initarg :y-slider-val
		 :documentation "This is the position of the y-coordinate
slider bar.")

   (new-slider-val :type ev:event
		   :accessor new-slider-val
		   :initform (ev:make-event)
		   :documentation "This is announced when the slider
bar values are updated by clicking the mouse.")

   ;; define the scale factor between the left axis labels and right
   ;; axis labels
   (x-scale-factor :reader x-scale-factor
		   :initarg :x-scale-factor
		   :documentation "The ratio of bottom units to top units.")

   (y-scale-factor :reader y-scale-factor
		   :initarg :y-scale-factor
		   :documentation "The ratio of left units to right units.")

   (redraw :accessor redraw
	   :initform t
	   :documentation "This holds the state for redrawing the plot.")

   ;; private widget slots
   (series-coll :reader series-coll
		:initarg :series-coll
		:documentation "A list of lists of pairs of numbers...")

   (widgets :accessor widgets
	    :documentation "A list of widgets to destroy when 
the plot is destroyed.")

   (notifies :accessor notifies
	     :initform nil
	     :documentation "A list of notifies to destroy when the
plot is destroyed.")

   (plot-picture :type picture
		 :accessor plot-picture
		 :initform nil
		 :documentation "This is the picture to draw the plots into.")

   )

  (:default-initargs :title "SLIK 2D Plot"
    :bottom-label "X-Axis" :top-label ""
    :left-label "Y-Axis" :right-label ""
    :pad 40
    :max-x-value 100 :min-x-value 0
    :max-y-value 100 :min-y-value 0
    :epsilon 1
    :x-units-per-tick 20
    :y-units-per-tick 20
    :tick-style :grid
    :x-scale-factor nil
    :y-scale-factor nil
    :x-slider-val 0
    :y-slider-val 0
    ;; (list (list 0 'red '(0 0) (1 20) (2 10)))
    :series-coll (coll:make-collection)
    :width 300 :height 300)

  (:documentation "A 2d-plot is designed to display multiple series
of 2d data pairs.")

  )

;;;---------------------------------------------

(defun remove-series (plot id)

  (coll:delete-element id (series-coll plot)
		       :test #'(lambda (id elem)
				 (equal id (first elem))))
  (when (redraw plot) (draw-plot-lines plot)))

;;;---------------------------------------------

(defun update-series (plot id gc series)

  (let ((current-redraw (redraw plot)))
    (setf (redraw plot) nil)
    (remove-series plot id)
    (setf (redraw plot) current-redraw)
    (coll:insert-element (list id gc series) (series-coll plot) 
			 :test #'(lambda (a b)
				   (declare (ignore a b))
				   nil))
    (when (redraw plot) (draw-plot-lines plot))))

;;;---------------------------------------------

(defun make-2d-plot (width height &rest other-initargs)

  (let* ((p (apply 'make-instance '2d-plot
		   :width width :height height other-initargs))
	 (pad (pad p))
	 (double-pad (* 2 pad))
	 (trough 5)
	 (box-width (- double-pad (* 2 trough)))
	 (box-height 25)
	 (ytick-text (make-textline box-width box-height
				    :parent (window p)
				    :numeric t
				    :upper-limit most-positive-single-float
				    :lower-limit least-positive-single-float
				    :label "Ygrid " :font helvetica-medium-12
				    :info (format nil "~s"
						  (y-units-per-tick p))
				    :ulc-x trough
				    :ulc-y (- pad trough box-height)))
	 (xtick-text (make-textline box-width box-height
				    :parent (window p) :numeric t
				    :upper-limit most-positive-single-float
				    :lower-limit least-positive-single-float
				    :label "Xgrid " :font helvetica-medium-12
				    :info (format nil "~s"
						  (x-units-per-tick p))
				    :ulc-x (- (width p)
					      (- double-pad trough))
				    :ulc-y (- (height p)
					      (- pad trough))))
	 (maxy-text (make-textline box-width box-height 
				   :parent (window p) :numeric t
				   :upper-limit most-positive-single-float
				   :lower-limit most-negative-single-float
				   :ulc-x trough :ulc-y pad))
	 (miny-text (make-textline box-width box-height 
				   :parent (window p) :numeric t
				   :upper-limit most-positive-single-float
				   :lower-limit most-negative-single-float
				   :ulc-x trough
				   :ulc-y (- (height p) pad box-height)))
	 (maxy-text2
	  (if (y-scale-factor p) 
	      (make-textline box-width box-height
			     :parent (window p) :numeric t
			     :upper-limit most-positive-single-float
			     :lower-limit most-negative-single-float
			     :ulc-x (- (width p) (- double-pad trough))
			     :ulc-y pad)
	    nil))
	 (miny-text2 
	  (if (y-scale-factor p)
	      (make-textline box-width box-height
			     :parent (window p) :numeric t
			     :upper-limit most-positive-single-float
			     :lower-limit most-negative-single-float
			     :ulc-x (- (width p) (- double-pad trough))
			     :ulc-y (- (height p) pad box-height))
	    nil))
	 (minx-text (make-textline box-width box-height 
				   :parent (window p) :numeric t
				   :ulc-x double-pad 
				   :upper-limit most-positive-single-float
				   :lower-limit most-negative-single-float
				   :ulc-y (- (height p) (- pad trough))))
	 (maxx-text (make-textline box-width box-height 
				   :parent (window p) :numeric t
				   :ulc-x (- (width p)
					     (* 2 (- double-pad trough)))
				   :upper-limit most-positive-single-float
				   :lower-limit most-negative-single-float
				   :ulc-y (- (height p) (- pad trough))))
	 (minx-text2 
	  (if (x-scale-factor p)
	      (make-textline box-width box-height 
			     :parent (window p) :numeric t
			     :ulc-x double-pad 
			     :upper-limit most-positive-single-float
			     :lower-limit most-negative-single-float
			     :ulc-y (max 0 (- pad box-height trough)))
	    nil))
	 (maxx-text2 
	  (if (x-scale-factor p)
	      (make-textline box-width box-height 
			     :parent (window p) :numeric t
			     :ulc-x (- (width p) (* 2 (- double-pad trough)))
			     :upper-limit most-positive-single-float
			     :lower-limit most-negative-single-float
			     :ulc-y (max 0 (- pad box-height trough)))
	    nil))
	 (pic (make-picture (- (width p) (* 2 double-pad))
			    (- (height p) double-pad)
			    :parent (window p)
			    :ulc-x double-pad :ulc-y pad)))
    ;; initialize textline values and keep track of
    ;; newly created widgets
    (setf (info miny-text) (min-y-value p)
	  (info maxy-text) (max-y-value p)
	  (info minx-text) (min-x-value p)
	  (info maxx-text) (max-x-value p)
	  (widgets p) (list maxy-text miny-text minx-text maxx-text 
			    xtick-text ytick-text pic))
    (when (y-scale-factor p)
      (setf (info miny-text2) (* (min-y-value p) (y-scale-factor p)))
      (setf (info maxy-text2) (* (max-y-value p) (y-scale-factor p)))
      (push maxy-text2 (widgets p))
      (push miny-text2 (widgets p)))
    (when (x-scale-factor p)
      (setf (info minx-text2) (* (min-y-value p) (x-scale-factor p)))
      (setf (info maxx-text2) (* (max-y-value p) (x-scale-factor p)))
      (push maxx-text2 (widgets p))
      (push minx-text2 (widgets p)))
    ;; assign slot for the picture
    (setf (plot-picture p) pic)
    (push (list p 'button-release pic) (notifies p))
    (ev:add-notify p (button-release pic)
		   #'(lambda (pan pic code x y)
		       (when (= code 1)	;left button
			 (let ((xmin (min-x-value p))
			       (xmax (max-x-value p))
			       (ymin (min-y-value p))
			       (ymax (max-y-value p))
			       (w    (width pic))
			       (h    (height pic)))
			   (setf (x-slider-val pan)
			     (+ xmin (* (/ x w) (- xmax xmin))))
			   (setf (y-slider-val pan)
			     (- ymax (* (/ y h) (- ymax ymin))))
			   (ev:announce pan (new-slider-val pan)
					(x-slider-val pan)
					(y-slider-val pan))
			   (draw-plot-lines pan)))))
    (let* ((busy nil)
	   (hi-low-check #'(lambda (hi low which-new)
			     (let ((thresh (epsilon p)))
			       (when (< hi (+ low thresh))
				 (if (eq which-new 0)
				     (setf hi (+ low thresh))
				   (setf low (- hi thresh))))
			       (if (eq which-new 0) hi low))))
	   (constraint-check
	    #'(lambda (boxes ratio which-new)
		;; (info (first boxes))
		;; (info (second boxes)))
		;; must ensure that (not (eq info nil)) @@@@@@@@@
		(let* ((hi1 (or (read-from-string (info (first boxes)))
				100))
		       (low1 (or (read-from-string (info (second boxes)))
				 0))
		       (hi2 (if ratio
				(or (read-from-string (info (third boxes)))
				    100)
			      0))
		       (low2 (if ratio 
				 (or (read-from-string (info (fourth boxes)))
				     0) 
			       0))
		       (vals (list hi1 low1 hi2 low2))
		       (digits (format nil "~~~df" 
				       (max 1 (floor (- (pad p) 10) 5)))))
		  ;; ensure that max > min
		  (setf (nth which-new vals)
		    (if (< which-new 2)
			(funcall hi-low-check hi1 low1 (mod which-new 2))
		      (funcall hi-low-check hi2 low2 (mod which-new 2))))
		  ;; ensure that the ratio invariant holds
		  (when ratio
		    (setf (nth (mod (+ which-new 2) 4) vals)
		      (if (< which-new 2)
			  (* (nth which-new vals) ratio)
			(/ (nth which-new vals) ratio))))
		  ;; update the text-box values
		  (setf (info (first boxes))
		    (format nil digits (first vals)))
		  (setf (info (second boxes))
		    (format nil digits (second vals)))
		  (when ratio
		    (setf (info (third boxes))
		      (format nil digits (third vals)))
		    (setf (info (fourth boxes))
		      (format nil digits (fourth vals))))
		  (list (first vals) (second vals)))))
	   (check-y #'(lambda (which)
			(when (not busy)
			  (setf busy t)
			  (let ((result
				 (funcall constraint-check
					  (list maxy-text miny-text 
						maxy-text2 miny-text2)
					  (y-scale-factor p) which)))
			    (when result
			      (setf (max-y-value p) (first result))
			      (setf (min-y-value p) (second result)))
			    (setf busy nil)))))
	   (check-x #'(lambda (which)
			(when (not busy)
			  (setf busy t)
			  (let ((result
				 (funcall constraint-check
					  (list maxx-text minx-text 
						maxx-text2 minx-text2)
					  (x-scale-factor p)  which)))
			    (when result
			      (setf (max-x-value p) (first result))
			      (setf (min-x-value p) (second result)))
			    (setf busy nil))))))
      ;; respond to changes in tick-marks
      (push (list p 'new-info ytick-text) (notifies p))
      (ev:add-notify p (new-info ytick-text)
		     #'(lambda (plot tb newval)
			 (declare (ignore tb))
			 (setf (y-units-per-tick plot) 
			   (read-from-string newval))
			 (draw-plot-lines plot)))
      (push (list p 'new-info xtick-text) (notifies p))
      (ev:add-notify p (new-info xtick-text)
		     #'(lambda (plot tb newval)
			 (declare (ignore tb))
			 (setf (x-units-per-tick plot) 
			   (read-from-string newval))
			 (draw-plot-lines plot)))
      ;; respond to changes in y scales
      (push (list p 'new-info maxy-text) (notifies p))
      (ev:add-notify p (new-info maxy-text) 
		     #'(lambda (plot tb newval) 
			 (declare (ignore tb newval))
			 (funcall check-y 0)
			 (draw-plot-lines plot)))
      (push (list p 'new-info miny-text) (notifies p))
      (ev:add-notify p (new-info miny-text) 
		     #'(lambda (plot tb newval) 
			 (declare (ignore tb newval))
			 (funcall check-y 1)
			 (draw-plot-lines plot)))
      (when (y-scale-factor p)
	(push (list p 'new-info maxy-text2) (notifies p))
	(ev:add-notify p (new-info maxy-text2)
		       #'(lambda (plot tb newval)
			   (declare (ignore tb newval))
			   (funcall check-y 2)
			   (draw-plot-lines plot)))
	(push (list p 'new-info miny-text2) (notifies p))
	(ev:add-notify p (new-info miny-text2)
		       #'(lambda (plot tb newval)
			   (declare (ignore tb newval))
			   (funcall check-y 3)
			   (draw-plot-lines plot))))
      ;; respond to changes in x scales
      (push (list p 'new-info minx-text) (notifies p))
      (ev:add-notify p (new-info minx-text) 
		     #'(lambda (plot tb newval) 
			 (declare (ignore tb newval))
			 (funcall check-x 1)
			 (draw-plot-lines plot)))
      (push (list p 'new-info maxx-text) (notifies p))
      (ev:add-notify p (new-info maxx-text) 
		     #'(lambda (plot tb newval) 
			 (declare (ignore tb newval))
			 (funcall check-x 0)
			 (draw-plot-lines plot)))
      (when (x-scale-factor p)
	(push (list p 'new-info maxx-text2) (notifies p))
	(ev:add-notify p (new-info maxx-text2)
		       #'(lambda (plot tb newval)
			   (declare (ignore tb newval))
			   (funcall check-x 2)
			   (draw-plot-lines plot)))
	(push (list p 'new-info minx-text2) (notifies p))
	(ev:add-notify p (new-info minx-text2)
		       #'(lambda (plot tb newval)
			   (declare (ignore tb newval))
			   (funcall check-x 3)
			   (draw-plot-lines plot)))))
    (push (list p 'exposure pic) (notifies p))
    (ev:add-notify p (exposure pic)
		   #'(lambda (plot pic x y width height count) 
		       (declare (ignore pic x y width height count))
		       (draw-plot-lines plot)))
    p))

;;;---------------------------------------------

(defun draw-plot-lines (p)
  
  "draw-plot-lines p

Draw the plot lines for the graph."

  (let* ((pic (plot-picture p))
	 (win (pixmap pic))
	 (cm (colormap pic))
	 (gc (color-gc (fg-color pic) cm))
	 (prevx 0.0)
	 (prevy 0.0)
	 (curx  0.0)
	 (cury  0.0)
	 (width (width pic))
	 (height (height pic))
	 ;; pixels per unit
	 (xmin (min-x-value p))
	 (ymin (min-y-value p))
	 (xppu (/ width (- (max-x-value p)
			   xmin)))
	 (yppu (/ height (- (max-y-value p)
			    ymin)))
	 (xhash (* (- (x-units-per-tick p) 
		      (mod xmin (x-units-per-tick p))) xppu))
	 (xdelta (* (x-units-per-tick p) xppu))
	 (yhash (* (- (y-units-per-tick p)
		      (mod ymin (y-units-per-tick p))) yppu))
	 (ydelta (* (y-units-per-tick p) yppu))
	 (tick-size 6))
    ;; clear the grid area and redraw the grid frame
    (xlib:draw-rectangle win (color-gc (bg-color pic) cm)
			0 0 (width pic) (height pic) t)
    (if (eq (tick-style p) :grid)
	(progn
	  (loop as i from xhash to width by xdelta do 
		(xlib:draw-line win (color-gc 'gray-dashed cm) 
			       (round i) 0 (round i) height))
	  (loop as i from yhash to height by ydelta do 
		(xlib:draw-line win (color-gc 'gray-dashed cm) 
			       0 (round (- height i))
			       width (round (- height i))))))
    (if (eq (tick-style p) :tick) 
	(progn
	  (loop as i from xhash to width by xdelta do 
		(xlib:draw-line win gc (round i) (- height tick-size)
			       (round i) height))
	  (loop as i from yhash to height by ydelta do 
		(xlib:draw-line win gc 0 (round (- height i))
			       tick-size (round (- height i))))))
    ;; draw the slider-bars
    (when (numberp (y-slider-val p))
      (let ((y-bar-pixel (- height (floor (* yppu (- (y-slider-val p)
						     ymin))))))
	(if (and (<= y-bar-pixel height) (>= y-bar-pixel 0)) 
	    (xlib:draw-line win (color-gc 'white-dashed cm)
			   0     y-bar-pixel
			   width y-bar-pixel))))
    (when (numberp (x-slider-val p))
      (let ((x-bar-pixel (floor (* xppu (- (x-slider-val p) xmin)))))
	(if (and (<= x-bar-pixel width) (>= x-bar-pixel 0))
	    (xlib:draw-line win (color-gc 'white-dashed cm)
			   x-bar-pixel 0
			   x-bar-pixel height))))
    ;; draw plot lines
    (let ((seriesgc nil)
	  (series-lst (coll:elements (series-coll p)))
	  (series nil))
      (when (listp series-lst)
	(dolist (series-rec series-lst)
	  (when series-rec
	    (setq seriesgc (color-gc (second series-rec) cm))
	    (setq series (third series-rec))
	    (setq prevx (max (min (floor (* xppu
					    (- (first (first series))
					       xmin))) 
				  32000) -32000))
	    (setq prevy (max (min (- height
				     (floor (* yppu
					       (- (second (first series))
						  ymin)))) 
				  32000) -32000))
	    (dolist (point series)
	      (setq curx (max (min (floor (* xppu (- (first point) xmin))) 
				   32000) -32000))
	      (setq cury (max (min (- height (floor (* yppu
						       (- (second point)
							  ymin)))) 
				   32000) -32000))
	      (xlib:draw-line win seriesgc prevx prevy curx cury)
	      (setq prevx curx)
	      (setq prevy cury))))))
    (xlib:draw-rectangle win gc
			0 0 (- (width pic) 1) (- (height pic) 1))
    (draw-border pic)
    (erase pic)
    (flush-output)))

;;;---------------------------------------

(defun draw-text (widget text x y &key 
				  (orientation :horizontal)
				  (justify :left)
				  (alignment :bottom))
  
  "draw horizontal or vertical text with various alignments."

  (declare (type string text)
	   (type xlib:card16 x y)
	   (type (member :left :center :right) justify)
	   (type (member :horizontal :vertical) orientation)
	   (type (member :top :center :bottom) alignment))
  (let* ((win (window widget))
	 (gc (color-gc (fg-color widget) (colormap widget)))
	 ;; (bgc (color-gc (bg-color widget) (colormap widget)))
	 (fnt (xlib:gcontext-font gc))
	 (len (length text))
	 (asc (xlib:font-ascent fnt))
	 (desc (xlib:font-descent fnt))
	 (ch-wid (xlib:max-char-width fnt))
	 (line-height (+ asc desc)))
    (if (eq orientation :horizontal)
	(progn
	  (cond
	   ((eq justify :center) (decf x (floor (xlib:text-width fnt text) 2)))
	   ((eq justify :right)  (decf x (xlib:text-width fnt text))))
	  (cond
	   ((eq alignment :top) (incf y asc))
	   ((eq alignment :center) (incf y (floor asc 2))))
	  ;; (xlib:draw-rectangle win gc
	  ;;	                 (- x 5) (- y asc 5)
	  ;;                     (+ (xlib:text-width fnt text) 10)
	  (+ asc desc 10)
	  (xlib:draw-glyphs win gc x y text))
      ;; if vertical
      (progn
	(cond
	 ((eq justify :center) (decf x (floor ch-wid 2)))
	 ((eq justify :right)  (decf x ch-wid)))
	(cond
	 ((eq alignment :top) (incf y asc))
	 ((eq alignment :center)
	  (decf y (floor (+ (* (+ asc desc) (- len 2)) desc) 2)))
	 ((eq alignment :bottom) (decf y (* (+ asc desc) (- len 1)))))
	;; (xlib:draw-rectangle win gc (- x 5) (- y asc 5)
	;;  (+ ch-wid 10) (+ 10 desc (* len line-height)))
	(dotimes (i len)
	  (xlib:draw-glyph win gc x y (char text i))
	  (incf y line-height))))))

;;;---------------------------------------

(defun draw-four-sides (p)

  (let* ((win (window p))
	 (text-pad (pad p)))
    (xlib:draw-rectangle win (color-gc (bg-color p) (colormap p))
			0 0 (width p) (height p) t)
    (when (x-scale-factor p)
      (draw-text p (top-label p)
		 (floor (width p) 2) 
		 (max 10 (floor text-pad 2)) 
		 :orientation :horizontal
		 :justify :center
		 :alignment :center))
    (draw-text p (bottom-label p) 
	       (floor (width p) 2) 
	       (- (height p) (max 10 (floor text-pad 2)))
	       :orientation :horizontal
	       :justify :center
	       :alignment :center)
    (draw-text p (left-label p) text-pad (floor (height p) 2)
	       :orientation :vertical
	       :justify :center
	       :alignment :center)
    (when (y-scale-factor p)
      (draw-text p (right-label p) (- (width p) text-pad)
		 (floor (height p) 2)
		 :orientation :vertical
		 :justify :center
		 :alignment :center))))

;;;---------------------------------------

(defmethod refresh :before ((p 2d-plot))

  "Redraws the labels for the plot."

  ;; The plot is a picture so it knows how to refresh itself 
  (draw-four-sides p))

;;;---------------------------------------

(defmethod destroy :before ((p 2d-plot))

  (dolist (n (notifies p))
    (ev:remove-notify (first n) (slot-value (third n) (second n))))
  (dolist (e (widgets p))
    (destroy e)))

;;;-----------------------------------

(defun print-2dplot (strm p width height slider)

  "print-2dplot strm p width height slider

writes PostScript output to stream strm, representing a printed
rendition of the contents of 2d-plot p, in a region of size width by
height inches, assuming a starting point, the lower left corner, has
already been defined by prior PostScript output to the stream.  Labels
are size 12 point, no matter what size the graph is.  If slider is t,
the vertical and horizontal sliding lines are included."

  (format strm "gsave  gsave~%")
  (format strm "72 72 rmoveto~%")
  ;;move to graph, draw lines
  (format strm "currentpoint translate~%")
  (format strm "~A ~A scale newpath 0 0 moveto~%" 
	  (float (/ (* 72 (- width 2))
		    (- (max-x-value p) (min-x-value p))))
	  (float (/ (* 72 (- height 2))
		    (- (max-y-value p) (min-y-value p)))))
	  
  ;;draw the grid, then the series
  (let ((repts (floor (/ (- (max-x-value p) (min-x-value p))
			 (x-units-per-tick p)))))
    (format strm "1 8 div setlinewidth~%")
    (dotimes (i (+ repts 1))
      (format strm "~A ~A moveto ~A ~A lineto stroke~%"
	      (* i (x-units-per-tick p)) 0 (* i (x-units-per-tick p)) 
	      (- (max-y-value p) (min-y-value p)))))
  (format strm "~A ~A moveto ~A ~A lineto stroke~%"
	  (- (max-x-value p) (min-x-value p)) 0
	  (- (max-x-value p) (min-x-value p)) 
	  (- (max-y-value p) (min-y-value p))) 
  (format strm "0 0 moveto~%")
  
  (let ((repts (floor (/ (- (max-y-value p) (min-y-value p))
			 (y-units-per-tick p)))))
    (dotimes (i (+ repts 1))
      (format strm "~A ~A moveto ~A ~A lineto stroke~%"
	      0 (* i (y-units-per-tick p))
	      (- (max-x-value p) (min-x-value p)) 
	      (* i (y-units-per-tick p)))))
  (format strm "~A ~A moveto ~A ~A lineto stroke~%"
	  0 (- (max-y-value p) (min-y-value p))
	  (- (max-x-value p) (min-x-value p)) 
	  (- (max-y-value p) (min-y-value p))) 
  (format strm "0 0 moveto~%")
  (format strm "~A ~A rlineto ~A ~A rlineto ~%"
	  0 (- (max-y-value p) (min-y-value p))
	  (- (max-x-value p) (min-x-value p)) 0)
  (format strm "~A ~A rlineto closepath clip newpath~%" 0 
	  (- (min-y-value p) (max-y-value p)))
  (format strm "1 4 div setlinewidth~%")
  (dolist (series (coll:elements (series-coll p)))
    (format strm "gsave~%")
    (let ((color (cadr series)))
      (cond
       ((eq color 'sl:black) (ps:set-graphics strm :color '(0 0 0)))
       ((eq color 'sl:red)   (ps:set-graphics strm :color '(1 0 0)))
       ((eq color 'sl:blue)  (ps:set-graphics strm :color '(0 0 1)))
       ((eq color 'sl:magenta) (ps:set-graphics strm :color '(.7 0 1)))
       ((eq color 'sl:green) (ps:set-graphics strm :color '(0 1 0)))
       ((eq color 'sl:white) (ps:set-graphics strm :color '(0 0 0)))
       ((eq color 'sl:yellow) (ps:set-graphics strm :color '(0 0 0)))
       ((eq color 'sl:cyan) (ps:set-graphics strm :color '(0 1 1)))
       (t  (ps:set-graphics strm :color '(.5 .5 .5)))))
    (format strm "~A ~A moveto~%" 
	    (float (caar (caddr series))) (float (cadar (caddr series))))
    (dolist (point (caddr series))
      (format strm "~A ~A lineto~%"(float (- (car point) (min-x-value p)))
	      (float (- (cadr point) (min-y-value p)))))
    (format strm "stroke grestore~%"))
  ;; print slider lines if asked for
  (if slider 
      (format strm "[1 1] 0 setdash 0 ~A moveto ~A ~A lineto stroke~%" 
	      (float (- (y-slider-val p) (min-y-value p)))
	      (float (- (max-x-value p) (min-x-value p))) 
	      (float (- (y-slider-val p) (min-y-value p)))))
  (if slider 
      (format strm "~A 0 moveto ~A ~A lineto stroke~%" 
	      (float (- (x-slider-val p) (min-x-value p)))
	      (float (- (x-slider-val p) (min-x-value p))) 
	      (float (- (max-y-value p) (min-y-value p)))))
  
  (format strm "grestore~%")
  ;;print labels around graph. have an inch on each side to do.
  (format strm "gsave currentpoint translate~%")
  (format strm "/Courier findfont 12 scalefont setfont~%")
  (format strm "72 40 moveto (~A) show~%" (bottom-label p))
  (format strm "~A 57 moveto (~,2F) show~%" 
	  (* 72 (- width 1)) (max-x-value p))
  (format strm "72 57 moveto (~,2F) show~%" (min-x-value p))
  (format strm "55 ~A moveto (~,2F) show~%" 
	  (* 72 (- height 1)) (max-y-value p))
  (format strm "40 72 moveto (~,2F) show~%" (min-y-value p))
  (format strm "72 ~A moveto (~A) show~%" 
	  (- (* 72 height) 14) (top-label p))
  (when slider
    (format strm "~A 35 moveto (X Slider Value: ~,2F) show~%" 
	    (* 72 (- width 3)) (float (x-slider-val p)))
    (format strm "~A 20 moveto (Y Slider Value: ~,2F) show~%" 
	    (* 72 (- width 3)) (float (y-slider-val p))))
  
  ;;print side labels
  (format strm "50 ~A moveto~%" (* 72 (- height 1.5)))
  (dotimes (i (length (left-label p)))
    (format strm "(~A) show 50 currentpoint exch pop 14 sub moveto~%"
	    (elt (left-label p) i))) 

  (format strm "~A ~A moveto~%"
	  (- (* 72 width) 57) (* 72 (- height 1.5)))
  (dotimes (i (length (right-label p)))
    (format strm "(~A) show ~A currentpoint exch pop 14 sub moveto~%"
	    (elt (right-label p) i)
	    (- (* 72 width) 57)))                    
  ;;print (if they exist) alternate scales 
  (if (y-scale-factor p)
      (format strm "~A ~A moveto (~,2F) show~%"
	      (* 72 (- width 1)) (- (* height 72) 87) 
	      (* (y-scale-factor p) (max-y-value p))))
  (if (x-scale-factor p)
      (format strm "~A ~A moveto (~,2F) show~%"
	      (* 72 (- width 1.5)) 
	      (- (* 72 height) 69) (* (x-scale-factor p) (max-x-value p))))

  ;;print tick scale:
  (format strm "72 22 moveto (X Units (per tick)) show~%")
  (format strm "72 8 moveto (   ~,2F) show~%" (x-units-per-tick p))
  (format strm "0 163 moveto (Y Units) show~%")
  (format strm "0 149 moveto ((per tick)) show~%")
  (format strm "0 135 moveto (~,2F) show ~%" (y-units-per-tick p)) 
  
  (format strm "grestore~%")
  );;end function, for now

;;;-----------------------------------
;;; End.









