;;;
;;; initialize - contains the SLIK initialize function and its
;;;associated details.  In a separate file to avoid circular
;;;dependencies with OpenGL support (initialize calls load-gl).
;;;
;;;  5-Aug-2004 I. Kalet split off from xlib-support.  Note that
;;; initialize loads GL libraries but does not depend on any functions
;;; in them.  The library locations are now configurable variables
;;; instead of constants.
;;;  3-Jul-2006 I. Kalet change to new location of X libraries for
;;; Debian and X.org
;;;  4-Jan-2009 I. Kalet remove OpenGL library load, move out of SLIK
;;;

(in-package :slik)

;;;--------------------------------------------

(defun open-named-fonts ()

  (setq courier-bold-12
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*courier*bold-r*")))
	courier-bold-14
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*courier*bold-r*")))
	courier-bold-18
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*courier*bold-r*")))
	times-bold-12
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*times*bold-r*")))
	times-bold-14
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*times*bold-r*")))
	times-bold-18
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*times*bold-r*")))
	helvetica-medium-12
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*helvetica*medium-r*")))
	helvetica-medium-14
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*helvetica*medium-r*")))
	helvetica-medium-18
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*helvetica*medium-r*")))
	helvetica-bold-12
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*helvetica*bold-r*")))
	helvetica-bold-14
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*helvetica*bold-r*")))
	helvetica-bold-18
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*helvetica*bold-r*")))
	schoolbook-bold-12
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*schoolbook*bold-r*")))
	schoolbook-bold-14
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*schoolbook*bold-r*")))
	schoolbook-bold-18
	(xlib:open-font *display*
		       (first (xlib:list-font-names
			       *display* "*schoolbook*bold-r*")))
	)
  (setq *default-font* (symbol-value *default-font-name*)))

;;;--------------------------------------------

(defun make-primary-gc (colormap)

  "make-primary-gc colormap

Creates the graphic contexts for the primary colors, to save
performance on drawing in different colors."

  (let ((tmp-black (xlib:alloc-color
		    colormap
		    (xlib:make-color :red 0.0 :green 0.0 :blue 0.0)))
	(tmp-white (xlib:alloc-color
		    colormap
		    (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))))
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap 
				       (xlib:make-color
					:red 1.0 :green 0.0 :blue 0.0))
			  :background tmp-black))
	  red)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color
					:red 0.0 :green 1.0 :blue 0.0))
			  :background tmp-black))
	  green)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color
					:red 0.0 :green 0.0 :blue 1.0))
			  :background tmp-black))
	  blue)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color
					:red 1.0 :green 0.0 :blue 1.0))
			  :background tmp-black))
	  magenta)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color
					:red 0.0 :green 1.0 :blue 1.0))
			  :background tmp-black))
	  cyan)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color
					:red 1.0 :green 1.0 :blue 0.0))
			  :background tmp-black))
	  yellow)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground tmp-white
			  :background tmp-black))
	  white)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground tmp-black
			  :background tmp-white))
	  black)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 2
			  :foreground tmp-black
			  :background tmp-white))
	  black2)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color
					:red 0.5 :green 0.5 :blue 0.5))
			  :background tmp-black))
	  gray)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color :red *fg-level*
						       :green *fg-level*
						       :blue *fg-level*))
			  :background tmp-black))
	  default-fg)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color :red *bg-level*
						       :green *bg-level*
						       :blue *bg-level*))
			  :background tmp-black))
	  default-bg)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :font *default-font*
			  :function boole-2 ; signifies DST only, or NO-OP
			  :foreground tmp-white
			  :background tmp-black))
	  invisible)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap 
				       (xlib:make-color 
					:red 1.0 :green 0.0 :blue 0.0))
			  :background tmp-black))
	  red-dashed)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color 
					:red 0.0 :green 1.0 :blue 0.0))
			  :background tmp-black))
	  green-dashed)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color 
					:red 0.0 :green 0.0 :blue 1.0))
			  :background tmp-black))
	  blue-dashed)
    (push (list colormap (xlib:create-gcontext
			  :line-style :dash
			  :drawable *screen-root*
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color 
					:red 1.0 :green 0.0 :blue 1.0))
			  :background tmp-black))
	  magenta-dashed)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color 
					:red 0.0 :green 1.0 :blue 1.0))
			  :background tmp-black))
	  cyan-dashed)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color 
					:red 1.0 :green 1.0 :blue 0.0))
			  :background tmp-black))
	  yellow-dashed)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground tmp-white
			  :background tmp-black))
	  white-dashed)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground tmp-black
			  :background tmp-white))
	  black-dashed)
    (push (list colormap (xlib:create-gcontext
			  :drawable *screen-root*
			  :line-style :dash
			  :font *default-font*
			  :line-width 1
			  :foreground (xlib:alloc-color
				       colormap
				       (xlib:make-color 
					:red 0.5 :green 0.5 :blue 0.5))
			  :background tmp-black))
	  gray-dashed)))

;;;--------------------------------------------

(defun assign-gray-pixels (colormap num-pixels)

  "assign-gray-pixels colormap num-pixels

Requests num-pixels gray scale values from the colormap for shared use
and assigns them to a color table, which is returned.  This table is
indexed from 0 through num-pixels - 1.  Its entries are the pixel
values in the colormap corresponding to the allocated gray scale
values (which do not necessarily start at colormap entry 0 or are
necessarily contiguous)."

  (let ((val 0.0)
        (inc (float (/ (1- num-pixels))))
	(gray-pixels (make-array num-pixels :element-type 'xlib:pixel)))
    (declare (single-float val inc)
	     (fixnum num-pixels))
    (dotimes (i num-pixels gray-pixels)
      (setf (aref gray-pixels i)
        (xlib:alloc-color colormap
                         (xlib:make-color :red val :green val :blue val)))
      (incf val inc))))

;;;--------------------------------------------

(defun initialize (&optional (host *host*) (alloc-gray t))

  "initialize &optional (host *host*) (alloc-gray t)

Opens the display on specified host, sets the global variables for the
toolkit (including *host*), allocates a bunch of gray levels in the
screen default colormap unless disabled by providing a nil value for
alloc-gray, and returns T if successful."

  (let* ((colon-pos (position #\: host))
	 (hostname (subseq host 0 colon-pos))
	 (disp-no (if colon-pos
		      (let ((remainder (subseq host (1+ colon-pos))))
			(read-from-string (subseq remainder 0
						  (position #\. remainder))))
		    0)))
    (if host (setq *host* hostname))
    (if (setq *display* (xlib:open-display hostname :display disp-no))
	(progn (setq *screen* (xlib:display-default-screen *display*))
	       (setq *screen-default-colormap*
		 (xlib:screen-default-colormap *screen*))
	       (setq *screen-root* (xlib:screen-root *screen*))
	       (setq *screen-root-depth* (xlib:screen-root-depth
					  *screen*))
	       (setq *image-bits-per-pixel*
		 (xlib:pixmap-format-bits-per-pixel
		  (find *screen-root-depth*
			(xlib:display-pixmap-formats *display*)
			:key #'xlib:pixmap-format-depth)))
	       (open-named-fonts)
	       (make-primary-gc *screen-default-colormap*)
	       (if alloc-gray
		   (setf *default-gray-pixels*
		     (assign-gray-pixels *screen-default-colormap*
					 *num-gray-pixels*)))
	       ;; event handling state initialization
	       (setf *current-event-level* 0)
	       (setf *background-event-queue* nil)
	       nil)
      (format nil "Could not open display ~A on ~A~%" disp-no hostname))))

;;;--------------------------------------------
;;; End.
