;;;
;;; xlib-support
;;;
;;; This module contains all the basic CLX support for SLIK, the revised
;;; small LISP toolkit, based on Mark Niehaus's minitools.
;;;
;;; 13-Jan-1992 I. Kalet started
;;; 13-Apr-1992 I. Kalet change colors from pixel to graphic contexts
;;; 01-May-1992 I. Kalet delete unnecessary functions
;;; 03-May-1992 I. Kalet move image stuff to images file
;;; 15-May-1992 I. Kalet add font-height function
;;; 24-May-1992 I. Kalet move exported symbols to slik-exports
;;; 14-Jul-1992 I. Kalet add make-duplicate-gc function
;;;  8-Oct-1992 I. Kalet make parameter in make-duplicate-gc &optional
;;; 30-Oct-1992 I. Kalet added *linespace* parameter, deleted
;;; text-height, added named font variables
;;; 23-Mar-1993 J. Unger add type declaration to assign-gray-pixels
;;;  3-Aug-1993 I. Kalet add color invisible, which uses NOOP draw
;;;  operation in its gcontext.
;;; 25-Apr-1994 I. Kalet add make-square-pixmap, extracted from Prism
;;;  5-Jun-1994 I. Kalet add host function - not strictly reliable...
;;; 03-Oct-1994 J. Unger add support for dashed colors.
;;;  3-Jan-1995 I. Kalet remove proclaim form, make initialize function
;;;  return nil if successful, as documented, put *kp-enter-keysym* here
;;;  as global, but leave terminate function in event-loop.
;;; 18-Feb-1996 I. Kalet add new globals to handle different display
;;; configurations.
;;;  8-Oct-1996 I. Kalet move find-dashed-color and find-solid-color
;;;  here from Prism.
;;; 25-Feb-1998 I. Kalet cosmetic changes
;;; 21-Jul-1998 I. Kalet add optional arg to initialize to not
;;; allocate gray scale in the screen default colormap.
;;; 16-Dec-1998 I. Kalet add hack for wierdness in default host for
;;; HP-UX 10.20 X support.
;;;  1-Apr-1999 C. Wilcox added event-level and background-event-queue
;;;  initialization to the slik initialize function.
;;; 25-Apr-1999 I. Kalet big overhaul to add support for multiple
;;; colormaps.
;;; 31-May-2000 I. Kalet add support for Helvetica medium fonts,
;;; provide new exported global, *default-font-name* so an application
;;; can set it before calling initialize.
;;;  4-Aug-2000 I. Kalet add support for display other than display 0,
;;; allow host in initialize to include display number.
;;; 25-Aug-2000 I. Kalet call load-gl in initialize.
;;; 27-Dec-2000 I. Kalet make localhost and blank string equivalent in
;;; host function.
;;; 18-Mar-2001 I. Kalet add default foreground and background colors
;;; and border style - black on gray, raised borders, but user configurable.
;;; Make HPUX-10 host hack independent of Allegro version.
;;; 23-Jun-2001 I. Kalet add egregious hack to fix an obscure CLX bug -
;;; see end of this file.
;;; 30-Jul-2004 I. Kalet move initialize and related code to separate
;;; file to untangle dependency circularity with OpenGL code.
;;; 17-May-2008 I. Kalet take out ref to HP-UX, long gone.
;;;

(in-package :slik)

;;;--------------------------------------------
;;; The following global variables are used in places throughout the SLIK
;;; system, but are not intended to be manipulated by users of the
;;; package. Instead they are managed by function calls to SLIK
;;; functions.
;;;--------------------------------------------

(defvar *host* "" "Name of the graphic display host")
(defvar *display* nil "Only one open-display call made at initialization")
(defvar *screen* nil "Display default screen")
(defvar *screen-root* nil "Root window for *screen*")
(defvar *screen-default-colormap* nil "Shared default colormap")
(defvar *screen-root-depth* 8 "Depth of the screen root window")
(defvar *image-bits-per-pixel* 8 "Bits per pixel corresponding to the
pixmap format of the screen default depth - needed for images and not
always equal to screen root depth.")

;;;--------------------------------------------
;;; Define/bind the number of entries in the gray pixel ramp here.
;;;--------------------------------------------

(defvar *default-gray-pixels* "For gray scale images")
(defparameter *num-gray-pixels* 128)

;;;--------------------------------------------

(defconstant *up-arrow-keysym* 65362 "The X keysym for the up arrow.")
(defconstant *down-arrow-keysym* 65364 "The X keysym for the down arrow.")
(defconstant *kp-enter-keysym* 65421 "The X keysym for the keypad enter key")
(defconstant *button-1* 1 "The X keycode for mouse button 1")

;;;--------------------------------------------

(defparameter *linespace* 10 
"Pixels vertical space between lines of text.")

;;;--------------------------------------------

(defvar *default-border-style* :flat)

;;;--------------------------------------------
;;; named and default fonts
;;;--------------------------------------------

(defvar *default-font-name* 'helvetica-medium-14
  "Symbol, can be changed by application before calling initialize.")

(defvar *default-font* nil "Default font for primary graphic contexts")
(defvar courier-bold-12)
(defvar courier-bold-14)
(defvar courier-bold-18)
(defvar times-bold-12)
(defvar times-bold-14)
(defvar times-bold-18)
(defvar helvetica-medium-12)
(defvar helvetica-medium-14)
(defvar helvetica-medium-18)
(defvar helvetica-bold-12)
(defvar helvetica-bold-14)
(defvar helvetica-bold-18)
(defvar schoolbook-bold-12)
(defvar schoolbook-bold-14)
(defvar schoolbook-bold-18)

;;;--------------------------------------------
;;; The following global variables hold graphic contexts for the
;;; primary colors.  In places where colors are stored as attributes
;;; for objects, use the symbols sl:red, sl:blue, etc. and get the
;;; graphic contexts by (color-gc (color obj) colormap).  Their values
;;; are set by the make-primary-gc function below, called by the
;;; initialize function.
;;;--------------------------------------------

(defvar red nil)
(defvar green nil)
(defvar blue nil)
(defvar magenta nil)
(defvar cyan nil)
(defvar yellow nil)
(defvar black nil)
(defvar black2 nil) ;; used for button edge shadows
(defvar white nil)
(defvar gray nil)
(defvar default-fg nil) ;; used for widget foreground
(defvar default-bg nil) ;; used for widget background
(defvar invisible nil)

(defvar red-dashed nil)
(defvar green-dashed nil)
(defvar blue-dashed nil)
(defvar magenta-dashed nil)
(defvar cyan-dashed nil)
(defvar yellow-dashed nil)
(defvar black-dashed nil)
(defvar white-dashed nil)
(defvar gray-dashed nil)

(defvar *fg-level* 0.0) ;; used for making default-fg
(defvar *bg-level* 0.75) ;; used for making default-bg

;;;--------------------------------------------

(defun host ()

  "host

returns the string naming the host for the current display"

  (if (or (string-equal *host* "")
	  (string-equal *host* "localhost"))
      (short-site-name)
    *host*))

;;;--------------------------------------------

(defun color-gc (color &optional (colormap *screen-default-colormap*))

  "color-gc color &optional (colormap *screen-default-colormap*)

returns the graphic context for the symbol color, naming one of the
predefined colors.  The colormap parameter is used to look it up in
the association list bound to the symbol."

  (second (find colormap (symbol-value color) :key #'first)))

;;;--------------------------------------------

(defun make-duplicate-gc (&optional base-gc)

  "make-duplicate-gc &optional base-gc

returns a fresh xlib:gcontext object whose parameters are identical to
those of base-gc.  If base-gc is null, white is used."

  (unless base-gc (setq base-gc (color-gc 'white)))
  (let ((new-gc (xlib:create-gcontext :drawable *screen-root*)))
    (xlib:copy-gcontext base-gc new-gc)
    new-gc))

;;;--------------------------------------------

(defun flush-output ()

  "flush-output

force any pending graphics operation on the SLIK display."

  (xlib:display-force-output *display*))

;;;--------------------------------------------

(defun font-height (f)

  "font-height f

Returns the sum of the maximum character ascent and maximum character
descent of font f."

  (+ (xlib:max-char-descent f)
     (xlib:max-char-ascent f)))

;;;--------------------------------------------

(defun make-square-pixmap (size &optional fill-p drawable depth)

  "make-square-pixmap size &optional fill-p drawable depth

Creates and returns a pixmap with the specified parameter attributes.
Fills the pixmap with a black background if fill-p is true.  If not
provided, depth and drawable are taken from the screen root window."

  (unless drawable (setq drawable *screen-root*))
  (unless depth (setq depth (xlib:drawable-depth drawable)))
  (let ((pm (xlib:create-pixmap :width size
			       :height size
			       :depth depth
			       :drawable drawable)))
    (when fill-p (xlib:draw-rectangle pm (color-gc 'black)
				     0 0 size size t))
    pm))

;;;--------------------------------------------

(defmacro aif (test-form then-form &optional else-form)

  "anaphoric if from Graham, On Lisp."

  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;;;--------------------------------------------

(defun find-dashed-color (col)

  "find-dashed-color col

Given the gc for a solid color col, finds and returns the gc for the
corresponding dashed color.  Returns nil if a solid color was not
supplied.  The invisible color maps to invisible."

  (or
   (aif (find col red :key #'second)
	(second (find (first it) red-dashed :key #'first)))
   (aif (find col green :key #'second)
	(second (find (first it) green-dashed :key #'first)))
   (aif (find col blue :key #'second)
	(second (find (first it) blue-dashed :key #'first)))
   (aif (find col yellow :key #'second)
	(second (find (first it) yellow-dashed :key #'first)))
   (aif (find col magenta :key #'second)
	(second (find (first it) magenta-dashed :key #'first)))
   (aif (find col cyan :key #'second)
	(second (find (first it) cyan-dashed :key #'first)))
   (aif (find col white :key #'second)
	(second (find (first it) white-dashed :key #'first)))
   (aif (find col black :key #'second)
	(second (find (first it) black-dashed :key #'first)))
   (aif (find col gray :key #'second)
	(second (find (first it) gray-dashed :key #'first)))
   (if (find col invisible :key #'second) col)))

;;;--------------------------------------------

(defun find-solid-color (col)

  "find-solid-color col

Given the gc for a dashed color col, finds and returns the gc for the
corresponding solid color.  Returns nil if a dashed color was not
supplied.  The invisible color maps to invisible."

  (or
   (aif (find col red-dashed :key #'second)
	(second (find (first it) red :key #'first)))
   (aif (find col green-dashed :key #'second)
	(second (find (first it) green :key #'first)))
   (aif (find col blue-dashed :key #'second)
	(second (find (first it) blue :key #'first)))
   (aif (find col yellow-dashed :key #'second)
	(second (find (first it) yellow :key #'first)))
   (aif (find col magenta-dashed :key #'second)
	(second (find (first it) magenta :key #'first)))
   (aif (find col cyan-dashed :key #'second)
	(second (find (first it) cyan :key #'first)))
   (aif (find col white-dashed :key #'second)
	(second (find (first it) white :key #'first)))
   (aif (find col black-dashed :key #'second)
	(second (find (first it) black :key #'first)))
   (aif (find col gray-dashed :key #'second)
	(second (find (first it) gray :key #'first)))
   (if (find col invisible :key #'second) col)))

;;;----------------------------------------------------------
;;; It seems that Allegro did not quite track between changes in ANSI
;;; Common Lisp and the old (1989) implementation of CLX - this is a
;;; temporary hack to prevent a crash when users type control
;;; characters into textlines.

(in-package :xlib)

#+allegro
(defun default-keysym-translate (display state object)
  (declare (type display display)
	   (type card16 state)
	   (type t object)
	   (ignore display state)
	   (values t))
  object)

;;;--------------------------------------------
;;; End.
