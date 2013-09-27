;;;
;;; postscript
;;;
;;; This module contains a collection of little functions that provide
;;; a higher level interface to Postscript text and graphics output.
;;;
;;; 30-Apr-1998 I. Kalet written
;;; 19-May-1998 I. Kalet move prism-logo here from charts, parametrize
;;; it, add draw-rectangle.
;;; 13-Oct-1998 I. Kalet add support for gray scale image output as
;;; background to the graphics, and put clipping into a separate
;;; function.
;;;  7-May-1999 I. Kalet optimize draw-image
;;; 18-Jun-1999 J. Zeman add draw-grid function
;;; 15-Jun-2000 I. Kalet cosmetic changes in documentation.
;;; 13-Aug-2000 I. Kalet add function for drawing a mesh inside a polygon.
;;; 12-Mar-2001 I. Kalet change PS version to level 2 and use level 2
;;; device control to select paper and orientation from pagewidth and
;;; pageheight.
;;;
;;;---------------------------------------------

(defpackage "POSTSCRIPT" (:nicknames "PS") (:use "COMMON-LISP")
	    (:export "DRAW-IMAGE" "DRAW-LINE" "DRAW-LINES" "DRAW-POINT"
		     "DRAW-POLY-MESH" "DRAW-RECTANGLE" "DRAW-TEXT"
		     "DRAW-GRID" "FINISH-PAGE" "INDENT" "INITIALIZE"
		     "NEWLINE" "PRISM-LOGO" "PUT-TEXT"
		     "SET-CLIP" "SET-FONT" "SET-GRAPHICS" "SET-POSITION"
		     "TRANSLATE-ORIGIN"))

;;;---------------------------------------------

(in-package :postscript)

;;;---------------------------------------------

(defun initialize (strm left bottom width height
		   &optional (pagewidth 8.5) (pageheight 11.0))

  "initialize strm left bottom width height
              &optional (pagewidth 8.5) (pageheight 11.0)

Writes to the output stream strm a collection of low level subroutine
definitions used by the Postscript package, and sets the margins and
clipping area according to the parameters, left bottom width height,
which are in inches."

  ;; write a short prologue (required for some print spoolers
  (format strm "%!PS-Adobe-2.0~%")
  (format strm "%%Creator: Prism Postscript system~%")
  (format strm "%%EndComments~%")

  ;; define conversion from inches to points
  (format strm "/inch {72 mul} def~%")

  ;; set paper size selection
  (format strm "<</PageSize [~A ~A]>> setpagedevice~%"
	  (round (* pagewidth 72)) (round (* pageheight 72)))

  ;; set some layout parameters
  (format strm "/leftmargin ~A inch def~%" left)
  (format strm "/topmargin ~A inch def~%" (- pageheight height bottom))
  (format strm "/textwidth ~A inch def~%" width)
  (format strm "/textheight ~A inch def~%" height)
  (format strm "/pagewidth ~A inch def~%" pagewidth)
  (format strm "/pageheight ~A inch def~%" pageheight)

  (set-clip strm left bottom width height)

  ;; define text type size parameter and set a default
  (format strm "/size 12 def~%" ) ;; default value - 12 pt

  ;; define and initialize horizontal and vertical position
  ;; parameters, where hpos is used for column indentation
  (format strm "/vpos ~A inch size sub def~%" (+ bottom height))
  (format strm "/hpos leftmargin def hpos vpos moveto~%")

  ;; define a font setting command and set a default
  (format strm "/choosefont {findfont size scalefont setfont} def~%")
  (format strm "/Courier choosefont~%")

  ;; define the newline command - uses hpos
  (format strm "/newline ")
  (format strm "{/vpos vpos size sub def hpos vpos moveto} def~%")

  ;; that's all for now...
  nil)

;;;---------------------------------------------

(defun set-clip (strm left bottom width height)

  "set-clip strm left bottom width height

set the clipping window according to the margins and size specified,
relative to the current origin."

  (format strm "newpath ~A inch ~A inch moveto~%" left bottom)
  (format strm "~A inch ~A inch lineto~%" left (+ bottom height))
  (format strm "~A inch ~A inch lineto~%"
	  (+ left width) (+ bottom height))
  (format strm "~A inch ~A inch lineto closepath clip~%"
	  (+ left width) bottom))

;;;---------------------------------------------

(defun set-font (strm fontname size)
  
  "set-font strm fontname size

writes the commands to select the specified font by name and set the
current type size to size, in points."

  (format strm "/size ~A def /~A choosefont~%" size fontname))

;;;---------------------------------------------

(defun set-position (strm horiz vert)

  "set-position strm horiz vert

sets the current text position to horiz and vert in inches, allowing
for the left margin, where vert is the distance down from the top.
This assumes that the origin is at the lower left corner of the page."

  (format strm "newpath ~A inch leftmargin add~%" horiz)
  (format strm "/vpos pageheight topmargin sub ~A inch sub def~%" vert)
  (format strm "vpos moveto~%"))

;;;---------------------------------------------

(defun put-text (strm str)

  "put-text strm str

writes the string str at the current position and sets the current
position to the beginning of the next line."

  (format strm "(~A) show newline~%" str))

;;;---------------------------------------------

(defun translate-origin (strm x y)

  "translate-origin strm x y

translates the origin by a displacement of x and y inches from the
current origin."

  (format strm "~A inch ~A inch translate~%" x y))

;;;---------------------------------------------

(defun indent (strm indentation)

  "indent strm indentation

sets the horizontal position to indentation in inches, to make columns
that are not at the left margin.  To reset, pass in a value of 0."

  (format strm "/hpos leftmargin ~A inch add def~%" indentation))

;;;---------------------------------------------

(defun set-graphics (strm &key color width pattern)

  "set-graphics strm &key color width pattern

sets the current color, line width and line dash pattern according to
color, a list of RGB values, width, a number, and pattern, a string
containing a Postscript dash array with brackets, and a number, the
offset.  If a parameter is omitted, that graphic attribute is not
changed."

  (if color (apply #'format strm "~A ~A ~A setrgbcolor~%" color))
  (if width (format strm "~A setlinewidth~%" width))
  (if pattern (format strm "~A setdash~%" pattern)))

;;;---------------------------------------------

(defun draw-image (strm x y width height xpix ypix image)

  "draw-image strm x y width height xpix ypix image

draws a gray scale image with lower left corner at position x,y in
inches relative to the current origin, in a rectangle of dimensions
width and height, in inches, from the array, image, of 8-bit bytes,
which is xpix columns by ypix rows.  The byte values are assumed to
range between 0 and 127."

  (declare (type (simple-array (unsigned-byte 8) 2) image))
  (let ((hexarray (make-array 128 :element-type 'string)))
    (declare (type (simple-array string (128)) hexarray))
    (dotimes (i 128)
      (setf (aref hexarray i) (format nil "~2,'0X" (* 2 i))))
    (format strm "gsave~%")
    ;; use a string buffer one raster line in length
    (format strm "/pixels ~A string def~%" xpix)
    (format strm "~A inch ~A inch translate~%" x y)
    (format strm "~A inch ~A inch scale~%" width height)
    (format strm "~A ~A 8~%" xpix ypix)
    (format strm "[~A 0 0 ~A 0 ~A]~%" xpix (- ypix) ypix)
    ;; read a raster line of hex at a time from the PS file
    (format strm "{currentfile pixels readhexstring pop}~%image~%~%")
    ;; the hex data follow - write 32 bytes per line
    (let ((counter 0))
      (declare (fixnum counter))
      (dotimes (j ypix)
	(declare (fixnum j))
	(dotimes (i xpix)
	  (declare (fixnum i))
	  ;; princ seems to be faster than format here...
	  (princ (aref hexarray (aref image j i)) strm)
	  (when (= (incf counter) 32)
	    (setq counter 0)
	    (terpri strm)))))
    (format strm "~%~%")
    (format strm "~A inch ~A inch scale~%" (/ 1.0 width) (/ 1.0 height))
    (format strm "~A inch ~A inch translate grestore~%" (- x) (- y))))

;;;---------------------------------------------

(defun draw-line (strm x1 y1 x2 y2)

  "draw-line strm x1 y1 x2 y2

draws a line from x1, y1 to x2, y2, coordinates in inches, relative to
the current origin, in the current color, line width and dash
pattern.  The path is reset before drawing."

  (format strm
	  "newpath ~A inch ~A inch moveto ~A inch ~A inch lineto stroke~%"
	  x1 y1 x2 y2))

;;;---------------------------------------------

(defun draw-lines (strm vertex-list &optional close fill)

  "draw-lines strm vertex-list &optional close fill

draws the lines specified by vertex-list, a list of x,y pairs, vertex
coordinates in inches, as a series of connected segments, in the
current color, line width and dash pattern, optionally filling with
the current color."

  (let ((start (first vertex-list)))
    (format strm "newpath ~A inch ~A inch moveto~%"
	    (first start) (second start))
    (dolist (vert (rest vertex-list))
      (format strm " ~A inch ~A inch lineto~%"
	      (first vert) (second vert)))
    (if close (format strm " closepath"))
    (format strm " ~A~%" (if fill "fill" "stroke"))))

;;;---------------------------------------------

(defun draw-rectangle (strm x y w h &optional fill)

  "draw-rectangle strm x y w h &optional fill

draws the rectangle specified by lower left corner x,y and width w and
height h, in the current color, line width and dash pattern."

  (let ((x2 (+ x w))
	(y2 (+ y h)))
    (format strm
	    "newpath ~A inch ~A inch moveto ~A inch ~A inch lineto~%"
	    x y x2 y)
    (format strm "~A inch ~A inch lineto ~A inch ~A inch lineto~%"
	    x2 y2 x y2)
    (format strm "closepath ~A~%" (if fill "fill" "stroke"))))

;;;---------------------------------------------

(defun draw-text (strm x y chars)

  "draw-text strm x y chars

draws the string chars starting at location x, y in inches in the
current coordinate system, without starting a new line or changing the
text line pointers."

  (format strm "~A inch ~A inch moveto (~A) show~%" x y chars))

;;;---------------------------------------------

(defun draw-point (strm x y label size)

  "draw-point strm x y label

draws a plus mark whose lines are size long, at the location x, y and
a label to the upper right."

  (let ((delta (* 0.5 size)))
    (draw-line strm (- x delta) y (+ x delta) y)
    (draw-line strm x (- y delta) x (+ y delta))
    (draw-text strm (+ x delta) y label)))

;;;---------------------------------------------

(defun draw-grid (strm width height columns rows)

  "draw-grid strm width height columns rows

Writes to strm a postscript-defined grid width inches wide, height
inches high, and with the amount of rows and columns specified. It
requires a defined current drawing position, which becomes the lower
left corner of grid.  The final drawing position is the same as the
start position."

  (setf height (* 72 height))
  (setf width (* 72 width))
  (format strm "gsave~%") ;; store position
  ;; set up loops to draw columns and rows.
  (format strm "~A {0 ~A rlineto ~A ~A rmoveto} repeat~%" (+ columns 1) 
	  height (float(/ width columns)) (* -1 height))
  ;; draw lines, then back to start
  (format strm "stroke grestore gsave~%")
  (format strm "~A {~A 0 rlineto ~A ~A rmoveto} repeat~%" (+ rows 1)
	  width (* -1 width) (float (/ height rows)))
  (format strm "stroke grestore ~%"))

;;;---------------------------------------------

(defun draw-poly-mesh (strm polygon mesh-size)

  "draw-poly-mesh strm polygon mesh-size

fills the region defined by polygon with a mesh whose line spacing is
mesh-size, in the current color, restoring the current drawing
position and clip region after completion.  Only the mesh lines are
drawn.  The space between the lines is undisturbed."

  (format strm "gsave~%")
  (let* ((start (first polygon))
	 (xlist (mapcar #'first polygon))
	 (ylist (mapcar #'second polygon))
	 (llc-x (apply #'min xlist))
	 (wid (- (apply #'max xlist) llc-x))
	 (llc-y (apply #'min ylist))
	 (hgt (- (apply #'max ylist) llc-y)))
    (format strm "newpath ~A inch ~A inch moveto~%"
	    (first start) (second start))
    (dolist (vert (rest polygon))
      (format strm " ~A inch ~A inch lineto~%"
	      (first vert) (second vert)))
    (format strm "clip~%")
    (format strm "~A inch ~A inch moveto~%" llc-x llc-y)
    (draw-grid strm wid hgt
	       (round (/ wid mesh-size)) (round (/ hgt mesh-size)))
    (format strm "grestore~%")))

;;;---------------------------------------------

(defun finish-page (strm &optional newpage)

  "finish-page strm &optional newpage

outputs the current page and optionally starts a new one."

  (format strm "showpage~%")
  (when newpage
    (format strm "/vpos pageheight topmargin sub def newline~%")))

;;;----------------------------------------------------

(defun prism-logo (strm ulc-x ulc-y version)

  "prism-logo strm ulc-x ulc-y version

writes Postscript commands to stream strm that will produce a Prism
logo with version string specified, at location ulc-x ulc-y, relative
to the current origin, with the values in inches."

  (format strm "gsave~%")
  (format strm
"2 setlinecap 2 setlinejoin
~A inch ~A inch translate
0.8 0.8 scale
3 setlinewidth
% Polyline - the red trace
1.0 0 0 setrgbcolor 
newpath 17 -53 moveto 52 -30 lineto 224 -30 lineto  stroke
% Polyline - the green trace
0 1.0 0 setrgbcolor 
newpath 17 -54 moveto 58 -40 lineto 114 -40 lineto  stroke
% Polyline - the blue trace
0 0 1.0 setrgbcolor 
newpath 17 -55 moveto 59 -50 lineto 114 -50 lineto  stroke
0 0 0 setrgbcolor 
% Polyline - the input trace
newpath 0 -70 moveto 16 -53 lineto  stroke
% Polyline - the triangle
newpath 39 0 moveto 69 -70 lineto 9 -70 lineto closepath  stroke
/Helvetica findfont 14.000 scalefont setfont
134 -55 moveto (~A) show
/Helvetica-Bold findfont 18.000 scalefont setfont
64 -20 moveto 
(Prism RTP system) show
1.25 1.25 scale
~A inch ~A inch translate
/Courier findfont 12.000 scalefont setfont~%"
ulc-x ulc-y version (- ulc-x) (- ulc-y))
(format strm "grestore ~%"))

;;;---------------------------------------------
;;; End.
