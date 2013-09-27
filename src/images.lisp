;;;
;;; images
;;;
;;; A collection of basic stuff for computing and displaying images.
;;;
;;; 03-May-1992 I. Kalet created
;;; 24-May-1992 I. Kalet move exported symbols to slik-exports
;;;  4-Nov-1992 I. Kalet change name of parameter in make-graymap
;;; 26-Mar-1993 I. Kalet add :bits-per-pixel parameter to call to
;;; xlib:create-image in map-image-to-xlib, for CMUCL compatibility.
;;; 20-Jan-1994 I. Kalet try some optimizations.
;;; 10-May-1994 I. Kalet prevent index out of range errors in
;;;  make-graymap when window may extend below 0 or above range-top,
;;;  also add image mapping functions for raw gray values.
;;; 23-May-1994 J. Unger make efficiency enhancements to map-raw-image
;;;  and map-image-to-xlib.
;;;  3-Jan-1995 I. Kalet remove proclaim form and add optional
;;;  parameter to make-graymap and make-raw-graymap
;;; 31-Jan-1996 I. Kalet take out VAXLISP hack.
;;; 18-Feb-1996 I. Kalet in map-image-to-xlib use new SLIK global
;;; *image-bits-per-pixel* parameter for compatibility with DEC Alpha,
;;; VAXstations, etc., also put-image to drawable rather than just
;;; returning a xlib:image data structure.
;;; 20-Jan-1998 I. Kalet add some optimizations.
;;; 25-Apr-1999 I. Kalet modify for multiple colormaps.
;;; 11-Jul-2000 I. Kalet split map-image-to-xlib to enable sharing code
;;; with gl support.
;;;  3-Sep-2000 I. Kalet can't use a cache of scratch arrays - they
;;; are returned and put somewhere, so can't reuse them.
;;;

(in-package :slik)

;;;---------------------------------------------------

(defun make-graymap (window level range-top
		     &key old-map (gray-pixels *default-gray-pixels*))

  "make-graymap window level range-top
&key old-map (gray-pixels *default-gray-pixels*)

Returns an array of pixel values, one for each possible image array
value, corresponding to the standard linear gray map used to map CT
image data to a gray scale displayed image.  Level is the image value
corresponding to the middle of the gray range and window is the width
of the ramp.  The range of gray values is determined by the size of
the gray-pixels array, which contains the pixel values corresponding
to each gray level from 0 to the maximum in use on the display.
Range-top is the highest value that can appear in an image array,
usually 4095.  If old-map is provided, it is used instead of creating
a new one."

  (declare (type (unsigned-byte 16) window level range-top)
	   (type (simple-array xlib:pixel 1) gray-pixels))
  (let* ((result (or old-map (make-array (1+ range-top)
					 :element-type 'xlib:pixel)))
         (top-gray (1- (length gray-pixels)))
         (low-ramp (- level (truncate (/ window 2))))
	 (bottom low-ramp) ;; since low-ramp may change
         (high-ramp (+ low-ramp window))
	 (dark (aref gray-pixels 0)) ;; the black pixel
	 (light (aref gray-pixels top-gray))) ;; the white pixel
    (declare (type (simple-array xlib:pixel 1) result)
	     (type (unsigned-byte 16) top-gray low-ramp high-ramp)
	     (type xlib:pixel dark light))
    ;; the following prevents index out of range errors
    (if (< low-ramp 0) (setq low-ramp 0))
    (if (> high-ramp range-top) (setq high-ramp range-top))
    (do ((i 0 (1+ i))) ((= i low-ramp))
      (declare (fixnum i))
      (setf (aref result i) dark))
    (do ((i low-ramp (1+ i))) ((= i high-ramp))
      (declare (fixnum i))
      (setf (aref result i)
	(aref gray-pixels ;; use bottom, not low-ramp
	      (the fixnum (round (/ (* top-gray (- i bottom))
				    window))))))
    (do ((i high-ramp (1+ i))) ((> i range-top))
      (declare (fixnum i))
      (setf (aref result i) light))
    result))

;;;----------------------------

(defun map-image (map image &optional result)

  "map-image map image &optional result

returns an array of pixels from image by composing image array with
the gray map.  The map must be an array specifying a pixel value to be
output for each possible image data value.  If the result array is
provided it is reused, otherwise a new array is created."

  #+nil(declare (type (simple-array xlib:pixel 1) map)
	   (type (simple-array xlib:pixel 2) result)
	   (type (simple-array (unsigned-byte 16) 2) image))
  (let* ((x-dim (array-dimension image 1))
         (y-dim (array-dimension image 0))
         (temparray (or result
			(case *image-bits-per-pixel*
			  (8 (make-array (list y-dim x-dim)
					 :element-type
					 '(unsigned-byte 8)))
			  (16 (make-array (list y-dim x-dim)
					  :element-type
					  '(unsigned-byte 16)))
			  (32 (make-array (list y-dim x-dim)
					  :element-type
					  '(unsigned-byte 32)))
			  ))))
    (declare (type fixnum x-dim y-dim))
    (dotimes (j y-dim)
      (declare (type fixnum j))
      (dotimes (i x-dim)
	(declare (type fixnum i))
	(setf (aref temparray j i) (aref map (aref image j i)))))
    temparray))

;;;----------------------------

(defun write-image-xlib (image drawable)

  "write-image-xlib image drawable

Writes image array to drawable using xlib functions.  The image array
should be an array of xlib pixels."

  (declare (type (simple-array xlib:pixel 2) image))
  (let ((x-dim (array-dimension image 1))
	(y-dim (array-dimension image 0)))
    (declare (type fixnum x-dim y-dim))
    (xlib:put-image drawable (color-gc 'sl:white)
		   (xlib:create-image :width x-dim :height y-dim
				     :depth (xlib:drawable-depth drawable)
				     :bits-per-pixel *image-bits-per-pixel*
				     :data image
				     :format :z-pixmap)
		   :x 0 :y 0)))

;;;---------------------------------------------------

(defun make-raw-graymap (window level range-top
			 &key old-map (num-pixels *num-gray-pixels*))

  "make-raw-graymap num-pixels window level range-top
                    &key old-map (num-pixels *num-gray-pixels*)

Returns an array of byte values, one for each possible image array
value, corresponding to the standard linear gray map used to map CT
image data to a gray scale displayed image.  Level is the image value
corresponding to the middle of the gray range and window is the width
of the ramp.  The range of gray values is determined by num-pixels,
and the values returned are just numbers in the range from 0 for black
to num-pixels minus 1, for white.  Range-top is the highest value that
can appear in an image array, usually 4095.  If old-map is provided it
is used instead of creating a new one."

  (let* ((result (or old-map
		     (make-array (1+ range-top)
				 :element-type '(unsigned-byte 8))))
         (top-gray (1- num-pixels))
         (low-ramp (- level (truncate (/ window 2))))
	 (bottom low-ramp) ;; since low-ramp may change
         (high-ramp (+ low-ramp window)))
    (declare (type (simple-array (unsigned-byte 8) 1) result)
	     (type (unsigned-byte 16)
		   window level range-top top-gray low-ramp high-ramp))
    ;; the following prevents index out of range errors
    (if (< low-ramp 0) (setq low-ramp 0))
    (if (> high-ramp range-top) (setq high-ramp range-top))
    (do ((i 0 (1+ i))) ((= i low-ramp))
      (declare (fixnum i))
      (setf (aref result i) 0)) ;; black
    (do ((i low-ramp (1+ i))) ((= i high-ramp))
      (declare (fixnum i))
      (setf (aref result i) ;; use bottom, not low-ramp
	(the (unsigned-byte 8)
	  (round (/ (* top-gray (- i bottom)) window)))))
    (do ((i high-ramp (1+ i))) ((> i range-top))
      (declare (fixnum i))
      (setf (aref result i) top-gray))
    result))

;;;-----------------------------------

(defun map-raw-image (raw-image window level range &optional old-array)

  "map-raw-image raw-image window level range &optional old-array

returns an array of bytes the same dimensions as raw-image, but with
the values in raw-image converted to gray scale values in the range 0
to *num-gray-levels* according to the linear ramp determined by window
and level, the width and center of the ramp.  Range is the highest
value that can occur in the raw-image.  If old-array is provided it
must be the same dimensions as raw-image, and it is recycled instead
of allocating a new array."

  #+nil(declare (fixnum window level range)
	   (type (simple-array (unsigned-byte 8) 2) old-array)
	   (type (simple-array (unsigned-byte 16) 2) raw-image))
  (let* ((x-dim (array-dimension raw-image 1))
         (y-dim (array-dimension raw-image 0))
         (temparray (or old-array
			(make-array (list y-dim x-dim)
				    :element-type '(unsigned-byte 8))))
	 (map (make-raw-graymap window level range)))
    (declare (type fixnum x-dim y-dim))
    (declare (type (simple-array (unsigned-byte 8) 2) temparray))
    (declare (type (simple-array (unsigned-byte 8)) map))
    (dotimes (j y-dim)
      (declare (fixnum j))
      (dotimes (i x-dim)
	(declare (fixnum i))
	(setf (aref temparray j i) (aref map (aref raw-image j i)))))
    temparray))

;;;-----------------------------------

(defun get-z-array (vox z0 zsize z)

  "get-z-array vox z0 zsize z

extracts and returns a 2-d array from the vox 3-d array, at the
specified z, given the z origin and overall size in the z direction."

  (declare (type (simple-array (unsigned-byte 16) 3) vox))
  (let* ((x-dim (array-dimension vox 2))
         (y-dim (array-dimension vox 1))
	 (nz (1- (array-dimension vox 0)))
	 (index (round (* nz (/ (- z z0) zsize))))
	 (pix (make-array (list y-dim x-dim) 
			  :element-type '(unsigned-byte 16))))
    (declare (type (simple-array (unsigned-byte 16) 2) pix)
	     (type fixnum x-dim y-dim index)
	     (type single-float z z0 zsize))
    (dotimes (j y-dim)
      (dotimes (i x-dim)
	(setf (aref pix j i) (aref vox index j i))))
    pix))

;;;---------------------------------
;;; End.
