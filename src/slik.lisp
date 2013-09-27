;;;
;;; slik
;;;
;;; contains the definition of the slik package and any other
;;; initializations for it.
;;;
;;; 30-Jul-2003 I. Kalet derived from slik-system
;;; 21-Jun-2004 I. Kalet put CLX nickname here - it belongs with SLIK
;;; 16-Jul-2004 BobGian add IN-PACKAGE form - silences compiler complaint.
;;; 31-Jan-2005 A. Simms removed in-package call
;;; 22-Mar-2007 I. Kalet put require :xlib here so don't need to
;;; preload in the lisp image.
;;; 26-Jun-2009 I. Kalet wrap require in eval-when to avoid warning,
;;; also add require acldns for standalone image build
;;;

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (require :clx))

#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :acldns)) ;; autoload needed for non-local X connection

;;;-------------------------------------
;;; In most lisps, must add CLX as nickname to XLIB.

#+(or allegro cmu clisp)
(rename-package "XLIB" "XLIB" 
		(cons "CLX" (package-nicknames
			     (find-package "XLIB"))))

;;;-------------------------------------

(defpackage "SLIK"
  (:nicknames "SL")
  (:use "COMMON-LISP")
  (:export "*BG-LEVEL*" "*DEFAULT-BORDER-STYLE*"
	   "*DEFAULT-FONT-NAME*" "*FG-LEVEL*" "*NUM-GRAY-PIXELS*"
	   "ACKNOWLEDGE" "ACTIVE" "ADD-PICKABLE-OBJ"
	   "ALLOW-BUTTON-2" "ANGLE"
	   "ASSIGN-GRAY-PIXELS"
	   "BG-COLOR" "BG-GRAY" "BLACK" "BLACK-DASHED" "BLUE"
	   "BLUE-DASHED" "BORDER-COLOR" "BORDER-WIDTH"
	   "BUTTON-2-ON"
	   "BUTTON-HEIGHT" "BUTTON-OFF" "BUTTON-ON"
	   "BUTTON-PRESS" "BUTTON-RELEASE" "BUTTON-WIDTH"
	   "BUTTONS"
	   "CELL-OBJECT" "COLOR" "COLOR-GC" "COLORMAP"
	   "CONFIRM" "CONFIRM-EXIT" "CONTENTS"
	   "COURIER-BOLD-12" "COURIER-BOLD-14"
	   "COURIER-BOLD-18"
	   "CYAN" "CYAN-DASHED"
	   "DELETE-BUTTON" "DELETED" 
	   "DEQUEUE-BG-EVENT" "DESELECT-BUTTON"
	   "DESELECTED" "DESTROY" "DISPLAY-PICTURE"
	   "DRAW-BORDER" "DRAW-PLOT-LINES"
	   "ENABLED" "ENTER-NOTIFY" "ENQUEUE-BG-EVENT"
	   "ERASE" "ERASE-BG"
	   "ERASE-CONTENTS" "EXPOSURE"
	   "FG-COLOR" "FILLED" "FIND-PICKABLE-OBJS"
	   "FIND-DASHED-COLOR" "FIND-SOLID-COLOR"
	   "FLUSH-OUTPUT" "FONT" "FONT-HEIGHT" "FRAME"
	   "GET-Z-ARRAY" "GL-COLOR" "GRAY" "GRAY-DASHED"
	   "GREEN" "GREEN-DASHED"
	   "HEIGHT" "HELVETICA-BOLD-12" "HELVETICA-BOLD-14"
	   "HELVETICA-BOLD-18" "HELVETICA-MEDIUM-12"
	   "HELVETICA-MEDIUM-14" "HELVETICA-MEDIUM-18" "HOST"
	   "INFO" "INITIALIZE" "INSERT-BUTTON" "INSERTED"
	   "INVISIBLE" "ITEMS"
	   "KEY-PRESS" "KNOB-SCALE"
	   "LABEL" "LEAVE-NOTIFY"
	   "MAGENTA" "MAGENTA-DASHED"
	   "MAKE-2D-PLOT"
	   "MAKE-ADJUSTABLE-SLIDERBOX" "MAKE-ARROW-BUTTON"
	   "MAKE-BUTTON" "MAKE-CIRCLE"
	   "MAKE-DIAL" "MAKE-DIALBOX"
	   "MAKE-DUPLICATE-GC" "MAKE-EXIT-BUTTON"
	   "MAKE-FRAME" "MAKE-GRAYMAP" "MAKE-GL-BUFFER"
	   "MAKE-ICON-BUTTON" "MAKE-LABEL-SLIDERBOX"
	   "MAKE-LIST-BUTTON"
	   "MAKE-AND-INSERT-LIST-BUTTON"
	   "MAKE-MENU" "MAKE-PICTURE" "MAKE-PRIMARY-GC"
	   "MAKE-RADIO-MENU"
	   "MAKE-RADIO-SCROLLING-LIST" "MAKE-RAW-GRAYMAP"
	   "MAKE-READOUT" "MAKE-RECTANGLE" "MAKE-SCROLLBAR"
	   "MAKE-SCROLLING-LIST" "MAKE-SEGMENT"
	   "MAKE-SLIDER" "MAKE-SLIDERBOX" "MAKE-SPREADSHEET"
	   "MAKE-SQUARE" "MAKE-SQUARE-PIXMAP" "MAKE-TEXTBOX"
	   "MAKE-TEXTLINE"
	   "MAP-IMAGE" "MAP-RAW-IMAGE"
	   "MAXIMUM" "MAXIMUM-CHANGED" "MINIMUM"
	   "MINIMUM-CHANGED" "MOTION" "MOTION-NOTIFY"
	   "NEW-INFO" "NEW-SLIDER-VAL"
	   "OBJECT" "ON"
	   "PICTURE" "PICK-LIST" "PIXMAP"
	   "POINT-NEAR-SEGMENT" "POP-EVENT-LEVEL"
	   "POPUP-COLOR-MENU" "POPUP-MENU"
	   "POPUP-SCROLL-MENU" "POPUP-TEXTBOX"
	   "POPUP-TEXTLINE" "PRINT-2DPLOT"
	   "PROCESS-EVENTS" "PUSH-EVENT-LEVEL"
	   "RED" "RED-DASHED" "REMOVE-PICKABLE-OBJS"
	   "REMOVE-SERIES" "REORDER-BUTTONS"
	   "SCHOOLBOOK-BOLD-12" "SCHOOLBOOK-BOLD-14"
	   "SCHOOLBOOK-BOLD-18" "SELECT-BUTTON" "SELECT-GL"
	   "SELECTED" "SET-BUTTON" "SET-CONTENTS" "SETTING"
	   "SPREADSHEET" "SERIES-COLL"
	   "TERMINATE" "THICKNESS" "TIMES-BOLD-12"
	   "TIMES-BOLD-14" "TIMES-BOLD-18"
	   "TITLE" "TOLERANCE"
	   "ULC-X" "ULC-Y" "UPDATE-PICKABLE-OBJECT"
	   "UPDATE-SERIES" "USER-INPUT"
	   "VALUE-CHANGED"
	   "WHITE" "WHITE-DASHED" "WIDTH" "WINDOW"
	   "WRITE-IMAGE-CLX" "WRITE-IMAGE-GL"
	   "X1" "X2" "X-CENTER" "X-SLIDER-VAL"
	   "Y-SLIDER-VAL"
	   "Y1" "Y2" "Y-CENTER" "YELLOW" "YELLOW-DASHED"
	   ))

;;;-------------------------------------
;;; End.
