;;;
;;; menus
;;;
;;; A simple menu class which provides a vertical menu from a list of
;;; strings, announces selection or deselection, and provides the item
;;; number.  The programmer using this has to provide an action
;;; function that knows what to do with the menu item number.
;;;
;;; 30-Apr-1992 I. Kalet created
;;; 15-May-1992 I. Kalet add radio-menu
;;; 24-May-1992 I. Kalet move exports to slik-exports
;;; 26-May-1992 I. Kalet make sure buttons have ulc-x set to 0
;;;  2-Jul-1992 I. Kalet change be: to ev: and behavior to event
;;;  6-Jul-1992 I. Kalet take out unnecessary sl: prefixes and
;;;  radio-menu-button-on function
;;;  8-Oct-1992 I. Kalet add select-button and deselect-button methods
;;; 28-Oct-1992 I. Kalet use parameter *linespace*
;;;  3-Jan-1995 I. Kalet insure that you cannot deselect a button in a
;;;  radio menu.
;;;

(in-package :slik)

;;;------------------------------------

(defclass menu (frame)

  ((items :type list
	  :accessor items
	  :initarg :items
	  :documentation "This is a list of strings that are the text
items appearing on the menu.")

   (selected :type ev:event
	     :accessor selected
	     :initform (ev:make-event)
	     :documentation "Announced when the user selects an item
from the menu, by pressing the left mouse button when the pointer is
over an item.")

   (deselected :type ev:event
	     :accessor deselected
	     :initform (ev:make-event)
	     :documentation "Announced when the user deselects an item
from the menu, which corresponds to the menu button being turned off,
which in turn depends on the type of buttons that are created.")

   (buttons :type list
	    :accessor buttons
	    :documentation "Each menu item is implemented by a button.
We need to keep track of them in order to know which item is selected
or deselected and to be able to destroy them when the menu is destroyed.")

   )

  (:default-initargs :title "SLIK menu" :items nil :buttons nil)

  (:documentation "A menu is a vertical array of text items to choose
from with the mouse left button.")

  )

;;;------------------------------------

(defmethod initialize-instance :after ((m menu) &rest other-initargs
				       &key item-height
				       &allow-other-keys)

  (let ((width (width m))
	(ulc-y (- item-height)) ; so first one is at 0
	)
    (setf (buttons m)
	  (mapcar #'(lambda (item)
		      (apply 'make-button width item-height
			     :parent (window m)
			     :ulc-x 0
			     :ulc-y (setq ulc-y (+ ulc-y item-height))
			     :label item other-initargs))
		  (items m)))
    ))

;;;------------------------------------

(defun make-menu (items &rest other-initargs &key font &allow-other-keys)

  "MAKE-MENU items &rest other-initargs

Returns a menu using each of the items as a menu text item."

  (let* ((ft (or font *default-font*))
	 (max-item-width (apply 'max
				(mapcar #'(lambda (item)
					    (xlib:text-width ft item))
					items)))
	 (item-height (+ (font-height ft) *linespace*))
	 (m (apply 'make-instance 'menu
		   :width (+ max-item-width 10)
		   :height (* (length items) item-height)
		   :items items
		   :item-height item-height
		   other-initargs))
	 )
    (mapc #'(lambda (b)
	      (ev:add-notify m (button-on b) #'menu-button-on)
	      (ev:add-notify m (button-off b) #'menu-button-off))
	  (buttons m))
    m))

;;;------------------------------------

(defun menu-button-on (m b)

  "MENU-BUTTON-ON m b

is the action function that each button in the menu calls when it is
turned on.  It in turn just announces SELECTED with the button number
as a parameter."

  (ev:announce m (selected m) (position b (buttons m)))
  )

;;;------------------------------------

(defun menu-button-off (m b)

  "MENU-BUTTON-OFF m b

is the action function that each button in the menu calls when it is
turned off.  It in turn just announces DESELECTED with the button number
as a parameter."

  (ev:announce m (deselected m) (position b (buttons m)))
  )

;;;------------------------------------

(defmethod select-button (button-no (m menu))

  "Sets button button-no on."

  (setf (on (nth button-no (buttons m))) t)
  )

;;;------------------------------------

(defmethod deselect-button (button-no (m menu))

  "Sets button button-no off."

  (setf (on (nth button-no (buttons m))) nil)
  )

;;;------------------------------------

(defmethod destroy :before ((m menu))

  (mapc #'destroy (buttons m))
  )

;;;------------------------------------

(defun make-radio-menu (items &rest other-initargs)

  "MAKE-RADIO-MENU items &rest other-initargs

Returns a menu using each of the items as a menu text item, exactly as
for MAKE-MENU, with the additional constraint that when a menu item is
selected any other item that is selected will be deselected."

  (let ((m (apply #'make-menu items other-initargs)))
    (mapc #'(lambda (b)
	      (ev:add-notify m (button-on b)
			     #'(lambda (m1 b1)
				 (setf (active b1) nil)
				 (mapc #'(lambda (other-b)
					   (when (and (on other-b)
						      (not (eq b1 other-b)))
					     (setf (on other-b) nil)
					     (setf (active other-b) t)
					     ))
				       (buttons m1))
				 (ev:announce m1 (selected m1)
					      (position b1 (buttons m1))))))
	  (buttons m))
    m))

;;;------------------------------------
