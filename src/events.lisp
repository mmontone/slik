;;;
;;; events
;;;
;;; A very stripped down bare minimum implementation of events, like
;;; John MacDonald's announcements but much simpler, no global entities.
;;;
;;; 14-Apr-1992 I, Kalet written
;;; 24-Jun-1992 I. Kalet move defpackage etc. to config file
;;; 03-Jan-1993 I. Kalet modify add-notify so it does replace the
;;; action function instead of just ignoring the input if an entry is
;;; already present for a party.
;;; 17-Sep-1993 I. Kalet fix error in add-notify - test with party, 
;;; not list of party and action
;;;  3-Jan-1995 I. Kalet move defpackage etc. here from config so this
;;;  file is standalone but can also be a module in a system.
;;;  1-Feb-1996 I. Kalet drop make-package, assume defpackage.
;;; 06-Jun-1997 BobGian redefine ADD-NOTIFY: ADJOIN -> CONS.
;;;
;;;----------------------------------------------------------

(defpackage "EVENTS" (:nicknames "EV") (:use "COMMON-LISP")
	    (:export "EVENT" "MAKE-EVENT" "ANNOUNCE" "ADD-NOTIFY"
		     "REMOVE-NOTIFY"))

;;;----------------------------------------------------------

(in-package :events)

;;;----------------------------------------------------------

(deftype event () 'list) ; an event is a simple a-list

(defun make-event () nil) ; initially empty

;;;---------------------------------------------------

(defmacro add-notify (party event action)

  "ADD-NOTIFY party event action

Adds the party, action pair to the specified event, which should be
a place designation suitable for setf."

  `(setf ,event (cons (list ,party ,action)
		      (remove ,party ,event :test #'eq :key #'car))))

;;;---------------------------------------------------

(defmacro remove-notify (party event)

  "REMOVE-NOTIFY party event

removes the entry for party in event."

  `(setf ,event (remove ,party ,event :test #'eq :key #'car)))

;;;---------------------------------------------------

(defun announce (object event &rest args)

  "ANNOUNCE object event &rest args

announces the event, i.e., applies the action part of each entry to
the party part of each entry, with object and args as additional
arguments."

  (dolist (entry event)			; event is an a-list
    (apply (second entry) (first entry) object args)))

;;;---------------------------------------------------
;;; End.
