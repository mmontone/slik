;;;
;;; collections
;;;
;;; An implemetation of sets and binary relations as Abstract
;;; Behavioral Types.  It uses stuff from the events package so be
;;; sure to load that first.
;;;
;;; 29-May-1992 I. Kalet created
;;;  2-Jun-1992 I. Kalet modify export list
;;;  3-Jun-1992 I. Kalet finish relations code
;;; 24-Jun-1992 I. Kalet move defpackage etc. to config file.  Also
;;; added keyword argument :test to functions that check for membership,
;;; to default to equal rather than eql and caller may provide
;;; alternate tests as well.
;;;  6-Jul-1992 I. Kalet change behavior to event and be: to ev:
;;; 30-Jun-1994 I. Kalet enforce constraints that insertion and
;;; deletion preserves order of elements, and adds new elements at
;;; end, not beginning of list.
;;;  3-Jan-1995 I. Kalet move defpackage here so this file can be
;;;  standalone or used as a module in a system.  NOTE however that
;;;  this module depends on the events module so the events module
;;;  must be loaded first.
;;;  1-Feb-1996 I. Kalet drop make-package, assume defpackage
;;; 18-Apr-1997 I. Kalet drop support for old CMU with PCL, assume
;;; native CLOS
;;; 29-Jun-1997 I. Kalet use find instead of member, in
;;; delete-element, so can announce the actual item deleted, not the
;;; item provided as the parameter.  They may not be the same object.
;;;
;;;----------------------------------------------------------


(defpackage "COLLECTIONS" (:nicknames "COLL")
	    (:use "COMMON-LISP")
	    (:export "MAKE-COLLECTION" "ELEMENTS" "INSERTED" "DELETED"
		     "INSERT-ELEMENT" "DELETE-ELEMENT"
		     "COLLECTION-SIZE" "COLLECTION-MEMBER"
		     "MAKE-RELATION" "PROJECTION" "INVERSE-RELATION")) 

;;;----------------------------------------------------------

(in-package :collections)

;;;----------------------------------------------------------

(defclass collection ()

  ((elements :type list
	     :accessor elements
	     :initarg :elements
	     :initform nil
	     :documentation "The list of actual objects in the set.")

   (inserted :type ev:event
	     :accessor inserted
	     :initform (ev:make-event)
	     :documentation "Announced when a new element is inserted.")

   (deleted :type ev:event
	    :accessor deleted
	    :initform (ev:make-event)
	    :documentation "Announced when an element is deleted.")

   )

  (:documentation "The collection class implements the abstract
behavioral type SET.")

  )

;;;---------------------------------

(defun make-collection (&optional initial-contents)

  "MAKE-COLLECTION &optional initial-contents

returns an instance of a collection, with elements set to the value of
initial-contents."

  (make-instance 'collection :elements initial-contents))

;;;---------------------------------

(defun insert-element (el coll &key (test #'equal))

  "INSERT-ELEMENT el coll &key test

inserts the object el into the collection coll if not already
present.  The test function if provided is used to test whether to
insert the element, and defaults to equal.  The new element is added
at the end, not the front of the list."

  (unless (member el (elements coll) :test test)
    (setf (elements coll) (append (elements coll) (list el)))
    (ev:announce coll (inserted coll) el)))

;;;---------------------------------

(defun delete-element (el coll &key (test #'equal))

  "DELETE-ELEMENT el coll &key test

deletes the object el from the collection coll if it is present.  The
test function is used to decide if the element is present, and defaults
to equal.  The order of the remaining elements is preserved."

  (let ((item (find el (elements coll) :test test)))
    (when item
      (setf (elements coll) (remove item (elements coll)))
      (ev:announce coll (deleted coll) item))))

;;;---------------------------------

(defun collection-size (coll)

  "COLLECTION-SIZE coll

returns the number of elements in collection coll."

  (length (elements coll)))

;;;---------------------------------

(defun collection-member (el coll &key (test #'equal))

  "COLLECTION-MEMBER el coll &key test

if object el satisfies test for some member of the collection, the
result of test is returned.  The default for test is equal, i.e., it
tests if el is a member of collection coll.  If no element of coll
satisfies the test, collection-member returns nil."

  (some #'(lambda (item) (funcall test el item))
	(elements coll)))

;;;---------------------------------

(defclass relation (collection)

  () ; no additional slots

  (:documentation "A relation is a collection in which the elements
are two element lists, i.e., the relation table is implemented as an
association list for now.")

  )

;;;---------------------------------

(defun make-relation (&optional initial-elements)

  (make-instance 'relation :elements initial-elements))

;;;---------------------------------

(defun projection (el rel &key (test #'equal))

  "PROJECTION el rel &key test

returns the image or projection of the element el under the relation
rel using the test function test.  The default for test is equal."

  (remove nil (mapcar #'(lambda (pair)
			  (if (apply test (list el (first pair)))
			      (second pair)))
		      (elements rel))))

;;;---------------------------------

(defun inverse-relation (rel)

  "INVERSE-RELATION rel

returns the inverse relation of rel."

  (make-relation (mapcar 'reverse (elements rel))))

;;;---------------------------------
;;; End.
