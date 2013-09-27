(in-package :cl-user)

(defpackage slik-system
  (:use :cl :asdf))
  
(in-package :slik-system)

(defsystem :slik
    :name "SLIK"
    :description "SLIK (Simple Lisp Interface Kit) is a CLOS-based GUI toolkit that provides a thin abstraction layer over CLX. SLIK is part of PRISM, a radiation therapy planning system developed at Radiation Oncology Department, University of Washington, Seattle."
    :components
    ((:module :src
	      :components
	      ((:file "slik")
	       (:file "events")
	       (:file "postscript")
	       (:file "collections" :depends-on ("events"))

	       ;; elementary machinery
	       (:file "clx-support" :depends-on ("slik")) ;; fixed dependency on event-loop
	       (:file "event-loop" :depends-on ("clx-support"))
	       (:file "initialize" :depends-on ("clx-support" "event-loop"))

	       ;; widgets
	       (:file "frames" :depends-on ("events" "clx-support"))
	       (:file "dials" :depends-on ("events" "frames"))
	       (:file "sliders" :depends-on ("events" "frames"))
	       (:file "buttons" :depends-on ("events" "clx-support" "frames"))
	       (:file "scrollbars" :depends-on ("events" "frames" "sliders" "buttons"))
	       (:file "menus" :depends-on ("events" "clx-support" "frames" "buttons"))
	       (:file "textboxes" :depends-on ("events" "clx-support" "frames")) 
	       (:file "dialogboxes" :depends-on ("clx-support" "event-loop" "frames"
							       "buttons" "menus" "textboxes"))
	       (:file "scrolling-lists" :depends-on ("events" "clx-support" "frames"
							      "buttons" "scrollbars"
							      "dialogboxes"))
	       (:file "readouts" :depends-on ("clx-support" "frames"))
	       (:file "textlines" :depends-on ("events" "clx-support" "frames" "readouts"
							"buttons" "dialogboxes"))
	       (:file "dialboxes" :depends-on ("events" "clx-support" "frames" "dials"
							"textlines"))
	       (:file "sliderboxes" :depends-on ("events" "clx-support" "frames"
							  "sliders" "textlines"))
	       (:file "adj-sliderboxes" :depends-on ("events" "clx-support" "sliders"
							      "textlines" "sliderboxes"))
	       (:file "spreadsheets" :depends-on ("events" "clx-support" "frames" "readouts"
							   "textlines" "buttons"))
	       (:file "pictures" :depends-on ("events" "clx-support" "frames"))
	       (:file "2d-plot" :depends-on ("pictures" "frames" "textlines" "postscript"))
	       (:file "images" :depends-on ("clx-support")))))
    :serial t
    :depends-on (:clx))
