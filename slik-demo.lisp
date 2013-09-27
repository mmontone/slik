;;;
;;; demo - the examples from the SLIK manual
;;;
;;; 15-Feb-1996 I. Kalet created
;;; 31-May-1998 I. Kalet added declare ignore forms.
;;; 24-Feb-2000 I. Kalet update for new multi-colormap arrangement for
;;; getting graphic contexts from color symbols.
;;; 15-Feb-2003 I. Kalet make fg-color explicitly white in sample1
;;;

(defun sample1 (&optional host)

  "SAMPLE1 &optional host

puts a two button Hello World window on the default display on host
host, or the local display if host is omitted."

  (sl:initialize host)
  (let* ((fr (sl:make-frame 150 225 :bg-color 'sl:gray))
         (win (sl:window fr))
         (eb (sl:make-exit-button 100 75
                                  :parent win :label "I'm done"
				  :fg-color 'sl:white
				  :ulc-x 25 :ulc-y 125))
         (hb (sl:make-button 100 75
                             :parent win :label "Press here"
                             :ulc-x 25 :ulc-y 25
			     :fg-color 'sl:white :bg-color 'sl:blue)))
    (ev:add-notify hb (sl:button-on hb) 'say-hello)
    (ev:add-notify hb (sl:button-off hb) 'reset-it)
    (sl:process-events)
    (sl:terminate)))

(defun say-hello (rcvr btn)
  (declare (ignore rcvr))
  (setf (sl:label btn) "Hello World"))

(defun reset-it (rcvr btn)
  (declare (ignore rcvr))
  (setf (sl:label btn) "Press here"))

;;;-------------------------------------------

(defun sample2 (&optional host)

  "SAMPLE2 &optional host

puts up a more complex demo with a dialbox, a color button and a
slider controlling a ball on a rod."

  (sl:initialize host)
  (let* ((radius 100) ;; initial radius of orbit
         (angle 45.0) ;; initial angle position
         (color 'sl:red) ;; initial color of planet
         (fr (sl:make-frame 790 512))
         (win (sl:window fr))
         (eb1 (sl:make-exit-button 100 50 :label "Done" :parent win
				   :ulc-x 90 :ulc-y 450))
         (d1 (sl:make-dialbox 50 :parent win :ulc-x 85 :ulc-y 40
			      :title "Orbit Angle" :angle angle))
         (b1 (sl:make-button 100 50 :label "Color" :parent win
			     :ulc-x 90 :ulc-y 250 :button-type :momentary
			     :fg-color color))
         (s1 (sl:make-sliderbox 250 30 20.0 250.0 250.0
				:title "Radius: "
				:parent win :setting radius
				:fg-color 'sl:cyan :ulc-x 10 :ulc-y 350))
         (pic (sl:make-picture 512 512 :parent win :fg-color 'sl:green
			       :ulc-x 278)))
    (ev:add-notify pic (sl:value-changed s1) ;; respond to slider
		   #'(lambda (pict sb newrad)
		       (declare (ignore sb))
		       (setf radius newrad)
		       (drawpic pict radius angle color)))
    (ev:add-notify pic (sl:button-on b1) ;; respond to new color
		   #'(lambda (pict bt)
		       (let ((temp (sl:popup-color-menu)))
			 (when temp
			   (setf color temp)
			   (setf (sl:fg-color bt) temp)
			   (drawpic pict radius angle color)))
		       (setf (sl:on bt) nil)))
    (ev:add-notify pic (sl:value-changed d1) ;; respond to dialbox
		   #'(lambda (pict db newang)
		       (declare (ignore db))
		       (setf angle newang)
		       (drawpic pict radius angle color)))
    (drawpic pic radius angle color) ;; need to draw initially
    (sl:process-events) ;; from here on it is automatic
    (sl:terminate)))

;;;-------------------------------------------

(defconstant pi-over-180 (/ pi 180.0) "A handy constant")

(defun drawpic (pic rad ang col)
   (let ((bg (sl:color-gc (sl:bg-color pic)))
         (fg (sl:color-gc (sl:fg-color pic)))
         (px (sl:pixmap pic))
         (x (round (* rad (cos (* pi-over-180 ang)))))
         (y (round (* rad (sin (* pi-over-180 ang))))))
     (xlib:draw-rectangle px bg 0 0 512 512 t) ;; erase first
     (xlib:draw-rectangle px fg 240 240 32 32 t) ;; draw center
     (xlib:draw-line px fg 256 256 x (- y) t) ;; draw radial line
     (xlib:draw-arc px (sl:color-gc col) (+ 256 x -10) (- 256 y 10)
            20 20 0.0 (* 2.0 pi) t) ;; draw ball
     (sl:erase pic))) ;; to make the pixmap data appear in the window

;;;-------------------------------------------
