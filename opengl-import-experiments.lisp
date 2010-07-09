(defpackage #:how.opengl-import
  (:use :cl))

(in-package :how.opengl-import)

(gl:define-gl-array-format vertex
  (gl:vertex :type :float :components (x y z)))

(defvar *screen-height* 400)
(defvar *screen-width* 500)

(defun warp-mouse (x y)
  (sdl:set-event-filter :mouse-motion-event
			(lambda (event) nil))
  (sdl-cffi::sdl-warp-mouse (multiple-value-bind
				  (xx) (round
					(float x))
			      xx)
			    (multiple-value-bind
				  (yy) (round
					(float y))
			      yy))
  (sdl:set-event-filter :mouse-motion-event
			(lambda (event) t)))

(defun 3d-test-1 ()
  (sdl:with-init ()
    (sdl:window *screen-width* *screen-height*
                :title-caption "OpenGL Example"
                :icon-caption "OpenGL Example"
                :opengl t
                :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
    (sdl:enable-key-repeat nil nil)
    (sdl:show-cursor nil)
    (warp-mouse (/ *screen-width* 2) (/ *screen-height* 2))
    (gl:viewport 0 0 
		 (* *screen-width* 2)
		 (* *screen-height*))
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 45.0 (/ *screen-width* *screen-height*) 1.0 500.0)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:clear-color 0 0 0 0)
    (gl:translate 0 0 -60)
    (sdl:with-events ()
      (:quit-event ()
                   t)
      (:key-repeat-on t)
      (:key-down-event (:key key)
                       (when (sdl:key= key :sdl-key-escape)
			 (sdl:push-quit-event))
		       (when (sdl:key= key :sdl-key-r)
			 (warp-mouse (/ *screen-width* 2)
				     (/ *screen-height* 2)))
		       (when (sdl:key= key :sdl-key-down)
			 (gl:rotate 1 1 0 0))
		       (when (sdl:key= key :sdl-key-up)
			 (gl:rotate -1 1 0 0))
		       (when (sdl:key= key :sdl-key-left)
			 (gl:rotate 1 0 1 0))
		       (when (sdl:key= key :sdl-key-right)
			 (gl:rotate -1 0 1 0))
		       (when (sdl:key= key :sdl-key-q)
			 (gl:rotate 1 0 0 1))
		       (when (sdl:key= key :sdl-key-w)
			 (gl:rotate -1 0 0 1)))
      (:mouse-motion-event (:x-rel x-rel :y-rel y-rel :x x :y y)
			;   (format t "x-rel: ~d y-rel: ~d x: ~d y: ~d~%"
			;	   x-rel y-rel x y)
			   (gl:viewport (- x 500) (+ (- y))
					(* *screen-width* 2)
					(* *screen-height* 2)))
			;   (gl:translate (float (/ x-rel 20)) ;real buggy
			;		 (float (/ y-rel 20)) 0))
      (:mouse-button-down-event (:button button)
			 	(when (= button sdl:sdl-button-wheel-up)
			 	  (gl:translate 0 0 1))
			 	(when (= button sdl:sdl-button-wheel-down)
				  (gl:translate 0 0 -1)))
      (:idle ()
             (gl:clear :color-buffer-bit)
             (gl:color 1 1 1)
             (gl:polygon-mode :front-and-back :fill)
             (gl:enable-client-state :vertex-array)
             (draw-thing)
             (gl:flush)
             (sdl:update-display)))))

(defun normalize (vector)
  (let ((d (sqrt (apply #'+ (loop for i across vector
			       collect (* i i))))))
    (apply #'vector (loop for i across vector
			 collect (float (/ i d))))))


(defun draw-thing ()
  (gl:with-gl-array-values (arr1 'vertex '(x y z))
      '(#(5.0 5.0 -5.0) #(5.0 -5.0 -5.0)
        #(-5.0 -5.0 -5.0) #(-5.0 5.0 -5.0)
        #(5.0 5.0 5.0) #(5.0 -5.0 5.0)
        #(-5.0 -5.0 5.0) #(-5.0 5.0 5.0))
    (gl:bind-gl-vertex-array arr1)
    ;(gl:rotate 1 1 1 3)
    (gl:polygon-mode :front-and-back :line)
    (gl:with-gl-array-values (arr2 :unsigned-int)
        '(0 1 2 3 4 7 6 5 0 4 5 1 1 5 6 2 2 6 7 3 4 0 3 7)
      (gl:draw-elements :polygon arr2))))

#+ () (defparameter *source* (cxml:parse-file "/home/james/blender25/.x3d" (cxml-dom:make-dom-builder)))






