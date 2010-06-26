(defpackage #:how
  (:use :cl :lispbuilder-sdl))

(in-package :how)

(defun start-sdl ()
  (sdl:with-init (sdl:sdl-init-video )
    (sdl:window 320 240)
    (sdl:draw-line (sdl:point) (sdl:point :x 100 :y 100))
    (let ((counter 0)
	  (horizontal 0)
	  (vertical 0))
      (setf (sdl:frame-rate) 200)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event ()
			 (sdl:draw-line (sdl:point :x 200 :y 200)
					(sdl:point :x 0 :y 33))
			 (incf counter)
			 (when (= counter 2) (sdl:push-quit-event)))
	(:idle ()
	       (sdl:draw-pixel (sdl:point :x (incf horizontal) :y vertical))
	       (when (= horizontal 320)
		 (incf vertical)
		 (setq horizontal 0))
	       (sdl:update-display))))))
