(in-package :how)

(defun load-image (name &key (directory +image-directory+))
  "Load image NAME from `+image-directory+'."
  (declare (string name))
  (sdl:load-image (merge-pathnames name directory)))

(defun start-sdl ()
  (sdl:with-init (sdl:sdl-init-video )
    (sdl:window 320 240)

    (setf (sdl:frame-rate) 200)
    (let ((house (load-image "house_64x64.bmp"))
          (dude (load-image "stickfigure_64x64.bmp")))
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
                         (sdl:push-quit-event))
        (:mouse-motion-event (:x x :y y)
                             (sdl:clear-display sdl:*green*)
                             (sdl:draw-surface-at-* house 100 100)
                             (sdl:draw-surface-at-* dude x y))
        (:idle ()
               (sdl:update-display))))))

;;; END
