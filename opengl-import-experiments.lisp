(defpackage #:how.opengl-import
  (:use :cl)
  (:import-from :how.3d-experiments #:render-cube))

(in-package :how.opengl-import)

(defun 3d-test-1 ()
  (let ((angle 0))
    (sdl:with-init ()
      (sdl:window 800 600
                  :title-caption "OpenGL Example"
                  :icon-caption "OpenGL Example"
                  :opengl t
                  :opengl-attributes '((:SDL-GL-DOUBLEBUFFER 1)))
      (gl:viewport 0 0 800 600)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (glu:perspective 45.0 (/ 800 600) 1.0 500.0)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:clear-color 0 0 0 0)
      (gl:translate 3 2 -60)
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key key)
                         (when (sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))
        (:idle ()
               (gl:clear :color-buffer-bit)
               (gl:color 1 1 1)
               (gl:rotate 1 1 1 1)
               (gl:polygon-mode :front-and-back :fill)
               (render-cube 10 10 0 5 :quads)
               (gl:polygon-mode :front-and-back :line)
               (render-cube 0 0 0 5 :quads)
               (gl:polygon-mode :front-and-back :point)
               (render-cube -10 -10 0 5 :quads)
               (gl:flush)
               (sdl:update-display))))))


(defun parse-points (string)
  "Parse a list of .x3d points into a list of vectors.

Our result is something like (list #(1 2 3) #(3.4 3.4 4.5))"
  (declare (string string))
  (mapcar (lambda (vect)
            (apply #'vector
                   (mapcar #'read-from-string
                           (split-sequence:split-sequence
                            #\Space vect :remove-empty-subseqs t))))
          (remove " "
                  (split-sequence:split-sequence #\, string :remove-empty-subseqs t)
                  :test #'string=)))





