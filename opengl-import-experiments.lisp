(defpackage #:how.opengl-import
  (:use :cl)
  (:import-from :how.3d-experiments #:render-cube))

(in-package :how.opengl-import)

(defun render-thing (points)
  "Renders POINTS as a group of vertexes"
  (gl:with-primitive :quads
    (mapc (lambda (point) (apply #'cl-opengl:vertex point)) points)))

(defun 3d-test-1 ()
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
      (:quit-event ()
                   t)
      (:key-down-event (:key key)
                       (when (sdl:key= key :sdl-key-escape) (sdl:push-quit-event)))
      (:idle ()
             (gl:clear :color-buffer-bit)
             (gl:color 1 1 1)
             (gl:rotate 1 1 1 1)
                                        ;(gl:polygon-mode :front-and-back :fill)
                                        ;(render-cube 10 10 0 5 :quads)
             (gl:polygon-mode :front-and-back :point)
             (render-thing (parse-points (dom:get-attribute (aref (dom:get-elements-by-tag-name *source* "Coordinate") 0) "point")))
                                        ;(render-cube 0 0 0 5 :quads)
                                        ;(gl:polygon-mode :front-and-back :point)
                                        ;(render-cube -10 -10 0 5 :quads)
            ; (gl:with-primitives :points (gl:vertex 10 10 10))
             (gl:enable-client-state :vertex-array)
             (gl:enable-client-state :vertex-array)
             (gl:with-gl-array (arr 'vertex :count 3)
               (setf (getvert arr) #(10 10 0))
               (setf (getvert arr 1) #(5 5 5))
               (setf (getvert arr 2) #(0 0 0))
               (gl:bind-gl-vertex-array arr)
               (%gl:draw-arrays :points 0 3))
             #+ () (gl:with-gl-array (arr 'ooh :count 3)
               (setf (gl:glaref arr 0 'x) 10)
               (setf (gl:glaref arr 0 'y) 10)
               (setf (gl:glaref arr 0 'z) 0)
               (setf (gl:glaref arr 1 'x) 5)
               (setf (gl:glaref arr 1 'y) 5)
               (setf (gl:glaref arr 1 'z) 0)
               (setf (gl:glaref arr 2 'x) 0)
               (setf (gl:glaref arr 2 'y) 0)
               (setf (gl:glaref arr 2 'z) 0)
               (gl:bind-gl-vertex-array arr))
            #+ () (gl:with-gl-array (index :int :count 3)
                 (setf (gl:glaref index 0) 0)
                 (setf (gl:glaref index 1) 1)
                 (setf (gl:glaref index 2) 1)
                 (setf (gl:glaref index 3) 0)
                 (gl:draw-elements :lines index :count 4))
             (gl:flush)
             (sdl:update-display)))))

(gl:define-gl-array-format vertex
  (gl:vertex :type :int :components (x y z)))


(defun getvert (gl-vector &optional (index 0))
  "Gets an cl-opengl vertex."
  (declare (nutils:non-negative-fixnum index))
  (vector (gl:glaref gl-vector index 'x)
          (gl:glaref gl-vector index 'y)
          (gl:glaref gl-vector index 'z)))

(defun (setf getvert) (vector gl-vector &optional (index 0))
  (setf (gl:glaref gl-vector index 'x) (aref vector 0)
        (gl:glaref gl-vector index 'y) (aref vector 1)
        (gl:glaref gl-vector index 'z) (aref vector 2)))



(defun parse-points (string)
  "Parse a list of .x3d points into a list of vectors.

Our result is something like (list #(1 2 3) #(3.4 3.4 4.5))"
  (declare (string string))
  (mapcar (lambda (vect)
            (mapcar #'read-from-string
                    (split-sequence:split-sequence
                     #\Space vect :remove-empty-subseqs t)))
          (remove " "
                  (split-sequence:split-sequence #\, string :remove-empty-subseqs t)
                  :test #'string=)))


(defparameter *source* (cxml:parse-file "/home/james/blender25/.x3d" (cxml-dom:make-dom-builder)))

