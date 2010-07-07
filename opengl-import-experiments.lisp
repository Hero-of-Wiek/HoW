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
             (gl:polygon-mode :front-and-back :fill)
             (gl:enable-client-state :vertex-array)
             (draw-thing)
             (gl:flush)
             (sdl:update-display)))))

(defun draw-thing ()
  (with-gl-array-values (arr 'vertex '(x y z))
      '(#(10 10 -10) #(5 15 5) #(-20 0 0) #(10 -12 10))
    (gl:bind-gl-vertex-array arr)
    (with-gl-array-values (arr :unsigned-int) '(0 1 1 3 2 0)
      (gl:draw-elements :polygon arr))))

(gl:define-gl-array-format vertex
  (gl:vertex :type :int :components (x y z)))

(defmacro with-gl-array-values ((var type &optional components count) values
                                &body body)
  (nutils:once-only (values)
    `(gl:with-gl-array (,var ,type :count (or ,count (length ,values)))
       (setf (get-arrays arr ,components) ,values)
       ;; Might want to have something that lets us bind stuff
       ;;automatically in teh arglists, but not a top priority.
       ;;(gl:bind-gl-vertex-array ,var)
       ,@body)))

(defun get-array (gl-vector &optional (index 0) components)
  (if components
      (apply #'vector
             (mapcar (lambda (component)
                       (gl:glaref gl-vector index component))
                     components))
      (gl:glaref gl-vector index)))

(defun (setf get-array) (item gl-vector &optional (index 0) components)
  (if components
      (loop for element across item
         for component in components
         do (setf (gl:glaref gl-vector index component) element))
      (setf (gl:glaref gl-vector index) item)))

(defun get-arrays (gl-array &optional components (count (gl::gl-array-size gl-array)))
  (loop for i from 0 to (1- count)
       collect (get-array gl-array i components)))

(defun (setf get-arrays) (vectors gl-array
                          &optional components (count (gl::gl-array-size gl-array)))
    (assert (<= (length vectors) count) () "More vectors then ~D
Vectors are: ~A" count vectors)
  (loop for vector in vectors
     for i from 0
     do (setf (get-array gl-array i components) vector)))

(defun sf-bool-p (input)
  "True if INPUT contains true or false as a string."
  (when (stringp input)
    (member input '("True" "False" "") :test #'equalp)))

(defun parse-sf-bool (string)
  (declare (string string))
  (check-type string (satisfies sf-bool-p))
  (equalp "True" string))


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

#+ () (defparameter *source* (cxml:parse-file "/home/james/blender25/.x3d" (cxml-dom:make-dom-builder)))






