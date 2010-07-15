(in-package :cl-mdl)

(gl:define-gl-array-format vertex
  (gl:vertex :type :float :components (x y z)))

(defun load-model (model-name &optional (path-name how::+model-directory+))
  (read (open (merge-pathnames (format nil
				       "~a.lmdl"
				       model-name) path-name))))

(defun draw-model-at-* (model x y)
  (let ((vertices (cdr (assoc :vertices model)))
	(faces1 (cdr (assoc :faces model)))
	(faces2 '()))
    (gl:with-gl-array-values (verts 'vertex '(x y z)) vertices
      (gl:bind-gl-vertex-array verts)
      (gl:with-gl-array-values (faces :unsigned-int)
	  (loop for i in faces1 do (setf faces2 (append faces2 i)))
	(gl:draw-elements :quads faces)))))

(defun draw-model-at (model point)
  (apply 'draw-model-at-* (append (list model) (loop for i across point
						  collect i))))

(defun draw-model (model)
  (draw-model-at #(0 0) model))