(in-package :cl-mdl)

(defun load-model (model-name &optional (path-name how::+model-directory+))
  (read (open (merge-pathnames (format nil
				       "~a.lmdl"
				       model-name) path-name))))

(defun draw-model-at-* (model x y)
  (let ((vertices (cdr (assoc :vertices model)))
	(faces (cdr (assoc :faces model))))
    (gl:with-gl-array-values (verts 'vertex '(x y z)) vertices
      (gl:bind-gl-vertex-array verts)
      (gl:with-gl-array-values (faces :unsigned-int)
	  (loop for i across faces collect)))))

(defun draw-model-at (model point)
  (apply 'draw-model-at-* (append (list model) (loop for i across point
						  collect i))))