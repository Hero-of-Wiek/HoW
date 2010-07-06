(defpackage #:how.gl.util
  (:use :cl))

(defun defcarray (array type)
  "Make a C ARRAY of TYPE."
  (declare (vector array))
  (let ((pointer (cffi:foreign-alloc type)))
    (loop for a across array for i = 0 then (incf i) do
         (cond ((vectorp a) (setf (cffi:mem-aref pointer :pointer i)
                                  (defcarray a type)))
               (t (setf (cffi:mem-aref pointer type i)
                        (cffi:convert-to-foreign a type)))))
    (values pointer)))
