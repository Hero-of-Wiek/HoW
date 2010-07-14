(in-package :how)

(eval-when (:compile-toplevel :load-toplevel)
  (defparameter +root-directory+
    (make-pathname :directory
                   (pathname-directory
                    (load-time-value
                     (or #.*compile-file-truename*
                         *load-truename*))))
    "The root of the HoW game directory.

We use this for locating data and configuration information for HoW. This
may run into some issues in the future but for the near term future this
solves most issues."))

(defparameter +image-directory+
  (merge-pathnames #P"images/" +root-directory+))

(defparameter +model-directory+
  (merge-pathnames #P"models/" +root-directory+))

(defvar *game-frame-size-x* 640
  "X direction for the game's frame.")
(defvar *game-frame-size-y* 480
  "Y direction for the game's frame.")

;;; END
