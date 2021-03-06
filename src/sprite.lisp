(in-package :how.sprite)

(defun generate-cells (x y width height)
  "Generate a cell list for use with sprite sheets"
  (loop for cell-y from 0 to (- height y) by y
                            append (loop for cell-x from 0 to (- width x) by x
                                         collect (list cell-x cell-y x y))))

(defun load-sprite-sheet (sprite-sheet &key x y)
  "Set the cells for a sprite sheet at `sdl:cells' using `generate-cells' returning sprite-sheet"
  (setf (sdl:cells sprite-sheet) (generate-cells x y (sdl:width sprite-sheet) (sdl:height sprite-sheet)))
  (values sprite-sheet))

(defun draw-sprite-sheet (sprite-sheet &key cell
                             (surface sdl:*default-surface*))
  "draw surface for a sprite sheet"
  (sdl:draw-surface sprite-sheet :cell cell
                       :surface surface))

(defun draw-sprite-sheet-at (sprite-sheet point &key cell
                             (surface sdl:*default-surface*))
  "draw surface for a sprite sheet"
  (sdl:draw-surface-at sprite-sheet point :cell cell
                       :surface surface))

(defun draw-sprite-sheet-at-* (sprite-sheet x y &key cell
                              (surface sdl:*default-surface*))
  "draw surface for a sprite sheet"
  (declare ((integer 0 #.most-positive-fixnum) x y))
  (sdl:draw-surface-at-* sprite-sheet x y :cell cell :surface surface))
