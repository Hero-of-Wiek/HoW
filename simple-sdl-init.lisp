(in-package :how)

(defgeneric handle-key-down-event
    (key state &key &allow-other-keys)
  (:documentation "Handle key down presses."))

(defmethod handle-key-down-event :around
    ((key t) (state t) &key (surface sdl:*default-surface*)
     (display sdl:*default-display*))
  "Bind display and surface before calling onwards."
  (let ((sdl:*default-display* display)
        (sdl:*default-surface* surface))
    (call-next-method)))

(defgeneric optional-arg-form (input &key gensym-name))
(defmethod optional-arg-form ((input (eql nil)) &key (gensym-name "G"))
  `(,(gensym gensym-name) t))
(defmethod optional-arg-form ((input symbol) &key (gensym-name "G"))
  `(,(gensym gensym-name) (eql ,input)))
(defmethod optional-arg-form ((input list) &key (gensym-name "G"))
  (declare (ignore gensym-name))
  input)

(defmacro define-key-down-event ((key &optional state &rest keys) &body body)
  `(defmethod handle-key-down-event
       (,(optional-arg-form key :gensym-name "KEY-")
        ,(optional-arg-form state :gensym-name "STATE-") &key ,@keys)
     ,@body))

(define-key-down-event (:sdl-key-escape)
  "Leave the game!"
  (sdl:push-quit-event))
(define-key-down-event ((key t) (state t))
  "If we don't handle the key, say something!"
  (format t "No key-down event! Key: ~S State: ~S~%" key state))

(let (fire)
  (defmethod handle-key-down-event :before ((key t) (state (eql :fire)) &key)
    (or fire (progn (setq fire (how.sprite::load-sprite-sheet (load-image "ani2.bmp") :x 64 :y 64)) fire)))
  (define-key-down-event (:sdl-key-1 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 0))
  (define-key-down-event (:sdl-key-2 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 1))
  (define-key-down-event (:sdl-key-3 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 2))
  (define-key-down-event (:sdl-key-4 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 3))
  (define-key-down-event (:sdl-key-5 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 4))
  (define-key-down-event (:sdl-key-6 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 5))
  (define-key-down-event (:sdl-key-7 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 6))
  (define-key-down-event (:sdl-key-8 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 7))
  (define-key-down-event (:sdl-key-9 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 8))
  (define-key-down-event (:sdl-key-0 :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 9))
  (define-key-down-event (:sdl-key-q :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 10))
  (define-key-down-event (:sdl-key-w :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 11))
  (define-key-down-event (:sdl-key-e :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 12))
  (define-key-down-event (:sdl-key-r :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 13))
  (define-key-down-event (:sdl-key-t :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 14))
  (define-key-down-event (:sdl-key-y :fire)
    (how.sprite::draw-sprite-sheet-at fire :x 30 :y 30 :cell 15)))

(defun start-how ()
  (sdl:with-init (sdl:sdl-init-video )
    (sdl:window *game-frame-size-x* *game-frame-size-y*)
    (setf (sdl:frame-rate) 60)
    (sdl:enable-key-repeat nil nil)
    (let ((house (load-image "house_64x64.bmp"))
	  (dude (make-instance 'how.characters::actor
			       :surface (load-image "stickfigure_64x64.bmp")
			       :name "Igoru"))
	  (dude-position-x 0)
	  (dude-position-y 0))
      (sdl:clear-display sdl:*green*)
      (sdl:draw-surface-at-* house 100 100)
      (sdl:draw-surface-at-* (how.characters::surface dude)
                             dude-position-x dude-position-y)
      (handle-key-down-event :sdl-key-1 :fire)
      (how.health::draw-health-at* (make-instance 'how.health::health :current 14 :maximum 20)
                                   :y 0 :x (- *game-frame-size-x* (* 10 how.health::*heart-image-side-length*)))
      (sdl:with-events ()
        (:quit-event ()
                     (sdl:save-image sdl:*default-display* (merge-pathnames #P"last-surface.bmp" +root-directory+))
                     t)
	(:key-repeat-on)
	(:key-down-event (:key key)
                         (handle-key-down-event key t)
                         (when (sdl:key= key :sdl-key-up) (setf dude-position-y (- dude-position-y 1)))
			 (when (sdl:key= key :sdl-key-down) (setf dude-position-y (+ dude-position-y 1)))
			 (when (sdl:key= key :sdl-key-left) (setf dude-position-x (- dude-position-x 1)))
			 (when (sdl:key= key :sdl-key-right) (setf dude-position-x (+ dude-position-x 1)))
                         (sdl:clear-display sdl:*green*)
                         (sdl:draw-surface-at-* house 100 100)
                         (sdl:draw-surface-at-* (how.characters::surface dude)
                                                dude-position-x dude-position-y)
                         (handle-key-down-event key :fire)

                         (how.health::draw-health-at* (make-instance 'how.health::health :current 14 :maximum 20)
                                                      :y 0 :x (- *game-frame-size-x* (* 10 how.health::*heart-image-side-length*))))
	(:idle ()

	       (sdl:update-display))))))
;;; END
