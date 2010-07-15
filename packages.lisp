(in-package :cl-user)

(defpackage #:how
  (:use :cl))

(defpackage #:how.characters
  (:use :cl))

(defpackage #:how.sprite
  (:use :cl))

(defpackage #:how.health
  (:use :cl)
  (:export #:health))

(defpackage #:cl-mdl
  (:use :cl)
  (:export #:load-model
           #:draw-model-at-*
	   #:draw-model-at
	   #:draw-model))