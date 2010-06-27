(in-package :how)

(defvar +root-directory+
  (asdf:system-relative-pathname (asdf:find-system :how) "/")
  "The root of where the asdf source is at.

We use this for locating data and configuration information for HoW. This
may run into some issues in the future but for the near term future this
solves most issues.")
