(in-package :xcb.clx)

(defstruct (color (:conc-name color-)
                  (:constructor make-color))
  (red 0.0 :type (single-float 0.0 1.0))
  (green 0.0 :type (single-float 0.0 1.0))
  (blue 0.0 :type (single-float 0.0 1.0)))

 ;; 9.2 Color Functions

;; Most by DEFSTRUCT

(defun color-rgb (color)
  (values (color-red color)
          (color-blue color)
          (color-green color)))


