(in-package :xcb.clx)

(defstruct (color (:conc-name color-)
                  (:constructor make-color))
  (red 0.0 :type (single-float 0.0 1.0))
  (green 0.0 :type (single-float 0.0 1.0))
  (blue 0.0 :type (single-float 0.0 1.0)))

(declaim (inline make-color-from-ints))
(defun make-color-from-ints (&key red green blue)
  (flet ((i2f (i) (if (= 0 i) 0.0 (/ i 65535.0))))
    (make-color :red (i2f red) :green (i2f green) :blue (i2f blue))))

(declaim (inline color-red-int color-green-int color-blue-int))
(defun color-red-int (c) (truncate (* 65535 (color-red c))))
(defun color-green-int (c) (truncate (* 65535 (color-green c))))
(defun color-blue-int (c) (truncate (* 65535 (color-blue c))))

 ;; 9.2 Color Functions

;; Most by DEFSTRUCT

(defun color-rgb (color)
  (values (color-red color)
          (color-blue color)
          (color-green color)))


