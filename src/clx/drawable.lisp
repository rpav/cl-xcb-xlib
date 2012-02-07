(in-package :xcb.clx)

(defstruct (drawable (:conc-name %drawable-)
                     (:constructor %make-drawable))
  (display nil :type display)
  (id 0 :type (integer 0 4294967295)))

(defmethod print-object ((object drawable) stream)
  (print-unreadable-object (object stream)
    (format stream "Drawable (ID:~A)" (%drawable-id object))))

 ;; 4.1 Drawables

(declaim (inline drawable-display))
(defun drawable-display (drawable)
  (%drawable-display drawable))

(defun drawable-equal (d-1 d-2)
  (and (eq (%drawable-display d-1) (%drawable-display d-2))
       (eq (%drawable-id d-1) (%drawable-id d-2)))) 

;; DRAWABLE-P is implicit in DEFSTRUCT DRAWABLE

(stub drawable-plist (drawable))

 ;; 4.3 Window attributes

(stub drawable-border-width (drawable))
(stub (setf drawable-border-width) (drawable))
(stub drawable-depth (drawable))
(stub drawable-height (drawable))
(stub drawable-width (drawable))
(stub drawable-x (drawable))
(stub drawable-y (drawable))

(stub-macro with-state (drawable &body body))

 ;; 4.5 Window Hierarchy

(stub drawable-root (drawable))
