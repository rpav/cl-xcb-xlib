(in-package :xcb.clx)

(defstruct (drawable (:conc-name %drawable-)
                     (:constructor %make-drawable))
  (id 0 :type (integer 0 4294967295)))

 ;; 4.1 Drawables

(stub drawable-display (drawable))
(stub drawable-equal (drawable))

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
