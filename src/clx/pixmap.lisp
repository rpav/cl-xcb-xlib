(in-package :xcb.clx)

(defstruct (pixmap (:include drawable)
                   (:conc-name %pixmap-)
                   (:constructor %make-pixmap)))

(defmethod print-object ((object pixmap) stream)
  (print-unreadable-object (object stream)
    (format stream "Pixmap (ID:~A)" (xid object))))

 ;; 4.8 Pixmaps

(defun create-pixmap (&key width height depth drawable)
  (xchk (drawable c id)
      (xcb-create-pixmap c depth id (xid drawable) width height)))

(defun free-pixmap (pixmap)
  (xchk (pixmap c)
      (xcb-free-pixmap c (xid pixmap))))

(defun pixmap-display (pixmap)
  (%pixmap-display pixmap))

(defun pixmap-equal (pixmap-1 pixmap-2)
  (xid-equal pixmap-1 pixmap-2))

(defun pixmap-id (pixmap)
  (xid pixmap))

;; PIXMAP-P is implicit in DEFSTRUCT PIXMAP

(defun pixmap-plist (pixmap)
  (xid-plist pixmap))

(defun (setf pixmap-plist) (v pixmap)
  (setf (xid-plist pixmap) v))

