(in-package :xcb.clx)

(defstruct (pixmap (:include drawable)
                   (:conc-name %pixmap-)
                   (:constructor %make-pixmap)))

 ;; 4.8 Pixmaps

(stub create-pixmap (&key width height depth drawable))
(stub free-pixmap (pixmap))
(stub pixmap-display (pixmap))
(stub pixmap-equal (pixmap-1 pixmap-2))
(stub pixmap-id (pixmap))

;; PIXMAP-P is implicit in DEFSTRUCT PIXMAP

(stub pixmap-plist (pixmap))
(stub (setf pixmap-plist) (v pixmap))
