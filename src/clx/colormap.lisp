(in-package :xcb.clx)

(defstruct (colormap (:conc-name %colormap-)
                     (:constructor %make-colormap))
  (xcb-colormap 0 :type (integer 0 4294967295)))

 ;; 9.3.1 Creating Colormaps

(stub create-colormap (visual window &optional alloc-p))
(stub copy-colormap-and-free (colormap))
(stub free-colormap (colormap))

 ;; 9.3.2 Installing Colormaps

(stub install-colormap (colormap))
(stub installed-colormaps (window &key (result-type 'list)))
(stub uninstall-colormap (colormap))

 ;; 9.3.3 Allocating Colors

(stub alloc-color (colormap color))
(stub alloc-color-cells (colormap colors
                         &key (planes 0) contiguous-p (result-type 'list)))
(stub alloc-color-planes (&key (reds 0) (greens 0) (blues 0)
                            contiguous-p (result-type 'list)))
(stub free-colors (colormap pixels &optional (plane-mask 0)))

 ;; 9.3.4 Finding Colors

(stub lookup-color (colormap name))
(stub query-colors (colormap pixels &key (result-type 'list)))

 ;; 9.3.5 Changing Colors

(stub store-color (colormap pixel color
                   &key (red-p t) (green-p t) (blue-p t)))
(stub store-colors (colormap pixel-colors
                    &key (red-p t) (green-p t) (blue-p t)))

 ;; 9.3.6 Colormap Attributes

(stub colormap-display (colormap))
(stub colormap-equal (c-1 c-2))
(stub colormap-id (colormap))

;; COLORMAP-P

(stub colormap-plist (colormap))
(stub (setf colormap-plist) (v colormap))

