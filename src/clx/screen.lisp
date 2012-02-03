(in-package :xcb.clx)

(defstruct (screen (:conc-name %screen-)
                   (:constructor %make-screen))
  (xcb-screen (null-pointer) :type #.(type-of (null-pointer))))

 ;; 3.2 Screen Attributes

(stub screen-backing-stores (screen))
(stub screen-black-pixel (screen))
(stub screen-default-colormap (screen))
(stub screen-depths (screen))
(stub screen-event-mask-at-open (screen))
(stub screen-height (screen))
(stub screen-height-in-millimeters (screen))
(stub screen-max-installed-maps (screen))
(stub screen-min-installed-maps (screen))

;; SCREEN-P is implicit in DEFSTRUCT SCREEN

(stub screen-plist (screen))
(stub (setf screen-plist) (screen))

(stub screen-root (screen))
(stub screen-root-depth (screen))
(stub screen-root-save-unders-p (screen))
(stub screen-white-pixel (screen))

(stub screen-width (screen))
(stub screen-width-in-millimeters (screen))

