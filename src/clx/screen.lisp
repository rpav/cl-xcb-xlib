(in-package :xcb.clx)

(defstruct (screen (:conc-name %screen-)
                   (:constructor %make-screen))
  (display nil :type display)
  (xcb-screen (null-pointer) :type #.(type-of (null-pointer))))

 ;; 3.2 Screen Attributes

(defun screen-backing-stores (screen)
  (xcb-screen-t-backing-stores (%screen-xcb-screen screen)))

(defun screen-black-pixel (screen)
  (xcb-screen-t-black-pixel (%screen-xcb-screen screen)))

(defun screen-default-colormap (screen)
  (%make-colormap :xcb-colormap
                  (xcb-screen-t-default-colormap (%screen-xcb-screen screen))))

(stub screen-depths (screen))
(stub screen-event-mask-at-open (screen))

(defun screen-height (screen)
  (xcb-screen-t-height-in-pixels (%screen-xcb-screen screen)))

(defun screen-height-in-millimeters (screen)
  (xcb-screen-t-height-in-millimeters (%screen-xcb-screen screen)))

(defun screen-max-installed-maps (screen)
  (xcb-screen-t-max-installed-maps (%screen-xcb-screen screen)))

(defun screen-min-installed-maps (screen)
  (xcb-screen-t-min-installed-maps (%screen-xcb-screen screen)))

;; SCREEN-P is implicit in DEFSTRUCT SCREEN

(stub screen-plist (screen))
(stub (setf screen-plist) (screen))

(defun screen-root (screen)
  (%make-window
   :display (%screen-display screen)
   :id (xcb-screen-t-root (%screen-xcb-screen screen))))
  
(defun screen-root-depth (screen)
  (xcb-screen-t-root-depth (%screen-xcb-screen screen)))

(defun screen-root-save-unders-p (screen)
  (xcb-screen-t-save-unders (%screen-xcb-screen screen)))

(defun screen-white-pixel (screen)
  (xcb-screen-t-white-pixel (%screen-xcb-screen screen)))

(defun screen-width (screen)
  (xcb-screen-t-width-in-pixels (%screen-xcb-screen screen)))

(defun screen-width-in-millimeters (screen)
  (xcb-screen-t-width-in-millimeters (%screen-xcb-screen screen)))
