(in-package :xcb.clx)

(defstruct (display (:conc-name %display-)
                    (:constructor %make-display))
  (xcb-connection (null-pointer) :type #.(type-of (null-pointer)))
  (xlib-display (null-pointer) :type #.(type-of (null-pointer))))

 ;; 2.2 Opening the Display

(defun open-display (host &key (display 0) protocol)
  (declare (ignore protocol))
  (let ((d (%make-display))
        (dpy (xopen-display (concatenate 'string host ":"
                                         (princ-to-string display)))))
    (if (null-pointer-p dpy)
        (error "Error opening display ~A" display))
    (setf (%display-xlib-display d) dpy)
    (setf (%display-xcb-connection d) (xget-xcbconnection dpy))
    d))

 ;; 2.3 Display Attributes

(stub display-authorization-data (display))
(stub display-authorization-name (display))
(stub display-bitmap-format (display))
(stub display-byte-order (display))
(stub display-display (display))
(stub display-error-handler (display))
(stub (setf display-error-handler) (display))
(stub display-image-lsb-first-p (display))
(stub display-keycode-range (display))
(stub display-max-keycode (display))
(stub display-max-request-length (display))
(stub display-min-keycode (display))
(stub display-motion-buffer-size (display))

;; DISPLAY-P is implicit for DEFSTRUCT DISPLAY

(stub display-pixmap-formats (display))
(stub display-plist (display))
(stub (setf display-plist) (display))
(stub display-protocol-major-version (display))
(stub display-protocol-minor-version (display))
(stub dipslay-protocol-version (display))
(stub display-resource-id-base (display))
(stub display-resource-id-mask (display))
(stub display-roots (display)) ;;
(stub display-vendor (display))
(stub display-vendor-name (display))
(stub display-version-number (display))
(stub display-xid (display))
(stub-macro with-display (display &body body))

 ;; 2.4 Managing the Output Buffer

(stub display-after-function (display))
(stub (setf display-after-function) (display))

(stub display-force-output (display))
(stub display-finish-output (display))

 ;; 2.5 Closing the Display

(stub close-display (display))
