(in-package :xcb.clx)

(defstruct (display (:conc-name %display-)
                    (:constructor %make-display))
  (xlib-display (null-pointer) :type #.(type-of (null-pointer)))
  (xcb-connection (null-pointer) :type #.(type-of (null-pointer)))
  (xcb-setup (null-pointer) :type #.(type-of (null-pointer)))
  (event-queue (make-queue) :type queue))

(defmethod print-object ((object display) stream)
  (print-unreadable-object (object stream)
    (format stream "Display")))

 ;; 2.2 Opening the Display

(defun open-display (host &key (display 0) protocol)
  (declare (ignore protocol))
  (let ((d (%make-display))
        (dpy (xopen-display (concatenate 'string host ":"
                                         (princ-to-string display)))))
    (if (null-pointer-p dpy)
        (error "Error opening display ~A" display))
    (xset-event-queue-owner dpy :+xcbowns-event-queue+)
    (let* ((c (xget-xcbconnection dpy))
           (s (xcb-get-setup c)))
      (setf (%display-xlib-display d) dpy)
      (setf (%display-xcb-connection d) c)
      (setf (%display-xcb-setup d) s))
    d))

 ;; 2.3 Display Attributes

(defun display-authorization-data (display) "")
(defun display-authorization-name (display) "")

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

(defun display-protocol-major-version (display)
  (xcb-setup-t-protocol-major-version (%display-xcb-setup display)))

(defun display-protocol-minor-version (display)
  (xcb-setup-t-protocol-minor-version (%display-xcb-setup display)))

(defun display-protocol-version (display)
  (values (display-protocol-major-version display)
          (display-protocol-minor-version display)))

(stub display-resource-id-base (display))
(stub display-resource-id-mask (display))

(defun display-roots (display)
  (let ((dpyptr (%display-xlib-display display))
        (i -1))
    (mapcar (lambda (ptr)
              (incf i)
              (%make-screen :display display
                            :xcb-screen ptr
                            :xlib-screen (xscreen-of-display dpyptr i)))
            (xcb-setup-roots-iterator (%display-xcb-setup display)))))

(defun display-vendor-name (display)
  (let ((end (xcb-setup-vendor-length (%display-xcb-setup display))))
    (subseq (xcb-setup-vendor (%display-xcb-setup display)) 0 end)))

;; FIXME?
(defun display-release-number (display)
  (xcb-setup-t-release-number (%display-xcb-setup display)))

(defun display-vendor (display)
  (values (display-vendor-name display)
          (display-release-number display)))

;; FIXME?
(defun display-version-number (display)
  (values (display-protocol-major-version display)
          (display-protocol-minor-version display)))

(stub display-xid (display))

(stub-macro with-display (display &body body))

 ;; 2.4 Managing the Output Buffer

(stub display-after-function (display))
(stub (setf display-after-function) (display))

(defun display-force-output (display)
  (xcb-flush (%display-xcb-connection display)))

;; FIXME .. more should be done here?
(defun display-finish-output (display)
  (display-force-output display))

 ;; 2.5 Closing the Display

(defun close-display (display)
  (xcb-disconnect (%display-xcb-connection display))
  (setf (%display-xcb-connection display) (null-pointer))
  (setf (%display-xcb-setup display) (null-pointer))
  (setf (%display-xlib-display display) (null-pointer))
  (values))
