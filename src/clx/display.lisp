(in-package :xcb.clx)

 ;; Threading

(defvar *display* nil)

(defstruct display-msg
  return-channel fn)

(defun display-thread-loop (display)
  (let ((*display* display)
        (channel (%display-send-channel display)))
    (loop while *display* do
      (let ((msg (chanl:recv channel)))
        (handler-case
            (chanl:send (display-msg-return-channel msg)
                        (multiple-value-list
                         (funcall (display-msg-fn msg))))
          (xlib-io-error (e)
            ;; special case of CLOSE-DISPLAY
            (run-hooks (close-display-hook display) display nil)
            (setf *display* nil)
            (setf (%display-xcb-connection display) (null-pointer))
            (setf (%display-xcb-setup display) (null-pointer))
            (setf (%display-xlib-display display) (null-pointer))
            (setf (%display-send-channel display) nil)
            (chanl:send (display-msg-return-channel msg) e)
            (return-from display-thread-loop))
          (error (e)
            (chanl:send (display-msg-return-channel msg) e)))))))

(defun display-funcall (display function)
  (let ((msg (make-display-msg :return-channel (make-instance 'chanl:channel)
                               :fn function)))
    (chanl:send (%display-send-channel display) msg)
    (let ((result (chanl:recv (display-msg-return-channel msg))))
      (etypecase result
        (request-error
         (with-slots (error-key major minor sequence current-sequence)
             result
           (funcall (%display-error-handler display)
                    display (type-of result)
                    :current-sequence current-sequence
                    :major major
                    :minor minor
                    :sequence sequence
                    :condition result)))
        (error (error result))
        (t (values-list result))))))

(defun default-error-handler (display error
                              &key current-sequence major minor sequence
                                resource-id atom-id value
                                condition
                              &allow-other-keys)
  (declare (ignore display error current-sequence major minor sequence
                   resource-id atom-id value))
  (error condition))

(defmacro do-on-display (display &body body)
  `(display-funcall ,display (lambda () ,@body)))

 ;; DISPLAY type

(defstruct (display (:conc-name %display-)
                    (:constructor %make-display))
  (number 0 :type fixnum)
  (host nil)
  (xlib-display (null-pointer) :type #.(type-of (null-pointer)))
  (xcb-connection (null-pointer) :type #.(type-of (null-pointer)))
  (xcb-setup (null-pointer) :type #.(type-of (null-pointer)))
  (event-queue (make-queue) :type queue)
  (queue-lock (bt:make-recursive-lock))
  (send-channel (make-instance 'chanl:channel) :type (or null chanl:channel))
  (error-handler #'default-error-handler :type function)
  (key-symbols (null-pointer) :type #.(type-of (null-pointer)))
  (pixmap-formats nil :type list)
  (close-down-mode :destroy :type keyword)
  (close-hook nil)
  (plist nil :type list))

(defmethod print-object ((object display) stream)
  (print-unreadable-object (object stream)
    (format stream "Display ~A" (%display-number object))))

(defgeneric display-for (object))
(defmethod display-for ((object display)) object)

(declaim (inline display-ptr-xlib display-ptr-xcb))
(defun display-ptr-xlib (object)
  (%display-xlib-display (display-for object)))

(defun display-ptr-xcb (object)
  (%display-xcb-connection (display-for object)))

 ;; DISPLAY-ID-PAIR type

(defstruct (display-id-pair (:conc-name %xid-)
                            (:constructor %make-xid))
  (display nil :type display)
  (id 0 :type (integer 0 4294967295))
  (plist nil :type list))

(declaim (inline xid xid-plist))
(defun xid (display-id-pair)
  (if display-id-pair
      (%xid-id display-id-pair)
      0))

(defmethod display-for ((object display-id-pair))
  (%xid-display object))

(defun xid-equal (x-1 x-2)
  (and (eq (%xid-display x-1) (%xid-display x-2))
       (eq (xid x-1) (xid x-2))))

(defun xid-plist (x)
  (%xid-plist x))

(defun (setf xid-plist) (v x)
  (setf (%xid-plist x) v))

 ;; 2.2 Opening the Display

(defun open-display (host &key (display 0) protocol)
  (declare (ignore protocol))
  (let* ((d (%make-display))
         (fn (lambda () (display-thread-loop d))))
    (chanl:pcall fn)
    (do-on-display d
      (let ((dpy (xopen-display (concatenate 'string host ":"
                                             (princ-to-string display)))))
        (if (null-pointer-p dpy)
            (error "Error opening display ~A" display))
        (xlock-display dpy)
        (xset-event-queue-owner dpy :+xcbowns-event-queue+)
        (let* ((c (xget-xcbconnection dpy))
               (s (xcb-get-setup c)))
          (setf (%display-number d) display
                (%display-host d) host
                (%display-xlib-display d) dpy
                (%display-xcb-connection d) c
                (%display-xcb-setup d) s
                (%display-key-symbols d) (xcb-key-symbols-alloc c)))
        (xset-error-handler (callback xcb::xlib-error-handler))
        (xset-ioerror-handler (callback xcb::xlib-io-error-handler))
        d))))

 ;; 2.3 Display Attributes

(defun display-authorization-data (display)
  (declare (ignore display)) "")

(defun display-authorization-name (display)
  (declare (ignore display)) "")

(defstruct bitmap-format
  unit pad lsb-first-p)

(defun display-bitmap-format (display)
  (let ((setup (%display-xcb-setup display)))
    (make-bitmap-format :unit (xcb-setup-t-bitmap-format-scanline-unit setup)
                        :pad (xcb-setup-t-bitmap-format-scanline-pad setup)
                        :lsb-first-p
                        (= 0 (xcb-setup-t-bitmap-format-bit-order setup)))))

;; Does XCB otherwise provide this?  Not sure there's any need.
(defun display-byte-order (display)
  (let ((setup (%display-xcb-setup display)))
    (if (= 0 (xcb-setup-t-image-byte-order setup))
        :lsbfirst :msbfirst)))

(defun display-display (display)
  (%display-number display))

(defun display-error-handler (display)
  (%display-error-handler display))

(defun (setf display-error-handler) (handler display)
  (setf (%display-error-handler display) handler))

(defun display-image-lsb-first-p (display)
  (let ((setup (%display-xcb-setup display)))
    (if (= 0 (xcb-setup-t-image-byte-order setup))
        :lsbfirst :msbfirst)))

(defun display-keycode-range (display)
  (let ((setup (%display-xcb-setup display)))
    (values (xcb-setup-t-min-keycode setup)
            (xcb-setup-t-max-keycode setup))))

(defun display-max-keycode (display)
  (let ((setup (%display-xcb-setup display)))
    (xcb-setup-t-max-keycode setup)))

(defun display-max-request-length (display)
  (let ((setup (%display-xcb-setup display)))
    (xcb-setup-t-maximum-request-length setup)))

(defun display-min-keycode (display)
  (let ((setup (%display-xcb-setup display)))
    (xcb-setup-t-min-keycode setup)))

(defun display-motion-buffer-size (display)
  (let ((setup (%display-xcb-setup display)))
    (xcb-setup-t-motion-buffer-size setup)))

;; DISPLAY-P is implicit for DEFSTRUCT DISPLAY

(defstruct pixmap-format
  depth bits-per-pixel scanline-pad)

(defun display-pixmap-formats (display)
  (or (%display-pixmap-formats display)
      (let* ((setup (%display-xcb-setup display))
             (formats (map-result-list 'list
                                       (lambda (ptr)
                                         (make-pixmap-format :depth (xcb-format-t-depth ptr)
                                                             :bits-per-pixel
                                                             (xcb-format-t-bits-per-pixel ptr)
                                                             :scanline-pad
                                                             (xcb-format-t-scanline-pad ptr)))
                                       #'xcb-setup-pixmap-formats
                                       #'xcb-setup-pixmap-formats-length
                                       setup 'xcb-format-t)))
        (setf (%display-pixmap-formats display) formats))))

(defun find-pixmap-format (display &key depth bpp)
  (find-if (lambda (fmt)
             (and (if depth (= depth (pixmap-format-depth fmt)) t)
                  (if bpp   (= bpp (pixmap-format-bits-per-pixel fmt)) t)))
           (display-pixmap-formats display)))

(defun display-plist (display)
  (%display-plist display))

(defun (setf display-plist) (v display)
  (setf (%display-plist display) v))

(defun display-protocol-major-version (display)
  (xcb-setup-t-protocol-major-version (%display-xcb-setup display)))

(defun display-protocol-minor-version (display)
  (xcb-setup-t-protocol-minor-version (%display-xcb-setup display)))

(defun display-protocol-version (display)
  (values (display-protocol-major-version display)
          (display-protocol-minor-version display)))

(defun display-resource-id-base (display)
  (let ((setup (%display-xcb-setup display)))
    (xcb-setup-t-resource-id-base setup)))

(defun display-resource-id-mask (display)
  (let ((setup (%display-xcb-setup display)))
    (xcb-setup-t-resource-id-mask setup)))

(defun display-roots (display)
  (let* ((list (mapcar (lambda (ptr)
                         (%make-screen :display display :xcb-screen ptr))
                       (xcb-setup-roots-iterator (%display-xcb-setup display))))
         (len (1- (length list))))
    (dolist (screen list)
      (setf (%screen-number screen) len)
      (decf len))
    (nreverse list)))

(defun display-vendor-name (display)
  (let ((end (xcb-setup-vendor-length (%display-xcb-setup display))))
    (subseq (xcb-setup-vendor (%display-xcb-setup display)) 0 end)))

;; FIXME?
(defun display-release-number (display)
  (xcb-setup-t-release-number (%display-xcb-setup display)))

(defun display-vendor (display)
  (values (display-vendor-name display)
          (display-release-number display)))

;; McCLIM apparently uses this
(defun display-host (display)
  (%display-host display))

;; FIXME?
(defun display-version-number (display)
  (values (display-protocol-major-version display)
          (display-protocol-minor-version display)))

(defun generate-id (display)
  (xcb-generate-id (display-ptr-xcb display)))

(defun display-xid (display)
  (declare (ignore display))
  #'generate-id)

(defmacro with-display (display &body body)
  (let ((fn (gensym "FN")))
    `(flet ((,fn () ,@body))
       (if (eq ,display *display*)
           (,fn)
           (display-funcall ,display #',fn)))))

 ;; 2.4 Managing the Output Buffer

;; FIXME, or don't.
(stub display-after-function (display))
(stub (setf display-after-function) (display))

(defun display-force-output (display)
  (with-display display
    (xcb-flush (%display-xcb-connection display))))

;; FIXME .. more should be done here?
(defun display-finish-output (display)
  (display-force-output display))

 ;; 2.5 Closing the Display

(declaim (inline close-display-hook (setf close-display-hook)))

(defun close-display-hook (display)
  (%display-close-hook display))

(defun (setf close-display-hook) (v display)
  (setf (%display-close-hook display) v))

(defun close-display (display)
  (with-display display
    (run-hooks (close-display-hook display) display t)
    (setf *display* nil)
    (xcb-key-symbols-free (%display-key-symbols display))
    (xclose-display (%display-xlib-display display))
    (xunlock-display (%display-xlib-display display))
    (setf (%display-xcb-connection display) (null-pointer)
          (%display-xcb-setup display) (null-pointer)
          (%display-xlib-display display) (null-pointer))
    (values)))
