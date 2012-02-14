(in-package :xcb.clx)

(defstruct (window (:include drawable)
                   (:conc-name %window-)
                   (:constructor %make-window)))

(defmethod print-object ((object window) stream)
  (print-unreadable-object (object stream)
    (format stream "Window (ID:#x~8,'0X)" (xid object))))

 ;; 4.2 Creating Windows

(defvar *window-class-to-xcb*
  '((:copy . :+xcb-window-class-copy-from-parent+)
    (:input-output . :+xcb-window-class-input-output+)
    (:input . :+xcb-window-class-input-only+)))

(define-enum-table window-attr (xcb-cw-t "XCB-CW")
  (:background :back-pixel) (:border :border-pixel)
  :bit-gravity (:gravity :win-gravity) :backing-store
  :backing-planes :backing-pixel :override-redirect
  :save-under :event-mask (:do-not-propagate-mask :dont-propagate)
  :colormap :cursor)

(defconstant +max-window-attrs+ 16)

(defun create-window (&key parent x y width height (depth 0) (border-width 0)
                        (class :copy) (visual :copy) 
                        background border bit-gravity gravity
                        backing-store backing-planes backing-pixel
                        override-redirect save-under event-mask
                        do-not-propagate-mask colormap cursor)
  (let* ((display (drawable-display parent))
         (wid (xcb-generate-id (display-ptr-xcb display)))
         (window (%make-window :display display :id wid))
         (colormap (if colormap (%colormap-xcb-colormap colormap) nil))
         (value-mask 0)
         (attr-count 0))
    (with-foreign-object (values-ptr 'uint-32-t +max-window-attrs+)
      ;; This is very much order-dependent:
      (vl-maybe-set-many (window-attr values-ptr value-mask attr-count)
        background border bit-gravity gravity
        backing-store backing-planes backing-pixel
        override-redirect save-under event-mask
        do-not-propagate-mask colormap cursor)
      (xerr display
          (xcb-create-window-checked
           (display-ptr-xcb display) depth wid (xid parent)
           x y width height
           border-width (foreign-enum-value
                         'xcb-window-class-t
                         (cdr (assoc class *window-class-to-xcb*)))
           (if (eq visual :copy)
               (window-visual parent)
               visual)
           value-mask values-ptr))
      window)))

 ;; 4.3 Window Attributes

(stub window-all-event-masks (window))

(stub (setf window-background) (background window))

(stub window-backing-pixel (window))
(stub (setf window-backing-pixel) (v window))

(stub window-backing-planes (window))
(stub (setf window-backing-planes) (v window))

(stub window-backing-store (window))
(stub (setf window-backing-store) (v window))
 
(stub window-bit-gravity (window))
(stub (setf window-bit-gravity) (v window))

(stub (setf window-border) (border window))

(stub window-class (window))
(stub window-colormap (window))
(stub window-colormap-installed-p (window))

(stub (setf window-cursor) (cursor window))

(stub window-display (window))

(stub window-do-not-propagate-mask (window))
(stub (setf window-do-not-propagate-mask) (mask window))

(stub window-equal (window-1 window-2))

(stub window-event-mask (window))
(stub (setf window-event-mask) (mask window))

(stub window-gravity (window))
(stub (setf window-gravity) (gravity window))

(stub window-id (window))
(stub window-map-state (window))

(stub window-override-redirect (window))
(stub (setf window-override-redirect) (override-redirect window))

;; WINDOW-P is implicit in DEFSTRUCT WINDOW

(stub window-plist (window))
(stub (setf window-plist) (v window))

(stub set-window-priority (window-priority &optional sibling) (mode))

(stub window-save-under (window))
(stub (setf window-save-under) (v window))

(defun window-visual (window)
  (let* ((con (display-ptr-xcb window))
         (cookie (xcb-get-window-attributes-unchecked con (xid window))))
    (with-xcb-clx-reply ((display-for window) cookie ptr err)
        (xcb-get-window-attributes-reply con cookie err)
      (xcb-get-window-attributes-reply-t-visual ptr))))

 ;; 4.4 Stacking Order

(stub circulate-window-down (window))
(stub circulate-window-up (window))

 ;; 4.5 Window Hierarchy

(stub query-tree (window &key (result-type 'list)))
(stub reparent-window (window parent x y))

(stub translate-coordinates (source source-x source-y destination))

 ;; 4.6 Mapping

(defun map-window (window)
  (xerr window (xcb-map-window-checked (display-ptr-xcb window)
                                       (xid window))))

(stub map-subwindows (window))
(stub unmap-window (window))
(stub unmap-subwindows (window))

 ;; 4.7 Destroying Windows

(defun destroy-window (window)
  (xerr window (xcb-destroy-window-checked (display-ptr-xcb window)
                                           (xid window))))

(stub destroy-subwindows (window))

