(in-package :xcb.clx)

(defstruct (window (:include drawable)
                   (:conc-name %window-)
                   (:constructor %make-window)))

(defmethod print-object ((object window) stream)
  (print-unreadable-object (object stream)
    (format stream "Window (ID:#x~8,'0X)" (xid object))))

 ;; 4.2 Creating Windows

(define-enum-table xcb-window-class (xcb-window-class-t "XCB-WINDOW-CLASS")
  (:copy :copy-from-parent)
  :input-output
  (:input :input-only))

(defun create-window (&key parent x y width height (depth 0) (border-width 0)
                        (class :copy) (visual :copy) 
                        background border bit-gravity gravity
                        backing-store backing-planes backing-pixel
                        override-redirect save-under event-mask
                        do-not-propagate-mask colormap cursor)
  (let* ((display (drawable-display parent))
         (wid (xcb-generate-id (display-ptr-xcb display)))
         (window (%make-window :display display :id wid))
         (colormap (if colormap (%colormap-id colormap) nil))
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
           border-width (xcb-window-class class)
           (if (eq visual :copy)
               (window-visual parent)
               (or visual 0))
           value-mask values-ptr))
      window)))

 ;; 4.3 Window Attributes

(define-attr-accessor window-all-event-masks all-event-masks)

(stub (setf window-background) (background window))

(define-attr-accessor window-backing-pixel backing-pixel)
(define-attr-accessor window-backing-planes backing-planes)
(define-attr-accessor window-backing-store backing-store)
;; FIXME: keywords not numbers
(define-attr-accessor window-bit-gravity bit-gravity)

(define-attr-accessor window-border border :getter-p nil
                      :in (lambda (w)
                            (etypecase w
                              ((unsigned-byte 32) w)
                              (pixmap (xid w))
                              (keyword w))))

(define-attr-accessor window-class -class :setter-p nil)
(define-attr-accessor window-colormap colormap
    :in (lambda (cm) (%colormap-id cm))
    :out (lambda (cid) (%make-colormap :display (display-for window)
                                       :id cid)))
(define-attr-accessor window-colormap-installed-p map-is-installed
    :setter-p nil)

;; FIXME: once cursors are
(stub (setf window-cursor) (cursor window))

(defun window-display (window)
  (%window-display window))

(define-attr-accessor window-do-not-propagate-mask do-not-propagate-mask)

(defun window-equal (window-1 window-2)
  (drawable-equal window-1 window-2))

(define-attr-accessor window-event-mask your-event-mask)
(define-attr-accessor window-gravity win-gravity)

(defun window-id (window) (%window-id window))

(define-attr-accessor window-map-state map-state :setter-p nil)
(define-attr-accessor window-override-redirect override-redirect)

;; WINDOW-P is implicit in DEFSTRUCT WINDOW

(defun window-plist (window)
  (xid-plist window))

(defun (setf window-plist) (v window)
  (setf (xid-plist window) v))

;; stack-mode and sibling
(stub set-window-priority (window-priority &optional sibling) (mode))

(define-attr-accessor window-save-under save-under)
(define-attr-accessor window-visual visual)

 ;; 4.4 Stacking Order

(define-enum-table circulate (xcb-circulate-t "XCB-CIRCULATE")
  :raise-lowest :lower-highest)

(defun circulate-window-down (window)
  (xerr window
      (xcb-circulate-window (display-ptr-xcb window)
                            (circulate :lower-highest)
                            (xid window))))

(defun circulate-window-up (window)
    (xerr window
      (xcb-circulate-window (display-ptr-xcb window)
                            (circulate :raise-lowest)
                            (xid window))))

 ;; 4.5 Window Hierarchy

(defun query-tree (window &key (result-type 'list))
  (do-request-response (window c ck reply err)
      (xcb-query-tree c (xid window))
      (xcb-query-tree-reply c ck err)
    (let ((dpy (display-for window)))
      (map result-type
           (lambda (wid) (%make-window :display dpy :id wid))
           (loop with ptr = (xcb-query-tree-children reply)
                 for i from 0 below (xcb-query-tree-children-length reply)
                 collect (mem-aref ptr 'xcb-window-t i))))))

(defun reparent-window (window parent x y)
  (xerr window
      (xcb-reparent-window-checked (display-ptr-xcb window)
                                   (xid window) (xid parent) x y)))

(defun translate-coordinates (source source-x source-y destination)
  (do-request-response (source c ck reply err)
      (xcb-translate-coordinates c (xid source) (xid destination)
                                 source-x source-y)
      (xcb-translate-coordinates-reply c ck err)
    (let* ((wid (xcb-translate-coordinates-reply-t-child reply))
           (child (when (/= 0 wid)
                    (%make-window :display (display-for source)
                                  :id wid))))
      (values (xcb-translate-coordinates-reply-t-dst-x reply)
              (xcb-translate-coordinates-reply-t-dst-y reply)
              child))))

 ;; 4.6 Mapping

(defun map-window (window)
  (xerr window (xcb-map-window-checked (display-ptr-xcb window)
                                       (xid window))))

(defun map-subwindows (window)
  (xerr window (xcb-map-subwindows-checked (display-ptr-xcb window)
                                           (xid window))))

(defun unmap-window (window)
  (xerr window (xcb-unmap-window-checked (display-ptr-xcb window)
                                         (xid window))))

(defun unmap-subwindows (window)
  (xerr window (xcb-unmap-subwindows-checked (display-ptr-xcb window)
                                             (xid window))))

 ;; 4.7 Destroying Windows

(defun destroy-window (window)
  (xerr window (xcb-destroy-window-checked (display-ptr-xcb window)
                                           (xid window))))

(defun destroy-subwindows (window)
  (xerr window (xcb-destroy-subwindows-checked (display-ptr-xcb window)
                                               (xid window))))

