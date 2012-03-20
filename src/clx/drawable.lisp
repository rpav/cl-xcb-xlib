(in-package :xcb.clx)

(defstruct (drawable (:include display-id-pair)
                     (:conc-name %drawable-)
                     (:constructor %make-drawable)))

(defmethod print-object ((object drawable) stream)
  (print-unreadable-object (object stream)
    (format stream "Drawable (ID:#x~8,'0X)" (xid object))))

 ;; 4.1 Drawables

(declaim (inline drawable-display))
(defun drawable-display (drawable)
  (%drawable-display drawable))

(defun drawable-equal (d-1 d-2)
  (xid-equal d-1 d-2))

;; DRAWABLE-P is implicit in DEFSTRUCT DRAWABLE

(defun drawable-plist (drawable)
  (xid-plist drawable))

(defun (setf drawable-plist) (v drawable)
  (setf (xid-plist drawable) v))

 ;; 4.3 Window attributes

(define-enum-table window-attr (xcb-cw-t "XCB-CW")
  (:background :back-pixel) (:border :border-pixel)
  :bit-gravity (:gravity :win-gravity) :backing-store
  :backing-planes :backing-pixel :override-redirect
  :save-under :event-mask (:do-not-propagate-mask :dont-propagate)
  :colormap :cursor) ;; *window-attr-map*

(defconstant +max-window-attrs+ (length *window-attr-map*))

(define-enum-table window-config (xcb-config-window-t "XCB-CONFIG-WINDOW")
  :x :y :width :height :border-width :sibling :stack-mode)

(defconstant +max-window-config+ (length *window-config-map*))

(define-enum-table window-stack-mode (xcb-stack-mode-t "XCB-STACK-MODE")
  :above :below :top-if :bottom-if :opposite)

(defconstant +max-window-stack+ (length *window-stack-mode-map*))

(defvar *drawable-attributes* nil)
(defvar *drawable-geometry* nil)
(defvar *drawable-attributes-changed* nil)
(defvar *drawable-hold-send* nil)

(defmacro with-attributes ((drawable &optional (attr-var (gensym))) &body body)
  (let ((fn (gensym "BODY"))
        (c (gensym "C"))
        (reply (gensym "REPLY"))
        (err (gensym "ERR")))
    `(let ((*drawable-attributes* *drawable-attributes*)
           (*drawable-attributes-changed*
             (or *drawable-attributes-changed* (make-hash-table))))
       (flet ((,fn ()
                (let ((,attr-var *drawable-attributes*))
                  (declare (ignorable ,attr-var))
                  (prog1
                      ,@body
                    (when (and (not *drawable-hold-send*)
                               (> (hash-table-count *drawable-attributes-changed*) 0))
                      (%send-changes ,drawable))))))
         (if *drawable-attributes*
             (,fn)
             (do-request-response (,drawable ,c ,reply ,err)
                 (xcb-get-window-attributes ,c (xid ,drawable))
               (setf *drawable-attributes* ,reply)
               (,fn)))))))

(defmacro with-geometry ((drawable &optional (geom-var (gensym))) &body body)
  (let ((fn (gensym "BODY"))
        (c (gensym "C"))
        (reply (gensym "REPLY"))
        (err (gensym "ERR")))
    `(let ((*drawable-geometry* *drawable-geometry*))
       (flet ((,fn () (let ((,geom-var *drawable-geometry*)) ,@body)))
         (if *drawable-geometry*
             (,fn)
             (do-request-response (,drawable ,c ,reply ,err)
                 (xcb-get-geometry ,c (xid ,drawable))
               (setf *drawable-geometry* ,reply)
               (,fn)))))))

(defmacro hash-let ((hash-table &rest symbols) &body body)
  (let ((symbols (if (consp (car symbols)) (car symbols) symbols)))
    `(let (,@(loop for s in symbols collect
                   `(,s (gethash ',s ,hash-table))))
       ,@body)))

(defun %send-changes (drawable)
  (hash-let (*drawable-attributes-changed*
             background border bit-gravity gravity
             backing-store backing-planes backing-pixel
             override-redirect save-under event-mask
             do-not-propagate-mask colormap cursor)
    (with-foreign-object (values-ptr 'uint-32-t +max-window-attrs+)
      (let ((value-mask 0) (attr-count 0))
        (vl-maybe-set-many (window-attr values-ptr value-mask attr-count)
          background border bit-gravity gravity
          backing-store backing-planes backing-pixel
          override-redirect save-under event-mask
          do-not-propagate-mask colormap cursor)
        (when (> attr-count 0)
          (xerr drawable
              (xcb-change-window-attributes-checked (display-for drawable)
                                                    (xid drawable)
                                                    value-mask values-ptr))))))
  (hash-let (*drawable-attributes-changed*
             x y width height border-width sibling stack-mode)
    (with-foreign-object (values-ptr 'uint-32-t +max-window-config+)
      (let ((value-mask 0) (attr-count 0))
        (vl-maybe-set-many (window-config values-ptr value-mask attr-count)
          x y width height border-width sibling stack-mode)
        (when (> attr-count 0)
          (xerr drawable
              (xcb-configure-window-checked (display-for drawable)
                                            (xid drawable)
                                            value-mask values-ptr)))))))

(declaim (inline %setattr))
(defun %setattr (attr val)
  (setf (gethash attr *drawable-attributes-changed*) val))

(defmacro define-attr-accessor (name field &key (getter-p t) (setter-p t) in out)
  (let ((struct-field (intern (format nil "XCB-GET-WINDOW-ATTRIBUTES-REPLY-T-~A" field))))
  `(progn
     ,(when getter-p
        `(defun ,name (window)
           (with-attributes (window ptr)
             ,(if out
                  `(funcall ,out (,struct-field ptr))
                  `(,struct-field ptr)))))
     ,(when setter-p
        `(defun (setf ,name) (val window)
           (with-attributes (window)
             (%setattr ',field
                       ,(if in
                            `(funcall ,in val)
                            'val))))))))

(defmacro define-geom-accessor (name field &key (setter-p t))
  (let ((struct-field (intern (format nil "XCB-GET-GEOMETRY-REPLY-T-~A" field))))
    `(progn
       (defun ,name (drawable)
         (with-geometry (drawable ptr) (,struct-field ptr)))
       ,(when setter-p
          `(defun (setf ,name) (val drawable)
             (with-attributes (drawable)
               (%setattr ',field val)))))))

(define-geom-accessor drawable-border-width border-width)
(define-geom-accessor drawable-depth depth :setter-p nil)
(define-geom-accessor drawable-height height)
(define-geom-accessor drawable-width width)
(define-geom-accessor drawable-x x)
(define-geom-accessor drawable-y y)

(defmacro with-state (drawable &body body)
  `(let ((*drawable-hold-send* t))
     (with-attributes (,drawable)
       (with-geometry (,drawable)
         (prog1
             ,@body
           (setf *drawable-hold-send* nil))))))

 ;; 4.5 Window Hierarchy

(stub drawable-root (drawable))

 ;; Additional

(define-enum-table query-shape (xcb-query-shape-of-t "XCB-QUERY-SHAPE-OF")
  :largest-cursor :fastest-tile :fastest-stipple)

(defun query-best-size (width height drawable shape)
  (do-request-response (drawable c reply err)
      (xcb-query-best-size c (query-shape shape) drawable width height)
    (values (xcb-query-best-size-reply-t-width reply)
            (xcb-query-best-size-reply-t-height reply))))
