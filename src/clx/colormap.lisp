(in-package :xcb.clx)

(defstruct (colormap (:include display-id-pair)
                     (:conc-name %colormap-)
                     (:constructor %make-colormap)))

 ;; 9.3.1 Creating Colormaps

(defun create-colormap (visual window &optional alloc-p)
  (let* ((con (display-ptr-xcb window))
         (cid (xcb-generate-id con))
         (cm (%make-colormap :display (display-for window)
                             :id cid)))
    (xerr window (xcb-create-colormap con (if alloc-p 1 0)
                                      cid (xid window) visual))
    cm))

(defun copy-colormap-and-free (colormap)
  (let* ((id (xcb-generate-id (display-for colormap)))
         (copy (%make-colormap :display (display-for colormap)
                               :id id)))
    (xerr colormap
        (xcb-copy-colormap-and-free-checked (display-ptr-xcb colormap)
                                            copy
                                            (xid colormap)))
    copy))
    
(defun free-colormap (colormap)
  (xerr colormap
      (xcb-free-colormap (display-ptr-xcb colormap)
                         (xid colormap))))

 ;; 9.3.2 Installing Colormaps

(defun install-colormap (colormap)
  (xerr colormap
      (xcb-install-colormap-checked (display-ptr-xcb colormap)
                                    (xid colormap))))

(defun installed-colormaps (window &key (result-type 'list))
  (do-request-response (window c ck reply err)
      (xcb-list-installed-colormaps c (xid window))
      (xcb-list-installed-colormaps-reply c ck err)
    (map-result-list result-type
                     (lambda (id)
                       (%make-colormap :display (display-for window)
                                       :id id))
                     #'xcb-list-installed-colormaps-cmaps
                     #'xcb-list-installed-colormaps-cmaps-length
                     reply 'xcb-colormap-t)))

(defun uninstall-colormap (colormap)
  (xerr colormap
      (xcb-uninstall-colormap (display-ptr-xcb colormap)
                              (xid colormap))))

 ;; 9.3.3 Allocating Colors

(defun alloc-color (colormap color)
  (do-request-response (colormap c ck reply err)
      (xcb-alloc-color c (xid colormap)
                       (color-red color)
                       (color-green color)
                       (color-blue color))
      (xcb-alloc-color-reply c ck err)))

(defun alloc-color-cells (colormap colors
                          &key (planes 0) contiguous-p (result-type 'list))
  (do-request-response (colormap c ck reply err)
      (xcb-alloc-color-cells c (if contiguous-p 1 0) (xid colormap)
                             colors planes)
      (xcb-alloc-color-cells-reply c ck err)
    (map-result-list result-type
                     #'identity
                     #'xcb-alloc-color-cells-pixels
                     #'xcb-alloc-color-cells-masks-length
                     reply 'uint32-t)))
 
(defun alloc-color-planes (colormap colors &key (reds 0) (greens 0) (blues 0)
                             contiguous-p (result-type 'list))
  (do-request-response (colormap c ck reply err)
      (xcb-alloc-color-planes c (if contiguous-p 1 0) (xid colormap)
                              colors reds greens blues)
      (xcb-alloc-color-cells-reply c ck err)
    (values
     (map-result-list result-type
                      #'identity
                      #'xcb-alloc-color-planes-pixels
                      #'xcb-alloc-color-planes-pixels-length
                      reply 'uint32-t)
     (xcb-alloc-color-planes-reply-t-red-mask reply)
     (xcb-alloc-color-planes-reply-t-green-mask reply)
     (xcb-alloc-color-planes-reply-t-blue-mask reply))))

(defun free-colors (colormap pixels &optional (plane-mask 0))
  (let ((len (length pixels)))
    (with-foreign-object (ptr :uint32 len)
      (copy-to-foreign ptr len pixels :uint32)
      (xerr colormap
          (xcb-free-colors-checked (display-ptr-xcb colormap) (xid colormap)
                                   plane-mask len ptr)))))

 ;; 9.3.4 Finding Colors

(defun lookup-color (colormap name)
  (with-foreign-string ((str len) name)
    (do-request-response (colormap c ck reply err)
        (xcb-lookup-color c (xid colormap) (1- len) str)
        (xcb-lookup-color-reply c ck err)
      (values
       (make-color-from-ints
        :red (xcb-lookup-color-reply-t-visual-red reply)
        :green (xcb-lookup-color-reply-t-visual-green reply)
        :blue (xcb-lookup-color-reply-t-visual-blue reply))
       (make-color-from-ints
        :red (xcb-lookup-color-reply-t-exact-red reply)
        :green (xcb-lookup-color-reply-t-exact-green reply)
        :blue (xcb-lookup-color-reply-t-exact-blue reply))))))
       
(defun query-colors (colormap pixels &key (result-type 'list))
  (let ((len (length pixels)))
    (with-foreign-object (ptr :uint32 len)
      (copy-to-foreign ptr len pixels :uint32)
      (do-request-response (colormap c ck reply err)
          (xcb-query-colors c (xid colormap) len ptr)
          (xcb-query-colors-reply c ck err)
        (map-result-list result-type
                         (lambda (ptr)
                           (make-color-from-ints
                            :red (xcb-rgb-t-red ptr)
                            :green (xcb-rgb-t-green ptr)
                            :blue (xcb-rgb-t-blue ptr)))
                         #'xcb-query-colors-colors
                         #'xcb-query-colors-colors-length
                         reply 'xcb-rgb-t)))))

 ;; 9.3.5 Changing Colors

(define-enum-table color-flag (xcb-color-flag-t "XCB-COLOR-FLAG")
  :red :green :blue)

(defun store-color (colormap pixel color
                    &key (red-p t) (green-p t) (blue-p t))
  (store-colors colormap (list pixel color)
                red-p green-p blue-p))

(defun store-colors (colormap pixel-colors
                     &key (red-p t) (green-p t) (blue-p t))
  (let ((len (length pixel-colors)))
    (with-foreign-object (ptr 'xcb-coloritem-t len)
      (copy-to-foreign ptr len pixel-colors 'xcb-coloritem-t
                       (lambda (ptr el)
                         (setf (xcb-coloritem-t-pixel ptr) (car el))
                         (setf (xcb-coloritem-t-red ptr) (color-red-int (cadr el)))
                         (setf (xcb-coloritem-t-green ptr) (color-green-int (cadr el)))
                         (setf (xcb-coloritem-t-blue ptr) (color-blue-int (cadr el)))
                         (color-flag-logior (and red-p :red)
                                            (and green-p :green)
                                            (and blue-p :blue))))
      (xerr colormap
          (xcb-store-colors-checked (display-ptr-xcb colormap)
                                    (xid colormap)
                                    len ptr)))))

 ;; 9.3.6 Colormap Attributes

(defun colormap-display (colormap)
  (%colormap-display colormap))

(defun colormap-equal (c-1 c-2)
  (xid-equal c-1 c-2))

(defun colormap-id (colormap)
  (%colormap-id colormap))

;; COLORMAP-P

(defun colormap-plist (colormap)
  (xid-plist colormap))

(defun (setf colormap-plist) (v colormap)
  (setf (xid-plist colormap) v))
