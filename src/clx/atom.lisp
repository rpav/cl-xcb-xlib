(in-package :xcb.clx)

;; CLX represents atoms as keywords.
(deftype xatom () '(or string symbol))

 ;; Tables

(define-enum-table atoms (xcb-atom-enum-t "XCB-ATOM")
  :NONE
  :ANY
  :PRIMARY
  :SECONDARY
  :ARC
  :ATOM
  :BITMAP
  :CARDINAL
  :COLORMAP
  :CURSOR
  (:CUT_BUFFER0 :cut-buffer-0)
  (:CUT_BUFFER1 :cut-buffer-1)
  (:CUT_BUFFER2 :cut-buffer-2)
  (:CUT_BUFFER3 :cut-buffer-3)
  (:CUT_BUFFER4 :cut-buffer-4)
  (:CUT_BUFFER5 :cut-buffer-5)
  (:CUT_BUFFER6 :cut-buffer-6)
  (:CUT_BUFFER7 :cut-buffer-7)
  :DRAWABLE
  :FONT
  :INTEGER
  :PIXMAP
  :POINT
  :RECTANGLE
  (:RESOURCE_MANAGER :RESOURCE-MANAGER)
  (:RGB_COLOR_MAP :RGB-COLOR-MAP)
  (:RGB_BEST_MAP :RGB-BEST-MAP)
  (:RGB_BLUE_MAP :RGB-BLUE-MAP)
  (:RGB_DEFAULT_MAP :RGB-DEFAULT-MAP)
  (:RGB_GRAY_MAP :RGB-GRAY-MAP)
  (:RGB_GREEN_MAP :RGB-GREEN-MAP)
  (:RGB_RED_MAP :RGB-RED-MAP)
  :STRING
  :VISUALID
  :WINDOW
  (:WM_COMMAND :WM-COMMAND)
  (:WM_HINTS :WM-HINTS)
  (:WM_CLIENT_MACHINE :WM-CLIENT-MACHINE)
  (:WM_ICON_NAME :WM-ICON-NAME)
  (:WM_ICON_SIZE :WM-ICON-SIZE)
  (:WM_NAME :WM-NAME)
  (:WM_NORMAL_HINTS :WM-NORMAL-HINTS)
  (:WM_SIZE_HINTS :WM-SIZE-HINTS)
  (:WM_ZOOM_HINTS :WM-ZOOM-HINTS)
  (:MIN_SPACE :MIN-SPACE)
  (:NORM_SPACE :NORM-SPACE)
  (:MAX_SPACE :MAX-SPACE)
  (:END_SPACE :END-SPACE)
  (:SUPERSCRIPT_X :SUPERSCRIPT-X)
  (:SUPERSCRIPT_Y :SUPERSCRIPT-Y)
  (:SUBSCRIPT_X :SUBSCRIPT-X)
  (:SUBSCRIPT_Y :SUBSCRIPT-Y)
  (:UNDERLINE_POSITION :UNDERLINE-POSITION)
  (:UNDERLINE_THICKNESS :UNDERLINE-THICKNESS)
  (:STRIKEOUT_ASCENT :STRIKEOUT-ASCENT)
  (:STRIKEOUT_DESCENT :STRIKEOUT-DESCENT)
  (:ITALIC_ANGLE :ITALIC-ANGLE)
  (:X_HEIGHT :X-HEIGHT)
  (:QUAD_WIDTH :QUAD-WIDTH)
  :WEIGHT
  (:POINT_SIZE :POINT-SIZE)
  :RESOLUTION
  :COPYRIGHT
  :NOTICE
  (:FONT_NAME :FONT-NAME)
  (:FAMILY_NAME :FAMILY-NAME)
  (:FULL_NAME :FULL-NAME)
  (:CAP_HEIGHT :CAP-HEIGHT)
  (:WM_CLASS :WM-CLASS)
  (:WM_TRANSIENT_FOR :WM-TRANSIENT-FOR))

 ;; 11.1 Atoms

(defun atom-name (display atom-id)
  (let* ((c (display-ptr-xcb display))
         (ck (xcb-get-atom-name c atom-id)))
    (with-xcb-clx-reply (display ck reply err)
        (xcb-get-atom-name-reply c ck err)
      (values
       (intern 
        (foreign-string-to-lisp (xcb-get-atom-name-name reply)
                                :offset 0
                                :count (xcb-get-atom-name-reply-t-name-len reply))
        :keyword)))))

(defun %intern-atom (display atom-name only-if-exists-p)
  (or (atoms (if (keywordp atom-name) atom-name (intern atom-name 'keyword)))
      (with-foreign-string ((ptr len) (string atom-name))
        (let* ((c (display-ptr-xcb display))
               (ck (xcb-intern-atom c (if only-if-exists-p 1 0)
                                    (1- len) ptr)))
          (with-xcb-clx-reply (display ck reply err)
              (xcb-intern-atom-reply c ck err)
            (xcb-intern-atom-reply-t-atom reply))))))

(defun find-atom (display atom-name)
  (%intern-atom display atom-name t))

(defun intern-atom (display atom-name)
  (%intern-atom display atom-name nil))

 ;; 11.2 Properties

(define-enum-table prop-mode (xcb-prop-mode-t "XCB-PROP-MODE")
  :replace :prepend :append)

(defun change-property (window property data type format
                       &key (mode :replace) (start 0) end transform)
  (let* ((c (display-ptr-xcb window))
         (len (- (or end (length data)) (or start 0))))
    (with-foreign-object (ptr (ecase format
                                (8 :uint8)
                                (16 :uint16)
                                (32 :uint32))
                          len)
      (loop for i from 0 below len
            as element = (funcall transform (elt data (+ start i)))
            do (ecase format
                 ;; Avoid expensive calls to PARSE-TYPE:
                 (8 (setf (mem-aref ptr :uint8 i) element))
                 (16 (setf (mem-aref ptr :uint16 i) element))
                 (32 (setf (mem-aref ptr :uint32 i) element))))
      (xerr window
          (xcb-change-property-checked c (prop-mode mode) (xid window)
                                       (find-atom window property)
                                       (find-atom window type)
                                       format len ptr)))))

(defun delete-property (window property)
  (let* ((c (display-ptr-xcb window)))
    (xerr window
        (xcb-delete-property-checked c (xid window)
                                     (find-atom window property)))))

(defun %convert-propval (ptr len fmt transform type)
  (unless (= fmt 0)
    (coerce (loop for i from 0 below len
                  collect (funcall transform
                                   (ecase fmt
                                     (8 (mem-aref ptr :uint8 i))
                                     (16 (mem-aref ptr :uint16 i))
                                     (32 (mem-aref ptr :uint32 i)))))
            type)))

(defun get-property (window property
                    &key type (start 0) end delete-p (result-type 'list)
                      (transform 'identity))
  (let* ((c (display-ptr-xcb window))
         (len (- (or end 0) (or start 0)))
         (ck (xcb-get-property c (if delete-p 1 0) (xid window)
                               (find-atom window property)
                               (and type (find-atom window type))
                               (or start 0) len)))
    (with-xcb-clx-reply (window ck reply err)
        (xcb-get-property-reply c ck err)
      (values (%convert-propval (xcb-get-property-value reply)
                                (xcb-get-property-reply-t-value-len reply)
                                (xcb-get-property-reply-t-format reply)
                                transform result-type)
              (atom-name window (xcb-get-property-reply-t-type reply))
              (xcb-get-property-reply-t-format reply)
              (xcb-get-property-reply-t-bytes-after reply)))))

(defun list-properties (window &key (result-type 'list))
  (let* ((c (display-ptr-xcb window))
         (ck (xcb-list-properties c (xid window))))
    (with-xcb-clx-reply (window ck reply err)
        (xcb-list-properties-reply c ck err)
      (map result-type
           (lambda (id) (atom-name window id))
           (loop with ptr = (xcb-list-properties-atoms reply)
                 for i from 0 below (xcb-list-properties-atoms-length reply)
                 collect (mem-aref ptr 'xcb-atom-t i))))))

(defun rotate-properties (window properties &optional (delta 1))
  (let ((c (display-ptr-xcb window))
        (len (length properties)))
    (with-foreign-object (atoms 'xcb-atom-t len)
      (loop for i from 0 below len
            for atom in properties
            do (setf (mem-aref atoms 'xcb-atom-t i) (find-atom window atom)))
      (xerr window
          (xcb-rotate-properties-checked c (xid window) len delta atoms)))))

 ;; 11.3 Selections

(stub convert-selection (selection type requestor &optional property time))
(stub selection-owner (display selection &optional time))
