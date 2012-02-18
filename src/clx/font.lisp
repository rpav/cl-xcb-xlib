(in-package :xcb.clx)

(defstruct font-charinfo
  left-side-bearing right-side-bearing
  width ascent descent attributes)

(defstruct font-properties
  name min-bounds max-bounds
  min-char-or-byte2 max-char-or-byte2
  default-char draw-direction
  min-byte1 max-byte1 all-chars-exist
  ascent descent)

(defstruct (font (:include display-id-pair)
                 (:conc-name %font-)
                 (:constructor %make-font))
  (properties nil :type (or nil font-properties)))

 ;; 8.2 Opening Fonts

(defun open-font (display name)
  (xchk (display c id (font (%make-font :display display :id id)))
      (with-foreign-string ((ptr len) name)
        (xcb-open-font c id (1- len) ptr))
    font))

(defun close-font (font)
  (xchk (font c)
      (xcb-close-font-checked c (xid font))))

;; We don't keep any state, so this does nothing
(defun discard-font-info (fonts))

 ;; 8.3 Listing Fonts

(defun font-path (display &key (result-type 'list))
  (do-request-response (display c ck reply err)
      (xcb-get-font-path c)
      (xcb-get-font-path-reply c ck err)
    (map result-type
         #'xcb-str-to-lisp
         (xcb-get-font-path-path-iterator reply))))

(defun list-font-names (display pattern
                        &key (max-fonts 65535) (result-type 'list))
  (with-foreign-string ((ptr len) pattern)
   (do-request-response (display c ck reply err)
       (xcb-list-fonts c max-fonts len ptr)
       (xcb-list-fonts-reply c ck err)
     (map result-type
          #'xcb-str-to-lisp
          (xcb-list-fonts-names-iterator reply)))))

(defun list-fonts (display pattern
                   &key (max-fonts 65535) (result-type 'list)))

 ;; 8.4 Font Attributes

(stub font-all-chars-exist-p (font))
(stub font-ascent (font))
(stub font-default-char (font))
(stub font-descent (font))
(stub font-direction (font))
(stub font-display (font))
(stub font-equal (font-1 font-2))
(stub font-id (font))
(stub font-max-byte1 (font))
(stub font-max-byte2 (font))
(stub font-max-char (font))
(stub font-min-byte1 (font))
(stub font-min-byte2 (font))
(stub font-min-char (font))
(stub font-name (font))

;; FONT-P per DEFSTRUCT

(stub font-plist (font))
(stub (setf font-plist) (v font))

(stub font-properties (font))
(stub font-property (font name))

(stub max-char-ascent (font))
(stub max-char-attributes (font))
(stub max-char-descent (font))
(stub max-char-left-bearing (font))
(stub max-char-right-bearing (font))
(stub max-char-width (font))

(stub min-char-ascent (font))
(stub min-char-attributes (font))
(stub min-char-descent (font))
(stub min-char-left-bearing (font))
(stub min-char-right-bearing (font))
(stub min-char-width (font))

 ;; 8.5 Character Attributes

(stub char-ascent (font index))
(stub char-attributes (font index))
(stub char-descent (font index))
(stub char-left-bearing (font index))
(stub char-right-bearing (font index))
(stub char-width (font index))

 ;; 8.6 Querying Text Size

(stub text-extents (font sequence &key (start 0) end translate))
(stub text-width (font sequence &key start 0 end translate))
