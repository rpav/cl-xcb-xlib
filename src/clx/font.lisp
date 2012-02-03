(in-package :xcb.clx)

(defstruct (font (:conc-name %font-)
                 (:constructor %make-font)))

 ;; 8.2 Opening Fonts

(stub open-font (display name))
(stub close-font (font))
(stub discard-font-info (fonts))

 ;; 8.3 Listing Fonts

(stub font-path (display &key (result-type 'list)))
(stub list-font-names (display pattern
                       &key (max-fonts 65535) (result-type 'list)))
(stub list-fonts (display pattern
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
