(in-package :xcb.clx)

(defstruct (font-charinfo (:constructor %make-font-charinfo))
  left-side-bearing right-side-bearing
  width ascent descent attributes)

(defstruct (font-info (:constructor %make-font-info))
  name min-bounds max-bounds
  min-char-or-byte2 max-char-or-byte2
  default-char draw-direction
  min-byte1 max-byte1 all-chars-exist
  ascent descent)

(defstruct (font (:include display-id-pair)
                 (:conc-name %font-)
                 (:constructor %make-font))
  info properties charinfo)

 ;; 8.2 Opening Fonts

(declaim (notinline make-font-charinfo make-font-info))
(defun make-font-charinfo (ptr)
  (%make-font-charinfo
   :left-side-bearing (xcb-charinfo-t-left-side-bearing ptr)
   :right-side-bearing (xcb-charinfo-t-right-side-bearing ptr)
   :width (xcb-charinfo-t-character-width ptr)
   :ascent (xcb-charinfo-t-ascent ptr)
   :descent (xcb-charinfo-t-descent ptr)
   :attributes (xcb-charinfo-t-attributes ptr)))

;; FIXME: draw direction?
(defun make-font-info (name ptr)
  (%make-font-info
   :name name
   :min-bounds (make-font-charinfo (xcb-query-font-reply-t-min-bounds ptr))
   :max-bounds (make-font-charinfo (xcb-query-font-reply-t-max-bounds ptr))
   :min-char-or-byte2 (xcb-query-font-reply-t-min-char-or-byte-2 ptr)
   :max-char-or-byte2 (xcb-query-font-reply-t-max-char-or-byte-2 ptr)
   :default-char (xcb-query-font-reply-t-default-char ptr)
   :draw-direction (if (= 0 (xcb-query-font-reply-t-draw-direction ptr))
                       :left-to-right :right-to-left)
   :min-byte1 (xcb-query-font-reply-t-min-byte-1 ptr)
   :max-byte1 (xcb-query-font-reply-t-max-byte-1 ptr)
   :all-chars-exist (= 1 (xcb-query-font-reply-t-all-chars-exist ptr))
   :ascent (xcb-query-font-reply-t-font-ascent ptr)
   :descent (xcb-query-font-reply-t-font-descent ptr)))

(defun make-font-properties (ptr head-fn len-fn)
  (declare (type function head-fn len-fn))
  (map-result-list 'list
                   (lambda (ptr)
                     (cons (xcb-fontprop-t-name ptr)
                           (xcb-fontprop-t-value ptr)))
                   head-fn len-fn
                   ptr '(:struct xcb-fontprop-t)))

(defun query-font (font name)
  (do-request-response (font c reply err)
      (xcb-query-font c (xid font))
    (values (make-font-info name reply)
            (make-font-properties reply
                                  #'xcb-query-font-properties
                                  #'xcb-query-font-properties-length)
            (map-result-list 'vector
                             #'make-font-charinfo
                             #'xcb-query-font-char-infos
                             #'xcb-query-font-char-infos-length
                             reply '(:struct xcb-charinfo-t)))))

(defun open-font (display name)
  (xchk (display c id (font (%make-font :display display :id id)))
      (with-foreign-string ((ptr len) name)
        (xcb-open-font c id (1- len) ptr))
    (multiple-value-bind (info properties charinfo)
        (query-font font name)
      (setf (%font-info font) info)
      (setf (%font-properties font) properties)
      (setf (%font-charinfo font) charinfo))
    font))

(defun close-font (font)
  (xchk (font c)
      (xcb-close-font-checked c (xid font))))

(defun discard-font-info (font)
  (setf (%font-info font) nil)
  (setf (%font-properties font) nil)
  (setf (%font-charinfo font) nil))

 ;; 8.3 Listing Fonts

(defun font-path (display &key (result-type 'list))
  (do-request-response (display c reply err)
      (xcb-get-font-path c)
    (map result-type
         #'xcb-str-to-lisp
         (xcb-get-font-path-path-iterator reply))))

(defun list-font-names (display pattern
                        &key (max-fonts 65535) (result-type 'list))
  (with-foreign-string ((ptr len) pattern)
   (do-request-response (display c reply err)
       (xcb-list-fonts c max-fonts len ptr)
     (map result-type
          #'xcb-str-to-lisp
          (xcb-list-fonts-names-iterator reply)))))

;; FIXME: caching? allow (open-font PSEUDO-FONT)?
(defun list-fonts (display pattern
                   &key (max-fonts 65535) (result-type 'list))
  (with-foreign-string ((ptr len) pattern)
    ;; This is "special", so we can't use do-request-response
    (let* ((c (display-ptr-xcb display))
           (ck (xcb-list-fonts-with-info c max-fonts len ptr))
           fonts)
      (loop do
        (with-xcb-clx-reply (display ck reply err)
            (xcb-list-fonts-with-info-reply c ck err)
          (let ((name (foreign-string-to-lisp (xcb-list-fonts-with-info-name reply)
                                              :count
                                              (xcb-list-fonts-with-info-name-length reply))))
            (when (= 0 (length name)) (loop-finish))
            (push (%make-font :display display
                              :info (make-font-info name reply)
                              :properties (make-font-properties reply
                                                                #'xcb-list-fonts-with-info-properties
                                                                #'xcb-list-fonts-with-info-properties-length))
                  fonts))))
      (map result-type #'identity fonts))))

 ;; 8.4 Font Attributes

(defmacro %mkfattr (name &optional field)
  (let ((func (intern (format nil "FONT-~A" name)))
        (prop (intern (format nil "FONT-INFO-~A" (or field name)))))
   `(defun ,func (font)
      (,prop (%font-info font)))))

(%mkfattr all-chars-exist-p all-chars-exist)
(%mkfattr ascent)
(%mkfattr default-char)
(%mkfattr descent)
(%mkfattr direction draw-direction)
(%mkfattr max-byte1)
(%mkfattr max-byte2 max-char-or-byte2)
(%mkfattr max-char max-char-or-byte2)
(%mkfattr min-byte1)
(%mkfattr min-byte2 min-char-or-byte2)
(%mkfattr min-char min-char-or-byte2)
(%mkfattr name)

(defun font-display (font)
  (%font-display font))

(defun font-equal (f-1 f-2)
  (xid-equal f-1 f-2))

(defun font-id (font)
  (xid font))

;; FONT-P per DEFSTRUCT

(defun font-plist (font)
  (xid-plist font))

(defun (setf font-plist) (v font)
  (setf (xid-plist font) v))

(stub font-properties (font))
(stub font-property (font name))

(defmacro %mkfcattr (minmax name &optional field)
  (let ((func (intern (format nil "FONT-~A-CHAR-~A" minmax name)))
        (attr (intern (format nil "FONT-INFO-~A-BOUNDS" minmax)))
        (prop (intern (format nil "FONT-CHARINFO-~A" (or field name)))))
   `(defun ,func (font)
      (,prop (,attr (%font-properties font))))))

(%mkfcattr max ascent)
(%mkfcattr max attributes)
(%mkfcattr max descent)
(%mkfcattr max left-bearing left-side-bearing)
(%mkfcattr max right-bearing right-side-bearing)
(%mkfcattr max width)

(%mkfcattr min ascent)
(%mkfcattr min attributes)
(%mkfcattr min descent)
(%mkfcattr min left-bearing left-side-bearing)
(%mkfcattr min right-bearing right-side-bearing)
(%mkfcattr min width)

 ;; 8.5 Character Attributes

(defmacro %mkfchrattr (name &optional field)
  (let ((func (intern (format nil "CHAR-~A" name)))
        (prop (intern (format nil "FONT-CHARINFO-~A" (or field name)))))
    `(defun ,func (font index)
       (,prop (aref (%font-charinfo font) index)))))

(%mkfchrattr ascent)
(%mkfchrattr attributes)
(%mkfchrattr descent)
(%mkfchrattr left-bearing left-side-bearing)
(%mkfchrattr right-bearing right-side-bearing)
(%mkfchrattr width)

 ;; 8.6 Querying Text Size

(defun text-extents (font sequence &key (start 0) end translate)
  (do-translation sequence font translate
      (start end 0)
      (output fnd cont curwidth)
    (with-translated-text (ptr len output 16)
      (do-request-response (font c reply err)
          (xcb-query-text-extents c (xid font) len ptr)
        (values (xcb-query-text-extents-reply-t-overall-width reply)
                (xcb-query-text-extents-reply-t-overall-ascent reply)
                (xcb-query-text-extents-reply-t-overall-descent reply)
                (xcb-query-text-extents-reply-t-overall-left reply)
                (xcb-query-text-extents-reply-t-overall-right reply)
                (xcb-query-text-extents-reply-t-font-ascent reply)
                (if (= 0 (xcb-query-text-extents-reply-t-draw-direction reply))
                    :left-to-right :right-to-left)
                fnd)))))

(defun text-width (font sequence &key (start 0) end translate)
  (multiple-value-bind (width d0 d1 d2 d3 d4 fnd)
      (text-extents font sequence :start start :end end :translate translate)
    (declare (ignore d0 d1 d2 d3 d4))
    (values width fnd)))
