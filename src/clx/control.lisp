(in-package :xcb.clx)

 ;; 14.1 Grabbing the Server

(defun grab-server (display)
  (xchk (display c)
      (xcb-grab-server-checked c)))

(defun ungrab-server (display)
  (xchk (display c)
      (xcb-ungrab-server-checked c)))

(defmacro with-server-grabbed (display &body body)
  (let ((d (gensym "DISPLAY")))
    `(let ((,d ,display))
       (unwind-protect
            (progn
              (grab-server ,d)
              ,@body)
         (ungrab-server ,d)))))

 ;; 14.2 Pointer Control

(defun change-pointer-control (display &key acceleration threshold)
  (let ((accel (or acceleration 0)))
    (xchk (display c)
        (xcb-change-pointer-control-checked
         c
         (numerator accel)
         (denominator accel)
         threshold
         (not (null acceleration))
         (not (null threshold))))))

(defun pointer-control (display)
  (do-request-response (display c reply err)
      (xcb-get-pointer-control c)
    (values
     (/ (xcb-get-pointer-control-reply-t-acceleration-numerator reply)
        (xcb-get-pointer-control-reply-t-acceleration-denominator reply))
     (xcb-get-pointer-control-reply-t-threshold reply))))

(defun pointer-mapping (display &key (result-type 'list))
  (do-request-response (display c reply err)
      (xcb-get-pointer-mapping c)
    (map-result-list result-type
                     #'identity
                     #'xcb-get-pointer-mapping-map
                     #'xcb-get-pointer-mapping-map-length
                     reply :uint8)))

(defun set-pointer-mapping (display mapping)
  (let ((len (length mapping)))
    (with-foreign-object (array :uint8 len)
      (loop for i from 0
            for b in mapping do
              (setf (mem-aref array :uint8 i) b))
      (do-request-response (display c reply err)
          (xcb-set-pointer-mapping c len array)))))

(defsetf pointer-mapping set-pointer-mapping)

 ;; 14.3 Keyboard Control

(defun bell (display &optional (percent-from-normal 0))
  (xchk (display c)
      (xcb-bell-checked c percent-from-normal)))

(define-enum-table kb (xcb-kb-t "XCB-KB")
  :key-click-percent :bell-percent :bell-pitch :bell-duration
  :led :led-mode :key :auto-repeat-mode)

(defconstant +max-kb-entries+ (length *kb-map*))

(define-enum-table led-mode (xcb-led-mode-t "XCB-LED-MODE")
  :off :on)

(define-enum-table auto-repeat-mode (xcb-auto-repeat-mode-t "XCB-AUTO-REPEAT-MODE")
  :off :on :default)

(defun change-keyboard-control (display
                                &key key-click-percent bell-percent
                                bell-pitch bell-duration led led-mode
                                key auto-repeat-mode)
  (with-foreign-object (values-ptr :uint32 +max-kb-entries+)
    (let ((value-mask 0)
          (attr-count 0)
          (led-mode (led-mode led-mode))
          (auto-repeat-mode (auto-repeat-mode auto-repeat-mode)))
      (vl-maybe-set-many (kb values-ptr value-mask attr-count)
        key-click-percent bell-percent bell-pitch bell-duration
        led led-mode key auto-repeat-mode)
      (xchk (display c)
          (xcb-change-keyboard-control-checked c value-mask values-ptr)))))

(defun keyboard-control (display)
  (do-request-response (display c reply)
      (xcb-get-keyboard-control c)
    (let ((vec (make-array 32 :element-type '(unsigned-byte 8))))
      (loop for i from 0 below 32 do
        (setf (aref vec i)
              (xcb-get-keyboard-control-reply-t-auto-repeats reply i)))
      (values
       (xcb-get-keyboard-control-reply-t-key-click-percent reply)
       (xcb-get-keyboard-control-reply-t-bell-percent reply)
       (xcb-get-keyboard-control-reply-t-bell-pitch reply)
       (xcb-get-keyboard-control-reply-t-bell-duration reply)
       (xcb-get-keyboard-control-reply-t-led-mask reply)
       (auto-repeat-mode-key
        (xcb-get-keyboard-control-reply-t-global-auto-repeat reply))
       vec))))

(defun modifier-mapping (display)
  (do-request-response (display c reply)
      (xcb-get-modifier-mapping c)
    (let ((codes (map-result-list '(vector (unsigned-byte 8)) #'identity
                                  #'xcb-get-modifier-mapping-keycodes
                                  #'xcb-get-modifier-mapping-keycodes-length
                                  reply 'xcb-keycode-t))
          (mod-len (xcb-get-modifier-mapping-reply-t-keycodes-per-modifier reply)))
      (values-list
       (loop for i from 0 below 8
             as offset = (* i mod-len)
             collect
             (map 'list #'identity
                  (subseq codes offset (+ offset mod-len))))))))

(defun query-keymap (display)
  (do-request-response (display c reply)
      (xcb-query-keymap c)
    (static-vectors:with-static-vector (v 256 :element-type 'bit)
      (libc_memcpy (static-vectors:static-vector-pointer v)
                   (inc-pointer reply #.(foreign-slot-offset
                                         '(:struct xcb-query-keymap-reply-t) 'keys))
                   32)
      (copy-seq v))))

(define-enum-table mapping-status (xcb-mapping-status-t "XCB-MAPPING-STATUS")
  :success (:device-busy :busy) (:failed :failure))

(defun set-modifier-mapping (display &key shift lock control
                             mod1 mod2 mod3 mod4 mod5)
  (let ((codes-per-mod (reduce (lambda (a b) (max a (length b)))
                               (list shift lock control mod1 mod2 mod3 mod4 mod5)
                               :initial-value 0)))
    (with-foreign-object (mods 'xcb-keycode-t (* 8 codes-per-mod))
      (libc_memset mods 0 (* 8 codes-per-mod))
      (loop for i from 0
            for codes in (list shift lock control mod1 mod2 mod3 mod4 mod5)
            do (loop for code in codes
                     for j from 0 do
                       (setf (mem-aref mods 'xcb-keycode-t
                                       (+ j (* i codes-per-mod)))
                             code)))
      (do-request-response (display c reply)
          (xcb-set-modifier-mapping c codes-per-mod mods)
        (mapping-status-key (xcb-set-modifier-mapping-reply-t-status reply))))))

 ;; 14.4.2 Keyboard Mapping

(defun change-keyboard-mapping (display keysyms &key (start 0) end
                                (first-keycode start))
  (let* ((codes-per-key (array-dimension keysyms 1))
         (end (or end (array-dimension keysyms 0)))
         (keycode-count (- end start))
         (flattened-keysyms (make-array keycode-count :displaced-to keysyms
                                                      :element-type 'card32)))
    (declare (dynamic-extent flattened-keysyms))
    (static-vectors:with-static-vector (keys (* keycode-count codes-per-key))
      (replace keys flattened-keysyms)
      (xchk (display c)
          (xcb-change-keyboard-mapping-checked c keycode-count first-keycode codes-per-key keys)))))

;; FIXME: this ignores (array-dimension data 1)
(defun keyboard-mapping (display &key first-keycode start end data)
  (let* ((first-keycode (or first-keycode (display-min-keycode display)))
         (start (or start first-keycode))
         (end (or end (1+ (display-max-keycode display)))))
    (do-request-response (display c reply)
        (xcb-get-keyboard-mapping c first-keycode (- end start))
      (let* ((count (xcb-get-keyboard-mapping-keysyms-length reply))
             (flat-copy (if data
                            (make-array count :element-type 'card32 :displaced-to data)
                            (make-array count :element-type 'card32))))
        (static-vectors:with-static-vector (keys count :element-type 'card32)
          (libc_memcpy (static-vectors:static-vector-pointer keys)
                       (xcb-get-keyboard-mapping-keysyms reply)
                       (* count #.(foreign-type-size 'xcb-keysym-t)))
          (replace flat-copy keys)
          (if data data
              (make-array (list (- end start)
                                (xcb-get-keyboard-mapping-reply-t-keysyms-per-keycode reply))
                          :element-type 'card32
                          :displaced-to flat-copy)))))))

 ;; 14.4.3 Using Keycodes and Keysyms

(defparameter *keysym-map* (make-hash-table))
(defparameter *keysym-reverse-map* (make-hash-table))
(defparameter *keysym-alias-map* (make-hash-table))

;; This is not really meant to be compatible with CLX
(defun define-keysym (object value &key &allow-other-keys)
  (if (gethash value *keysym-map*)
      (setf (gethash object *keysym-alias-map*)
            (gethash value *keysym-map*))
      (setf (gethash value *keysym-map*) object))
  (setf (gethash object *keysym-reverse-map*) value))

(defun keycode->keysym (display keycode keysym-index)
  (xcb-key-symbols-get-keysym (%display-key-symbols display)
                              keycode keysym-index))

;; FIXME: probably not entirely CLX-compatible
(defun keysym->character (display keysym &optional (state 0))
  (declare (ignore display state))
  (let ((value (keysym->value keysym)))
    (if (characterp value) value nil)))

(defun keysym->value (keysym)
  (cond
    ((or (<= #x20 keysym #x7E)
         (<= #xA0 keysym #xFF))
     (code-char keysym))
    ((<= #x01000100 keysym #x0110FFFF)
     (code-char (- keysym #x01000000)))
    (t (gethash keysym *keysym-map*))))

(defun canonical-keysym (object)
  (gethash (gethash object *keysym-reverse-map*)
           *keysym-map*))

 ;; 14.5 Client Termination

(define-enum-table save-set-mode (xcb-set-mode-t "XCB-SET-MODE")
  :insert :delete)

(defun add-to-save-set (window)
  (xchk (window c)
      (xcb-change-save-set c (save-set-mode :insert) window)))

(define-enum-table close-down (xcb-close-down-t "XCB-CLOSE-DOWN")
  (:destroy :destroy-all) :retain-permanent :retain-temporary)

(defun close-down-mode (display)
  (%display-close-down-mode display))

(defun (setf close-down-mode) (v display)
  (xchk (display c)
      (xcb-set-close-down-mode-checked c (close-down v)))
  (setf (%display-close-down-mode display) v))

(defun kill-client (display resource-id)
  (xchk (display c)
      (xcb-kill-client-checked c resource-id)))

(defun kill-temporary-clients (display)
  (kill-client display 0))

(defun remove-from-save-set (window)
  (xchk (window c)
      (xcb-change-save-set c (save-set-mode :delete) window)))

 ;; 14.6 Managing Host Access

(define-enum-table access-control-mode (xcb-access-control-t "XCB-ACCESS-CONTROL")
  :disable :enable)

(defun access-control (display)
  (do-request-response (display c reply)
      (xcb-list-hosts c)
    (= 1 (xcb-list-hosts-reply-t-mode reply))))

(defun set-access-control (display mode)
  (xchk (display c)
      (xcb-set-access-control-checked c (if mode 1 0))))

(defun access-hosts (display &key (result-type 'list))
  (do-request-response (display c reply)
      (xcb-list-hosts c)
    (values
     (map result-type
          (lambda (ptr)
            (let* ((len (xcb-host-address-length ptr))
                   (v (make-array len :element-type '(unsigned-byte 8)))
                   (data (xcb-host-address ptr)))
              (loop for i from 0 below len do
                (setf (aref v i) (mem-aref data :uint8 i)))
              v))
          (xcb-list-hosts-hosts-iterator reply))
     (= 1 (xcb-list-hosts-reply-t-mode reply)))))

(define-enum-table access-host-mode (xcb-host-mode-t "XCB-HOST-MODE")
  :insert :delete)

(define-enum-table host-family (xcb-family-t "XCB-FAMILY")
  :internet :decnet :chaos :server-interpreted :internet-6)

(defun change-access-host (display host mode &optional (family :internet))
  (static-vectors:with-static-vector (sv (length host) :element-type '(unsigned-byte 8))
    (replace sv host)
    (xchk (display c)
        (xcb-change-hosts-checked c (access-host-mode mode)
                                  (host-family family) (length sv)
                                  (static-vectors:static-vector-pointer sv)))))

(defun add-access-host (display host &optional (family :internet))
  (change-access-host display host :insert family))

(defun remove-access-host (display host &optional (family :internet))
  (change-access-host display host :delete family))

 ;; 14.7 Screen Saver

(define-enum-table screen-saver-mode (xcb-screen-saver-t "XCB-SCREEN-SAVER")
  :reset :active)

(defun activate-screen-saver (display)
  (xchk (display c)
      (xcb-force-screen-saver c (screen-saver-mode :active))))

(defun reset-screen-saver (display)
  (xchk (display c)
      (xcb-force-screen-saver c (screen-saver-mode :reset))))

(define-enum-table blanking (xcb-blanking-t "XCB-BLANKING")
  (:no :not-preferred) (:yes :preferred) :default)

(define-enum-table exposures (xcb-exposures-t "XCB-EXPOSURES")
  (:no :not-allowed) (:yes :allowed) :default)

(defun screen-saver (display)
  (do-request-response (display c reply)
      (xcb-get-screen-saver c)
    (values (xcb-get-screen-saver-reply-t-timeout reply)
            (xcb-get-screen-saver-reply-t-interval reply)
            (blanking-key (xcb-get-screen-saver-reply-t-prefer-blanking reply))
            (exposures-key (xcb-get-screen-saver-reply-t-allow-exposures reply)))))

(defun set-screen-saver (display timeout period blanking exposures)
  (xchk (display c)
      (xcb-set-screen-saver-checked c timeout period
                                    (blanking blanking)
                                    (exposures exposures))))

