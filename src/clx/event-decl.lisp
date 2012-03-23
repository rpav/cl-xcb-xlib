(in-package :xcb.clx)

(defvar *event-slot-translators* (make-hash-table))

(defmacro define-slot-trans (type &key from to)
  "This is meant to define translations `:FROM` and `:TO` an XCB
struct value.  If the value is unable to be translated, return `NIL`.
If the value is *able* to be translated, and the translation is `NIL`,
return `(VALUES NIL T)` (as per `BOOLEAN` or `NULL`).  Otherwise,
return the value normally.

Both `:FROM` and `:TO` take the arguments `(ARGS VAL DPY)`.
`ARGS` is additional arguments to the `TYPE`, e.g., `(OR NULL WINDOW)`,
`TYPE` is `OR` and `ARGS` is `(NULL WINDOW)`.  `VAL` is the value being
translated.  `DPY` is the display in context."
  `(setf (gethash ',type *event-slot-translators*)
         (cons (lambda ,(car from)
                 (declare (ignorable ,@(car from)))
                 ,@(cdr from))
               (lambda ,(car to)
                   (declare (ignorable ,@(car to)))
                 ,@(cdr to)))))

(find-slots :exposure)

(defun find-slot-trans (type)
  (gethash type *event-slot-translators*))

(defun slot-translate-from (type ev-ptr val dpy)
  (let ((type (if (consp type) (car type) type))
        (args (if (consp type) (cdr type))))
    (funcall (car (find-slot-trans type)) ev-ptr args val dpy)))

(defun slot-translate-to (type ev val oldval dpy)
  (let ((type (if (consp type) (car type) type))
        (args (if (consp type) (cdr type)))
        (trans (cdr (find-slot-trans type))))
    (declare (ignore type))
    (funcall trans ev args val oldval dpy)))

(define-slot-trans window
  :from ((ev args val dpy) (%make-window :display dpy :id val))
  :to ((ev args val oldval dpy) (xid val)))

(define-slot-trans drawable
  :from ((ev args val dpy) (%make-drawable :display dpy :id val))
  :to ((ev args val oldval dpy) (xid val)))

(define-slot-trans colormap
  :from ((ev args val dpy) (%make-colormap :display dpy :id val))
  :to ((ev args val oldval dpy) (xid val)))

(define-slot-trans null
  :from ((ev args val dpy) (if (= val 0)
                               (values nil t)
                               (values nil nil)))
  :to ((ev args val oldval dpy) (unless val 0)))

(define-slot-trans boolean
  :from ((ev args val dpy) (if (= val 0)
                               (values nil t) t))
  :to ((ev args val oldval dpy) (if val 0 1)))

(define-slot-trans or
  :from
  ((ev args val dpy)
    (loop for type in args do
      (multiple-value-bind (output succ)
          (slot-translate-from type ev val dpy)
        (when (or output succ)
          (return output)))))
  :to
  ((ev args val oldval dpy)
    (loop for type in args do
      (multiple-value-bind (output succ)
          (slot-translate-to type ev val nil dpy)
        (when (or output succ)
          (return output))))))

(define-slot-trans card8
  :from ((ev args val dpy) val)
  :to ((ev args val oldval dpy) val))

(define-slot-trans card16
  :from ((ev args val dpy) val)
  :to ((ev args val oldval dpy) val))

(define-slot-trans card32
  :from ((ev args val dpy) val)
  :to ((ev args val oldval dpy) val))

(define-slot-trans int16
  :from ((ev args val dpy) val)
  :to ((ev args val oldval dpy) val))

(define-slot-trans member
  :from ((ev args val dpy) (if (member val args) val))
  :to ((ev args val oldval dpy) (if (member val args) val)))

(define-slot-trans atom
  :from ((ev args val dpy) (atom-name dpy val))
  :to ((ev args val oldval dpy) (intern-atom dpy val)))

(define-slot-trans client-data
  :from
  ((ev args val dpy)
    (let* ((size (xcb-client-message-event-t-format ev))
           (type `(unsigned-byte ,size))
           (len (ecase size (8 20) (16 10) (32 5)))
           (ctype (ecase size (8 :uint8) (16 :uint16) (32 :uint32)))
           (vec (make-array len :element-type type :initial-element 0)))
      (loop for i from 0 below len do
            (setf (aref vec i)
                  (mem-aref val ctype i)))
      vec))
  :to
  ((ev args val ptr dpy)
    (let* ((size (getf ev :format))
           (data (getf ev :data))
           (len (min (ecase size (8 20) (16 10) (32 5))))
           (ctype (ecase size (8 :uint8) (16 :uint16) (32 :uint32))))
      (loop for i from 0 below (length data) do
            (setf (mem-aref ptr ctype i)
                  (aref data i)))
      (loop for i from (length data) below len do
            (setf (mem-aref ptr ctype i) 0))
      nil)))

(define-slot-trans member-enum
  :from
  ((ev args val dpy)
    (let ((table (car args)))
      (cdr (assoc val table))))
  :to
  ((ev args val oldval dpy)
    (let ((table (car args)))
      (car (rassoc val table)))))

(define-slot-trans bit
  :from
  ((ev args val dpy) (logbitp val (car args)))
  :to
  ((ev args val oldval dpy)
    (dpb (if val 1 0) (byte 1 (car args)) oldval)))

(define-slot-trans bit-vector
  :from
  ((ev args ptr dpy)
    (let ((bv (make-array (car args) :element-type 'bit :initial-element 0)))
      (loop for n from 0 below (truncate (car args) 8) do
        (loop for i from 0 to 7
              do (if (logbitp i (mem-ref ptr :uint8 n))
                     (setf (aref bv (+ (* 8 n) i)) 1))))
      bv))
  :to
  ((ev args bv ptr dpy)
    (loop for n from 0 below (truncate (car args) 8) do
      (let ((byte 0))
        (loop for i from 0 to 7
              do (if (logbitp i (mem-ref ptr :uint8 n))
                     (setf byte (dpb 1 (byte 1 i) byte)))
              finally
                 (setf (mem-ref ptr :uint8 n) byte))))
    ptr))

 ;; 12.12.1 Keyboard and Pointer Events

(declare-event (:key-press :key-release :button-press :button-release)
  'xcb-key-press-event-t
  (card8 (detail code))
  (card16 sequence)
  (card32 time)
  (window root)
  (window (event event-window))
  ((or null window) child)
  (int16 root-x root-y (event-x x) (event-y y))
  (card16 state)
  (boolean (same-screen same-screen-p)))

(declare-event (:motion-notify)
  'xcb-motion-notify-event-t
  (boolean (detail code))
  (card16 sequence)
  (card32 time)
  (window root)
  (window (event event-window))
  ((or null window) child)
  (int16 root-x root-y (event-x x) (event-y y))
  (card16 state)
  (boolean (same-screen same-screen-p)))

;; FIXME? xcb defines some additional values for the following two tables:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *notify-mode-table*
    '((:+xcb-notify-mode-normal+ . :normal)
      (:+xcb-notify-mode-grab+ . :grab)
      (:+xcb-notify-mode-ungrab+ . :ungrab)
      (:+xcb-notify-mode-while-grabbed+ . :while-grabbed)))

  (defvar *notify-detail-table*
    '((:+xcb-notify-detail-ancestor+ . :ancestor)
      (:+xcb-notify-detail-virtual+ . :virtual)
      (:+xcb-notify-detail-inferior+ . :inferior)
      (:+xcb-notify-detail-nonlinear+ . :nonlinear)
      (:+xcb-notify-detail-nonlinear-virtual+ . :nonlinear-virtual)
      (:+xcb-notify-detail-pointer+ . :pointer)
      (:+xcb-notify-detail-pointer-root+ . :pointer-root)
      (:+xcb-notify-detail-none+ . :none))))

(declare-event (:enter-notify :leave-notify)
  'xcb-enter-notify-event-t
  ((member-enum #.*notify-detail-table*) (detail kind))
  ((member-enum #.*notify-mode-table*) mode)
  (card16 sequence)
  (card32 time)
  (window root)
  (window (event event-window))
  ((or null window) child)
  (int16 root-x root-y (event-x x) (event-y y))
  (card16 state)
  ((bit 0) (same-screen-focus focus-p))
  ((bit 1) (same-screen-focus same-screen-p)))

 ;; 12.12.2 Input Focus Event

(declare-event (:focus-in :focus-out)
  'xcb-focus-in-event-t
  ((member-enum #.*notify-detail-table*) (detail kind))
  ((member-enum #.*notify-mode-table*) mode)
  (card16 sequence)
  (window (event event-window)))

 ;; 12.12.3 Keyboard and Pointer State Events

(declare-event (:keymap-notify)
  'xcb-keymap-notify-event-t
  ((bit-vector 256) (keys keymap)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *mapping-notify-table*
    '((:+xcb-mapping-modifier+ . :modifier)
      (:+xcb-mapping-keyboard+ . :keyboard)
      (:+xcb-mapping-pointer+ . :pointer))))

(declare-event (:mapping-notify)
  'xcb-mapping-notify-event-t
  ((member-enum #.*mapping-notify-table*) request)
  (card8 (first-keycode start) count))

 ;; 12.12.4 Exposure Events

(declare-event (:exposure)
  'xcb-graphics-exposure-event-t
  (window (drawable window event-window))
  (card16 x y width height count))

(declare-event (:graphics-exposure)
  'xcb-graphics-exposure-event-t
  (drawable (drawable event-window))
  (card16 x y width height count))

(declare-event (:no-exposure)
  'xcb-no-exposure-event-t
  (drawable (drawable event-window))
  (card8 (major-opcode major))
  (card16 (minor-opcode minor)))

 ;; 12.12.5 Window State Events

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *circulate-notify-table*
    '((:+xcb-place-on-top+ . :top)
      (:+xcb-place-on-bottom+ . :bottom))))

(declare-event (:circulate-notify :circulate-request)
    'xcb-circulate-notify-event-t
  (window (event event-window parent) window)
  ((member-enum #.*circulate-notify-table*) place))

(declare-event (:configure-notify :create-notify)
    'xcb-configure-notify-event-t
  (window (event event-window parent) window)
  (int16 x y)
  (card16 width height border-width)
  ((or null window) above-sibling)
  (boolean (override-redirect override-redirect-p)))

(declare-event (:destroy-notify)
    'xcb-destroy-notify-event-t
  (window (event event-window) window))

(declare-event (:map-notify :unmap-notify)
    'xcb-map-notify-event-t
  (window (event event-window) window)
  (boolean (override-redirect override-redirect-p configure-p)))

(declare-event (:reparent-notify)
    'xcb-reparent-notify-event-t
  (window (event event-window) window parent)
  (int16 x y)
  (boolean (override-redirect override-redirect-p configure-p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *visibility-notify-table*
    '((:+xcb-visibility-unobscured+ . :unobscured)
      (:+xcb-visibility-partially-obscured+ . :partially-obscured)
      (:+xcb-visibility-fully-obscured+ . :fully-obscured))))

(declare-event (:visibility-notify)
    'xcb-visibility-notify-event-t
  (window (window event-window))
  ((member-enum #.*visibility-notify-table*) state))

 ;; 12.12.6 Structure Control Events

(declare-event (:colormap-notify)
    'xcb-colormap-notify-event-t
  (window (window event-window))
  ((or null colormap) colormap)
  (boolean (-new new-p) (state installed-p)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *stack-mode-table*
    '((:+xcb-stack-mode-above+ . :above)
      (:+xcb-stack-mode-below+ . :below)
      (:+xcb-stack-mode-top-if+ . :top-if)
      (:+xcb-stack-mode-bottom-if+ . :bottom-if)
      (:+xcb-stack-mode-opposite+ . :opposite))))

(declare-event (:configure-request)
    'xcb-configure-request-event-t
  (window (parent event-window) window)
  (int16 x y)
  (card16 width height border-width)
  ((member-enum #.*stack-mode-table*) stack-mode)
  ((or null window) (sibling above-sibling))
  (mask16 value-mask))

(declare-event (:map-request)
    'xcb-map-request-event-t
  (window (parent event-window) window))

(declare-event (:resize-request)
    'xcb-resize-request-event-t
  (window (window event-window))
  (card16 width height))

 ;; 12.12.7 Client Communication Events

(declare-event (:client-message)
    'xcb-client-message-event-t
  (window (window event-window))
  (atom type)
  (card8 format)
  (client-data data))

;; Hack alert .. special message format
(setf (gethash :data (gethash :client-message *event-xcb-map*))
      (lambda (x y) (declare (ignore x y))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *property-state-table*
    '((:+xcb-property-new-value+ . :new-value)
      (:+xcb-property-delete+ . :deleted))))

(declare-event (:property-notify)
    'xcb-property-notify-event-t
  (window (window event-window))
  (atom atom)
  ((member-enum #.*property-state-table*) state)
  (card32 time))

(declare-event (:selection-clear)
    'xcb-selection-clear-event-t
  (window (owner window event-window))
  (atom selection)
  (card32 time))

(declare-event (:selection-notify :selection-request)
    'xcb-selection-notify-event-t
  (window (requestor window event-window))
  (atom selection target)
  ((or null atom) property)
  (card32 time))
