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

(defun slot-translate-from (type val dpy)
  (let ((type (if (consp type) (car type) type))
        (args (if (consp type) (cdr type))))
    (funcall (car (find-slot-trans type)) args val dpy)))

(defun slot-translate-to (type val oldval dpy)
  (let ((type (if (consp type) (car type) type))
        (args (if (consp type) (cdr type)))
        (trans (cdr (find-slot-trans type))))
    (declare (ignore type))
    (funcall trans args val oldval dpy)))

(define-slot-trans window
  :from ((args val dpy) (%make-window :display dpy :id val))
  :to ((args val oldval dpy) (xid val)))

(define-slot-trans drawable
  :from ((args val dpy) (%make-drawable :display dpy :id val))
  :to ((args val oldval dpy) (xid val)))

(define-slot-trans null
  :from ((args val dpy) (if (= val 0)
                            (values nil t)
                            (values nil nil)))
  :to ((args val oldval dpy) (unless val 0)))

(define-slot-trans boolean
  :from ((args val dpy) (if (= val 0)
                            (values nil t) t))
  :to ((args val oldval dpy) (if val 0 1)))

(define-slot-trans or
  :from
  ((args val dpy)
    (loop for type in args do
      (multiple-value-bind (output succ)
          (slot-translate-from type val dpy)
        (when (or output succ)
          (return output)))))
  :to
  ((args val oldval dpy)
    (loop for type in args do
      (multiple-value-bind (output succ)
          (slot-translate-to type val nil dpy)
        (when (or output succ)
          (return output))))))

(define-slot-trans card8
  :from ((args val dpy) val)
  :to ((args val oldval dpy) val))

(define-slot-trans card16
  :from ((args val dpy) val)
  :to ((args val oldval dpy) val))

(define-slot-trans card32
  :from ((args val dpy) val)
  :to ((args val oldval dpy) val))

(define-slot-trans int16
  :from ((args val dpy) val)
  :to ((args val oldval dpy) val))

(define-slot-trans member
  :from ((args val dpy) (if (member val args) val))
  :to ((args val oldval dpy) (if (member val args) val)))

(define-slot-trans member-enum
  :from
  ((args val dpy)
    (let ((table (car args)))
      (cdr (assoc val table))))
  :to
  ((args val oldval dpy)
    (let ((table (car args)))
      (car (rassoc val table)))))

(define-slot-trans bit
  :from
  ((args val dpy) (logbitp val (car args)))
  :to
  ((args val oldval dpy)
    (dpb (if val 1 0) (byte 1 (car args)) oldval)))

(define-slot-trans bit-vector
  :from
  ((args ptr dpy)
    (let ((bv (make-array (car args) :element-type 'bit :initial-element 0)))
      (loop for n from 0 below (truncate (car args) 8) do
        (loop for i from 0 to 7
              do (if (logbitp i (mem-ref ptr :uint8 n))
                     (setf (aref bv (+ (* 8 n) i)) 1))))
      bv))
  :to
  ((args bv ptr dpy)
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
