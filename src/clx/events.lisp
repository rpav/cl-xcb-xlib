(in-package :xcb.clx)

 ;; Event Values

(define-enum-table event-mask (xcb-event-mask-t "XCB-EVENT-MASK")
  :no-event :key-press :key-release :button-press :button-release
  :enter-window :leave-window :pointer-motion :pointer-motion-hint
  :button-1-motion :button-2-motion :button-3-motion :button-4-motion
  :button-5-motion :button-motion :keymap-state :exposure :visibility-change
  :structure-notify :resize-redirect :substructure-notify
  :substructure-redirect :focus-change :property-change :color-map-change
  :owner-grab-button)

(defun make-event-mask (&rest keys)
  (apply #'event-mask-logior keys))

 ;;; FIXME? efficiency
(defun make-event-keys (mask)
  (let ((x 0) keys)
    (loop for i from 0
          if (logbitp i mask)
            do (push (event-mask-key (ash 1 i)) keys)
               (incf x)
          end
          while (< x (logcount mask)))
    keys))

(defvar *event-map* (make-hash-table))
(defvar *event-slot-map* (make-hash-table))
(defvar *event-xcb-map* (make-hash-table))
(defvar *event-xcb-type-map* (make-hash-table))

(define-const-table event-type ("XCB")
  :key-press :key-release :button-press :button-release :motion-notify
  :enter-notify :leave-notify :focus-in :focus-out :keymap-notify
  (:exposure :expose) :graphics-exposure :no-exposure :visibility-notify
  :create-notify :destroy-notify :unmap-notify :map-notify :map-request
  :reparent-notify :configure-notify :configure-request :gravity-notify
  :resize-request :circulate-notify :circulate-request :property-notify
  :selection-clear :selection-request :selection-notify :colormap-notify
  :client-message :mapping-notify)

 ;; 12.3 Processing Events

(defmacro with-event-queue ((display) &body body)
  `(bt:with-recursive-lock-held ((%display-queue-lock ,display))
     ,@body))

(defun make-event (display ptr)
  (let* ((type (event-type-key (logandc1 #x80 (xcb-generic-event-t-response-type ptr))))
         (slots (find-slots type))
         (ev (list type :event-key)))
    (let ((parsed
            (loop for slot in slots
                  as defn = (find-event-slot type slot)
                  as cval = (funcall (cdr defn) ptr)
                  as val = (slot-translate-from (car defn) ptr cval display)
                  do (push slot ev)
                     (push val ev)
                  finally (return (nreverse ev)))))
      parsed)))

(defun encode-event (display event-spec ptr)
  (let ((type (cadr event-spec)))
    (setf (xcb-generic-event-t-response-type ptr)
          (event-type type))
    (loop for slot in (cddr event-spec) by #'cddr
          for val in (cdddr event-spec) by #'cddr do
            (let* ((defn (find-event-slot type slot))
                   (oldval (when defn (funcall (cdr defn) ptr)))
                   (encoder (find-slot-encoder type slot)))
              (when encoder
                (funcall encoder ptr
                         (slot-translate-to (car defn) event-spec
                                            val oldval display)))))))

(defmacro with-encoded-event ((ptr display event-spec) &body body)
  (let ((event (gensym "EVENT")))
    `(let ((,event ,event-spec))
       (with-foreign-object (,ptr (gethash (cadr ,event) *event-xcb-type-map*))
         (encode-event ,display ,event ,ptr)
         ,@body))))

(defun %poll-next-event (display timeout)
  (let ((fd (xcb-get-file-descriptor (display-ptr-xcb display)))
        (ptr))
    (setf ptr (xcb-poll-for-event (display-ptr-xcb display)))
    (loop while (null-pointer-p ptr) do
      (let ((fds (car (poll (list fd) :events '(:in :error :hup :invalid)
                                      :timeout (if timeout
                                                   (truncate (* 1000 timeout))
                                                   -1)))))
        ;; Not strictly correct because EINTR isn't the only error possible
        (when fds
          (if (or (not (eq :in (cadr fds)))
                  (cddr fds))
              (loop-finish) ;; display likely closed or invalid
              (setf ptr (xcb-wait-for-event (display-ptr-xcb display)))))))
    (when (null-pointer-p ptr)
      (let ((code (xcb-connection-has-error (display-ptr-xcb display))))
        (when (/= 0 code)
          (error (make-condition 'xlib-io-error :code code)))))
    ptr))

(defun %read-queue-event (display timeout)
  (let (ev parsed-event)
    (unwind-protect
         (let ((ev (%poll-next-event display timeout)))
           (unless (null-pointer-p ev)
             (if (= 0 (xcb-generic-event-t-response-type ev))
                 (setf parsed-event (make-x-error
                                     (xcb-generic-error-t-sequence ev)
                                     display ev))
                 (setf parsed-event (make-event display ev)))
             (with-event-queue (display)
               (queue-add (%display-event-queue display) parsed-event))))
      (when (and ev (not (null-pointer-p ev)))
        (xcb::libc_free ev)))
    parsed-event))

(defun %peek-next-event (display timeout)
  (with-event-queue (display)
   (or (queue-peek (%display-event-queue display))
       (%read-queue-event display timeout))))

;; FIXME, doesn't handle handler-vector
(defun process-event (display &key handler timeout peek-p discard-p
                                (force-output-p t))
  (let ((handled)
        (dpyq (%display-event-queue display))
        (tq (make-queue)))
    (loop do
      (when force-output-p (display-force-output display))
      (let ((ev (%peek-next-event display timeout)))
        (unless ev (loop-finish))
        (setf handled (apply handler ev))
        (with-event-queue (display)
          (if handled
              (progn
                (if peek-p
                    (queue-pop-to dpyq tq)
                    (queue-pop dpyq))
                (loop-finish))
              (if discard-p
                  (queue-pop dpyq)
                  (queue-pop-to dpyq tq))))))
    (with-event-queue (display) (queue-prepend-to tq dpyq))
    handled))

(defmacro with-event-slots ((&rest slots) event &body body)
  `(destructuring-bind (&key ,@slots &allow-other-keys) ,event ,@body))

(defmacro event-cond ((display &key timeout peek-p discard-p
                                 (force-output-p t))
                      &body body)
  (let* ((tq (gensym "TQ"))
         (dpy (gensym "DPY"))
         (dpyq (gensym "DPYQ"))
         (ev (gensym "EV"))
         (handled (gensym "HANDLED"))
         (err (gensym "ERR"))
         (body (mapcar (lambda (stmt)
                         `(,(car stmt)
                           (with-event-slots ,(cadr stmt) ,ev
                             (when ,(caddr stmt) ,@(cdddr stmt)))))
                       body)))
    `(let* ((,dpy ,display)
            (,dpyq (%display-event-queue ,dpy))
            (,tq (make-queue))
            ,err ,handled)
       (loop do (when ,force-output-p (display-force-output ,dpy))
                (let ((,ev (%peek-next-event ,dpy ,timeout)))
                  (unless ,ev (loop-finish))
                  (when (typep ,ev 'error)
                    (setf ,err ,ev)
                    (loop-finish))
                  (setf ,handled (case (cadr ,ev) ,@body))
                  (with-event-queue (,dpy)
                    (if ,handled
                        (progn
                          (if ,peek-p
                              (queue-pop-to ,dpyq ,tq)
                              (queue-pop ,dpyq))
                          (loop-finish))
                        (if ,discard-p
                            (queue-pop ,dpyq)
                            (queue-pop-to ,dpyq ,tq))))))
       (with-event-queue (,dpy) (queue-prepend-to ,tq ,dpyq))
       (when ,err (error ,err))
       ,handled)))

(defmacro event-case ((display &key timeout peek-p discard-p
                                 (force-output-p t))
                      &body body)
  (let ((body (mapcar (lambda (stmt)
                        `(,(car stmt) ,(cadr stmt) t ,@(cddr stmt)))
                      body)))
    `(event-cond (,display :timeout ,timeout :peek-p ,peek-p
                           :discard-p ,discard-p :force-output-p ,force-output-p)
       ,@body)))

 ;; 12.4 Managing the Queue

(defun queue-event (display event-key &rest event-slots
                    &key append-p &allow-other-keys)
  ;; FIXME: event validation?
  (let ((ev (list* :event-key event-key event-slots)))
    (with-event-queue (display)
      (if append-p
          (queue-add (%display-event-queue display) ev)
          (queue-push (%display-event-queue display) ev))))
  (values))

(defun discard-current-event (display)
  (with-event-queue (display)
   (let ((ev (queue-pop (%display-event-queue display))))
     (and ev t))))

(defun event-listen (display &optional (timeout 0))
  (%read-queue-event display timeout)
  (length (queue-head (%display-event-queue display))))

 ;; 12.5 Sending Events

(defun send-event (window event-key event-mask &rest event-slots
                   &key propagate-p display &allow-other-keys)
  (with-encoded-event (ptr (or display (display-for window))
                           (list* :event-key event-key event-slots))
    (xerr window
        (xcb-send-event-checked (display-ptr-xcb window)
                                (if propagate-p 1 0) (xid window)
                                (apply #'make-event-mask event-mask) ptr))))

 ;; 12.6 Pointer Position

(defun query-pointer (window)
  (let ((display (display-for window)))
    (do-request-response (window c reply err)
        (xcb-query-pointer c (xid window))
      (values
       (xcb-query-pointer-reply-t-win-x reply)
       (xcb-query-pointer-reply-t-win-y reply)
       (= 1 (xcb-query-pointer-reply-t-same-screen reply))
       (if (/= 0 (xcb-query-pointer-reply-t-child reply))
           (%make-window :display display
                         :id (xcb-query-pointer-reply-t-child reply)))
       (xcb-query-pointer-reply-t-root-x reply)
       (xcb-query-pointer-reply-t-root-y reply)
       (xcb-query-pointer-reply-t-mask reply)
       (%make-window :display display
                     :id (xcb-query-pointer-reply-t-root reply))))))

(defun global-pointer-position (display)
  (multiple-value-bind (x y same-screen-p window root-x root-y mask screen-root)
      (query-pointer (screen-root (first (display-roots display))))
    (declare (ignore x y same-screen-p window mask))
    (values root-x root-y screen-root)))

(defun pointer-position (window)
  (multiple-value-bind (x y same-screen-p window root-x root-y mask screen-root)
      (query-pointer window)
    (declare (ignore root-x root-y mask screen-root))
    (values x y same-screen-p window)))

;; FIXME: result format
(defun motion-events (window &key start stop (result-type 'list))
  (do-request-response (window c reply err)
      (xcb-get-motion-events c (xid window)
                             (or start 0) (or stop 0))
    (map-result-list result-type
                     (lambda (ptr)
                       (list (xcb-timecoord-t-x ptr)
                             (xcb-timecoord-t-y ptr)
                             (xcb-timecoord-t-time ptr)))
                     #'xcb-get-motion-events-events
                     #'xcb-get-motion-events-events-length
                     reply '(:struct xcb-timecoord-t))))

(defun warp-pointer (destination dest-x dest-y)
  (xerr destination
      (xcb-warp-pointer-checked (display-ptr-xcb destination)
                                0 (xid destination) 0 0 0 0
                                dest-x dest-y)))

(defun warp-pointer-relative (display dest-x dest-y)
  (xerr display
      (xcb-warp-pointer-checked (display-ptr-xcb display)
                                0 0 0 0 0 0
                                dest-x dest-y)))

(defun warp-pointer-if-inside (destination dest-x dest-y
                              source source-x source-y
                              &optional (source-width 0) (source-height 0))
  (xerr destination
      (xcb-warp-pointer-checked (display-ptr-xcb destination)
                                (xid source) (xid destination)
                                source-x source-y
                                source-width source-height
                                dest-x dest-y)))

(defun warp-pointer-relative-if-inside
    (x-offset y-offset
     source source-x source-y
     &optional (source-width 0) (source-height 0))
  (xerr source
      (xcb-warp-pointer-checked (display-ptr-xcb source)
                                (xid source) 0
                                source-x source-y
                                source-width source-height
                                x-offset y-offset)))

 ;; 12.7 Managing Input Focus

(define-enum-table input-focus-type (xcb-input-focus-t "XCB-INPUT-FOCUS")
  :none :pointer-root :parent :follow-keyboard)

(defun set-input-focus (display focus revert-to &optional time)
  (xerr display
      (xcb-set-input-focus-checked (display-ptr-xcb display)
                                   (input-focus-type revert-to)
                                   (if (window-p focus)
                                       (xid focus)
                                       (input-focus-type focus))
                                   (or time 0))))

(defun input-focus (display)
  (do-request-response (display c reply err)
      (xcb-get-input-focus (display-ptr-xcb display))
    (values (or (input-focus-type-key (xcb-get-input-focus-reply-t-focus reply))
                (%make-window :display display
                              :id (xcb-get-input-focus-reply-t-focus reply)))
            (input-focus-type-key (xcb-get-input-focus-reply-t-revert-to reply)))))

 ;; 12.8 Grabbing the Pointer

(define-enum-table grab-status (xcb-grab-status-t "XCB-GRAB-STATUS")
  :success :already-grabbed :invalid-time :not-viewable :frozen)

(defun grab-pointer (window event-mask
                    &key owner-p sync-pointer-p sync-keyboard-p confine-to
                      cursor time)
  (do-request-response (window c reply err)
      (xcb-grab-pointer c (if owner-p 1 0) (xid window)
                        (apply #'make-event-mask event-mask)
                        (if sync-pointer-p 1 0)
                        (if sync-keyboard-p 1 0)
                        (xid confine-to)
                        (xid cursor)
                        (or time 0))
    (grab-status-key (xcb-grab-pointer-reply-t-status reply))))

(defun ungrab-pointer (display &key time)
  (xerr display
      (xcb-ungrab-pointer-checked (display-ptr-xcb display) (or time 0))))

(defun change-active-pointer-grab (display event-mask &optional cursor time)
  (xerr display
      (xcb-change-active-pointer-grab-checked (display-ptr-xcb display)
                                              (xid cursor) (or time 0)
                                              (make-event-mask event-mask))))

 ;; 12.9 Grabbing a Button

(define-enum-table mod-mask (xcb-mod-mask-t "XCB-MOD-MASK")
  :shift :lock :control (:mod-1 :1) (:mod-2 :2) (:mod-3 :3) (:mod-4 :4) (:mod-5 :5) :any)

(defun encode-modifier (mod)
  (etypecase mod
    ((unsigned-byte 16) mod)
    (keyword (mod-mask mod))
    (list (apply #'mod-mask-logior mod))))

(defun grab-button (window button event-mask
                   &key (modifiers 0) owner-p sync-pointer-p sync-keyboard-p
                     confine-to cursor)
  (xerr window
      (xcb-grab-button-checked (display-ptr-xcb window)
                               (if owner-p 1 0) (xid window)
                               (apply #'make-event-mask event-mask)
                               (if sync-pointer-p 1 0)
                               (if sync-keyboard-p 1 0)
                               (xid confine-to)
                               (xid cursor)
                               button
                               (encode-modifier modifiers))))

(defun ungrab-button (window button &key (modifiers 0))
  (xerr window
      (xcb-ungrab-button-checked (display-ptr-xcb window) button
                                 (xid window) (encode-modifier modifiers))))

 ;; 12.10 Grabbing the Keyboard

(defun grab-keyboard (window &key owner-p sync-pointer-p sync-keyboard-p
                              time)
  (do-request-response (window c reply err)
      (xcb-grab-keyboard c (if owner-p 1 0)
                         (xid window) (or time 0)
                         (if sync-pointer-p 1 0)
                         (if sync-keyboard-p 1 0))
    (grab-status-key (xcb-grab-keyboard-reply-t-status reply))))

(defun ungrab-keyboard (display &key time)
  (xerr display
      (xcb-ungrab-keyboard (display-ptr-xcb display)
                           (or time 0))))

 ;; 12.11 Grabbing a Key

(defun grab-key (window key
                &key (modifiers 0) owner-p sync-pointer-p sync-keyboard-p
                  time)
  (declare (ignore time))
  (xerr window
      (xcb-grab-key-checked (display-ptr-xcb window)
                            (if owner-p 1 0) (xid window)
                            (encode-modifier modifiers)
                            (if (eq :any key) 0 key)
                            (if sync-pointer-p 1 0)
                            (if sync-keyboard-p 1 0))))

(defun ungrab-key (window key &key (modifiers 0))
  (xerr window
      (xcb-ungrab-key-checked (display-ptr-xcb window)
                              (if (eq :any key) 0 key)
                              (xid window) (encode-modifier modifiers))))

 ;; 12.12.(1-7) See event-decl.lisp

 ;; 12.12.8 Event Types, Declaring Events

(defmacro declare-event (event-codes xcb-type &rest slot-declarations)
  "This is not compatible with the 12.12.8 definition of `DECLARE-EVENT` simply
because it makes no sense to be compatible; events in `XCB.CLX` tie directly
to their XCB counterparts and therefore cannot exist without the correlating
struct.  This however acts similarly. `EVENT-CODES` is a list of events
declared for the specified `XCB-TYPE`; they must be identical cstructs,
because the accessors for `XCB-TYPE` will be used in all cases.

`SLOT-DECLARATIONS` are in the form `(TYPE SLOT-NAME*)`, similar to
the 12.12.8 definition.  `SLOT-NAME` must either be the name of the
`XCB-TYPE` slot or a list of aliases, where the first is the name of
the `XCB-TYPE` slot.

However, all `XCB-TYPE` slots need not be defined, nor need they necessarily
be in order, since the appropriate accessor is used to get the value."
  (let ((slot-map (make-hash-table))
        (xcb-string (string (eval xcb-type)))
        (slot-list nil)
        (slot-encoder-list))
    (flet ((slot-acc (name)
             (intern (concatenate 'string xcb-string "-" (string name)))))
      (loop for decl in slot-declarations
            as slot-type = (car decl)
            as slots = (cdr decl) do
              (loop for slot in slots
                    as acc = (slot-acc (if (consp slot) (car slot) slot))
                    do (multiple-value-bind (dummies vals new setter getter)
                           (get-setf-expansion `(,acc ptr))
                         (declare (ignore getter))
                         (let ((encoder `(lambda (,@vals ,@new)
                                           (let (,@(mapcar #'list dummies vals))
                                             ,setter))))
                           (if (consp slot)
                               (map 'nil (lambda (alias)
                                           (let ((kw (intern (string alias) :keyword)))
                                             (push kw slot-list)
                                             (setf (gethash kw slot-map) (cons slot-type acc))
                                             (push `(setf (gethash ,kw encoders) ,encoder) slot-encoder-list)))
                                    slot)
                               (progn
                                 (let ((kw (intern (string slot) :keyword)))
                                   (push kw slot-list)
                                   (setf (gethash kw slot-map) (cons slot-type acc))
                                   (push `(setf (gethash ,kw encoders) ,encoder) slot-encoder-list)))))))))
    `(map 'nil (lambda (code)
                 (setf (gethash code *event-slot-map*) ',slot-list)
                 (setf (gethash code *event-map*) ,slot-map)
                 (setf (gethash code *event-xcb-type-map*) ,xcb-type)
                 (setf (gethash code *event-xcb-map*)
                       (let ((encoders (make-hash-table)))
                         ,@slot-encoder-list
                         encoders)))
          ',event-codes)))

(declaim (inline find-event-slot find-slots))
(defun find-event-slot (event-type slot-name)
  (gethash slot-name (gethash event-type *event-map*)))

(defun find-slots (type)
  (gethash type *event-slot-map*))

(defun find-slot-encoder (event-type slot-name)
  (gethash slot-name (gethash event-type *event-xcb-map*)))

 ;; 12.13 Releasing Queued Events

(define-enum-table allow (xcb-allow-t "XCB-ALLOW")
  :async-pointer :sync-pointer :replay-pointer :async-keyboard
  :sync-keyboard :replay-keyboard :async-both :sync-both)

(defun allow-events (display mode &optional time)
  (xerr display
      (xcb-allow-events-checked (display-ptr-xcb display)
                                (allow mode) (or time 0))))
