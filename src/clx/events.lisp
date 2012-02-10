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

(defun make-event (display ptr)
  (let* ((type (event-type-key (xcb-generic-event-t-response-type ptr)))
         (slots (find-slots type))
         (ev (list type :event-key)))
    (let ((parsed
            (loop for slot in slots
                  as defn = (find-event-slot type slot)
                  as cval = (funcall (cdr defn) ptr)
                  as val = (slot-translate-from (car defn) cval display)
                  do (push slot ev)
                     (push val ev)
                  finally (return (nreverse ev)))))
      parsed)))

(defun %read-queue-event (display timeout)
  (let ((ev)
        (parsed-event)
        (fd (xcb-get-file-descriptor (display-ptr-xcb display))))
    (unwind-protect
         (let ((fds (poll (list fd) :events '(:in :error :hup :invalid)
                                    :timeout (if timeout
                                                 (truncate (* 1000 timeout))
                                                 -1))))
           (when fds
             (setf ev (xcb-poll-for-event (display-ptr-xcb display)))
             (unless (null-pointer-p ev)
               (setf parsed-event (make-event display ev))
               (queue-add (%display-event-queue display) parsed-event))))
      (when (and ev (not (null-pointer-p ev)))
        (xcb::libc_free ev)))
    parsed-event))

(defun %peek-next-event (display timeout)
  (or (queue-peek (%display-event-queue display))
      (%read-queue-event display timeout)))

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
        (if handled
            (progn
              (if peek-p
                  (queue-pop-to dpyq tq)
                  (queue-pop dpyq))
              (loop-finish))
            (if discard-p
                (queue-pop dpyq)
                (queue-pop-to dpyq tq)))))
    (queue-prepend-to tq dpyq)
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
         (body (mapcar (lambda (stmt)
                         `(,(car stmt)
                           (with-event-slots ,(cadr stmt) ,ev
                             (when ,(caddr stmt) ,@(cdddr stmt)))))
                       body)))
    `(let* ((,dpy ,display)
            (,dpyq (%display-event-queue ,dpy))
            (,tq (make-queue))
            ,handled)
       (loop do (when ,force-output-p (display-force-output ,dpy))
                (let ((,ev (%peek-next-event ,dpy ,timeout)))
                  (unless ,ev (loop-finish))
                  (setf ,handled (case (cadr ,ev) ,@body))
                  (if ,handled
                      (progn
                        (if ,peek-p
                            (queue-pop-to ,dpyq ,tq)
                            (queue-pop ,dpyq))
                        (loop-finish))
                      (if ,discard-p
                          (queue-pop ,dpyq)
                          (queue-pop-to ,dpyq ,tq)))))
       (queue-prepend-to ,tq ,dpyq)
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

(stub queue-event (display event-key &rest event-slots
                   &key append-p &allow-other-keys))

(defun discard-current-event (display)
  (let ((ev (queue-pop (%display-event-queue display))))
    (and ev t)))

(defun event-listen (display &optional (timeout 0))
  (%read-queue-event display timeout)
  (length (queue-head (%display-event-queue display))))

(stub-macro with-event-queue ((display) &body body))

 ;; 12.5 Sending Events

(stub send-event (window event-key event-mask &rest event-slots
                  &key propagete-p display &allow-other-keys))

 ;; 12.6 Pointer Position

(stub query-pointer (window))
(stub global-pointer-position (display))
(stub pointer-position (window))
(stub motion-events (window &key start stop (result-type 'list)))
(stub warp-pointer (destination dest-x dest-y))
(stub warp-pointer-relative (display dest-x dest-y))
(stub warp-pointer-if-inside (destination dest-x dest-y
                              source source-x source-y
                              &optional (source-width 0) (source-height 0)))
(stub warp-pointer-relative-if-inside
    (x-offset y-offset
     source source-x source-y
     &optional (source-width 0) (source-height 0)))

 ;; 12.7 Managing Input Focus

(stub set-input-focus (display focus revert-to &optional time))
(stub input-focus (display))

 ;; 12.8 Grabbing the Pointer

(stub grab-pointer (window event-mask
                    &key owner-p sync-pointer-p sync-keyboard-p confine-to
                      cursor time))

(stub ungrab-pointer (display &key time))
(stub change-active-pointer-grab (display event-mask &optional cursor time))

 ;; 12.9 Grabbing a Button

(stub grab-button (window button event-mask
                   &key (modifiers 0) owner-p sync-pointer-p sync-keyboard-p
                     confine-to cursor))

(stub ungrab-button (window button &key (modifiers 0)))

 ;; 12.10 Grabbing the Keyboard

(stub grab-keyboard (window &key owner-p sync-pointer-p sync-keyboard-p
                              time))

(stub ungrab-keyboard (display &key time))

 ;; 12.11 Grabbing a Key

(stub grab-key (window key
                &key (modifiers 0) owner-p sync-pointer-p sync-keyboard-p
                  time))

(stub ungrab-key (window &key time))

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
        (xcb-type (string (eval xcb-type)))
        (slot-list nil))
    (flet ((slot-acc (name)
             (intern (concatenate 'string xcb-type "-" (string name)))))
      (loop for decl in slot-declarations
            as slot-type = (car decl)
            as slots = (cdr decl) do
              (loop for slot in slots
                    as acc = (slot-acc (if (consp slot) (car slot) slot))
                    do (if (consp slot)
                           (map 'nil (lambda (alias)
                                       (let ((kw (intern (string alias) :keyword)))
                                         (push kw slot-list)
                                         (setf (gethash kw slot-map) (cons slot-type acc))))
                                slot)
                           (progn
                             (let ((kw (intern (string slot) :keyword)))
                               (push kw slot-list)
                               (setf (gethash kw slot-map) (cons slot-type acc))))))))
    `(map 'nil (lambda (code)
                 (setf (gethash code *event-slot-map*) ',slot-list)
                 (setf (gethash code *event-map*) ,slot-map))
          ',event-codes)))

(declaim (inline find-event-slot find-slots))
(defun find-event-slot (event-type slot-name)
  (gethash slot-name (gethash event-type *event-map*)))

(defun find-slots (type)
  (gethash type *event-slot-map*))

 ;; 12.13 Releasing Queued Events

(stub allow-events (display mode &optional time))
