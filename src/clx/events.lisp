(in-package :xcb.clx)

 ;; Event Values

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun xcbevm (type)
    (let ((kw (intern (concatenate 'string "+XCB-EVENT-MASK-"
                                   (string type) "+")
                      (find-package :keyword))))
      (cons type (foreign-enum-value 'xcb-event-mask-t kw)))))

(defvar *event-mask-map*
  '(#.(xcbevm :no-event)
    #.(xcbevm :key-press)
    #.(xcbevm :key-release)
    #.(xcbevm :button-press)
    #.(xcbevm :button-release)
    #.(xcbevm :enter-window)
    #.(xcbevm :leave-window)
    #.(xcbevm :pointer-motion)
    #.(xcbevm :pointer-motion-hint)
    #.(xcbevm :button-1-motion)
    #.(xcbevm :button-2-motion)
    #.(xcbevm :button-3-motion)
    #.(xcbevm :button-4-motion)
    #.(xcbevm :button-5-motion)
    #.(xcbevm :button-motion)
    #.(xcbevm :keymap-state)
    #.(xcbevm :exposure)
    #.(xcbevm :visibility-change)
    #.(xcbevm :structure-notify)
    #.(xcbevm :resize-redirect)
    #.(xcbevm :substructure-notify)
    #.(xcbevm :substructure-redirect)
    #.(xcbevm :focus-change)
    #.(xcbevm :property-change)
    #.(xcbevm :color-map-change)
    #.(xcbevm :owner-grab-button)))

(defun make-event-mask (&rest keys)
  (apply #'logior
         (mapcar (lambda (type) (cdr (assoc type *event-mask-map*)))
                 keys)))

;; FIXME? efficiency
(defun make-event-keys (mask)
  (let ((x 0) keys)
    (loop for i from 0
          if (logbitp i mask)
            do (push (car (rassoc (ash 1 i) *event-mask-map*)) keys)
               (incf x)
          end
          while (< x (logcount mask)))
    keys))

(defvar *event-type-map*
  '((+xcb-motion-notify+ . :motion-notify)
    (+xcb-button-press+ . :button-press)
    (+xcb-button-release+ . :button-release)
    (+xcb-colormap-notify+ . :colormap-notify)
    (+xcb-enter-notify+ . :enter-notify)
    (+xcb-expose+ . :exposure)
    (+xcb-focus-in+ . :focus-in)
    (+xcb-focus-out+ . :focus-out)
    (+xcb-key-press+ . :key-press)
    (+xcb-key-release+ . :key-release)
    (+xcb-keymap-notify+ . :keymap-notify)
    (+xcb-leave-notify+ . :leave-notify)
    (+xcb-motion-notify+ . :motion-notify)
    (+xcb-property-notify+ . :property-notify)
    (+xcb-resize-request+ . :resize-request)
    (+xcb-circulate-notify+ . :circulate-notify)
    (+xcb-configure-notify+ . :configure-notify)
    (+xcb-destroy-notify+ . :destroy-notify)
    (+xcb-gravity-notify+ . :gravity-notify)
    (+xcb-map-notify+ . :map-notify)
    (+xcb-reparent-notify+ . :reparent-notify)
    (+xcb-unmap-notify+ . :unmap-notify)
    (+xcb-circulate-request+ . :circulate-request)
    (+xcb-configure-request+ . :configure-request)
    (+xcb-map-request+ . :map-request)
    (+xcb-visibility-notify+ . :visibility-notify)))

 ;; 12.3 Processing Events

(stub handler-function (&rest event-slots
                        &key display event-key send-event-p
                        &allow-other-keys))

(stub process-event (display &key handler timeout peek-p discard-p
                               (force-output-p t)))

(stub-macro event-case ((display &key timeout peek-p discard-p
                                   (force-output-p t))
                        &body body))

(stub-macro event-cond ((display &key timeout peek-p discard-p
                                   (force-output-p t))
                        &body body))

 ;; 12.4 Managing the Queue

(stub queue-event (display event-key &rest event-slots
                   &key append-p &allow-other-keys))

(stub discard-current-event (display))
(stub event-listen (display &optional (timeout 0)))

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

 ;; 12.12.8 Event Types, Declaring Events

(stub-macro declare-event (event-codes &rest slot-declarations))

 ;; 12.13 Releasing Queued Events

(stub allow-events (display mode &optional time))
