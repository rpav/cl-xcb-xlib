(in-package :xcb.clx)

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
