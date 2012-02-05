(in-package :xcb)

 ;; Basic Types
(make-cstruct-accessors xcb-screen-iterator-t)
(make-cstruct-accessors xcb-screen-t)
(make-cstruct-accessors xcb-auth-info-t)
(make-cstruct-accessors xcb-setup-t)
(make-cstruct-accessors xcb-get-window-attributes-reply-t)

 ;; Events
(make-cstruct-accessors xcb-generic-event-t)
(make-cstruct-accessors xcb-ge-event-t)
(make-cstruct-accessors xcb-key-press-event-t)
(make-cstruct-accessors xcb-button-press-event-t)
(make-cstruct-accessors xcb-motion-notify-event-t)
(make-cstruct-accessors xcb-enter-notify-event-t)
(make-cstruct-accessors xcb-focus-in-event-t)
(make-cstruct-accessors xcb-keymap-notify-event-t)
(make-cstruct-accessors xcb-expose-event-t)
(make-cstruct-accessors xcb-graphics-exposure-event-t)
(make-cstruct-accessors xcb-no-exposure-event-t)
(make-cstruct-accessors xcb-visibility-notify-event-t)
(make-cstruct-accessors xcb-create-notify-event-t)
(make-cstruct-accessors xcb-destroy-notify-event-t)
(make-cstruct-accessors xcb-unmap-notify-event-t)
(make-cstruct-accessors xcb-map-notify-event-t)
(make-cstruct-accessors xcb-map-request-event-t)
(make-cstruct-accessors xcb-reparent-notify-event-t)
(make-cstruct-accessors xcb-configure-notify-event-t)
(make-cstruct-accessors xcb-configure-request-event-t)
(make-cstruct-accessors xcb-gravity-notify-event-t)
(make-cstruct-accessors xcb-resize-request-event-t)
(make-cstruct-accessors xcb-circulate-notify-event-t)
(make-cstruct-accessors xcb-property-notify-event-t)
(make-cstruct-accessors xcb-selection-clear-event-t)
(make-cstruct-accessors xcb-selection-request-event-t)
(make-cstruct-accessors xcb-selection-notify-event-t)
(make-cstruct-accessors xcb-colormap-notify-event-t)
(make-cstruct-accessors xcb-client-message-event-t)
(make-cstruct-accessors xcb-mapping-notify-event-t)

 ;; Type Translation
(defmethod translate-from-foreign (ptr (type xcb-screen-iterator-t-tclass))
  (let (screens)
    (loop while (> (xcb-screen-iterator-t-rem ptr) 0)
          do (push (xcb-screen-iterator-t-data ptr) screens)
             (xcb-screen-next ptr))
    screens))