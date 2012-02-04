(in-package :xcb)

(make-cstruct-accessors xcb-screen-iterator-t)
(make-cstruct-accessors xcb-screen-t)
(make-cstruct-accessors xcb-auth-info-t)
(make-cstruct-accessors xcb-setup-t)
(make-cstruct-accessors xcb-get-window-attributes-reply-t)

(defmethod translate-from-foreign (ptr (type xcb-screen-iterator-t-tclass))
  (let (screens)
    (loop while (> (xcb-screen-iterator-t-rem ptr) 0)
          do (push (xcb-screen-iterator-t-data ptr) screens)
             (xcb-screen-next ptr))
    screens))
