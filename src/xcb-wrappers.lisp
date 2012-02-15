(in-package :xcb)

 ;; Basic Types
(make-cstruct-accessors xcb-void-cookie-t)
(make-cstruct-accessors xcb-generic-iterator-t)
(make-cstruct-accessors xcb-screen-t)
(make-cstruct-accessors xcb-auth-info-t)
(make-cstruct-accessors xcb-setup-t)
(make-cstruct-accessors xcb-get-window-attributes-reply-t)
(make-cstruct-accessors xcb-intern-atom-reply-t)
(make-cstruct-accessors xcb-get-atom-name-reply-t)
(make-cstruct-accessors xcb-get-property-reply-t)
(make-cstruct-accessors xcb-get-selection-owner-reply-t)
(make-cstruct-accessors xcb-translate-coordinates-reply-t)

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

 ;; Errors
(make-cstruct-accessors xcb-generic-error-t)
(make-cstruct-accessors xcb-request-error-t)
(make-cstruct-accessors xcb-value-error-t)

 ;; Graphics
(make-cstruct-accessors xcb-point-t)

 ;; Xlib

(make-cstruct-accessors xvisual-info)
(make-cstruct-accessors xerror-event)

 ;; poll
(defbitfield (poll-events-mask :short)
  (:in #.+pollin+)
  (:pri #.+pollpri+)
  (:out #.+pollout+)
  (:error #.+pollerr+)
  (:hup #.+pollhup+)
  (:invalid #.+pollnval+))

(defcstruct pollfd
  (fd :int)
  (events poll-events-mask)
  (revents poll-events-mask))

(make-cstruct-accessors pollfd)

(defcfun ("poll" libc-poll) :int
  (fds :pointer)
  (nfds nfds-t)
  (timeout :int))

(defun poll (fds &key (events '(:in :out :error)) (timeout -1))
  (let ((nfds (length fds)))
    (with-foreign-object (fds-ptr '(:struct pollfd) nfds)
      (loop for i from 0 below nfds
            for fd in fds
            as pollfd = (mem-pref fds-ptr 'pollfd i)
            do (setf (pollfd-fd pollfd) fd)
               (setf (pollfd-events pollfd) events))
      (let ((err (libc-poll fds-ptr nfds timeout))
            (outfds))
        (cond
          ((= err -1) ())
          ((> err 0)
           (loop for i from 0 below nfds
                 while (> err 0)
                 as pollfd = (mem-pref fds-ptr 'pollfd i)
                 as revents = (pollfd-revents pollfd)
                 do (when revents
                      (push (cons (pollfd-fd pollfd) revents) outfds)
                      (decf err)))))
        outfds))))

(export 'poll)

 ;; Type Translation
(declaim (inline xcb-cookie-val))
(defun xcb-cookie-val (ptr)
  (xcb-void-cookie-t-sequence ptr))

(defmacro wrap-iterators (&rest types)
  `(progn
     ,@(loop for type in types
             as tclass = (intern (format nil "XCB-~A-ITERATOR-T-TCLASS" type))
             as nextfn = (intern (format nil "XCB-~A-NEXT" type))
             collect
             `(defmethod translate-from-foreign (ptr (type ,tclass))
                (let (list)
                  (loop while (> (xcb-generic-iterator-t-rem ptr) 0)
                        do (push (xcb-generic-iterator-t-data ptr) list)
                           (,nextfn ptr))
                  list)))))

(wrap-iterators screen)

(defmacro wrap-xcb-reply-cookies (&rest types)
  `(progn
     ,@(loop for i in types
             as type = (intern (format nil "XCB-~A-COOKIE-T-TCLASS" i))
             collect
             `(defmethod expand-from-foreign (ptr (type ,type))
                `(xcb-cookie-val ,ptr)))))

(wrap-xcb-reply-cookies
  alloc-color
  alloc-color-cells
  alloc-color-planes
  alloc-named-color
  get-atom-name
  get-font-path
  get-geometry
  get-image
  get-input-focus
  get-keyboard-control
  get-keyboard-mapping
  get-modifier-mapping
  get-motion-events
  get-pointer-control
  get-pointer-mapping
  get-property
  get-screen-saver
  get-selection-owner
  get-window-attributes
  grab-keyboard
  grab-pointer
  intern-atom
  list-extensions
  list-fonts
  list-fonts-with-info
  list-hosts
  list-installed-colormaps
  list-properties
  lookup-color
  query-best-size
  query-colors
  query-extension
  query-font
  query-keymap
  query-pointer
  query-text-extents
  query-tree
  set-modifier-mapping
  set-pointer-mapping
  translate-coordinates
  void)
