(in-package :xcb)

 ;; Basic Types
(make-cstruct-accessors xcb-void-cookie-t
                        xcb-generic-iterator-t
                        xcb-screen-t
                        xcb-auth-info-t
                        xcb-setup-t
                        xcb-get-window-attributes-reply-t
                        xcb-intern-atom-reply-t
                        xcb-get-atom-name-reply-t
                        xcb-get-property-reply-t
                        xcb-get-selection-owner-reply-t
                        xcb-translate-coordinates-reply-t
                        xcb-get-geometry-reply-t
                        xcb-alloc-color-planes-reply-t
                        xcb-lookup-color-reply-t
                        xcb-query-colors-reply-t
                        xcb-rgb-t
                        xcb-coloritem-t
                        xcb-query-best-size-reply-t
                        xcb-grab-keyboard-reply-t
                        xcb-grab-pointer-reply-t
                        xcb-get-input-focus-reply-t
                        xcb-query-pointer-reply-t
                        xcb-timecoord-t
                        xcb-format-t
                        xcb-list-fonts-with-info-reply-t
                        xcb-query-font-reply-t
                        xcb-charinfo-t
                        xcb-fontprop-t
                        xcb-char-2b-t
                        xcb-query-text-extents-reply-t
                        xcb-query-extension-reply-t
                        xcb-get-image-reply-t
                        xcb-depth-t
                        xcb-visualtype-t
                        xcb-get-pointer-control-reply-t
                        xcb-get-keyboard-control-reply-t
                        xcb-get-modifier-mapping-reply-t
                        xcb-query-keymap-reply-t
                        xcb-set-modifier-mapping-reply-t
                        xcb-get-keyboard-mapping-reply-t
                        xcb-list-hosts-reply-t)

 ;; Events
(make-cstruct-accessors xcb-generic-event-t
                        xcb-ge-event-t
                        xcb-key-press-event-t
                        xcb-button-press-event-t
                        xcb-motion-notify-event-t
                        xcb-enter-notify-event-t
                        xcb-focus-in-event-t
                        xcb-keymap-notify-event-t
                        xcb-expose-event-t
                        xcb-graphics-exposure-event-t
                        xcb-no-exposure-event-t
                        xcb-visibility-notify-event-t
                        xcb-create-notify-event-t
                        xcb-destroy-notify-event-t
                        xcb-unmap-notify-event-t
                        xcb-map-notify-event-t
                        xcb-map-request-event-t
                        xcb-reparent-notify-event-t
                        xcb-configure-notify-event-t
                        xcb-configure-request-event-t
                        xcb-gravity-notify-event-t
                        xcb-resize-request-event-t
                        xcb-circulate-notify-event-t
                        xcb-property-notify-event-t
                        xcb-selection-clear-event-t
                        xcb-selection-request-event-t
                        xcb-selection-notify-event-t
                        xcb-colormap-notify-event-t
                        xcb-client-message-event-t
                        xcb-mapping-notify-event-t
                        xcb-get-screen-saver-reply-t)

 ;; Errors
(make-cstruct-accessors xcb-generic-error-t
                        xcb-request-error-t
                        xcb-value-error-t)

 ;; Graphics
(make-cstruct-accessors xcb-point-t)

 ;; Xlib

(make-cstruct-accessors xvisual-info
                        xerror-event)

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

(wrap-iterators screen str format depth visualtype host)

(defun xcb-str-to-lisp (ptr)
  (foreign-string-to-lisp (xcb-str-name ptr)
                          :count (xcb-str-name-length ptr)))

(export 'xcb-str-to-lisp)

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
