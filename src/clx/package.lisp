(defpackage :xcb.clx
  (:use #:cl #:cffi #:xcb)
  (:nicknames #:xlib)
  (:export

   *unimplemented*
   *display*

   ;; ATOM
   atom-name
   find-atom
   intern-atom
   change-property
   delete-property
   get-property
   list-properties
   rotate-properties
   convert-selection
   selection-owner

   ;; COLOR
   make-color
   color
   color-rgb
   color-red
   color-green
   color-blue
   color-p

   ;; COLORMAP
   colormap
   create-colormap
   copy-colormap-and-free
   free-colormap
   install-colormap
   installed-colormaps
   uninstall-colormap
   alloc-color
   alloc-color-cells
   alloc-color-planes
   free-colors
   lookup-color
   query-colors
   store-color
   store-colors
   colormap-display
   colormap-equal
   colormap-id
   colormap-plist
   colormap-p

   ;; CONTROL
   grab-server
   ungrab-server
   with-server-grabbed
   change-pointer-control
   pointer-control
   pointer-mapping
   bell
   change-keyboard-control
   keyboard-control
   modifier-mapping
   query-keymap
   set-modifier-mapping
   change-keyboard-mapping
   keyboard-mapping
   keycode->keysym
   keysym->character
   keysym->value
   canonical-keysym
   add-to-save-set
   close-down-mode
   kill-client
   kill-temporary-clients
   remove-from-save-set
   access-control
   access-hosts
   add-access-host
   remove-access-host
   activate-screen-saver
   reset-screen-saver
   screen-saver
   set-screen-saver

   ;; CURSOR
   cursor
   create-cursor
   create-glyph-cursor
   free-cursor
   query-best-cursor
   recolor-cursor
   cursor-display
   cursor-equal
   cursor-id
   cursor-plist
   cursor-p

   ;; DISPLAY
   default-error-handler
   display
   display-for
   display-ptr-xcb
   display-ptr-xlib
   open-display
   display-authorization-data
   display-authorization-name
   display-bitmap-format
   display-byte-order
   display-display
   display-error-handler
   display-image-lsb-first-p
   display-keycode-range
   display-max-keycode
   display-max-request-length
   display-min-keycode
   display-motion-buffer-size
   display-pixmap-formats
   display-plist
   display-p
   display-protocol-major-version
   display-protocol-minor-version
   display-protocol-version
   display-resource-id-base
   display-resource-id-mask
   display-roots
   display-vendor-name
   display-release-number
   display-vendor
   display-host
   display-version-number
   display-xid
   display-after-function
   display-force-output
   display-finish-output
   close-display
   with-display
   close-display-hook

   display-funcall
   do-on-display

   ;; DRAWABLE
   drawable
   xid
   drawable-display
   drawable-equal
   drawable-plist
   drawable-p
   drawable-border-width
   drawable-depth
   drawable-height
   drawable-width
   drawable-x
   drawable-y
   drawable-root

   query-best-size

   with-state

   ;; EVENTS
   make-event-mask
   make-event-keys
   process-event
   event-case
   event-cond
   queue-event
   discard-current-event
   event-listen
   with-event-queue
   send-event
   query-pointer
   global-pointer-position
   pointer-position
   motion-events
   warp-pointer
   warp-pointer-relative
   warp-pointer-if-inside
   warp-pointer-relative-if-inside
   set-input-focus
   input-focus
   grab-pointer
   ungrab-pointer
   change-active-pointer-grab
   grab-button
   ungrab-button
   grab-keyboard
   ungrab-keyboard
   grab-key
   ungrab-key
   declare-event
   allow-events

   ;; ERRORS
   x-error
   request-error
   resource-error
   access-error
   alloc-error
   atom-error
   closed-display
   colormap-error
   connection-failure
   cursor-error
   device-busy
   drawable-error
   font-error
   gcontext-error
   id-choice-error
   implementation-error
   length-error
   lookup-error
   match-error
   missing-parameter
   name-error
   pixmap-error
   reply-length-error
   reply-timeout
   sequence-error
   server-disconnect
   unexpected-reply
   unknown-error
   value-error
   window-error

   ;; FONT
   font
   open-font
   close-font
   discard-font-info
   font-path
   list-font-names
   list-fonts
   font-all-chars-exist-p
   font-ascent
   font-default-char
   font-descent
   font-direction
   font-display
   font-equal
   font-id
   font-max-byte1
   font-max-byte2
   font-max-char
   font-min-byte1
   font-min-byte2
   font-min-char
   font-name
   font-plist
   font-p
   font-properties
   font-property
   max-char-ascent
   max-char-attributes
   max-char-descent
   max-char-left-bearing
   max-char-right-bearing
   max-char-width
   min-char-ascent
   min-char-attributes
   min-char-descent
   min-char-left-bearing
   min-char-right-bearing
   min-char-width
   char-ascent
   char-attributes
   char-descent
   char-left-bearing
   char-right-bearing
   char-width
   text-extents
   text-width

   ;; GCONTEXT
   gcontext
   create-gcontext
   gcontext-arc-mode
   gcontext-background
   gcontext-cache-p
   gcontext-cap-style
   gcontext-clip-mask
   set-gcontext-clip-mask
   gcontext-clip-x
   gcontext-clip-y
   gcontext-dash-offset
   gcontext-dashes
   gcontext-display
   gcontext-equal
   gcontext-exposures
   gcontext-fill-rule
   gcontext-fill-style
   gcontext-font
   set-gcontext-font
   gcontext-foreground
   gcontext-function
   gcontext-id
   gcontext-join-style
   gcontext-line-style
   gcontext-line-width
   gcontext-plane-mask
   gcontext-plist
   gcontext-p
   gcontext-stipple
   gcontext-subwindow-mode
   gcontext-tile
   gcontext-ts-x
   gcontext-ts-y
   query-best-stipple
   query-best-tile
   copy-gcontext
   copy-gcontext-components
   free-gcontext
   force-gcontext-changes
   with-gcontext

   ;; GRAPHICOPS
   clear-area
   copy-area
   copy-plane
   draw-point
   draw-points
   draw-line
   draw-lines
   draw-segments
   draw-rectangle
   draw-rectangles
   draw-arc
   draw-arcs
   translate-default
   draw-glyph
   draw-glyphs
   draw-image-glyph
   draw-image-glyphs

   ;; IMAGE
   image
   image-blue-mask
   image-depth
   image-green-mask
   image-height
   image-name
   image-plist
   image-p
   image-red-mask
   image-x-hot
   image-y-hot
   image-xy-bitmap-list
   image-z-bits-per-pixel
   image-z-pixarray
   create-image
   copy-image
   get-image
   put-image
   read-bitmap-file
   write-bitmap-file
   get-raw-image
   put-raw-image

   ;; PIXMAP
   pixmap
   create-pixmap
   free-pixmap
   pixmap-display
   pixmap-equal
   pixmap-id
   pixmap-plist
   pixmap-p

   ;; SCREEN
   visual-info
   screen
   screen-backing-stores
   screen-black-pixel
   screen-default-colormap
   screen-depths
   visual-info-id
   visual-info-blue-mask
   visual-info-green-mask
   visual-info-red-mask
   visual-info-bits-per-rgb
   visual-info-colormap-entries
   visual-info-ptr
   find-visual-info
   x-find-visual-info
   screen-event-mask-at-open
   screen-height
   screen-height-in-millimeters
   screen-max-installed-maps
   screen-min-installed-maps
   screen-plist
   screen-p
   screen-root
   screen-root-depth
   screen-root-save-unders-p
   screen-white-pixel
   screen-width
   screen-width-in-millimeters

   ;; TYPES
   angle
   card8
   card16
   card29
   card32
   int8
   int16
   int32
   mask16
   mask32
   resource-id

   ;; WINDOW
   window
   create-window
   window-all-event-masks
   window-backing-pixel
   window-backing-planes
   window-backing-store
   window-bit-gravity
   window-class
   window-colormap
   window-colormap-installed-p
   window-display
   window-do-not-propagate-mask
   window-equal
   window-event-mask
   window-gravity
   window-id
   window-map-state
   window-override-redirect
   window-plist
   window-p
   set-window-priority
   window-save-under
   window-visual
   circulate-window-down
   circulate-window-up
   query-tree
   reparent-window
   translate-coordinates
   map-window
   map-subwindows
   unmap-window
   unmap-subwindows
   destroy-window
   destroy-subwindows

   ;; ICCCM

   wm-protocols
   wm-name

   ;; UNDOCUMENTED
   bitmap-image
   character->keysyms
   character-in-map-p
   decode-core-error
   default-keysym-index
   default-keysym-translate
   define-keysym-set
   display-invoke-after-function
   display-nscreens
   event-handler
   get-external-event-code
   get-standard-colormap
   get-wm-class
   icon-sizes
   iconify-window
   keysym->keycodes
   keysym-in-map-p
   keysym-set
   mapping-notify
   no-operation
   parse-color
   resource-database-timestamp
   resource-key
   rgb-colormaps
   root-resources
   rotate-cut-buffers
   set-access-control
   set-close-down-mode
   set-pointer-mapping
   set-standard-colormap
   set-standard-properties
   set-wm-class
   set-wm-properties
   set-wm-resources
   transient-for
   undefine-keysym
   window-cursor
   window-visual-info
   withdraw-window
   wm-client-machine
   wm-colormap-windows
   wm-command
   wm-hints
   wm-hints-flags
   wm-icon-name
   wm-normal-hints
   wm-resources
   wm-zoom-hints

   ;; Util
   stub
   stub-macro
   define-const-table
   define-enum-table
   add-hook
   run-hooks))

(defpackage :xcb.clx.ext.glx
  (:use #:cl #:cffi #:xcb #:xcb.clx)
  (:nicknames #:glx)
  (:export

   *unimplemented*
   *context*
   *drawable*

   create-context
   create-context-arb
   create-glx-window
   destroy-context
   destroy-glx-window
   make-current
   make-context-current
   choose-visual
   choose-fbconfig
   get-visual-from-fbconfig
   get-visualid-from-fbconfig
   swap-buffers
   wait-gl
   wait-x))
