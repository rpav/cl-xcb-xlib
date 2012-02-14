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
   color
   color-rgb

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

   ;; DISPLAY
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
   display-protocol-major-version
   display-protocol-minor-version
   display-protocol-version
   display-resource-id-base
   display-resource-id-mask
   display-roots
   display-vendor-name
   display-release-number
   display-vendor
   display-version-number
   display-xid
   display-after-function
   display-force-output
   display-finish-output
   close-display
   with-display

   display-funcall
   do-on-display

   ;; DRAWABLE
   drawable
   xid
   drawable-display
   drawable-equal
   drawable-plist
   drawable-border-width
   drawable-depth
   drawable-height
   drawable-width
   drawable-x
   drawable-y
   drawable-root

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
   translate-function
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

   ;; SCREEN
   screen
   screen-backing-stores
   screen-black-pixel
   screen-default-colormap
   screen-depths
   screen-event-mask-at-open
   screen-height
   screen-height-in-millimeters
   screen-max-installed-maps
   screen-min-installed-maps
   screen-plist
   screen-root
   screen-root-depth
   screen-root-save-unders-p
   screen-white-pixel
   screen-width
   screen-width-in-millimeters

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

   ;; Util
   stub
   stub-macro
   define-const-table
   define-enum-table))

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
