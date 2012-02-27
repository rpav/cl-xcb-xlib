(in-package :xcb.clx)

(stub bitmap-image (&optional plist &rest patterns))
(stub character->keysyms (character &optional display))
(stub character-in-map-p (display character keymap))
(stub decode-core-error (display event &optional arg))
(stub default-keysym-index (display keycode state))
(stub default-keysym-translate (display state object))
;; define-keysym -> control.lisp
(stub define-keysym-set (set first-keysym last-keysym))
(stub display-invoke-after-function (display))
(stub display-nscreens (display))
;; display-release-number -> display.lisp
(stub event-handler (handlers event-key))
(stub get-external-event-code (display event))
(stub get-standard-colormap (window property))
(stub get-wm-class (window))
(stub icon-sizes (window))
(stub iconify-window (window screen))
(stub keysym->keycodes (display keysym))
(stub keysym-in-map-p (display keysym keymap))
(stub keysym-set (keysym))
(stub mapping-notify (display request start count))
(stub no-operation (display))
(stub parse-color (colormap spec))
(stub resource-database-timestamp (database))
(stub resource-key (stringable))
(stub rgb-colormaps (window property))
(stub root-resources (screen &key database key test test-not))
(stub rotate-cut-buffers (display &optional (delta 1) (careful-p t)))
(stub set-access-control (display enabled-p))
(stub set-close-down-mode (display mode))
(stub set-pointer-mapping (display map))
;; set-selection-owner -> atoms?
(stub set-standard-colormap (window property colormap base-pixel max-color mult-color))
(stub set-standard-properties (window &rest options))
(stub set-wm-class (window resource-name resource-class))

(stub set-wm-properties (window &rest options
                         &key name icon-name resource-name resource-class
                           command client-machine hints normal-hints
                           zoom-hints (user-specified-position-p nil usppp)
                           (user-specified-size-p nil usspp)
                           (program-specified-position-p nil psppp)
                           (program-specified-size-p nil psspp)
                           x y width height min-width min-height
                           max-width max-height width-inc height-inc
                           min-aspect max-aspect base-width base-height
                           win-gravity input initial-state icon-pixmap
                           icon-window icon-x icon-y icon-mask window-group))

(stub set-wm-resources (database window &key write test test-not))
(stub transient-for (window))
;; translate-default -> graphicops.lisp
(stub undefine-keysym (object keysym &key display modifiers &allow-other-keys))
(stub visual-info-blue-mask (object))
(stub visual-info-green-mask (object))
(stub visual-info-red-mask (object))
(stub window-cursor (window))
(stub window-visual-info (window))
(stub withdraw-window (window screen))
(stub wm-client-machine (window))
(stub wm-colormap-windows (window))
(stub wm-command (window))
(stub wm-hints (window))
(stub wm-hints-flags (window))
(stub wm-icon-name (window))
(stub wm-name (window))
(stub wm-normal-hints (window))
(stub wm-protocols (window))
(stub wm-resources (window))
(stub wm-zoom-hints (window))
