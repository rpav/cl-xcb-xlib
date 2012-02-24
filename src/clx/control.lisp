(in-package :xcb.clx)

 ;; 14.1 Grabbing the Server

(stub grab-server (display))
(stub ungrab-server (display))
(stub-macro with-server-grabbed (display &body body))

 ;; 14.2 Pointer Control

(stub change-pointer-control (display &key acceleration threshold))
(stub pointer-control (display))
(stub pointer-mapping (display &key (result-type 'list)))

 ;; 14.3 Keyboard Control

(stub bell (display &optional (percent-from-normal 0)))
(stub change-keyboard-control (display
                               &key key-click-percent bell-percent
                                 bell-pitch bell-duration led led-mode
                                 key auto-repeat-mode))
(stub keyboard-control (display))
(stub modifier-mapping (display))
(stub query-keymap (display))
(stub set-modifier-mapping (display &key shift lock control
                                      mod1 mod2 mod3 mod4 mod5))

 ;; 14.4.2 Keyboard Mapping

(stub change-keyboard-mapping (display keysyms &key (start 0) end
                                                 (first-keycode start)))
(stub keyboard-mapping (display &key first-keycode start end data))

 ;; 14.4.3 Using Keycodes and Keysyms

(stub keycode->keysym (display keycode keysym-index))
(stub keysym->character (display keysym &optional (state 0)))

 ;; 14.5 Client Termination

(stub add-to-save-set (window))
(stub close-down-mode (display))
(stub kill-client (display resource-id))
(stub kill-temporary-clients (display))
(stub remove-from-save-set (window))

 ;; 14.6 Managing Host Access

(stub access-control (display))
(stub access-hosts (display &key (result-type 'list)))
(stub add-access-host (display host))
(stub remove-access-host (display host))

 ;; 14.7 Screen Saver

(stub activate-screen-saver (display))
(stub reset-screen-saver (display))
(stub screen-saver (display))
(stub set-screen-saver (display timeout period blanking exposures))

