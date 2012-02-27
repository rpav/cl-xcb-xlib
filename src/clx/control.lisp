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

(defparameter *keysym-map* (make-hash-table))
(defparameter *keysym-reverse-map* (make-hash-table))
(defparameter *keysym-alias-map* (make-hash-table))

;; This is not really meant to be compatible with CLX
(defun define-keysym (object value &key &allow-other-keys)
  (if (gethash value *keysym-map*)
      (setf (gethash object *keysym-alias-map*)
            (gethash value *keysym-map*))
      (setf (gethash value *keysym-map*) object))
  (setf (gethash object *keysym-reverse-map*) value))

(defun keycode->keysym (display keycode keysym-index)
  (xcb-key-symbols-get-keysym (%display-key-symbols display)
                              keycode keysym-index))

;; FIXME: probably not entirely CLX-compatible
(defun keysym->character (display keysym &optional (state 0))
  (declare (ignore display state))
  (let ((value (keysym->value keysym)))
    (if (characterp value) value nil)))

(defun keysym->value (keysym)
  (cond
    ((or (<= #x20 keysym #x7E)
         (<= #xA0 keysym #xFF))
     (code-char keysym))
    ((<= #x01000100 keysym #x0110FFFF)
     (code-char (- keysym #x01000000)))
    (t (gethash keysym *keysym-map*))))

(defun canonical-keysym (object)
  (gethash (gethash object *keysym-reverse-map*)
           *keysym-map*))

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

