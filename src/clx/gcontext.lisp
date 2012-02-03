(in-package :xcb.clx)

(defstruct (gcontext (:conc-name %gcontext-)
                     (:constructor %make-gcontext))
  (xcb-gcontext 0 :type (integer 0 4294967295)))

 ;; 5.2 Creating Graphics Contexts

(stub create-gcontext (&key arc-mode background (cache-p t) cap-style
                            clip-mask clip-ordering clip-x clip-y
                            dash-offset dashes drawable exposures
                            fill-rule fill-style font foreground function
                            join-style line-style line-width plane-mask
                            stipple subwindow-mode tile ts-x ts-y))

 ;; 5.3 Graphics Context Attributes

(stub gcontext-arc-mode (gcontext))
(stub (setf gcontext-arc-mode) (v gcontext))

(stub gcontext-background (gcontext))
(stub (setf gcontext-background) (v gcontext))

(stub gcontext-cache-p (gcontext))
(stub (setf gcontext-cache-p) (v gcontext))

(stub gcontext-cap-style (gcontext))
(stub (setf gcontext-cap-style) (v gcontext))

(stub gcontext-clip-mask (gcontext))
(stub set-gcontext-clip-mask (gcontext &optional ordering) (v))

(stub gcontext-clip-x (gcontext))
(stub (setf gcontext-clip-x) (v gcontext))

(stub gcontext-clip-y (gcontext))
(stub (setf gcontext-clip-y) (v gcontext))

(stub gcontext-dash-offset (gcontext))
(stub (setf gcontext-dash-offset) (v gcontext))

(stub gcontext-dashes (gcontext))
(stub (setf gcontext-dashes) (v gcontext))

(stub gcontext-display (gcontext))
(stub gcontext-equal (gcontext-1 gcontext-2))

(stub gcontext-exposures (gcontext))
(stub (setf gcontext-exposures) (v gcontext))

(stub gcontext-fill-rule (gcontext))
(stub (setf gcontext-fill-rule) (v gcontext))

(stub gcontext-fill-style (gcontext))
(stub (setf gcontext-fill-style) (v gcontext))

(stub gcontext-font (gcontext))
(stub set-gcontext-font (gcontext &optional metrics-p) (v))

(stub gcontext-foreground (gcontext))
(stub (setf gcontext-foreground) (v gcontext))

(stub gcontext-function (gcontext))
(stub (setf gcontext-function) (v gcontext))

(stub gcontext-id (gcontext)) ; **

(stub gcontext-join-style (gcontext))
(stub (setf gcontext-join-style) (v gcontext))

(stub gcontext-line-style (gcontext))
(stub (setf gcontext-line-style) (v gcontext))

(stub gcontext-line-width (gcontext))
(stub (setf gcontext-line-width) (v gcontext))

;; GCONTEXT-P is implicit in DEFSTRUCT GCONTEXT

(stub gcontext-plane-mask (gcontext))
;(stub (setf gcontext-plane-mask) (v gcontext))

(stub gcontext-plist (gcontext))
(stub (setf gcontext-plist) (v gcontext))

(stub gcontext-stipple (gcontext))
;(stub (setf gcontext-stipple) (v gcontext))

(stub gcontext-subwindow-mode (gcontext))
(stub (setf gcontext-subwindow-mode) (v gcontext))

(stub gcontext-tile (gcontext))
;(stub (setf gcontext-tile) (v gcontext))

(stub gcontext-ts-x (gcontext))
;(stub (setf gcontext-ts-x) (v gcontext))

(stub gcontext-ts-y (gcontext))
;(stub (setf gcontext-ts-y) (v gcontext))

(stub query-best-stipple (width height drawable))
(stub query-best-tile (width height drawable))

 ;; 5.3 Copying Graphics Contexts

(stub copy-gcontext (source destination))
(stub copy-gcontext-components (source destination &rest keys))

 ;; 5.5 Destroying Graphics Contexts

(stub free-gcontext (gcontext))

 ;; 5.6 Graphics Context Cache

(stub force-gcontext-changes (gcontext))

(stub-macro with-gcontext ((gcontext &key arc-mode background (cache-p
                                     t) cap-style clip-mask
                                     clip-ordering clip-x clip-y
                                     dash-offset dashes drawable
                                     exposures fill-rule fill-style
                                     font foreground function
                                     join-style line-style line-width
                                     plane-mask stipple subwindow-mode
                                     tile ts-x ts-y &allow-other-keys)
                           &body body))
