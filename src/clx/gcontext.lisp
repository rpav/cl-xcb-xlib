(in-package :xcb.clx)

(defstruct (gcontext (:conc-name %gcontext-)
                     (:constructor %make-gcontext)
                     (:copier %copy-gcontext))
  (display nil :type display)
  (xcb-gcontext 0 :type (integer 0 4294967295))
  (clip-ordering 0 :type (integer 0 3)))

(defmethod display-for ((object gcontext))
  (%gcontext-display object))

 ;; 5.2 Creating Graphics Contexts

(define-enum-table gc-attr (xcb-gc-t "XCB-GC")
  :function :plane-mask :foreground :background :line-style
  :line-width :cap-style :join-style :fill-style :fill-rule
  :tile :stipple (:ts-x :tile-stipple-origin-x)
  (:ts-y :tile-stipple-origin-y) :font :subwindow-mode
  (:exposures :graphics-exposures) (:clip-x :clip-origin-x)
  (:clip-y :clip-origin-y) :clip-mask :dash-offset
  (:dashes :dash-list) :arc-mode)

(defconstant +max-gc-attr-to-xcb+ (length *gc-attr-map*))

(define-enum-table clip-ordering
    (xcb-clip-ordering-t "XCB-CLIP-ORDERING")
  :unsorted :y-sorted :yx-sorted :yx-banded)

(define-enum-table gc-func (xcb-gx-t "XCB-GX")
  (boole-clr :clear) (boole-and :and) (boole-andc2 :and-reverse)
  (boole-1 :copy)  (boole-andc1 :and-inverted) (boole-2 :noop)
  (boole-xor :xor) (boole-ior :or) (boole-nor :nor) (boole-eqv :equiv)
  (boole-c2 :invert) (boole-orc2 :or-reverse) (boole-c1 :copy-inverted)
  (boole-orc1 :or-inverted) (boole-nand :nand) (boole-set :set))

(defun create-gcontext (&key arc-mode background (cache-p t) cap-style
                          clip-mask clip-ordering clip-x clip-y
                          dash-offset dashes drawable exposures
                          fill-rule fill-style font foreground function
                          join-style line-style line-width plane-mask
                          stipple subwindow-mode tile ts-x ts-y)
  ;; FIXME, implement caching, clip-ordering
  (declare (ignore cache-p clip-ordering))
  (let* ((dpy (drawable-display drawable))
         (id (xcb-generate-id (display-ptr-xcb dpy)))
         (gcon (%make-gcontext :display dpy :xcb-gcontext id))
         (function (gc-func function))
         (value-mask 0)
         (value-count 0))
    (with-foreign-object (values-ptr 'uint-32-t +max-gc-attr-to-xcb+)
      ;; Remember, order specified is critical:
      (vl-maybe-set-many (gc-attr values-ptr value-mask value-count)
        function plane-mask foreground background line-style line-width
        cap-style join-style fill-style fill-rule tile stipple
        ts-x ts-y font subwindow-mode exposures clip-x clip-y
        clip-mask dash-offset dashes arc-mode)
      (xcb-create-gc (display-ptr-xcb dpy) id
                     (xid drawable) value-mask values-ptr)
      gcon)))

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
(stub set-gcontext-clip-mask (gc v &optional ordering))

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

(defun free-gcontext (gcontext)
  (xcb-free-gc (display-ptr-xcb gcontext)
               (%gcontext-xcb-gcontext gcontext)))

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
