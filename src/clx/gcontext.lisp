(in-package :xcb.clx)

(defstruct (gcontext (:include display-id-pair)
                     (:conc-name %gcontext-)
                     (:constructor %make-gcontext)
                     (:copier %copy-gcontext))
  (current (make-hash-table) :type hash-table)
  (updates (make-hash-table) :type hash-table))

(defun %gcontext-set (gc &rest attrs)
  (let ((attrs (if (consp (car attrs)) (car attrs) attrs)))
      (loop for a in attrs by #'cddr
            for v in (cdr attrs) by #'cddr do
              (setf (gethash a (%gcontext-current gc)) v))))

(defun %gcontext-update (gc &rest attrs)
  (loop for a in attrs by #'cddr
        for v in (cdr attrs) by #'cddr do
          (if (equal (gethash a (%gcontext-current gc)) v)
              (remhash a (%gcontext-updates gc))
              (setf (gethash a (%gcontext-updates gc)) v))))

;; We always cache.  The docs don't actually make it clear that there
;; is any difference _without_ cache-p set.  Regardless, we are
;; apparently unable to query values, so we must cache.
(defun gcontext-cache-p (gcontext) (declare (ignore gcontext)) t)
(defun (setf gcontext-cache-p) (v gcontext) (declare (ignore v gcontext)) t)

 ;; 5.2 Creating Graphics Contexts

(eval-when (:compile-toplevel :load-toplevel :execute)
 (define-enum-table gc-attr (xcb-gc-t "XCB-GC")
   :function :plane-mask :foreground :background :line-style
   :line-width :cap-style :join-style :fill-style :fill-rule
   :tile :stipple (:ts-x :tile-stipple-origin-x)
   (:ts-y :tile-stipple-origin-y) :font :subwindow-mode
   (:exposures :graphics-exposures) (:clip-x :clip-origin-x)
   (:clip-y :clip-origin-y) :clip-mask :dash-offset
   (:dashes :dash-list) :arc-mode) ;; *gc-attr-map*

 (defvar *basic-gc-attrs*
   `(,@(loop for a in *gc-attr-map* collect (intern (string (car a)))))))

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

(define-enum-table gc-join-style (xcb-join-style-t "XCB-JOIN-STYLE")
  :miter :round :bevel)

(define-enum-table gc-cap-style (xcb-cap-style-t "XCB-CAP-STYLE")
  :not-last :butt :round :projecting)

(define-enum-table gc-line-style (xcb-line-style-t "XCB-LINE-STYLE")
  :solid :on-off-dash :double-dash)

(define-enum-table gc-fill-style (xcb-fill-style-t "XCB-FILL-STYLE")
  :solid :tiled :stippled :opaque-stippled)

(define-enum-table gc-fill-rule (xcb-fill-rule-t "XCB-FILL-RULE")
  :even-odd :winding)

(define-enum-table gc-arc-mode (xcb-arc-mode-t "XCB-ARC-MODE")
  :chord :pie-slice)

(define-enum-table gc-subwindow-mode (xcb-subwindow-mode-t "XCB-SUBWINDOW-MODE")
  :clip-by-children :include-inferiors)

(define-enum-table gc-exposures (xcb-exposures-t "XCB-EXPOSURES")
  (:off :not-allowed) (:on :allowed) :default)

(defun create-gcontext (&key (arc-mode :pie-slice)
                          (background 1) (cache-p t) (cap-style :butt)
                          (clip-mask :none) (clip-ordering :unsorted)
                          (clip-x 0) (clip-y 0) (dash-offset 0) (dashes 4)
                          drawable (exposures :on)
                          (fill-rule :even-odd) (fill-style :solid) font
                          (foreground 0) (function 'boole-1)
                          (join-style :miter) (line-style :solid)
                          (line-width 0) plane-mask
                          stipple (subwindow-mode :clip-by-children) tile
                          (ts-x 0) (ts-y 0))
  (declare (ignore cache-p))
  (let* ((dpy (drawable-display drawable))
         (id (xcb-generate-id (display-ptr-xcb dpy)))
         (gcon (%make-gcontext :display dpy :id id))
         (function (gc-func function))
         (cap-style (gc-cap-style cap-style))
         (join-style (gc-join-style join-style))
         (fill-style (gc-fill-style fill-style))
         (fill-rule (gc-fill-rule fill-rule))
         (subwindow-mode (gc-subwindow-mode subwindow-mode))
         (clip-mask (if (or (null clip-mask) (eq clip-mask :none))
                        0 (xid clip-mask)))
         (exposures (gc-exposures exposures))
         (line-style (gc-line-style line-style))
         (arc-mode (gc-arc-mode arc-mode))
         (value-mask 0)
         (value-count 0))
    (macrolet ((set-attrs (list) `(%gcontext-set gcon ,@list)))
     (with-foreign-object (values-ptr 'uint-32-t +max-gc-attr-to-xcb+)
       (vl-maybe-set-many (gc-attr values-ptr value-mask value-count)
         #.*basic-gc-attrs*)
       (set-attrs #.(loop for a in *basic-gc-attrs* collect `',a collect a))
       (%gcontext-set gcon 'clip-ordering (clip-ordering clip-ordering))
       (xerr dpy
           (xcb-create-gc-checked (display-ptr-xcb dpy) id
                                  (xid drawable) value-mask values-ptr))
       gcon))))



 ;; 5.3 Graphics Context Attributes

(defmacro define-gc-accessor (name &key (setter-p t) in out)
  (let ((acc-name (intern (format nil "GCONTEXT-~A" name))))
   `(progn
      (defun ,acc-name (gcontext)
        (let ((val
               (or (gethash ',name (%gcontext-updates gcontext))
                   (gethash ',name (%gcontext-current gcontext)))))
          ,(if out
               `(funcall ,out val)
               'val)))
      ,(when setter-p
         `(defun (setf ,acc-name) (val gcontext)
            (%gcontext-update gcontext ',name
                              ,(if in
                                   `(funcall ,in val)
                                   'val)))))))

(define-gc-accessor arc-mode)
(define-gc-accessor background)
(define-gc-accessor cap-style)
(define-gc-accessor clip-mask :setter-p nil)
(define-gc-accessor clip-ordering :setter-p nil)

(defun set-gcontext-clip-mask (gc v &optional ordering)
  (%gcontext-update gc 'clip-mask v)
  (when ordering
    (%gcontext-update gc 'clip-ordering ordering)))

(defsetf gcontext-clip-mask (gc &optional ordering) (v)
    `(set-gcontext-clip-mask ,gc ,v ,ordering))

(define-gc-accessor clip-x)
(define-gc-accessor clip-y)
(define-gc-accessor dash-offset)
(define-gc-accessor dashes)

(defun gcontext-display (gcontext)
  (%gcontext-display gcontext))

(defun gcontext-equal (gcontext-1 gcontext-2)
  (xid-equal gcontext-1 gcontext-2))

(define-gc-accessor exposures)
(define-gc-accessor fill-rule
    :in #'gc-fill-rule :out #'gc-fill-rule-key)
(define-gc-accessor fill-style)

;; FIXME: when fonts exist
(stub gcontext-font (gcontext))
(stub set-gcontext-font (gcontext &optional metrics-p) (v))

(define-gc-accessor foreground)
(define-gc-accessor function
    :in #'gc-func :out #'gc-func-key)

(defun gcontext-id (gcontext)
  (xid gcontext))

(define-gc-accessor join-style
    :in #'gc-join-style :out #'gc-join-style-key)

(define-gc-accessor line-style
    :in #'gc-line-style :out #'gc-line-style-key)

(define-gc-accessor line-width)

;; GCONTEXT-P is implicit in DEFSTRUCT GCONTEXT

(define-gc-accessor plane-mask :setter-p nil)

;; FIXME: plists
(stub gcontext-plist (gcontext))
(stub (setf gcontext-plist) (v gcontext))

(define-gc-accessor stipple :setter-p nil)
(define-gc-accessor subwindow-mode
    :in #'gc-subwindow-mode :out #'gc-subwindow-mode-key)
(define-gc-accessor tile :setter-p nil)
(define-gc-accessor ts-x :setter-p nil)
(define-gc-accessor ts-y :setter-p nil)

(defun query-best-stipple (width height drawable)
  (query-best-size width height drawable :fastest-stipple))

(defun query-best-tile (width height drawable)
  (query-best-size width height drawable :fastest-tile))

 ;; 5.3 Copying Graphics Contexts

(defun copy-gcontext (source destination)
  (xerr source
      (xcb-copy-gc-checked (display-ptr-xcb source)
                           (xid source) (xid destination)
                           #xFFFFFFFF)))

(defun copy-gcontext-components (source destination &rest keys)
  (xerr source
      (xcb-copy-gc-checked (display-ptr-xcb source)
                           (xid source) (xid destination)
                           (apply #'gc-attr-logior keys))))

 ;; 5.5 Destroying Graphics Contexts

(defun free-gcontext (gcontext)
  (xerr gcontext
      (xcb-free-gc-checked (display-ptr-xcb gcontext)
                           (xid gcontext))))

 ;; 5.6 Graphics Context Cache

;; and the biggest hairball of all
(defun force-gcontext-changes (gcontext)
  (unless (= 0 (hash-table-count (%gcontext-updates gcontext)))
    (hash-let ((%gcontext-updates gcontext) #.*basic-gc-attrs*)
      (let* ((c (display-ptr-xcb gcontext))
             (dashes-detailed (if (consp dashes) dashes nil))
             (dashes (if (consp dashes) nil dashes))
             (clip-mask-rects (if (consp clip-mask) clip-mask nil))
             (clip-mask (if (consp clip-mask) nil clip-mask))
             (value-mask 0)
             (value-count 0))
        (with-foreign-object (values-ptr 'uint-32-t +max-gc-attr-to-xcb+)
          (vl-maybe-set-many (gc-attr values-ptr value-mask value-count)
            #.*basic-gc-attrs*)
          ;; Avoid sending this if it will be duplicated below
          (unless (or (and dashes-detailed (= value-mask (gc-attr :dash-offset)))
                      (and clip-mask-rects
                           (= 0 (logandc1 value-mask (gc-attr-logior :clip-x :clip-y)))))
            (xerr gcontext
                (xcb-change-gc-checked c (xid gcontext)
                                       value-mask values-ptr))))
        (when dashes-detailed
          (let ((len (length dashes-detailed)))
            (with-foreign-object (ptr :uint8 len)
              (copy-to-foreign ptr len dashes-detailed :uint8)
              (xerr gcontext
                  (xcb-set-dashes-checked c (xid gcontext)
                                          (gcontext-dash-offset gcontext)
                                          len ptr)))))
      
        (when clip-mask-rects
          (let ((len (length clip-mask-rects)))
            ;; yes, i really did this, but then CLX defined rects
            (when (/= 8 #.(foreign-type-size 'xcb-rectangle-t))
              (warn "Size of xcb_rectangle_t has changed"))
            (with-foreign-object (ptr :uint16 (* 4 len))
              (copy-to-foreign ptr (* 4 len) clip-mask-rects :uint16)
              (xerr gcontext
                  (xcb-set-clip-rectangles-checked c (gcontext-clip-ordering gcontext)
                                                   (xid gcontext)
                                                   (gcontext-clip-x gcontext)
                                                   (gcontext-clip-y gcontext) len ptr)))))
        (merge-hash-tables (%gcontext-current gcontext)
                           (%gcontext-updates gcontext))
        (clrhash (%gcontext-updates gcontext))))))

(defmacro with-gcontext ((gcontext &rest gcontext-initializers)
                         &body body)
  `(let ((,gcontext (create-gcontext ,@gcontext-initializers)))
     (unwind-protect
          (progn ,@body)
       (free-gcontext ,gcontext))))

