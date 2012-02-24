(in-package :xcb.clx)

 ;; Utility
(defmacro with-points (point-list (ptr count) &body body)
  (let ((points (gensym "POINTS"))
        (point-ptr (gensym "PTR"))
        (x (gensym "X"))
        (y (gensym "Y"))
        (i (gensym "I")))
  `(let* ((,points ,point-list)
          (,count (truncate (length ,points) 2)))
     (with-foreign-object (,ptr 'xcb-point-t ,count)
       (loop for ,i from 0 below ,count
             for ,x in ,points by #'cddr
             for ,y in (cdr ,points) by #'cddr
             for ,point-ptr = (mem-pref ,ptr 'xcb-point-t ,i)
             do (setf (xcb-point-t-x ,point-ptr) ,x)
                (setf (xcb-point-t-y ,point-ptr) ,y))
       ,@body))))

 ;; Tables

(define-enum-table poly-shape (xcb-poly-shape-t "XCB-POLY-SHAPE")
  :complex (:non-convex :nonconvex) :convex)

(define-enum-table coord-mode (xcb-coord-mode-t "XCB-COORD-MODE")
  :origin :previous)

 ;; 6.2 Area and Plane Operations

(defun clear-area (window &key (x 0) (y 0) width height exposures-p)
  (xchk (window c)
      (xcb-clear-area-checked c (if exposures-p 1 0) (xid window)
                              x y (or width 0) (or height 0))))

(defun copy-area (source gcontext source-x source-y width height
                  destination dest-x dest-y)
  (xchk (source c)
      (xcb-copy-area-checked c (xid source) (xid destination) (xid gcontext)
                             source-x source-y dest-x dest-y width height)))

(defun copy-plane (source gcontext plane source-x source-y width height
                   destination dest-x dest-y)
  (xchk (source c)
      (xcb-copy-plane-checked c (xid source) (xid destination) (xid gcontext)
                              source-x source-y dest-x dest-y width height
                              plane)))

 ;; 6.3 Drawing Points

(defun draw-point (drawable gcontext x y)
  (draw-points drawable gcontext (list x y)))

(defun draw-points (drawable gcontext points &optional relative-p)
  (with-points points (ptr count)
    (xchk (drawable c)
        (xcb-poly-point-checked c (coord-mode (if relative-p :previous :origin))
                                (xid drawable) (xid gcontext) count ptr))))

 ;; 6.4 Drawing Lines

(defun draw-line (drawable gcontext x1 y1 x2 y2 &optional relative-p)
  (let ((con (display-ptr-xcb drawable))
        (points (list x1 y1 x2 y2))
        (mode (coord-mode (if relative-p :previous :origin))))
    (with-points points (ptr count)
      (xerr (display-for drawable)
          (xcb-poly-line-checked con mode (xid drawable)
                                 (xid gcontext) count ptr)))))

(defun draw-lines (drawable gcontext points
                   &key relative-p fill-p (shape :complex))
  (let ((con (display-ptr-xcb drawable))
        (mode (coord-mode (if relative-p :previous :origin))))
    (with-points points (ptr count)
      (if fill-p
          (xerr (display-for drawable)
              (xcb-fill-poly-checked con (xid drawable)
                             (xid gcontext) (poly-shape shape)
                             mode count ptr))
          (xerr (display-for drawable)
              (xcb-poly-line-checked con mode (xid drawable)
                             (xid gcontext) count ptr))))))

(defun draw-segments (drawable gcontext segments)
  (unless (= 0 (mod (length segments) 4))
    (error "Incomplete segment specified"))
  ;; If you think this is bad, it is
  (with-points segments (ptr count)
    (xchk (drawable c)
        (xcb-poly-segment-checked c (xid drawable) (xid gcontext)
                                  (truncate count 2) ptr))))

 ;; 6.5 Drawing Rectangles

(defun draw-rectangle (drawable gcontext x y width height &optional fill-p)
  (draw-rectangles drawable gcontext (list x y width height) fill-p))

(defun draw-rectangles (drawable gcontext rectangles &optional fill-p)
  (unless (= 0 (mod (length rectangles) 4))
    (error "Incomplete rectangle specified"))
  (with-points rectangles (ptr count)
    (xchk (drawable c)
        (if fill-p
            (xcb-poly-fill-rectangle-checked c (xid drawable) (xid gcontext)
                                             (truncate count 2) ptr)
            (xcb-poly-rectangle-checked c (xid drawable) (xid gcontext)
                                        (truncate count 2) ptr)))))

 ;; 6.6 Drawing Arcs

(defun draw-arc (drawable gcontext x y width height angle1 angle2
                 &optional fill-p)
  (draw-arcs drawable gcontext (list x y width height angle1 angle2) fill-p))

(defun draw-arcs (drawable gcontext arcs &optional fill-p)
  (unless (= 0 (mod (length arcs) 6))
    (error "Incomplete arc specified"))
  (with-points arcs (ptr count)
    (xchk (drawable c)
        (if fill-p
            (xcb-poly-fill-arc-checked c (xid drawable) (xid gcontext)
                                       (truncate count 3) ptr)
            (xcb-poly-arc-checked c (xid drawable) (xid gcontext)
                                  (truncate count 3) ptr)))))

 ;; 6.7 Drawing Text

;; Wow who came up with the CLX spec here. Horrible.
(defun translate-default (source source-start source-end font
                          destination destination-start)
  (declare (ignore font))
  (loop for i from 0 below (- source-end source-start) do
        (setf (aref destination (+ i destination-start))
              (char-code (aref source (+ i source-start)))))
  (values source-end nil nil))

(defmacro with-translated-text ((ptr len text size) &body body)
  (let ((ary (gensym)) (foreign-type (gensym)))
   `(let* ((,ary ,text)
           (,len (length ,ary))
           (,foreign-type (if (eq ,size 16) 'xcb-char-2b-t :uint8)))
      (with-foreign-object (,ptr ,foreign-type ,len)
        (copy-array-to-foreign ,ptr ,len ,ary ,foreign-type
                               (lambda (ptr el)
                                 (if (eq ,size 16)
                                     (progn
                                       (setf (xcb-char-2b-t-byte-1 ptr)
                                             (ash el -8))
                                       (setf (xcb-char-2b-t-byte-2 ptr)
                                             (logand #xF el)))
                                     (setf (mem-ref ptr :uint8) el))))
        ,@body))))

(defmacro do-translation (text font function
                          (src-start src-end dest-start)
                          (output first-not-done to-continue current-width)
                          &body body)
  (let ((str (gensym)))
   `(let* ((,str ,text)
           (,output (make-array (length ,str) :element-type '(unsigned-byte 16))))
      (multiple-value-bind (,first-not-done ,to-continue ,current-width)
          (funcall (or ,function #'translate-default)
                   ,str ,src-start ,src-end ,font ,output ,dest-start)
        ,@body
        (values ,first-not-done ,to-continue ,current-width)))))

(defmacro do-translations (text font function
                           (output first-not-done to-continue current-width
                            &optional start end)
                           &body body)
  (let ((src-start (gensym)) (src-end (gensym)) (dest-start (gensym)))
    `(let ((,src-start (or ,start 0))
           (,src-end (or ,end (length ,text)))
           (,dest-start 0))
       (loop do (multiple-value-bind (,first-not-done ,to-continue ,current-width)
                    (do-translation ,text ,font ,function
                        (,src-start ,src-end ,dest-start)
                        (,output ,first-not-done ,to-continue ,current-width)
                      ,@body)
                  (incf ,dest-start (- ,first-not-done ,src-start))
                  (setf ,src-start ,first-not-done)
                  (unless ,to-continue
                    (loop-finish)))))))

(defun draw-glyph (drawable gcontext x y element
                   &key translate width (size :default))
  (draw-glyphs drawable gcontext x y (list element)
               :translate translate :width width :size size))

;; Apparently xcb_poly_text_* are broken in XCB.  This is too!
(defun draw-glyphs (drawable gcontext x y sequence
                    &key (start 0) end translate width (size :default))
  (draw-image-glyphs drawable gcontext x y sequence
                     :start start :end end :translate translate
                     :width width :size size))

(defun draw-image-glyph (drawable gcontext x y element
                         &key translate width (size :default))
  (draw-image-glyphs drawable gcontext x y (list element)
                     :translate translate :width width :size size))

(defun draw-image-glyphs (drawable gcontext x y sequence
                          &key (start 0) end translate width (size :default))
  (declare (ignore width))
  (do-translation sequence (gcontext-font gcontext) translate
      (start (or end (length sequence)) 0)
      (output fnd cont curwidth)
    (with-translated-text (ptr len output size)
      (xchk (drawable c)
          (xcb-image-text-8-checked c len (xid drawable) (xid gcontext) x y ptr)))
    (values fnd curwidth)))
