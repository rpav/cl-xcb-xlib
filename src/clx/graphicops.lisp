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

(stub clear-area (window &key (x 0) (y 0) width height exposures-p))

(stub copy-area (source gcontext source-x source-y width height
                 destination dest-x dest-y))

(stub copy-plane (source gcontext plane source-x source-y width height
                  destination dest-x dest-y))

 ;; 6.3 Drawing Points

(stub draw-point (drawable gcontext x y))
(stub draw-points (drawable gcontext points &optional relative-p))

 ;; 6.4 Drawing Lines

(defun draw-line (drawable gcontext x1 y1 x2 y2 &optional relative-p)
  (let ((con (%display-xcb-connection (%drawable-display drawable)))
        (points (list x1 y1 x2 y2))
        (mode (coord-mode (if relative-p :previous :origin))))
    (with-points points (ptr count)
      (xcb-poly-line con mode (%drawable-id drawable)
                     (%gcontext-xcb-gcontext gcontext)
                     count ptr))))

(defun draw-lines (drawable gcontext points
                  &key relative-p fill-p (shape :complex))
  (let ((con (%display-xcb-connection (%drawable-display drawable)))
        (mode (coord-mode (if relative-p :previous :origin))))
    (with-points points (ptr count)
      (if fill-p
          (xcb-fill-poly con (%drawable-id drawable)
                         (%gcontext-xcb-gcontext gcontext)
                         (poly-shape shape) mode count ptr)
          (xcb-poly-line con mode (%drawable-id drawable)
                         (%gcontext-xcb-gcontext gcontext)
                         count ptr)))))

(stub draw-segments (drawable gcontext segments))

 ;; 6.5 Drawing Rectangles

(stub draw-rectangle (drawable gcontext x y width height &optional fill-p))
(stub draw-rectangles (drawable gcontext rectangles &optional fill-p))

 ;; 6.6 Drawing Arcs

(stub draw-arc (drawable gcontext x y width height angle1 angle2
                &optional fill-p))
(stub draw-arcs (drawable gcontext arcs &optional fill-p))

 ;; 6.7 Drawing Text

(stub translate-function (source source-start source-end font
                          destination destination-start))

(stub draw-glyph (drawable gcontext x y element
                  &key translate width (size :default)))

(stub draw-glyphs (drawable gcontext x y sequence
                   &key (start 0) end translate width (size :default)))

(stub draw-image-glyph (drawable gcontext x y element
                        &key translate width (size :default)))

(stub draw-image-glyphs (drawable gcontext x y sequence
                         &key (start 0) end translate width (size :default)))

