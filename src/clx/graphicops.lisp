(in-package :xcb.clx)

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

(stub draw-line (drawable gcontext x1 y1 x2 y2 &optional relative-p))
(stub draw-lines (drawable gcontext points
                  &key relative-p fill-p (shape :complex)))
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

