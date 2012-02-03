(in-package :xcb.clx)

(defstruct (cursor (:conc-name %cursor-)
                     (:constructor %make-cursor))
  (id 0 :type (integer 0 4294967295)))

 ;; 10.2 Creating Cursors

(stub create-cursor (&key source mask x y foreground background))
(stub create-glyph-cursor (&key source-font source-char mask-font
                             (mask-char 0) foreground background))
(stub free-cursor (cursor))

 ;; 10.3 Cursor Functions

(stub query-best-cursor (width height display))
(stub recolor-cursor (cursor foreground background))

 ;; 10.4 Cursor Attributes

(stub cursor-display (cursor))
(stub cursor-equal (c-1 c-2))
(stub cursor-id (cursor))

;; CURSOR-P

(stub cursor-plist (cursor))
(stub (setf cursor-plist) (v cursor))


