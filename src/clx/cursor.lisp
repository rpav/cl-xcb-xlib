(in-package :xcb.clx)

(defstruct (cursor (:include display-id-pair)
                   (:conc-name %cursor-)
                   (:constructor %make-cursor)))

 ;; 10.2 Creating Cursors

(defun create-cursor (&key source mask x y foreground background)
  (let* ((id (xcb-generate-id (display-ptr-xcb source)))
         (cursor (%make-cursor :display (display-for source)
                               :id id)))
    (xerr source
        (xcb-create-cursor-checked (display-ptr-xcb source)
                                   id (xid source) (xid mask)
                                   (color-red-int foreground)
                                   (color-green-int foreground)
                                   (color-blue-int foreground)
                                   (color-red-int background)
                                   (color-green-int background)
                                   (color-blue-int background)
                                   x y))
    cursor))

;; FIXME: when fonts exist
(stub create-glyph-cursor (&key source-font source-char mask-font
                             (mask-char 0) foreground background))

(defun free-cursor (cursor)
  (xerr cursor
      (xcb-free-cursor-checked (display-ptr-xcb cursor)
                               (xid cursor))))

 ;; 10.3 Cursor Functions

(defun query-best-cursor (width height drawable)
  (query-best-size width height drawable :largest-cursor))

(defun recolor-cursor (cursor foreground background)
  (xerr cursor
      (xcb-recolor-cursor-checked (display-ptr-xcb cursor)
                                  (xid cursor)
                                  (color-red-int foreground)
                                  (color-green-int foreground)
                                  (color-blue-int foreground)
                                  (color-red-int background)
                                  (color-green-int background)
                                  (color-blue-int background))))

 ;; 10.4 Cursor Attributes

(defun cursor-display (cursor)
  (%cursor-display cursor))

(defun cursor-equal (c-1 c-2)
  (xid-equal c-1 c-2))

(defun cursor-id (cursor)
  (%cursor-id cursor))

;; CURSOR-P

(stub cursor-plist (cursor))
(stub (setf cursor-plist) (v cursor))
