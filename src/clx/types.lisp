(in-package :xcb.clx)

(deftype angle ()
  `(number ,(* -2 PI) ,(* 2 PI)))

(deftype card8 () '(unsigned-byte 8))
(deftype card16 () '(unsigned-byte 16))
(deftype card29 () '(unsigned-byte 29))
(deftype card32 () '(unsigned-byte 32))

(deftype int8 () '(signed-byte 8))
(deftype int16 () '(signed-byte 16))
(deftype int32 () '(signed-byte 32))

(deftype mask16 () 'card16)
(deftype mask32 () 'card32)

(deftype resource-id () 'card29)

(deftype pixel-data-size () '(member 1 4 8 16 24 32))
(deftype pixel () '(unsigned-byte 32))
(deftype pixarray () '(or
                       (array pixel (* *))
                       (array card16 (* *))
                       (array card8 (* *))
                       (array (unsigned-byte 4) (* *))
                       (array bit (* *))))

