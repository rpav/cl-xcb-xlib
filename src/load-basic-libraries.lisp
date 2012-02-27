(in-package :xcb)

(define-foreign-library :libxcb
  (t "libxcb.so"))

(load-foreign-library :libxcb)

(define-foreign-library :libxcb-keysyms
  (t "libxcb-keysyms.so"))

(load-foreign-library :libxcb-keysyms)

(define-foreign-library :libx11-xcb
  (t "libX11-xcb.so"))

(load-foreign-library :libx11-xcb)
