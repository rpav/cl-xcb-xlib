(in-package :xcb)

(define-foreign-library :libxcb
  (t "libxcb.so"))

(load-foreign-library :libxcb)

(define-foreign-library :libx11
  (t "libX11.so"))

(load-foreign-library :libx11)

(define-foreign-library :libx11-xcb
  (t "libX11-xcb.so"))

(load-foreign-library :libx11-xcb)

