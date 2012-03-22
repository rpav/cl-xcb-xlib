(in-package :xcb)

(define-foreign-library :libx11
  (t "libX11.so"))

(load-foreign-library :libx11)

 ;; Needs done ASAP

(defcfun ("XInitThreads" x-init-threads) :int)
(defvar *x-init-threads* (x-init-threads))

(define-foreign-library :libxcb
  (t "libxcb.so"))

(load-foreign-library :libxcb)

(define-foreign-library :libxcb-keysyms
  (t "libxcb-keysyms.so"))

(load-foreign-library :libxcb-keysyms)

(define-foreign-library :libx11-xcb
  (t "libX11-xcb.so"))

(load-foreign-library :libx11-xcb)
