(in-package :xcb.clx)

(defparameter *unimplemented* nil)

(defmacro stub (name &rest args)
  (declare (ignore args))
  (push name *unimplemented*)
  `(defun ,name (&rest args)
     (declare (ignore args))
     (error "~A is unimplemented" ',name)))

(defmacro stub-macro (name &rest args)
  (declare (ignore args))
  (push name *unimplemented*)
  `(defmacro ,name (&rest args)
     (declare (ignore args))
     (error "~A is unimplemented" ',name)))
