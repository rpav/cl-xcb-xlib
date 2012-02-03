(in-package :xcb.clx)

(defmacro stub (name &rest args)
  (declare (ignore args))
  `(defun ,name (&rest args)
     (declare (ignore args))
     (error "~A is unimplemented" ',name)))

(defmacro stub-macro (name &rest args)
  (declare (ignore args))
  `(defmacro ,name (&rest args)
     (declare (ignore args))
     (error "~A is unimplemented" ',name)))
