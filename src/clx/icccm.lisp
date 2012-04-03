(in-package :xlib)

(defmacro define-wm-accessor (name atom type format transform-to transform-from
                              &optional (result-type ''list))
  `(progn
     (defun ,name (window)
       (let ((display (display-for window)))
         (declare (ignorable display))
         (get-property window ,atom
                       :type ,type :end 1024
                       :transform ,transform-to
                       :result-type ,result-type)))

     (defun (setf ,name) (value window)
       (let ((display (display-for window)))
         (declare (ignorable display))
         (change-property window ,atom value ,type ,format
                          :transform ,transform-from)))))

(define-wm-accessor wm-protocols :wm_protocols :atom 32
    (lambda (a) (atom-name display a))
    (lambda (a) (find-atom display a)))

(define-wm-accessor wm-name :wm_name :string 8 #'code-char #'char-code 'string)
