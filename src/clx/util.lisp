(in-package :xcb.clx)

 ;; Implementation tools

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

 ;; Stupid simple queue, no locking

(declaim (inline make-queue))
(defstruct queue head tail)

(defun queue-add-cons (q cons)
  (when cons
    (if (queue-tail q)
        (progn
          (rplacd (queue-tail q) cons)
          (setf (queue-tail q) (cdr (queue-tail q))))
        (progn
          (setf (queue-head q) cons)
          (setf (queue-tail q) cons)))
    cons))

(defun queue-add (q item)
  (car (queue-add-cons q (cons item nil))))

(defun queue-pop-cons (q)
  (let ((cons (queue-head q)))
    (when cons
      (setf (queue-head q) (cdr cons))
      (rplacd cons nil)
      (unless (queue-head q)
        (setf (queue-tail q) nil))
      cons)))

(defun queue-pop (q)
  (car (queue-pop-cons q)))

(defun queue-has-item-p (q)
  (not (null (queue-head q))))

(defun queue-peek (q)
  "Peek at the head of the queue."
  (car (queue-head q)))

(defun queue-pop-to (q1 q2)
  "Pop from `Q1`, adding to `Q2`, without consing."
  (let ((cons (queue-pop-cons q1)))
    (queue-add-cons q2 cons)
    (car cons)))

(defun queue-prepend-to (q1 q2)
  "Prepend all items in `Q1` to `Q2`, removing them from `Q1`"
  (unless (or (null (queue-head q1)))
    (rplacd (queue-tail q1) (queue-head q2))

    (when (null (queue-tail q2))
      (setf (queue-tail q2) (queue-tail q1)))

    (setf (queue-head q2) (queue-head q1))
    (setf (queue-head q1) nil)
    (setf (queue-tail q1) nil))
  (values))
