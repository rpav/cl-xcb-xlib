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

 ;; Building value_lists

(defmacro vl-maybe-set (attr list values-ptr value-mask value-count)
  `(when ,attr
     (let ((row (assoc ,(intern (string attr) :keyword)
                       ,list)))
       (unless row (error "Bad VALUE-LIST attribute for ~A: ~A"
                          ',list ',attr))
       (setf ,value-mask
             (logior ,value-mask (cdr row)))
       (setf (mem-aref ,values-ptr 'uint-32-t ,value-count) ,attr)
       (incf ,value-count))))

(defmacro vl-maybe-set-many ((list ptr mask count) &rest attrs)
  `(progn
     ,@(loop for a in attrs collect
             `(vl-maybe-set ,a ,list ,ptr ,mask ,count))))

 ;; Generalized enum tables

(defmacro define-enum-table (name (xcb-type xcb-prefix) &rest entries)
  (flet ((make-entry (name &optional xcb-name)
           (let ((kw (intern (format nil "+~A-~A+" xcb-prefix
                                     (or xcb-name name))
                             :keyword)))
             (cons name (foreign-enum-value xcb-type kw)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,name
         '(,@(loop for i in entries
                   collecting (if (consp i)
                                  (make-entry (car i) (cadr i))
                                  (make-entry i))))))))

(defmacro define-const-table (name &rest entries)
  (flet ((make-entry (name &optional xcb-name)
           (let ((sym (intern (format nil "+XCB-~A+" (or xcb-name name)) :xcb)))
             (cons name (symbol-value sym)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defparameter ,name
         '(,@(loop for i in entries
                   collecting (if (consp i)
                     (make-entry (car i) (cadr i))
                     (make-entry i))))))))

