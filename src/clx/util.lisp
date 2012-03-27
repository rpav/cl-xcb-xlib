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

(defun queue-push (q item)
  (setf (queue-head q)
        (cons item (queue-head q)))
  (unless (queue-tail q)
    (setf (queue-tail q) (queue-head q)))
  item)

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

(defmacro vl-maybe-set (attr type values-ptr value-mask value-count)
  (let ((val (gensym "VAL")))
    `(when ,attr
       (let ((,val (,type ,(intern (string attr) :keyword))))
         (unless ,val (error "Bad VALUE-LIST attribute for ~A: ~A"
                             ',type ',attr))
         (setf ,value-mask (logior ,value-mask ,val))
         (setf (mem-aref ,values-ptr 'uint-32-t ,value-count) ,attr)
         (incf ,value-count)))))

(defmacro vl-maybe-set-many ((type ptr mask count) &rest attrs)
  (let ((attrs (if (consp (car attrs)) (car attrs) attrs)))
    `(progn ,@(loop for a in attrs collect
                    `(vl-maybe-set ,a ,type ,ptr ,mask ,count)))))

 ;; Extracting result lists

;;; FIXME: this calls mem-[a]ref with a non-constant type, which probably
;;; calls PARSE-TYPE.
(defun map-result-list (result-type function head-function length-function
                        reply-pointer type)
  (map result-type
       function
       (loop with ptr = (funcall head-function reply-pointer)
             for i from 0 below (funcall length-function reply-pointer)
             collect (mem-aref ptr type i))))

 ;; Copy to foreign array

(defun copy-to-foreign (ptr len list type &optional function)
  (loop for i from 0 below len
        for el in list do
          (if function
              (funcall function (mem-pref ptr type i) el)
              (setf (mem-aref ptr type i) el))))

(defun copy-array-to-foreign (ptr len ary type &optional function)
  (let ((size (foreign-type-size type)))
   (loop for i from 0 below len
         for el across ary do
           (if function
               (funcall function (inc-pointer ptr (* size i)) el)
               (setf (mem-aref ptr type i) el)))))

 ;; Hash tables

(defun merge-hash-tables (h1 &rest tables)
  (loop for table in tables do
    (loop for k being each hash-key in table do
          (setf (gethash k h1) (gethash k table)))))

 ;; Generalized enum tables

(defmacro define-enum-table (name (xcb-type xcb-prefix) &rest entries)
  (flet ((make-entry (name &optional xcb-name)
           (let ((kw (intern (format nil "+~A-~A+" xcb-prefix
                                     (or xcb-name name))
                             :keyword)))
             (cons name (foreign-enum-value xcb-type kw)))))
    (let ((earmuff-name (intern (format nil "*~A-MAP*" name)))
          (key-name (intern (format nil "~A-KEY" name)))
          (ior-name (intern (format nil "~A-LOGIOR" name))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defparameter ,earmuff-name
           '(,@(loop for i in entries
                   collecting (if (consp i)
                                  (make-entry (car i) (cadr i))
                                  (make-entry i)))))
         (defun ,name (key)
           (cdr (assoc key ,earmuff-name)))
         (defun ,key-name (val)
           (car (rassoc val ,earmuff-name)))
         (defun ,ior-name (&rest keys)
           (reduce (lambda (v1 v2) (if v2 (logior v1 (,name v2)) v1))
                   keys :initial-value 0))))))

(defmacro define-const-table (name (prefix) &rest entries)
  (flet ((make-entry (name &optional xcb-name)
           (let ((sym (intern (format nil "+~A-~A+" prefix
                                      (or xcb-name name)))))
             (cons name (symbol-value sym)))))
    (let ((earmuff-name (intern (format nil "*~A-MAP*" name)))
          (key-name (intern (format nil "~A-KEY" name)))
          (ior-name (intern (format nil "~A-LOGIOR" name))))
      `(eval-when (:compile-toplevel :load-toplevel :execute) 
         (defparameter ,earmuff-name
           '(,@(loop for i in entries
                     collecting (if (consp i)
                                    (make-entry (car i) (cadr i))
                                    (make-entry i)))))
         (defun ,name (key)
           (cdr (assoc key ,earmuff-name)))
         (defun ,key-name (val)
           (car (rassoc val ,earmuff-name)))
         (defun ,ior-name (&rest keys)
           (reduce (lambda (v1 v2) (logior v1 (,name v2)))
                   keys :initial-value 0))))))

 ;; trivial hooks

(defmacro add-hook (place hook)
  `(pushnew ,hook ,place))

(defmacro run-hooks (place &rest args)
  (let ((hook (gensym "HOOK")))
    `(loop for ,hook in ,place do
      (funcall ,hook ,@args))))

 ;; etc

;; Brevity
(declaim (inline i/))
(defun i/ (number &optional (divisor 1))
  (truncate number divisor))
