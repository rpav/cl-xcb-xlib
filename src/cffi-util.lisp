(in-package :xcb)

 ;; cffi-fsbv changes mem-aref semantics

(defun mem-pref (ptr type &optional (index 0))
  (inc-pointer ptr (* index (foreign-type-size type))))

(define-compiler-macro mem-pref (&whole whole ptr type &optional (index 0))
  (if (constantp type)
      (if (constantp index)
          `(inc-pointer ,ptr ,(* (eval index) (foreign-type-size (eval type))))
          `(inc-pointer ,ptr (* ,index ,(foreign-type-size (eval type)))))
      whole))

(export 'mem-pref)

 ;; memory

(defcfun ("memcpy" libc_memcpy) :pointer
  (dest :pointer)
  (src :pointer)
  (n :sizet))

(defcfun ("memset" libc_memset) :void
  (s :pointer)
  (c :int)
  (n :sizet))

(defcfun ("free" libc_free) :void
  (ptr :pointer))

(export '(libc_free libc_memcpy libc_memset))

(defmacro with-xcb-reply ((ptr err) form &body body)
  `(let ((,ptr (null-pointer)))
     (unwind-protect
          (with-foreign-object (,err :pointer)
            (setf ,ptr ,form)
            ,@body)
       (unless (null-pointer-p ,ptr)
         (libc_free ,ptr)))))

(export 'with-xcb-reply)

 ;; cstruct accessors

(defmacro make-cstruct-accessor (name)
  (let ((slots (cffi::slots (cffi::parse-type `(:struct ,name)))))
    (flet ((slot-name (slot)
             (concatenate 'string
                          (string name) "-" (string slot))))
      `(progn
         ,@(loop for k being each hash-key in slots
                 as v = (gethash k slots)
                 collecting
                 (let ((offset (foreign-slot-offset `(:struct ,name) k))
                       (slot-type (cffi::slot-type v))
                       (slot-count (if (typep v 'cffi::aggregate-struct-slot)
                                       (cffi::slot-count v)))
                       (slot-name (intern (slot-name k))))
                   `(progn
                      (declaim (inline ,slot-name (setf ,slot-name)))
                      (defun ,slot-name ,(if slot-count '(ptr &optional (n 0)) '(ptr))
                        ,(if (and (consp slot-type)
                                  (or (eq (car slot-type) :struct)
                                      (eq (car slot-type) :union)))
                             `(inc-pointer ptr ,(if slot-count
                                                    `(+ ,offset n)
                                                    offset))
                             `(mem-ref (inc-pointer ptr ,offset) ',slot-type
                                       ,(if slot-count 'n 0))))
                      (defun (setf ,slot-name) (v ptr)
                        (setf (mem-ref (inc-pointer ptr ,offset) ',slot-type) v))
                      (export ',slot-name))))))))

(defmacro make-cstruct-accessors (&rest types)
  `(progn
     ,@(loop for type in types collect
             `(make-cstruct-accessor ,type))))

(export '(make-cstruct-accessor make-cstruct-accessors))
