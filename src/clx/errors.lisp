(in-package :xcb.clx)

 ;; Error definitions

(defgeneric initialize-x-error (error &key dpy ptr seq))

(define-condition x-error (error) ())

(define-condition request-error (x-error)
  (display error-key major minor sequence current-sequence)
  (:report (lambda (c s)
             (with-slots (display error-key major minor sequence current-sequence)
                 c
               (format s "~A (~A) on ~A: (major: ~A minor: ~A seq: ~A current: ~A)"
                       (type-of c) error-key display major minor sequence current-sequence)))))

(defmethod initialize-x-error ((e request-error) &key dpy ptr seq)
  (with-slots (display error-key major minor sequence current-sequence) e
    (setf display dpy)
    (setf error-key (xcb-request-error-t-error-code ptr))
    (setf major (xcb-request-error-t-major-opcode ptr))
    (setf minor (xcb-request-error-t-minor-opcode ptr))
    (setf sequence (xcb-request-error-t-sequence ptr))
    (setf current-sequence seq)))

(define-condition resource-error (request-error) ())
(define-condition access-error (request-error) ())
(define-condition alloc-error (request-error) ())
(define-condition atom-error (request-error) ())
(define-condition closed-display (x-error) ())
(define-condition colormap-error (resource-error) ())
(define-condition connection-failure (x-error) ())
(define-condition cursor-error (resource-error) ())
(define-condition device-busy (x-error) ())
(define-condition drawable-error (resource-error) ())
(define-condition font-error (resource-error) ())
(define-condition gcontext-error (resource-error) ())
(define-condition id-choice-error (resource-error) ())
(define-condition implementation-error (resource-error) ())
(define-condition length-error (resource-error) ())
(define-condition lookup-error (x-error) ())
(define-condition match-error (request-error) ())
(define-condition missing-parameter (x-error) ())
(define-condition name-error (request-error) ())
(define-condition pixmap-error (resource-error) ())

(define-condition reply-length-error (x-error)
  (reply-length expected-length display))

(define-condition reply-timeout (x-error) ())

(define-condition sequence-error (x-error)
  (display req-sequence msg-sequence))

(define-condition server-disconnect (x-error) ())
(define-condition unexpected-reply (x-error) ())

(define-condition unknown-error (request-error)
  (error-code))

(define-condition value-error (request-error) ())
(define-condition window-error (resource-error) ())

 ;; XCB codes

(defmacro define-error-table (name (prefix) &body codes)
  `(define-const-table ,name (,prefix)
     ,@(mapcar (lambda (c) (list (intern (format nil "~A-ERROR" c)) c))
               codes)))

(define-error-table error-code ("XCB")
  :request :value :window :pixmap :atom :cursor :font :match
  :drawable :access :alloc :colormap :g-context :id-choice
  :name :length :implementation) ;; *error-code-map*

 ;; XCB -> CLX

(defun make-x-error (seq dpy errptr)
  (unless (= 0 (xcb-generic-error-t-error-code errptr))
    (let* ((type (error-code-key (xcb-generic-error-t-error-code errptr)))
           (cond (make-condition type)))
      (initialize-x-error cond :seq seq :dpy dpy :ptr errptr)
      cond)))

 ;; Error checking

(defmacro xerr (display-or-linked form)
  "Check for an error from *no-reply* calls *only*; i.e., -CHECKED
variant.  For calls expecting a reply, use WITH-XCB-CLX-REPLY."
  (let ((cookie (gensym "COOKIE"))
        (err (gensym "ERR"))
        (cond (gensym "COND"))
        (dpy (gensym "DPY")))
    `(let* ((,cookie ,form)
            (,dpy (display-for ,display-or-linked))
            (,err (xcb-request-check (display-ptr-xcb ,dpy) ,cookie)))
       (unless (null-pointer-p ,err)
         (let ((,cond (make-x-error (cadr ,cookie) ,dpy ,err)))
           (xcb::libc_free ,err)
           (when ,cond (error ,cond)))))))

(defmacro with-xcb-clx-reply ((dpy cookie ptr err) form &body body)
  `(with-xcb-reply (,ptr ,err) ,form
     (unless (null-pointer-p ,err)
       (let ((err (make-x-error (cadr ,cookie) (display-for ,dpy) ,err)))
         (when err (error err))))
     ,@body))
