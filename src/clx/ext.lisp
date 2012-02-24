(in-package :xcb.clx)

 ;; 15.1 Extensions

(defun list-extensions (display &key (result-type 'list))
  (do-request-response (display c ck reply err)
      (xcb-list-extensions c)
      (xcb-list-extensions-reply c ck err)
    (map result-type
         #'xcb-str-to-lisp
         (xcb-list-extensions-names-iterator reply))))

(defun query-extension (display name)
  (with-foreign-string ((ptr len) name)
   (do-request-response (display c ck reply err)
       (xcb-query-extension c (1- len) ptr)
       (xcb-query-extension-reply c ck err)
     (values (xcb-query-extension-reply-t-major-opcode reply)
             (xcb-query-extension-reply-t-first-event reply)
             (xcb-query-extension-reply-t-first-error reply)))))

