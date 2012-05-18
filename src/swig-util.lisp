(in-package :xcb)

(defun custom-lispify (name flag &optional (package *package*))
  (cond
    ((eq flag 'structname) (list :struct (swig-lispify name flag package)))
    ((eq flag 'unionname) (list :union (swig-lispify name flag package)))
    ((eq flag 'structname-decl)
     `(,(swig-lispify name flag package)
       :class
       ,(alexandria:symbolicate
         (swig-lispify name flag package)
         '-tclass)))
    ((eq flag 'unionname-decl) (swig-lispify name flag package))
    (t (swig-lispify name flag package))))
  
