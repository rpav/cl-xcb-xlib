(in-package :xcb.clx)

(defstruct (image (:conc-name %image-)
                  (:constructor %make-image)))

 ;; 7.2.1 Basic Images

(stub image-blue-mask (image))
(stub (setf image-blue-mask) (v image))

(stub image-depth (image))

(stub image-green-mask (image))
(stub (setf image-green-mask) (v image))

(stub image-height (image))

(stub image-name (image))
(stub (setf image-name) (v image))

(stub image-plist (image))
(stub (setf image-plist) (v image))

(stub image-red-mask (image))
(stub (setf image-red-mask) (v image))

(stub image-x-hot (image))
(stub (setf image-x-hot) (v image))

(stub image-y-hot (image))
(stub (setf image-y-hot) (v image))

 ;; 7.2.2 XY-Format Images

(stub image-xy-bitmap-list (image))
(stub (setf image-xy-bitmap-list) (v image))

 ;; 7.2.3 Z-Format Images

(stub image-z-bits-per-pixel (image))
(stub (setf image-z-bits-per-pixel) (v image))

(stub image-z-pixarray (image))
(stub (setf image-z-pixarray) (v image))

 ;; 7.3 Image Functions

(stub create-image (&key bit-lsb-first-p bits-per-pixel blue-mask
                      byte-lsb-first-p bytes-per-line data depth
                      format green-mask height name plist red-mask
                      width x-hot y-hot))

(stub copy-image (image &key (x 0) (y 0) width height result-type))

(stub get-image (drawable &key x y width height plane-mask (format :z-format)
                            result-type))

(stub put-image (drawable gcontext image
                 &key (src-x 0) (src-y 0) x y width height bitmap-p))

 ;; 7.4 Image Files

(stub read-bitmap-file (pathname))
(stub write-bitmap-file (pathname))

 ;; 7.5 Direct Image Transfer

(stub get-raw-image (drawable &key data (start 0) x y width height plane-mask
                                (format :z-format) (result-type '(vector card8))))

(stub put-raw-image (drawable gcontext data
                     &key (start 0) depth x y width height (left-pad 0) format))

