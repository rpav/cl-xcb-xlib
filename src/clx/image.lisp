(in-package :xcb.clx)

(defstruct (image (:conc-name image-)
                  (:constructor %make-image)
                  (:copier %copy-image))
  (name nil :type (or null string))
  (x-hot 0 :type int16)
  (y-hot 0 :type int16)
  (red-mask 0 :type integer)
  (green-mask 0 :type integer)
  (blue-mask 0 :type integer)
  (bit-lsb-first-p t :type boolean)
  (byte-lsb-first-p t :type boolean)
  (height 0 :type card16)
  (width 0 :type card16)
  (depth 0 :type card8)
  (bpp 0 :type card8)
  (stride 0 :type card16)
  (data nil)
  (plist nil :type list))

(defstruct (image-xy (:include image)
                     (:conc-name image-xy-)
                     (:constructor %make-image-xy)
                     (:copier %copy-image-xy)))

(defstruct (image-z (:include image)
                    (:conc-name image-z-)
                    (:constructor %make-image-z)
                    (:copier %copy-image-z)))

(defun setup-image-finalization (image data)
  (tg:cancel-finalization image)
  (tg:finalize image (lambda () (static-vectors:free-static-vector data))))

(defun %image-stride (width bpp alignment)
  "Calculate the image stride for WIDTH pixels at BPP bits-per-pixel padding to ALIGNMENT"
  (* (1+ (i/ (1- (* bpp width)) alignment)) alignment))

;; This is wrong, but CLX is wrong
(defun %image-alignment (image)
  (find (image-stride image) '(32 16 8)
        :test (lambda (x y) (= 0 (mod x y)))))

 ;; 7.2.1 Basic Images

;; implicit

 ;; 7.2.2 XY-Format Images

;; mostly implicit

(defun image-xy-bitmap-list (image)
  (let ((len-1 (* (image-stride image) (image-height image))))
   (loop for i from 0 below (image-depth image)
         collect (make-array (list (image-height image)
                                   (image-stride image))
                             :element-type 'bit
                             :displaced-to (image-data image)
                             :displaced-index-offset (* i len-1)))))

(defun (setf image-xy-bitmap-list) (v image)
  (let ((static (list-to-static v (image-bpp image))))
    (static-vectors:free-static-vector (image-data image))
    (setf (image-data image) static)
    (setup-image-finalization image static)
    v))

 ;; 7.2.3 Z-Format Images

;; mostly implicit

(defun image-z-bits-per-pixel (image)
  (image-depth image))

(defun (setf image-z-bits-per-pixel) (v image)
  (setf (image-depth image) v))

(defun image-z-pixarray (image)
  (make-array (list (image-height image)
                    (image-width image))
              :element-type (array-element-type (image-data image))
              :displaced-to (image-data image)))

(defun (setf image-z-pixarray) (v image)
  (let ((static (copy-to-static v (image-bpp image))))
    (static-vectors:free-static-vector (image-data image))
    (setf (image-data image) static)
    (setup-image-finalization image static)
    v))

 ;; 7.3 Image Functions

(define-enum-table image-format (xcb-image-format-t "XCB-IMAGE-FORMAT")
  :xy-bitmap :xy-pixmap :z-pixmap)

(defun copy-to-static (array bpp)
  (let* ((len (reduce #'* (array-dimensions array)))
         (type `(unsigned-byte ,bpp))
         (flat (make-array len :displaced-to array)))
    (declare (dynamic-extent flat))
    (static-vectors:make-static-vector len
                                       :element-type type
                                       :initial-contents flat)))

(defun list-to-static (list bpp)
  (let* ((len-1 (reduce #'* (array-dimensions (car list))))
         (len (* len-1 bpp))
         (static (static-vectors:make-static-vector len :element-type 'bit
                                                    :initial-element 0)))
    (loop for array in list
          for i from 0 do
      (replace static array :start1 (* i len-1) :end1 (* (1+ i) len-1)))
    static))

(defun create-image (&key bit-lsb-first-p (bits-per-pixel 0)
                       (blue-mask 0) byte-lsb-first-p bytes-per-line
                       data (depth 0) format (green-mask 0) (height 0)
                       name plist (red-mask 0) (width 0) (x-hot 0) (y-hot 0))
  (declare (ignore format))
  (let* ((image-type (etypecase data
                       (list 'image-xy)
                       (pixarray 'image-z)
                       (array 'image)))
         (make-image-fun (ecase image-type
                           (image-xy #'%make-image-xy)
                           (image-z #'%make-image-z)
                           (image #'%make-image)))
         (depth (or depth
                    (and (eq image-type 'image-xy) (length data))
                    1))
         (bpp (or bits-per-pixel depth))
         (static-data
           (if (listp data)
               (list-to-static data bpp)
               (copy-to-static data bpp)))
         (image (funcall make-image-fun
                         :name name :x-hot x-hot :y-hot y-hot
                         :red-mask red-mask
                         :green-mask green-mask
                         :blue-mask blue-mask
                         :bit-lsb-first-p bit-lsb-first-p
                         :byte-lsb-first-p byte-lsb-first-p
                         :stride (or bytes-per-line
                                     (* width bits-per-pixel))
                         :height height
                         :width width
                         :depth depth
                         :bpp bpp
                         :data static-data
                         :plist plist)))
    (setup-image-finalization image static-data)
    image))

(defun copy-image-simple-p (image x y w h result-type)
  (and (= x 0) (= y 0)
       (or (null w) (= w (image-width image)))
       (or (null h) (= h (image-height image)))
       (or (null result-type)
           (eq result-type (type-of image)))))

(defun copy-image-simple (image)
  (let ((copy (%copy-image image))
        (data (image-data image)))
    (setf (image-data copy)
          (static-vectors:make-static-vector (length data)
                                             :element-type (array-element-type data)
                                             :initial-contents data))
    (setup-image-finalization image (image-data image))
    copy))

(defun copy-image-crop (image x y w h)
  (let* ((copy (%copy-image image))
         (data (image-data image))
         (alignment (%image-alignment image))
         (stride (%image-stride w (if (image-xy-p image) 1 (image-bpp image)) alignment))
         (new-len (if (image-xy-p image)
                      (* stride h (image-bpp image))
                      (* h (i/ stride (image-bpp image))))))
    (setf (image-width copy) w)
    (setf (image-height copy) h)
    (setf (image-stride copy) stride)
    (setf (image-data copy) (static-vectors:make-static-vector new-len
                                                               :element-type (array-element-type data)
                                                               :initial-element 0))
    (ecase (type-of image)
      (image-xy (copy-image-xy-crop copy image x y w h))
      (image-z (copy-image-z-crop copy image x y w h)))
    (setup-image-finalization image (image-data image))
    copy))

(defun copy-image-xy-crop (copy image x y w h)
  (let* ((dest (image-data copy))
         (src (image-data image))
         (dest-stride (image-stride copy))
         (src-stride (image-stride image))
         (dest-plane-len (* dest-stride h))
         (src-plane-len (* src-stride (image-height image))))
    (loop for plane from 0 below (image-depth image) do
      (loop for y1 from 0 below h
            as src-pos = (+ x (* plane src-plane-len) (* (+ y y1) src-stride))
            as dest-pos = (+ (* plane dest-plane-len) (* y1 dest-stride)) do
              (replace dest src
                       :start2 src-pos :end2 (+ src-pos w)
                       :start1 dest-pos :end1 (+ dest-pos w))))))

(defun copy-image-z-crop (copy image x y w h)
  (let* ((dest (image-data copy))
         (src (image-data image))
         (dest-stride (i/ (image-stride copy) (image-bpp copy)))
         (src-stride (i/ (image-stride image) (image-bpp image))))
    (loop for y1 from 0 below h
          as src-pos = (+ x (* (+ y y1) src-stride))
          as dest-pos = (+ (* y1 dest-stride)) do
            (replace dest src
                     :start2 src-pos :end2 (+ src-pos w)
                     :start1 dest-pos :end1 (+ dest-pos w)))))

(defun %image-pixel (image x y)
  (etypecase image
    (image-z (aref (image-data image)
                   (+ x (* y (i/ (image-stride image)
                                 (image-bpp image))))))
    (image-xy
     (let ((pixel 0)
           (len-1 (* (image-stride image) (image-height image))))
       (loop for i from 0 below (image-depth image)
             as plane = (- (image-depth image) i 1)
             as bit = (aref (image-data image)
                            (+ x
                               (* y (image-stride image))
                               (* i len-1)))
             do
                (setf pixel (logior pixel (ash bit plane))))
       pixel))))

(defun %set-image-pixel (image x y pixel)
  (etypecase image
    (image-z
     (setf (aref (image-data image)
                 (+ x (* y (i/ (image-stride image)
                               (image-bpp image)))))
           pixel))
    (image-xy
     (let ((len-1 (* (image-stride image) (image-height image))))
       (loop for i from 0 below (image-depth image)
             as plane = (- (image-depth image) i 1) do
               (setf (aref (image-data image)
                           (+ x
                              (* y (image-stride image))
                              (* i len-1)))
                     (ldb (byte 1 plane) pixel))))))
  pixel)

(defun copy-image-slowly (image x y w h result-type)
  (let* ((make-image-fun (ecase result-type
                           (image-xy #'%make-image-xy)
                           (image-z #'%make-image-z)
                           (t #'%make-image)))
         (w (or w (image-width image)))
         (h (or h (image-height image)))
         (alignment (%image-alignment image))
         (stride (if (eq result-type 'image-xy)
                     (+ (1- w) (- alignment (mod (1- w) alignment)))
                     (+ (* (1- w) (image-bpp image))
                        (- alignment (mod (* (1- w) (image-bpp image)) alignment)))))
         (copy (funcall make-image-fun
                        :red-mask (image-red-mask image)
                        :green-mask (image-green-mask image)
                        :blue-mask (image-blue-mask image)
                        :bit-lsb-first-p (image-bit-lsb-first-p image)
                        :byte-lsb-first-p (image-byte-lsb-first-p image)
                        :height h :width w
                        :bpp (image-bpp image)
                        :depth (image-depth image)
                        :stride stride
                        :data (if (eq 'image-xy result-type)
                                  (static-vectors:make-static-vector (* h stride (image-depth image))
                                                                     :element-type 'bit
                                                                     :initial-element 0)
                                  (static-vectors:make-static-vector (* h (i/ stride (image-bpp image)))
                                                                     :element-type `(unsigned-byte ,(image-bpp image))
                                                                     :initial-element 0)))))
    (setup-image-finalization copy (image-data copy))
    (loop for cur-y from 0 below h do
      (loop for cur-x from 0 below w do
        (%set-image-pixel copy cur-x cur-y
                          (%image-pixel image (+ cur-x x) (+ cur-y y)))))
    copy))

(defun copy-image (image &key (x 0) (y 0) width height result-type)
  (cond
    ((copy-image-simple-p image x y width height result-type)
     (copy-image-simple image))
    ((or (null result-type) (eq result-type (type-of image)))
     (copy-image-crop image x y width height))
    (t (copy-image-slowly image x y width height result-type))))

(defun %get-image-data (reply element-type depth bpp)
  (let* ((byte-count (xcb-get-image-data-length reply))
         (len (cond
                ((eq element-type 'card8) byte-count) ;; massive hack :(
                ((< depth 8) (* byte-count (i/ 8 depth)))
                (t (i/ byte-count (i/ bpp 8)))))
         (static (static-vectors:make-static-vector len :element-type element-type)))
    (xcb:libc_memcpy (static-vectors:static-vector-pointer static)
                     (xcb-get-image-data reply)
                     byte-count)
    static))

(defun %get-image (drawable format x y width height plane-mask &optional element-type)
  (do-request-response (drawable c reply err)
      (xcb-get-image c (image-format format) (xid drawable) x y
                     width height plane-mask)
    (let* ((display (display-for drawable))
           (visual (x-find-visual-info display (xcb-get-image-reply-t-visual reply)))
           (pixmap-format (find-pixmap-format display :depth (xcb-get-image-reply-t-depth reply)))
           (bpp (pixmap-format-bits-per-pixel pixmap-format))
           (type (or element-type
                     (ecase format
                       (:z-pixmap (list 'unsigned-byte bpp))
                       (:xy-pixmap 'bit))))
           (depth (ecase format
                    (:z-pixmap (xcb-get-image-reply-t-depth reply))
                    (:xy-pixmap 1))))
      (values
       (%get-image-data reply type depth bpp)
       visual pixmap-format))))

(defun get-image (drawable &key x y width height (plane-mask #xFFFFFFFF) (format :z-pixmap)
                             result-type)
  (multiple-value-bind (static-data visual pixmap-format)
      (%get-image drawable format x y width height plane-mask)
   (let* ((display (display-for drawable))
          (bpp (pixmap-format-bits-per-pixel pixmap-format))
          (stride-bpp (ecase format
                        (:z-pixmap bpp)
                        (:xy-pixmap 1)))
          (constructor (ecase format
                         (:z-pixmap '%make-image-z)
                         (:xy-pixmap '%make-image-xy)))
          (image (funcall constructor
                          :red-mask (visual-info-red-mask visual)
                          :green-mask (visual-info-green-mask visual)
                          :blue-mask (visual-info-blue-mask visual)
                          :bit-lsb-first-p (eq :lsbfirst (display-image-lsb-first-p display))
                          :byte-lsb-first-p (eq :lsbfirst (display-byte-order display))
                          :height height :width width
                          :bpp bpp
                          :depth (pixmap-format-depth pixmap-format)
                          :stride (%image-stride width stride-bpp (pixmap-format-scanline-pad pixmap-format))
                          :data static-data)))
     (setup-image-finalization image (image-data image))
     (if (typep image result-type)
         image
         (copy-image image :result-type result-type)))))

(defun %put-image (drawable gcontext format width height left-pad x y
                   depth static-data)
  (xchk (drawable c)
      (xcb-put-image-checked
       c (image-format format) (xid drawable)
       (xid gcontext) width height x y left-pad
       depth
       (length static-data)
       (static-vectors:static-vector-pointer static-data))))

(defun put-image (drawable gcontext image
                  &key (src-x 0) (src-y 0) x y width height bitmap-p)
  (declare (ignore bitmap-p))
  (let ((processed-image (if (or width height)
                             (copy-image image
                                         :x src-x :y src-y
                                         :width (or width (image-width image))
                                         :height (or height (image-height image)))
                             image)))
    (%put-image drawable gcontext
                (image-format processed-image)
                (image-width processed-image)
                (image-height processed-image)
                0 ;; left-pad
                x y
                (image-depth processed-image)
                (image-data processed-image))))

 ;; 7.4 Image Files

(stub read-bitmap-file (pathname))
(stub write-bitmap-file (pathname))

 ;; 7.5 Direct Image Transfer

(defun get-raw-image (drawable &key data (start 0) x y width height (plane-mask #xFFFFFFFF)
                                 (format :z-format) (result-type '(vector card8)))
  (multiple-value-bind (static-data visual pixmap-format)
      (%get-image drawable format x y width height plane-mask 'card8)
    (declare (ignore visual pixmap-format))
    (unwind-protect
         (if data
             (replace data static-data :start1 start)
             (map result-type #'identity static-data))
      (static-vectors:free-static-vector static-data))))

(defun put-raw-image (drawable gcontext data
                      &key (start 0) depth x y width height (left-pad 0) format)
  (static-vectors:with-static-vector (static (- (length data) start)
                                             :element-type (array-element-type data))
    (replace static data :start2 start)
    (%put-image drawable gcontext format width height left-pad
                x y depth static)))
