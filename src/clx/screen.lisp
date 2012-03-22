(in-package :xcb.clx)

(defstruct (screen (:conc-name %screen-)
                   (:constructor %make-screen))
  (display nil :type display)
  (number 0 :type fixnum)
  (xcb-screen (null-pointer) :type #.(type-of (null-pointer)))
  (depths nil :type list)
  (plist nil :type list))

(defmethod print-object ((object screen) stream)
  (print-unreadable-object (object stream)
    (format stream "Screen ~A" (%screen-number object))))

(defmethod display-for ((object screen))
  (%screen-display object))

 ;; Visual

(define-enum-table visual-class (xcb-visual-class-t "XCB-VISUAL-CLASS")
  :static-gray :gray-scale :static-color :pseudo-color :true-color :direct-color)

(defstruct visual-info
  (id 0 :type card29)
  (class nil :type (member :direct-color :gray-scale :pseudo-color :static-color
                           :static-gray :true-color))
  (red-mask 0 :type pixel)
  (blue-mask 0 :type pixel)
  (green-mask 0 :type pixel)
  (bits-per-rgb 0 :type card8)
  (colormap-entries 0 :type card16)
  (ptr (null-pointer) :type #.(type-of (null-pointer))))

 ;; 3.2 Screen Attributes

(defun screen-backing-stores (screen)
  (xcb-screen-t-backing-stores (%screen-xcb-screen screen)))

(defun screen-black-pixel (screen)
  (xcb-screen-t-black-pixel (%screen-xcb-screen screen)))

(defun screen-default-colormap (screen)
  (%make-colormap :id
                  (xcb-screen-t-default-colormap (%screen-xcb-screen screen))))

(defun screen-depths (screen)
  (or (%screen-depths screen)
      (setf (%screen-depths screen)
            (mapcar
             (lambda (ptr0)
               (cons (xcb-depth-t-depth ptr0)
                     (mapcar (lambda (ptr)
                               (make-visual-info :id (xcb-visualtype-t-visual-id ptr)
                                                 :class (visual-class-key (xcb-visualtype-t--class ptr))
                                                 :bits-per-rgb (xcb-visualtype-t-bits-per-rgb-value ptr)
                                                 :red-mask (xcb-visualtype-t-red-mask ptr)
                                                 :green-mask (xcb-visualtype-t-green-mask ptr)
                                                 :blue-mask (xcb-visualtype-t-blue-mask ptr)
                                                 :colormap-entries (xcb-visualtype-t-colormap-entries ptr)
                                                 :ptr ptr))
                             (xcb-depth-visuals-iterator ptr0))))
             (xcb-screen-allowed-depths-iterator (%screen-xcb-screen screen))))))

;; fixme: efficiency? hash on id might be nice
(defun find-visual-info (screen visual-id)
  (let ((depths (screen-depths screen)))
    (loop for depth in depths do
      (loop for visual in (cdr depth) do
            (when (= visual-id (visual-info-id visual))
              (return-from find-visual-info visual))))))

(defun x-find-visual-info (xid visual-id)
  (let ((display (display-for xid)))
   (loop for screen in (display-roots display) do
     (let ((visual (find-visual-info screen visual-id)))
       (when (and visual (= visual-id (visual-info-id visual)))
         (return-from x-find-visual-info visual))))))

(defun screen-event-mask-at-open (screen)
  (xcb-screen-t-current-input-masks (%screen-xcb-screen screen)))

(defun screen-height (screen)
  (xcb-screen-t-height-in-pixels (%screen-xcb-screen screen)))

(defun screen-height-in-millimeters (screen)
  (xcb-screen-t-height-in-millimeters (%screen-xcb-screen screen)))

(defun screen-max-installed-maps (screen)
  (xcb-screen-t-max-installed-maps (%screen-xcb-screen screen)))

(defun screen-min-installed-maps (screen)
  (xcb-screen-t-min-installed-maps (%screen-xcb-screen screen)))

;; SCREEN-P is implicit in DEFSTRUCT SCREEN

(defun screen-plist (screen)
  (%screen-plist screen))

(defun (setf screen-plist) (v screen)
  (setf (%screen-plist screen) v))

(defun screen-root (screen)
  (%make-window
   :display (%screen-display screen)
   :id (xcb-screen-t-root (%screen-xcb-screen screen))))
  
(defun screen-root-depth (screen)
  (xcb-screen-t-root-depth (%screen-xcb-screen screen)))

(defun screen-root-save-unders-p (screen)
  (xcb-screen-t-save-unders (%screen-xcb-screen screen)))

(defun screen-white-pixel (screen)
  (xcb-screen-t-white-pixel (%screen-xcb-screen screen)))

(defun screen-width (screen)
  (xcb-screen-t-width-in-pixels (%screen-xcb-screen screen)))

(defun screen-width-in-millimeters (screen)
  (xcb-screen-t-width-in-millimeters (%screen-xcb-screen screen)))
