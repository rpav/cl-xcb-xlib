(in-package :xcb.clx)

;; CLX represents atoms as keywords.
(deftype xatom () '(or string symbol))

 ;; 11.1 Atoms

(stub atom-name (display atom-id))
(stub find-atom (display atom-name))
(stub intern-atom (display atom-name))

 ;; 11.2 Properties

(stub change-property (window property data type format
                       &key (mode :replace) (start 0) end transform))
(stub delete-property (window property))
(stub get-property (window property
                    &key type (start 0) end delete-p (result-type 'list)
                      transform))
(stub list-property (window &key (result-type 'list)))
(stub rotate-properties (window properties &optional (delta 1)))

 ;; 11.3 Selections

(stub convert-selection (selection type requestor &optional property time))
(stub selection-owner (display selection &optional time))


