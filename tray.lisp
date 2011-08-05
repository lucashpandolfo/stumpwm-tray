(require 'clx)

(defparameter *display* nil)
(defparameter *window* nil)
(defparameter *client-window* nil)
(defparameter *root-window* nil)
(defparameter *client-windows* nil)
(defparameter *tray-icon-width* 32)
(defparameter *tray-icon-height* 32)

(defconstant +xembed-protocol-version+ 1)

(defconstant +xembed-embedded-notify+          0)
(defconstant +xembed-window-activate+          1)
(defconstant +xembed-window-deactivate+        2)
(defconstant +xembed-request-focus+            3)
(defconstant +xembed-focus-in+                 4)
(defconstant +xembed-focus-out+                5)
(defconstant +xembed-focus-next+               6)
(defconstant +xembed-focus-prev+               7)
;; 8-9 were used for xembed-grab-key/xembed-ungrab-key
(defconstant +xembed-modality-on+              10)
(defconstant +xembed-modality-off+             11)
(defconstant +xembed-register-accelerator+     12)
(defconstant +xembed-unregister-accelerator+   13)
(defconstant +xembed-activate-accelerator+     14)

;; details for  xembed-focus-in: 
(defconstant +xembed-focus-current+            0)
(defconstant +xembed-focus-first+              1)
(defconstant +xembed-focus-last+               2)

(defmacro make-reconfigure-mask (&rest keywords)
  (reduce #'logior 
	  (mapcar (lambda (x) (case x
				(:x 1)
				(:y 2)
				(:width 4)
				(:height 8)
				(:border-width 16)
				(:sibling 32)
				(:stack-mode 64)
				(t (error "~a is not a valid keyword in the reconfigure mask" x))))
		  keywords)))


(defun tray (&optional (host ""))
  (let* ((display (xlib:open-display host))
	 (screen (first (xlib:display-roots display)))
	 (black (xlib:screen-black-pixel screen))
	 (root-window (xlib:screen-root screen))
	 (my-window (xlib:create-window
		     :parent root-window
		     :x -1
		     :y -1
		     :depth 24
		     :width 10
		     :height 10
		     :background black
		     ;:override-redirect :ON
		     :event-mask (xlib:make-event-mask :exposure :property-change 
						       :key-press :visibility-change 
						       :substructure-notify :substructure-redirect))))
    (setf *root-window* root-window)
    (setf *display* display)
    (setf *window* my-window)
    (setf *client-windows* nil)

    (xlib:change-property my-window 
			  :WM_TRANSIENT_FOR (list (xlib:window-id root-window))
			  :WINDOW 32)
    
    (xlib:change-property *window* 
			  :WM_NAME (coerce "Stumpwm system tray" 'list) 
			  :STRING 8 :transform #'char-code)

    (xlib:change-property *window* 
			  :_NET_WM_NAME (coerce "Stumpwm system tray" 'list) 
			  :UTF8_STRING 8 :transform #'char-code)

    (xlib:map-window my-window)
    (when (acquire-selection my-window :_NET_SYSTEM_TRAY_S0)
      (format t "Selection acquired~%")
      (xlib:event-case (display :force-output-p t
				:discard-p t)
	(:selection-request () (format t "Selection request event~%"))
	(:selection-notify () (format t "Selection notify event~%"))
	(:selection-clear () (format t "Selection clear event~%") t)
	;(:key-press () (format t "Key press event~%") (process-click) nil)
	(:visibility-notify (state) (visibility-changed my-window state) nil)
	(:configure-request (width height x y window) (format t "W: ~a H: ~a X: ~a Y: ~a from window: ~a~%" width height x y window) nil)
	(:destroy-notify (window) (format t "Window ~a destroyed~%" window) 
			 (remove-client window) 
			 (reorganize-icons my-window *client-windows*) nil)
	(:client-message (type format data)
			 (process-client-message my-window type format data))))
    (xlib:destroy-window my-window)
    (xlib:close-display display)))

(defun acquire-selection (window selection-name)
  (setf (xlib:selection-owner (xlib:window-display window) selection-name (get-timestamp window)) window)
  (if (xlib:window-equal window (xlib:selection-owner (xlib:window-display window) selection-name))
      t
      nil))

(defgeneric process-client-message (window type format data)
  (:documentation "Process client messages"))

(defmethod process-client-message (window (type (eql :_XEMBED)) format data)
  (format t "_XEMBED message received: format:  ~a data: ~a ~%" format data))

(defmethod process-client-message (window (type (eql :_NET_SYSTEM_TRAY_OPCODE)) format data)
  (format t "Event type _NET_SYSTEM_TRAY_OPCODE~%")
  (case (aref data 1)
    (0 (format t "Window ~a requests dock~%" (aref data 2))
       (let* ((client-window (xlib::make-window :id (aref data 2) :display (xlib:window-display window)))
	      (client-protocol-version (xlib:get-property  client-window "_XEMBED_INFO" :type nil)))
	 (setf *client-window* client-window)
	 (push client-window *client-windows*)
	 (xlib:reparent-window client-window window 0 0)
	 (format t "Window reparented~%")
	 (format t "Client XEMBED protocol version: ~a~%" client-protocol-version)
	 (send-xembed-message window client-window +xembed-embedded-notify+)
	 (format t "XEMBED embedded notify message sent to ~a ~%" (get-window-name client-window))
	 (resize-drawable client-window *tray-icon-width* *tray-icon-height*)
	 (reorganize-icons window *client-windows*)
	 (xlib:map-window client-window)))
    (1 (format t "Begin Message event~%"))
    (2 (format t "End Message event~%"))
    (otherwise (format t "Unknown message received.")))
  nil)

(defun get-window-name (window)
  (xlib:get-property window :_NET_WM_NAME :type :UTF8_STRING :result-type 'string :transform #'code-char))

(defun send-xembed-message (window dst-window message-type &optional (subtype 0))
  (xlib:send-event dst-window
		   :client-message #xfff :propagate-p nil :display (xlib:window-display window)
		   :window dst-window :format 32
		   :type :_XEMBED :event-window dst-window
		   :data (append (list (get-timestamp window) message-type subtype)
				 (xembed-message-extra-data window message-type)))
  (xlib:display-finish-output (xlib:window-display window)))

(defun xembed-message-extra-data (window message-type)
  (let ((data
	 (if (eq message-type +xembed-embedded-notify+)
	     (list (xlib:window-id window) 1)
	     nil)))
    (format t "data: ~a~%" data)
    data))

(defun get-timestamp (window)
  (xlib:change-property window :WM_CLASS () :STRING 8 :mode :append) 
  (xlib:event-case ((xlib:window-display window) :force-output-p t
			    :discard-p nil)
    (:property-notify (time)
		      (format t "Timestamp: ~a~%" time)
		      time)
    (t () nil)))

(defun remove-client (window)
  (setf *client-windows* (remove window *client-windows* :test #'xlib:window-equal)))

(defun visibility-changed (window state)
  (format t "Visibility for window ~a changed to ~a~%" window state))

(defun resize-window (window width height)
  (xlib:send-event (get-root-window window) :configure-request #xfff 
		   :parent (get-root-window window) :window window 
		   :width width :height height 
		   :value-mask (make-reconfigure-mask :width :height))
  (xlib:display-finish-output (xlib:window-display window)))

(defun resize-drawable (drawable width height)
  (setf (xlib:drawable-height drawable) width)
  (setf (xlib:drawable-width drawable) height))

(defun position-window (window x y)
  (xlib:send-event (get-parent-window window) :configure-request #xfff 
		   :parent (get-parent-window window) :window window 
		   :x x :y y :value-mask (make-reconfigure-mask :x :y))
  (xlib:display-finish-output (xlib:window-display window)))

(defun position-subwindow (parent window x y)
  (xlib:reparent-window window parent x y)
  (xlib:display-finish-output (xlib:window-display window)))

(defun reorganize-icons (window clients)
  (loop for i in clients
     do
       (position-subwindow window i total-width 0)
     summing (xlib:drawable-width i) into total-width
     maximizing (xlib:drawable-height i) into total-height
     finally
       (format t "Ancho = ~a Alto = ~a ~%" total-width total-height)
       (if (and (> total-width 0) (> total-height 0))
	   (resize-window window total-width total-height)
	   (resize-window window 1 1)))
  nil)

(defun get-root-window (window)
  (xlib:drawable-root window))

(defun get-parent-window (window)
  (multiple-value-bind (children parent root)
      (xlib:query-tree window)
    (declare (ignore children root))
    parent))

(get-parent-window *client-window*)

(reorganize-icons *window* *client-windows*)
(resize-window *window* 100 100)
(resize-window  (first *client-windows*) 32 32)
(xlib:unmap-window (first *client-windows*))
(xlib:map-window (first *client-windows*))

(xlib:warp-pointer *root-window* 100 100)
(xlib:display-finish-output *display*)
(xlib:set-input-focus *display* *client-window* *client-window* (get-timestamp *display* *window*))

*display* *window* *client-window*

(xlib:get-property *client-window* :WM_ICON_NAME :type nil :transform #'code-char)
(xlib:change-property *window* :_NET_WM_STATE '(:_NET_WM_STATE_MODAL) :ATOM 32)

(car (xlib:display-roots *display*))
(xlib:get-property *client-window* :WM_NAME :type nil :transform #'code-char)
(xlib:get-property (third *client-windows*) :_NET_WM_ICON :type nil)
(xlib:get-property (third *client-windows*) :WM_NORMAL_HINTS :type nil)

(xlib:send-event *client-window* :exposure #xffff :window *client-window* :x 0 :y 0 :width 48 :height 48 :count 0)

(xlib:change-property *window* :WM_NAME (coerce "Stumpwm system tray" 'list) :STRING 8 :transform #'char-code)

(xlib:reparent-window *window* *root-window* 100 100)

(xlib:reparent-window *client-window* *window* 40 0)

(setf (xlib:drawable-height (first *client-windows*)) 22)
(setf (xlib:drawable-width (first *client-windows*)) 22)

(xlib:map-window (first *client-windows*)
(xlib:display-finish-output *display*)

(send-xembed-message *window* *client-window* +xembed-window-activate+)

(progn (xlib:query-extension *display* "XTEST")
       (xtest:fake-button-event *display* 1 t)
       (xtest:fake-button-event *display* 1 nil))

(tray)
