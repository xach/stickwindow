;;;; stickwindow.lisp

(in-package #:stickwindow)

;;; "stickwindow" goes here. Hacks and glory await!

(defun call-with-open-display (fun)
  (let (display)
    (unwind-protect
         (progn
           (setf display (xlib:open-default-display))
           (funcall fun display))
      (when display
        (xlib:close-display display)))))

(defmacro with-open-display (display &body body)
  `(call-with-open-display (lambda (,display) ,@body)))

(defun root-window (display)
  (let ((roots (xlib:display-roots display)))
    (when (and (consp roots) (endp (cdr roots)))
      (let ((screen-root (first roots)))
        (xlib:screen-root screen-root)))))

(defun send-message (root event-window &key type data)
  (xlib:send-event root
                   :client-message
                   '(:substructure-redirect :substructure-notify)
                   :event-window event-window :format 32
                   :type type :data data)
  (xlib:display-finish-output (xlib:window-display root)))

(defun stickify (window type)
  (let* ((code (ecase type
                 (:unstick 0)
                 (:stick 1)
                 (:toggle 2)))
         (root (xlib:drawable-root window))
         (display (xlib:window-display window))
         (atom (xlib:intern-atom display :_net_wm_state_sticky)))
      (send-message root window :type :_net_wm_state
                    :data (list code atom))))

(defun call-with-grabbed-pointer (window fun)
  (let* ((display (xlib:window-display window))
         (font (xlib:open-font display "cursor"))
         (black (xlib:make-color :red 0.0 :green 0.0 :blue 0.0))
         (white (xlib:make-color :red 1.0 :green 1.0 :blue 1.0))
         (cursor (xlib:create-glyph-cursor :source-font font
                                           :source-char 34
                                           :mask-font font
                                           :mask-char 35
                                           :foreground black
                                           :background white)))
    (unwind-protect
         (progn
           (xlib:grab-pointer window '(:button-press :button-release)
                              :cursor cursor
                              :sync-pointer-p t)
           (funcall fun))
      (xlib:ungrab-pointer display)
      (xlib:display-finish-output display))))

(defun window-selector-handler (&key child &allow-other-keys)
  (let ((window child))
    (loop
      (when (or (null window)
                (xlib:get-property window :wm_name))
        (return window))
      (setf window (first (xlib:query-tree window))))))

(defun select-window (root)
  (call-with-grabbed-pointer
   root
   (lambda ()
     (let ((display (xlib:window-display root)))
       (xlib:allow-events display :sync-pointer)
       (xlib:process-event display :handler #'window-selector-handler)))))

(defun stickwindow (argv)
  (with-open-display display
    (let* ((root (root-window display))
           (arg (second argv))
           (mode (cond ((equalp arg "--unstick")
                        :unstick)
                       ((equalp arg "--toggle")
                        :toggle)
                       (t
                        :stick))))
      (stickify (select-window root) mode))))
