;;;; stickwindow.asd

(asdf:defsystem #:stickwindow
  :serial t
  :depends-on (#:clx)
  :components ((:file "package")
               (:file "stickwindow")))

