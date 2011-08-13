(asdf:defsystem #:simpletray
  :description "Simple tray implementation for use with Stumpwm."
  :author "Lucas Pandolfo"
  :licence "LGPL"
  :version "0.1"
  :depends-on (#:clx #:bordeaux-threads)
  :components ((:file "package")
	       (:file "tray")))
