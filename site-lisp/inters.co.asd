;;;; System definition for INTERS.CO

(defsystem inters.co
  :components ((:file "packages")
               (:file "latex-preambles"
                      :depends-on ("packages"))
               (:file "html-widgets"
                      :depends-on ("packages"))
               (:file "metagen"
                      :depends-on ("packages"))
               (:file "bloggen"
                      :depends-on ("packages")))
  :depends-on ("macro-html"
	       "named-readtables"
               "open-geneva"
               "configuration"
               "rss-feed"
               "uiop"
               "cl-date-time-parser"))
