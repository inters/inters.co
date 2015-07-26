;;;; System definition for INTERSTELLAR.VENTURES.

(defsystem interstellar.ventures
  :components ((:file "packages")
               (:file "latex-preambles"
                      :depends-on ("packages"))
               (:file "html-widgets"
                      :depends-on ("packages"))
               (:file "metagen"
                      :depends-on ("packages")))
  :depends-on ("macro-html"
	       "named-readtables"
               "open-geneva"
               "configuration"))
