;;;; HTML widgtes for INTERSTELLAR.VENTURES.

(in-package :interstellar.ventures.html-widgets)

(in-readtable macro-html:syntax)

(defun html-widget-header ()
  "HTML header widget."
  (header [:class "logotype"]
    (a [:href "/"] (img :src "/site-assets/logotype.svg"))))

(defun current-year ()
  "Return current year."
  (nth-value 5 (decode-universal-time (get-universal-time))))

(defun html-widget-footer ()
  "HTML footer widget."
  (aside [:class "footer"]
    (p (format nil "© ~a Interstellar Ventures" (current-year)))
    (p (b "Interstellar Ventures") (br)
       "Weiherstr. 4" (br)
       "53111 Bonn" (br)
       "Germany")
    (p (a [:href "mailto:service@inters.co"] "service@inters.co"))))
