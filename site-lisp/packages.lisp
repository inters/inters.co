;;;; Package for LaTeX preambles.

(defpackage inters.co.latex
  (:nicknames :interstellar.ventures.latex)
  (:use :cl
        :texp
	:named-readtables)
  (:export :document-preamble
	   :handout-preamble
	   :presentation-preamble
           :latex-en
           :latex-de
           :presentation-en))

(defpackage inters.co.html-widgets
  (:nicknames :interstellar.ventures.html-widgets)
  (:use :cl
        :macro-html
        :named-readtables)
  (:shadow :map :time)
  (:export :html-widget-header
           :html-widget-footer))

(defpackage inters.co.metagen
  (:nicknames :interstellar.ventures.metagen)
  (:use :cl
        :geneva.cl
        :geneva.mk2
        :configuration
        :quicklisp)
  (:export :metagen))

(defpackage inters.co.bloggen
  (:nicknames :interstellar.ventures.bloggen)
  (:use :cl
        :geneva
        :geneva.mk2
        :configuration
        :rss-feed
        :uiop
        :cl-date-time-parser)
  (:export :bloggen))

(defpackage inters.co
  (:nicknames :interstellar.ventures)
  (:use :inters.co.latex
        :inters.co.html-widgets
        :inters.co.metagen
        :inters.co.bloggen)
  (:export :latex-en
           :latex-de
           :html-widget-header
           :html-widget-footer
           :metagen
           :bloggen))
