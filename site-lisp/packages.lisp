;;;; Package for LaTeX preambles.

(defpackage interstellar.ventures.latex
  (:use :cl
        :texp
	:named-readtables)
  (:export :document-preamble
	   :handout-preamble
	   :presentation-preamble
           :latex-en
           :latex-de))

(defpackage interstellar.ventures.html-widgets
  (:use :cl
        :macro-html
        :named-readtables)
  (:shadow :map :time)
  (:export :html-widget-header
           :html-widget-footer))

(defpackage interstellar.ventures.metagen
  (:use :cl
        :geneva.cl
        :geneva.mk2
        :configuration
        :quicklisp)
  (:export :metagen))

(defpackage interstellar.ventures
  (:use :interstellar.ventures.latex
        :interstellar.ventures.html-widgets
        :interstellar.ventures.metagen)
  (:export :latex-en
           :latex-de
           :html-widget-header
           :html-widget-footer
           :metagen))
