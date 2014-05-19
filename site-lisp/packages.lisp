;;;; Package for LaTeX preambles.

(defpackage interstellar.ventures.latex
  (:use :cl
        :mr.gy.latex)
  (:export :latex-en
           :latex-de))

(defpackage interstellar.ventures.html-widgets
  (:use :cl
        :macro-html
        :named-readtables)
  (:shadow :map :time)
  (:export :html-widget-header
           :html-widget-footer))

(defpackage interstellar.ventures
  (:use :interstellar.ventures.latex
        :interstellar.ventures.html-widgets)
  (:export :latex-en
           :latex-de
           :html-widget-header
           :html-widget-footer))
