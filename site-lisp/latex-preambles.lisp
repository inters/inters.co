;;;; Latex preambles for INTERSTELLAR.VENTURES.

(in-package :interstellar.ventures.latex)

(defparameter latex-en
  (document-preamble
   :language :english
   :format :a4-document)
  "LaTeX preamble for english documents.")

(defparameter latex-de
  (document-preamble
   :language :german
   :format :a4-document)
  "LaTeX preamble for german documents.")
