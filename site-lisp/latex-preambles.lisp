;;;; Latex preambles for INTERSTELLAR.VENTURES.

(in-package :interstellar.ventures.latex)

(setf (symbol-function 'latex-en)
      (document-preamble
       :language :english
       :format :a4-document))

(setf (symbol-function 'latex-de)
      (document-preamble
       :language :german
       :format :a4-document))
