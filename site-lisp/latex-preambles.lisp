;;;; Latex preambles for INTERSTELLAR.VENTURES.

(in-package :interstellar.ventures.latex)

(in-readtable texp:syntax)

(defun minimum-requirements ()
  "Minimum requirements for LATEX-DOCUMENT."
  (tex (usepackage {graphicx})
       (usepackage {tabularx})
       (usepackage {alltt})
       (usepackage {float})
       (usepackage [section] {placeins})))

(defun a4-document ()
  "Document class settings for A4."
  (tex (documentclass
	["a4paper,12pt,oneside"]
	{article})
       (usepackage
	["paper=a4paper,left=37.5264mm,right=37.5264mm,top=37.5264mm,bottom=37.5264mm"]
	{geometry}))
  (minimum-requirements)
  (tex
   (usepackage {titling})
   (setlength {(droptitle)} {-4em})
   (pretitle {(begin {flushright}) (bfseries) ("LARGE")})
   (posttitle {(end {flushright})})
   (preauthor {(begin {flushright})})
   (postauthor {(end {flushright})})
   (predate {(begin {flushright})})
   (postdate {(end {flushright})})))
  
(defun a4-paper ()
  "Document class settings for A4."
  (tex (documentclass
	["a4paper,10pt,twocolumn,oneside"]
	{article})
       (usepackage
	["paper=a4paper,left=20mm,right=20mm,top=20mm,bottom=63.66667mm"]
	{geometry})
       (setlength {(columnsep)} {"10mm"}))
  (minimum-requirements))

(defun a5-booklet ()
  "Document class settings for A5 booklet."
  (tex (documentclass
	["a5paper,10pt,twoside,titlepage"]
	{article})
       (usepackage
	["paper=a5paper,left=21.2018mm,right=10.6009mm,top=21.2018mm,bottom=33.8691mm"]
	{geometry}))
  (minimum-requirements))

(defun fontenc-t1 ()
  "Use T1 font encoding."
  (tex (usepackage ["T1"] {fontenc})))

(defun paladdio-font ()
  "Use URW Paladdio font."
  (tex (usepackage [sc] {mathpazo})
       (linespread {1.05})))

(defun inputenc-utf8x ()
  "Use utf8x input encoding."
  (tex (usepackage [utf8x] {inputenc})))

(defun dpans-symbols ()
  (tex (usepackage {stmaryrd})
       (usepackage {amsfonts})
       ("DeclareUnicodeCharacter" {12314} {"$" (llbracket) "$"})
       ("DeclareUnicodeCharacter" {12315} {"$" (rrbracket) "$"})
       ("DeclareUnicodeCharacter"  {9655} {"$" (rhd) "$"})))

(defun babel (language)
  "Set up babel for LANGUAGE."
  (ecase language
    (:german (tex (usepackage [ngerman] {babel})))
    (:english (tex (usepackage [english] {babel})))))

(defun distinct-captions ()
  "Make captions look distinct."
  (tex (usepackage ["font={small},labelformat=empty,labelsep=none"]
		   {caption})))

(defun presentation-sections ()
  "New page every section."
  (deftex dsection (header)
    (newpage)
    (section {($ header)}))
  (deftex dsubsection (header)
    (newpage)
    (subsection {($ header)}))
  (deftex dsubsubsection (header)
    (newpage)
    (subsubsection {($ header)})))

(defun presentation-captions ()
  "Make captions look distinct."
  (tex (usepackage ["labelformat=empty,labelsep=none"]
		   {caption})))

(defun no-clubs/widows/overflow ()
  "Avoid clubs widows and overflow."
  (tex (tolerance=10000)
       (clubpenalty=10000)
       (widowpenalty=10000)))

(defun document-preamble (&key (language :english) (format :a4-document))
  "Preamble for documents in LANGUAGE."
  (lambda ()
    (ecase format
      (:a4-document (a4-document))
      (:a4-paper (a4-paper))
      (:a5-booklet (a5-booklet)))
    (babel language)
    (fontenc-t1)
    (inputenc-utf8x)
    (dpans-symbols)
    (paladdio-font)
    (distinct-captions)
    (no-clubs/widows/overflow)
    (tex (frenchspacing))))

(defun handout-preamble (&key (language :english))
  "Preamble for presentation handouts."
  (lambda ()
    (funcall (document-preamble :language language))
    (deftex dsubsection (header))))

(defun presentation-preamble (&key (language :english))
  "Preamble for presentations."
  (lambda ()
    (tex (documentclass ["12pt,a6paper,landscape,"]
			{article})
	 (usepackage
	  ["paper=a6paper,landscape,left=12mm,right=12mm,top=12mm,bottom=12mm"]
	  {geometry})
	 (renewcommand* (familydefault) {(sfdefault)})
	 (usepackage {titlesec})
	 (newcommand {(sectionbreak)} {(clearpage)}))
    (minimum-requirements)
    (babel language)
    (fontenc-t1)
    (inputenc-utf8x)
    (paladdio-font)
    (presentation-sections)
    (presentation-captions)))

(setf (symbol-function 'latex-en)
      (document-preamble
       :language :english
       :format :a4-document))

(setf (symbol-function 'latex-de)
      (document-preamble
       :language :german
       :format :a4-document))
