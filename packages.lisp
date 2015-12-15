;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author, 
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(defpackage :anaphora
  (:use :cl)
  (:export
   #:self
   #:it
   #:alet
   #:slet
   #:aif
   #:alambda
   #:aand
   #:sor
   #:awhen
   #:aprog1
   #:acase
   #:aecase
   #:accase
   #:atypecase
   #:aetypecase
   #:actypecase
   #:acond
   #:sif
   #:asif
   #:ssetf
   #:swhen
   #:sunless
   #:scase
   #:secase
   #:sccase
   #:stypecase
   #:setypecase
   #:sctypecase
   #:scond)
  (:documentation 
   "ANAPHORA provides a full complement of anaphoric macros. Subsets of the
functionality provided by this package are exported from ANAPHORA-BASIC and
ANAPHORA-SYMBOL."))

(defpackage :anaphora-basic
  (:use :cl :anaphora)
  (:export
   #:self
   #:it
   #:alet
   #:aif
   #:aand
   #:awhen
   #:aprog1
   #:acase
   #:aecase
   #:accase
   #:atypecase
   #:aetypecase
   #:actypecase
   #:acond)
  (:documentation 
   "ANAPHORA-BASIC provides all normal anaphoric constructs, which bind
primary values to IT."))
   
(defpackage :anaphora-symbol
  (:use :cl :anaphora)
  (:export
   #:self
   #:it
   #:slet
   #:sor
   #:sif
   #:asif
   #:swhen
   #:sunless
   #:scase
   #:secase
   #:sccase
   #:stypecase
   #:setypecase
   #:sctypecase
   #:scond)
  (:documentation
   "ANAPHORA-SYMBOL provides ``symbolic anaphoric macros'', which bind forms
to IT via SYMBOL-MACROLET. 

Examples:

  (sor (gethash key table) (setf it default))

  (asif (gethash key table)
        (foo it)            ; IT is a value bound by LET here
        (setf it default))  ; IT is the GETHASH form bound by SYMBOL-MACROLET here
"))
