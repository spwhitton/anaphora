;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author, 
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(defpackage :anaphora
  (:use :cl)
  (:export
   #:it
   #:aif
   #:aand
   #:sor
   #:awhen
   #:acase
   #:aecase
   #:accase
   #:atypecase
   #:aetypecase
   #:actypecase
   #:acond
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
   #:scond))

(defpackage :anaphora-basic
  (:use :cl :anaphora)
  (:export
   #:it
   #:aif
   #:aand
   #:awhen
   #:acase
   #:aecase
   #:accase
   #:atypecase
   #:aetypecase
   #:actypecase
   #:acond))
   
(defpackage :anaphora-symbol
  (:use :cl :anaphora)
  (:export
   #:it
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
   #:scond))
