;;;; -*- Mode: Lisp; Base: 10; Syntax: ANSI-Common-lisp; -*-

;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author,
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(defsystem :anaphora
    :version "0.9.6"
    :description "The Anaphoric Macro Package from Hell"
    :author "Nikodemus Siivola <nikodemus@random-state.net>"
    :license "Public Domain"
    :components
    ((:file "packages")
     (:file "early" :depends-on ("packages"))
     (:file "symbolic" :depends-on ("early"))
     (:file "anaphora" :depends-on ("symbolic"))))

(defsystem :anaphora/test
    :description "Tests for anaphora"
    :author "Nikodemus Siivola <nikodemus@random-state.net>"
    :license "Public Domain"
    :depends-on (:anaphora :rt)
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :anaphora))))
  (test-system :anaphora/test))

(defmethod perform ((o test-op) (c (eql (find-system :anaphora/test))))
  (or (symbol-call :rt '#:do-tests)
      (error "test-op failed")))
