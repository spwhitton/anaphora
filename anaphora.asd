;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author, 
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(defsystem :anaphora
    :version "0.9.4"
    :components
    ((:file "packages")
     (:file "early" :depends-on ("packages"))
     (:file "symbolic" :depends-on ("early"))
     (:file "anaphora" :depends-on ("symbolic"))))

(defsystem :anaphora-test
    :depends-on (:anaphora :rt)
    :components ((:file "tests")))

(defmethod perform ((o test-op) (c (eql (find-system :anaphora))))
  (operate 'load-op :anaphora-test)
  (operate 'test-op :anaphora-test :force t))

(defmethod perform ((o test-op) (c (eql (find-system :anaphora-test))))
  (or (funcall (intern "DO-TESTS" :rt))
      (error "test-op failed")))
