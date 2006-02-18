;;;; Anaphora: The Anaphoric Macro Package from Hell
;;;;
;;;; This been placed in Public Domain by the author, 
;;;; Nikodemus Siivola <nikodemus@random-state.net>

(in-package :anaphora)

(defmacro anaphoric (op test &body body)
  `(let ((it ,test))
     (,op it ,@body)))

;;; This was the original implementation of SYMBOLIC --
;;; and still good for getting the basic idea. Brian Masterbrooks
;;; solution to infinite recusion during macroexpansion that nested
;;; forms of this are subject to is in symbolic.lisp.
;;;
;;; (defmacro symbolic (op test &body body &environment env)
;;;   `(symbol-macrolet ((it ,test))
;;;        (,op it ,@body)))

(defmacro aand (first &rest rest)
  "Like AND, except binds the first argument to IT (via LET) for the
scope of the rest of the arguments."
  `(anaphoric and ,first ,@rest))

(defmacro sor (first &rest rest)
  "Like OR, except binds the first argument to IT (via SYMBOL-MACROLET) for
the scope of the rest of the arguments. IT can be set with SETF."
  `(symbolic or ,first ,@rest))

(defmacro aif (test then &optional else)
  "Like IF, except binds the result of the test to IT (via LET) for
the scope of the then and else expressions."  
  `(anaphoric if ,test ,then ,else))

(defmacro sif (test then &optional else &environment env)
  "Like IF, except binds the test form to IT (via SYMBOL-MACROLET) for
the scope of the then and else expressions. IT can be set with SETF"
  `(symbolic if ,test ,then ,else))

(defmacro asif (test then &optional else)
  "Like IF, except binds the result of the test to IT (via LET) for
the the scope of the then-expression, and the test form to IT (via
SYMBOL-MACROLET) for the scope of the else-expression. Within scope of
the else-expression IT can be set with SETF."
    `(let ((it ,test))
       (if it
	   ,then
	   (symbolic ignore-first ,test ,else))))

(defmacro aprog1 (first &body rest)
  "Binds IT to the first form so that it can be used in the rest of the
forms. The whole thing returns IT."
  `(anaphoric prog1 ,first ,@rest))

(defmacro awhen (test &body body)
  "Like WHEN, except bind the result of the test to IT (via LET) for the scope
of the body."
  `(anaphoric when ,test ,@body))

(defmacro swhen (test &body body)
  `(symbolic when ,test ,@body))

(defmacro sunless (test &body body)
  `(symbolic unless ,test ,@body))

(defmacro acase (form &body cases)
  `(anaphoric case ,form ,@cases))

(defmacro scase (form &body cases)
  `(symbolic case ,form ,@cases))

(defmacro aecase (form &body cases)
  `(anaphoric ecase ,form ,@cases))

(defmacro secase (form &body cases)
  `(symbolic ecase ,form ,@cases))
  
(defmacro accase (form &body cases)
  `(anaphoric ccase ,form ,@cases))

(defmacro sccase (form &body cases)
  `(symbolic ccase ,form ,@cases))

(defmacro atypecase (form &body cases)
  `(anaphoric typecase ,form ,@cases))

(defmacro stypecase (form &body cases)
  `(symbolic typecase ,form ,@cases))

(defmacro aetypecase (form &body cases)
  `(anaphoric etypecase ,form ,@cases))

(defmacro setypecase (form &body cases)
  `(symbolic etypecase ,form ,@cases))

(defmacro actypecase (form &body cases)
  `(anaphoric ctypecase ,form ,@cases))

(defmacro sctypecase (form &body cases)
  `(symbolic ctypecase ,form ,@cases))

(defmacro acond (&body clauses)
  (labels ((rec (clauses)
	     (if clauses
		 (destructuring-bind ((test &body body) . rest)  clauses
		   (if body
		       `(anaphoric if ,test (progn ,@body) ,(rec rest))
		       `(anaphoric if ,test it ,(rec rest))))
		 nil)))
    (rec clauses)))

(defmacro scond (&body clauses)
  (labels ((rec (clauses)
	     (if clauses
		 (destructuring-bind ((test &body body) . rest) clauses
		   (if body
		       `(symbolic if ,test (progn ,@body) ,(rec rest))
		       `(symbolic if ,test it ,(rec rest))))
		 nil)))
    (rec clauses)))
