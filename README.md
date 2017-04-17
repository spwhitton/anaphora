# This branch

I've added two macros: Paul Graham's `alambda` and my `ssetf`.

`alambda` creates a lambda implicitly named `self`.

    (defun map-leaves (f tree)
      (funcall (alambda (x)
                 (if (atom x) (funcall f x)
                     (mapcar #'self x)))
               tree))   

    (map-leaves #'1+ '(0 1 (2 (0 3))))
    ;;; => '(1 2 (3 (1 4)))

`ssetf` names the place being set `it` via a `symbol-macrolet`, ie

    (defvar *x* (list 1 2 3))
    (ssetf (cadr x) (1+ it))
    ;; => *x* is now (list 1 3 3)

# Anaphora

Anaphora is the anaphoric macro collection from Hell: it includes many
new fiends in addition to old friends like `AIF` and `AWHEN`.
Anaphora has been placed in Public Domain by the author, [Nikodemus
Siivola](mailto:nikodemus@random-state.net).

# Installation

Use [quicklisp](http://www.quicklisp.org/), and simply:

```
CL-USER(1): (ql:quickload "anaphora")
```

# Documentation

Anaphoric macros provide implicit bindings for various
operations. Extensive use of anaphoric macros is not good style,
and probably makes you go blind as well — there's a reason why
Anaphora claims to be from Hell.

Anaphora provides two families of anaphoric macros, which can be
identified by their names and packages (both families are also
exported from the package `ANAPHORA`). The implicitly-bound symbol
`ANAPHORA:IT` is also exported from all three packages.

## Basic anaphora

#### Exported from package `ANAPHORA-BASIC`

These bind their first argument to `IT` via `LET`. In case of `COND`
all clauses have their test-values bound to `IT`.

Variants: `AAND`, `ALET`, `APROG1`, `AIF`, `ACOND`, `AWHEN`, `ACASE`,
`ACCASE`, `AECASE`, `ATYPECASE`, `ACTYPECASE`, and `AETYPECASE`.

## Symbol-macro anaphora

#### Exported from package `ANAPHORA-SYMBOL`

These bind their first argument (unevaluated) to `IT` via
SYMBOL-`MACROLET.`

Variants: `SOR`, `SLET`, `SIF`, `SCOND`, `SUNLESS`,
`SWHEN`, `SCASE`, `SCCASE`, `SECASE`, `STYPECASE`, `SCTYPECASE`,
`SETYPECASE`.

Also: `ASIF`, which binds via `LET` for the
then-clause, and `SYMBOL-MACROLET` for the else-clause.
