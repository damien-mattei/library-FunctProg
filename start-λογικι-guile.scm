;; this is the bootstrap file for λογικι (LOGIKI)

;; Damien Mattei

;; 22/11/2021

;; (load ".guile")
;; (load "start-λογικι-guile.scm")


(use-modules (srfi srfi-1) ;; for 'first' procedure
	     (srfi srfi-60) ;; for arithmetic-shift
	     ;;(srfi srfi-28) ;; for format and escape ~
	     (ice-9 format)
	     )

(include "rest.scm")

(include "increment.scm")

(include "r7rs/escape-char-r7rs-scheme.scm")
(include "r6rs/display-r6rs-scheme.scm")

(include "for-next-step.scm")

(include "set.scm")
(include "debug.scm")

(include "array.scm")
(include "symbolic.scm")
(include "simplify.scm")
(include "guile/binary-arithmetic.scm") ;; specialized for 'format'

(include "guile/map.scm") ;; specialized for 'andmap'
(include "list.scm")
(include "operation.scm")
(include "display-formula.scm")
(include "symbol.scm")
(include "minterms.scm")


