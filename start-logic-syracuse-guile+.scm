;; this is the bootstrap file for logic for syracuse conjecture

;; Damien Mattei

;; 25/08/2022

;; (load "start-logic-syracuse-guile+.scm")

;; examples:

;;scheme@(guile-user)> (infix-symb-min-dnf '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}} )

;; ((¬b ∧ ¬c) ∨ (c ∧ ¬d) ∨ (¬a ∧ b ∧ d))


;;scheme@(guile-user)> (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d))))

;; ((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))


;; verification with Mathematica:
;; In[1]:= BooleanMinimize[(!a && !b && !c && !d)
;;            ||
;;            (!a && !b && !c && d)
;;            ||
;;            (!a && !b && c && !d)
;;            ||
;;            (!a && b && !c && d)
;;            ||
;;            (!a && b && c && !d)
;;            ||
;;            (!a && b && c && d)
;;            ||
;;            (a && !b && !c && !d)
;;            ||
;;            (a && !b && !c && d)
;;            ||
;;            (a && !b && c && !d)
;;            ||
;;            (c && !d)]

;; Out[1]= (!a && b && d) || (!b && !c) || (c && !d)
;;


;; scheme@(guile-user)> (prefix->infix '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d))))

;; ((!a and !b and !c and !d) or (!a and !b and !c and d) or (!a and !b and c and !d) or (!a and b and !c and d) or (!a and b and c and !d) or (!a and b and c and d) or (a and !b and !c and !d) or (a and !b and !c and d) or (a and !b and c and !d) or (c and !d))


;; scheme@(guile-user)>  (cnf-infix-symb '{{A · B} ⊕ {Ci · {A ⊕ B}}})
;; $1 = ((A ∨ B) ∧ (A ∨ Ci) ∧ (B ∨ Ci))


(use-modules (Scheme+)
	     (srfi srfi-1) ;; for 'first' procedure
	     (srfi srfi-60) ;; for arithmetic-shift
	     ;;(srfi srfi-28) ;; for format and escape ~
	     (ice-9 format)
	     ;;(ice-9 hash-table) ;; built-in Hash Table
	     (srfi srfi-69) ;; SRFI 69 Hash Table
	     ;;((rnrs) :version (6)) ;; contains hash tables too
	     )

(include "rest.scm")

(include "increment.scm")

(include "r7rs/escape-char-r7rs-scheme.scm")
(include "r6rs/display-r6rs-scheme.scm")

(include "for-next-step.scm")

(include "set.scm")
(include "list.scm")
(include "debug.scm")

(include "array.scm")
(include "symbolic.scm")
(include "simplify.scm")
(include "guile/binary-arithmetic.scm") ;; specialized for 'format'

(include "guile/map.scm") ;; specialized for 'andmap'

(include "guile/operation+.scm")
(include "display-formula.scm")
(include "symbol.scm")
(include "minterms.scm")

(include "while-do-when-unless.scm")

(include "guile/logiki+.scm")
(include "logic-syracuse+.scm")



