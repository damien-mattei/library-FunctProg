;; this is the bootstrap file for λογικι (LOGIKI)

;; Damien Mattei

;; May 2023

;; export LTDL_LIBRARY_PATH=/usr/lib/llvm-14/lib # for OpenMP under Linux
;; export LTDL_LIBRARY_PATH=/opt/homebrew/opt/libomp/lib # for OpenMP under MacOS

;; (load "start-λογικι-guile+.scm")

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


;; scheme@(guile-user)> (cnf-infix-symb '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}} )

;; ((¬a ∨ ¬b ∨ c) ∧ (¬a ∨ ¬b ∨ ¬d) ∧ (¬a ∨ ¬c ∨ ¬d) ∧ (b ∨ ¬c ∨ ¬d) ∧ (¬b ∨ c ∨ d))



(use-modules (Scheme+)
	     (ice-9 futures)
	     ;;(ice-9 format)
	     (ice-9 threads)
	     ;;(ice-9 hash-table) ;; built-in Hash Table
	     (srfi srfi-1) ;; for 'first' procedure
	     (srfi srfi-28) ;; Basic Format Strings
	     (srfi srfi-60) ;; for arithmetic-shift
	     ;;(srfi srfi-28) ;; for format and escape ~
	     (srfi srfi-69) ;; SRFI 69 Hash Table
	     ;;((rnrs) :version (6)) ;; contains hash tables too
	     (srfi srfi-43) ;; vector library
	     ;;(srfi srfi-171) ;; transducers
	     ;;(system foreign) ;; for library loading
	     ;;(system foreign-library)
	     ((rnrs sorting) #:select (vector-sort))
	     ;;(parallel vector) ;; In procedure dlsym: Error resolving "timerfd_create": "dlsym(RTLD_DEFAULT, timerfd_create): symbol not found" with Mac OS
	     )

(include "rest.scm")

(include "increment.scm")

(include "r7rs/escape-char-r7rs-scheme.scm")
(include "r6rs/display-r6rs-scheme.scm")

(include "for_next_step.scm")


(include "debug.scm")

(include "guile/set+.scm")
(include "list.scm")

(include "array.scm")
(include "symbolic.scm")
(include "simplify.scm")
(include "guile/binary-arithmetic.scm") ;; specialized for 'format'

(include "guile/map.scm") ;; specialized for 'andmap'

(include "guile/operation+.scm")
(include "display-formula.scm")
(include "symbol.scm")
(include "guile/minterms+.scm")
(include "hash-table.scm")

(include "guile/subscript+.scm")
(include "guile/regex+.scm")
(include "../Scheme-PLUS-for-Guile/included-files/scheme-infix.scm")
(include "guile/logiki+.scm")



