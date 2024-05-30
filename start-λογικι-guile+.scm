;; this is the bootstrap file for λογικι (LOGIKI)

;; Damien Mattei

;; 2024

;; Warning: // tests give no speed up!
;; export LTDL_LIBRARY_PATH=/usr/lib/llvm-14/lib # for OpenMP under Linux
;; export LTDL_LIBRARY_PATH=/opt/homebrew/opt/libomp/lib # for OpenMP under MacOS

;; (load "start-λογικι-guile+.scm")

;; modify the file or touch it to be sure it is recompiled by guile         
 

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

	     (operation+)
	     (set+)
	     (minterms+)
	     (subscript+)
	     (regex+)

	     (array)
	     
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

(include "debug.scm")




;; overload tests

(define-overload-existing-procedure length)

(overload-existing-procedure length vector-length (vector?))
(overload-existing-procedure length string-length (string?))

;; scheme@(guile-user)> (length #(1 2 3 4))
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure string? (_)>)
;; new-funct : args = (#(1 2 3 4))
;; new funct :calling:#<procedure new-funct args>
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure vector? (_)>)
;; new-funct : args = (#(1 2 3 4))
;; new funct :calling:#<procedure vector-length (_)>
;; 4
;; scheme@(guile-user)> (length '(1 2 3))
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure string? (_)>)
;; new-funct : args = ((1 2 3))
;; new funct :calling:#<procedure new-funct args>
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure vector? (_)>)
;; new-funct : args = ((1 2 3))
;; new funct :calling:#<procedure length (_)>
;; 3
;; scheme@(guile-user)> (length "abcde")
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure string? (_)>)
;; new-funct : args = (abcde)
;; new funct :calling:#<procedure string-length (_)>
;; 5

;; (length #(1 2 3 4))
;; (length '(1 2 3))
(length "abcde")

(define-overload-procedure area)
;; "Il semble que la terminologie de carré parfait puisse également s’expliquer par l’étymologie. Au
;; 18ème siècle, un rectangle était en effet aussi appelé « quarré long » (voir l’Encyclopédie de Diderot
;; et d’Alembert à l’article « rectangle »). Ainsi la Maison Carrée de Nîmes est de forme...
;; rectangulaire. Voir également le dictionnaire de l’Académie française (4ème édition, 1764) à l’article
;; Carré."
(define (surf-carre-parfait x) (* x x))
(define (surf-carre-long x y) (* x y))

(overload-procedure area surf-carre-parfait (number?))
(overload-procedure area surf-carre-long (number? number?))

;; scheme@(guile-user)> (area 3.4)
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure number? (_)> #<procedure number? (_)>)
;; new-funct : args = (3.4)
;; new funct :calling:#<procedure new-funct args>
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure number? (_)>)
;; new-funct : args = (3.4)
;; new funct :calling:#<procedure surf-carre-parfait (x)>
;; 11.559999999999999
;; scheme@(guile-user)> (area 2.3 4.5)
;; new-funct: #<procedure new-funct args>
;; new-funct : pred-list = (#<procedure number? (_)> #<procedure number? (_)>)
;; new-funct : args = (2.3 4.5)
;; new funct :calling:#<procedure surf-carre-long (x y)>
;; 10.35



(display "before add-list-list") (newline)



(define-overload-existing-n-arity-operator +)

(define (add-list-list v1 v2) (implementation-add-list-list v1 v2))

(define (add-n-lists . vn-lst) (implementation-add-n-lists vn-lst))
(display "before overload-existing-n-arity-operator") (newline)
(overload-existing-n-arity-operator + add-n-lists (list? list?))

(display "before mult-num-list") (newline)
(define (mult-num-list k v) (map (λ (x) (* k x)) v))
(define-overload-existing-operator *)
(overload-existing-operator * mult-num-list (number? list?))

(define-overload-operator *b)

;; TODO: use overload with hash table (this will allow use of Scheme+ syntax in overloaded operators  without using a tierce procedure example: implementation-add-list-list)

(create-vector-2d (lambda (l c) (+ l c)) 2 3) ;; for test !

(define (implementation-add-list-list v1 v2) (map + v1 v2))

(define (implementation-add-n-lists vn-lst)
  {map-args <+ (cons + vn-lst)}
  (apply map map-args))

(include "list.scm")


(include "symbolic.scm")
(include "simplify.scm")
(include "guile/binary-arithmetic.scm") ;; specialized for 'format'

(include "guile/map.scm") ;; specialized for 'andmap'

(include "display-formula.scm")
(include "symbol.scm")

(include "hash-table.scm")


;; now in Scheme+

(include "guile/logiki+.scm")

;;(include "guile/logiki-.scm") ; parsed version created in command line or by Makefile





