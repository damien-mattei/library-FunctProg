#lang reader "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/SRFI/SRFI-105.rkt"

;;#lang reader "SRFI-105-toplevel.rkt"

;;
;;
;;                    λογικι
;;
;;                    LOGIKI
;;
;;
;; a program to compute logic symbolically
;;
;; Copyright (C) 2014-2023  Damien MATTEI
;;
;;
;; e-mail: damien.mattei@gmail.com
;;        
;;
;;
;;
;;
;; version 12 for Racket




;;(compile-enforce-module-constants #f)

;; for infix operator precedence
;; (define-namespace-anchor ankh)
;; (define bsns (namespace-anchor->namespace ankh))
;; (current-namespace bsns)

; DrRacket does not like greek characters in filenames
;(include "program-λογικι-2.8.scm")


;; test
;;(define λογικι #t)


;; (infix-symb-min-dnf '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}} )

;; '((¬b ∧ ¬c) ∨ (c ∧ ¬d) ∨ (¬a ∧ b ∧ d))



;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d))))

;; '((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))




(provide (all-defined-out)) ;; export all bindings ,when we are at a module level not at toplevel because of racket/lang emulate REPL 



;;(require srfi/69) ;; Basic hash tables

(require racket/future) ;; for //

;;(require (for-syntax r6rs/private/base-for-syntax)) ;; for macro syntax (for ... : identifier-syntax: undefined;

;;(require srfi/43) ;; vector library

;;(require "transducers.rkt")
;;(require srfi-171/transducers)


(require "operation+.rkt")
(require "set+.rkt")
(require "subscript+.rkt")
(require "minterms+.rkt")

;; should be included in Scheme+.rkt but 'require' use a relative path from here! and won't load it from Scheme+.rkt
(require "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/overload.rkt")

(include "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/Scheme+.rkt")

(include "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/assignment.rkt")
(include "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/apply-square-brackets.rkt")


(require "../../AI_Deep_Learning/matrix-by-vectors.rkt")

(include "display-racket-scheme.scm")

;;(include "../for_next_step.scm") ;; for macro syntax (for ... : identifier-syntax: undefined; use: (require (for-syntax r6rs/private/base-for-syntax))

(include "../debug.scm")

(include "../list.scm")

(include "../symbolic.scm")
(include "../simplify.scm")
(include "../binary-arithmetic.scm")

(include "../map.scm")

(include "display-formula.scm")

(include "../hash-table.scm")



;; overload tests

(display "before add-list-list") (newline)
(define (add-list-list v1 v2) (map + v1 v2))
(display "before define-overload") (newline)

(define-overload-existing-n-arity-operator +)
;;(define-overload-existing-operator +)
(define-overload-existing-operator *)

(define-overload-existing-n-arity-operator -)

(define (add-n-lists . vn-lst) (implementation-add-n-lists vn-lst))

(define (sub-n-lists . vn-lst) (implementation-sub-n-lists vn-lst))

(define-overload-existing-procedure length)
(define-overload-procedure foobie)

(include "../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/scheme-infix.rkt")


;; overload tests



(define (implementation-add-n-lists vn-lst)
  {map-args <+ (cons + vn-lst)}
  (apply map map-args))


(define (implementation-sub-n-lists vn-lst)
  {map-args <+ (cons - vn-lst)}
  (apply map map-args))


;;
;;                    λογικι
;;
;;                    LOGIKI
;;
;;
;; a program to compute logic symbolically
;;
;; Copyright (C) 2014-2023  Damien MATTEI
;;
;;
;; e-mail: damien.mattei@gmail.com 
;; 
;;
;;
;;
;;
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.

;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.

;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>
;;
;;
;; Scheme version (developped with MIT-scheme, Bigloo, Dr Racket, Guile)
;;
;; provides :
;;
;; transformation towards Disjunctive Normal Form
;; transformation towards Conjunctive Normal Form
;; transformation from prefix to infix notation
;; simplification of DNF and CNF
;; search for antilogies and tautologies
;; minimal form with Quine - Mc Cluskey and Petrick method 
;; 
;; example of expression put in DNF:

;; (infix-symb-min-dnf '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}} )

;; '((¬b ∧ ¬c) ∨ (c ∧ ¬d) ∨ (¬a ∧ b ∧ d))





;;
;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d))))
;;
;; '((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))
;;
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
;;
;;
;;
;; (dnf-infix-symb (minimal-dnf '(or (and (and A B) (not (and C (or (and A (not B)) (and (not A) B))))) (and (not (and A B)) (and C (or (and A (not B)) (and (not A) B))))))) -> '((B ^ C) v (A ^ C) v (A ^ B))
;;
;; (dnf-infix-symb (minimal-dnf '(or (and (not a) b (not c) d) (and (not a) b c d) (and a b (not c) (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d) (and a (not b) (not c) (not d)) (and a (not b) c (not d)))))
;;
;; '((b ^ d) v (a ^ !d))
;;
;;
;;
;; (dnf-infix-symb (minimal-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) c (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d))))
;;
;; '((b ^ d) v (!a ^ !b ^ !c ^ !d) v (a ^ c ^ !d))
;;
;;
;; (dnf-infix-symb (minimal-dnf '(and (or x0 x1) (or x1 x2) x3 x5))) -> '((x1 ^ x3 ^ x5) v (x0 ^ x2 ^ x3 ^ x5))
;;
;;
;; (pretty-display  (dnf-infix-symb '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))) -> (a ^ !b ^ !c)  v  (!a ^ b ^ !c)  v  (!a ^ !b ^ c)  v  (a ^ b ^ c)
;;
;; with DrRacket Scheme:
;;
;; (dnf-infix-symb '(((p . and . q) . => . r) . and . ((not (p . and . q)) . => . r))) -> 'r
;;
;; (dnf-infix-symb '(or (and (not a) b (not c) d) (and (not a) b c d) (and a b (not c) (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d) (and a (not b) (not c) (not d)) (and a (not b) c (not d))))
;;
;;  -> '((!a ^ b ^ !c ^ d) v (!a ^ b ^ c ^ d) v (a ^ b ^ !c ^ !d) v (a ^ b ^ !c ^ d) v (a ^ b ^ c ^ !d) v (a ^ b ^ c ^ d) v (a ^ !b ^ !c ^ !d) v (a ^ !b ^ c ^ !d))
;;
;;
;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) c (not d)) (and c d) (and a c (not d)) (and a b c d) (and a  (not c))))
;;
;; '((!b ^ !c ^ !d) v (c ^ d) v (b ^ d) v a)
;;
;; (dnf-infix-symb (minimal-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) c (not d)) (and c d) (and a c (not d)) (and a b c d) (and a  (not c)))))
;;
;; -> '((c ^ d) v (!b ^ !c ^ !d) v (b ^ d) v a)
;;
;;
;;
;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and a b c (not d))))
;;
;; '((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))
;;
;;
;; with others Schemes:
;;
;;  (dnf-infix-symb '(and (=> (and p q) r) (=> (not (and p q)) r))) -> r
;;
;; the same in CNF:
;;
;; with DrRacket Scheme+:
;;
;; (cnf-infix-symb '{{{p and q} => r} and {(not {p and q}) => r}})
;; '((p ∨ r) ∧ (¬p ∨ ¬q ∨ r) ∧ (q ∨ r))

;; with DrRacket Scheme:
;;
;;   (cnf-infix-symb '(((p . and . q) . => . r) . and . ((not (p . and . q)) . => . r))) -> '((!p v !q v r) ^ (p v r) ^ (q v r))
;;
;;  (cnf-infix-symb '(or (and (not a) b (not c) d) (and (not a) b c d) (and a b (not c) (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d) (and a (not b) (not c) (not d)) (and a (not b) c (not d))))
;;   ->
;; '((a v b v !c v d)
;;   ^
;;   (a v !b v !c v d)
;;   ^
;;   (a v !c v d)
;;   ^
;;   (a v b v c v d)
;;   ^
;;   (a v b v d)
;;   ^
;;   (a v !b v c v d)
;;   ^
;;   (a v c v d)
;;   ^
;;   (a v !b v d)
;;   ^
;;   (a v d)
;;   ^
;;   (a v b v !c v !d)
;;   ^
;;   (a v b v !c)
;;   ^
;;   (!a v b v !c v !d)
;;   ^
;;   (a v b v c v !d)
;;   ^
;;   (a v b v c)
;;   ^
;;   (a v b v !d)
;;   ^
;;   (a v b)
;;   ^
;;   (!a v b v c v !d)
;;   ^
;;   (!a v b v !d)
;;   ^
;;   (b v !c v !d)
;;   ^
;;   (b v c v !d)
;;   ^
;;   (b v !d))
;;
;;
;; '((a ∨ b) ∧ (a ∨ d) ∧ (b ∨ ¬d))


;; (cnf-infix-symb '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}})

;;  in Racket the memory must be increased because of product-set-with-set version (see comments in set.scm)
;; 128 Mb -> 1024 Mb
;; '((¬a ∨ ¬b ∨ c) ∧ (¬a ∨ ¬b ∨ ¬d) ∧ (¬a ∨ ¬c ∨ ¬d) ∧ (b ∨ ¬c ∨ ¬d) ∧ (¬b ∨ c ∨ d))


;;  with others Schemes:
;;
;;  (cnf-infix-symb '(and (=> (and p q) r) (=> (not (and p q)) r))) ->  '((p ∨ r) ∧ (¬p ∨ ¬q ∨ r) ∧ (q ∨ r))
;;
;;  (enlight-dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))) -> (a^b^c)v(!a^!b^c)v(!a^b^!c)v(a^!b^!c)
;;
;; (enlight-dnf '(or (and a b) a)) -> a
;;
;; (enlight-dnf  '(and (=> (and p q) r) (=> (not (and p q)) r))) -> r


;; (infix-symb-min-dnf  '(and (<=> (and p q) r) (<=> (not (and p q)) r)))
;; 'F

;; BooleanConvert[Xor[A,B,C]] in WolframMath
;; (infix-symb-min-dnf  '{A ⊕ B ⊕ Ci})
;; ((¬A ∧ ¬B ∧ Ci) ∨ (¬A ∧ B ∧ ¬Ci) ∨ (A ∧ ¬B ∧ ¬Ci) ∨ (A ∧ B ∧ Ci))

;; (cnf-infix-symb '{A ⊕ B ⊕ Ci})
;; '((¬A ∨ B ∨ ¬Ci) ∧ (¬A ∨ ¬B ∨ Ci) ∧ (A ∨ ¬B ∨ ¬Ci) ∧ (A ∨ B ∨ Ci))



;; macros and functions definitions are included in files


;; code below is copy/paste from Guile till // procedures




;; PHASE 0 : eliminate equivalence
;; a <=> b ----> (a => b) and (b => a)

;; scheme@(guile-user)> (elim-equivalence '{a <=> b})
;; $1 = (and (=> a b) (=> b a))
;; scheme@(guile-user)> (elim-equivalence '(not {a <=> b}))
;; $2 = (not (and (=> a b) (=> b a)))
;; scheme@(guile-user)> (elim-equivalence '(not {a <=> {b <=> (not c)}}))
;; $3 = (not (and (=> a (and (=> b (not c)) (=> (not c) b))) (=> (and (=> b (not c)) (=> (not c) b)) a)))

(define (elim-equivalence expr)
  (cond
   ((symbol? expr) expr)
   ((boolean? expr) expr)
   ((isNOT? expr) `(not ,(elim-equivalence (arg expr))))
   ((isIMPLIC? expr) `(=> ,(elim-equivalence (arg1 expr)) ,(elim-equivalence (arg2 expr))))
   ((isEQUIV? expr) ($+> ;; a <=> b ----> (a => b) and (b => a)
		     {a <+ (arg1 expr)}
		     {b <+ (arg2 expr)}
		     {ae <+ (elim-equivalence a)}
		     {be <+ (elim-equivalence b)}
		     `(and (=> ,ae ,be) (=> ,be ,ae))))
    (else `(,(operator expr) ,(elim-equivalence (arg1 expr)) ,(elim-equivalence (arg2 expr))))))



;; Eliminate the logical implications
;;
;; PHASE1 : on élimine les implications (voir livre "Premier cours de programmation avec Scheme" page 210)
;;
;; example:
;;
;;> (show (elim-implications '(or (=> p q) (not (=> q r)))))
;;? (elim-implications '(or (=> p q) (not (=> q r))))
;;--> (or (or (not p) q) (not (or (not q) r)))
;;
;; (elim-implications '(or (=> a b) (not (=> b c)))) -> '(or (or (not a) b) (not (or (not b) c)))

(define (elim-implications expr)
  (cond
   ((symbol? expr) expr)
   ((boolean? expr) expr)
   ((isNOT? expr) `(not ,(elim-implications (arg expr))))
   ((isIMPLIC? expr) `(or (not ,(elim-implications (arg1 expr))) ,(elim-implications (arg2 expr))))
   (else `(,(operator expr) ,(elim-implications (arg1 expr)) ,(elim-implications (arg2 expr))))))



(def (elim-exclusive-or expr)

     (cond

      ((symbol? expr) expr)
      ((boolean? expr) expr)
      ((isNOT? expr) `(not ,(elim-exclusive-or (arg expr))))
      ((isXOR? expr) ($+>
		      {a1 <+ (arg1 expr)}
		      {a2 <+ (arg2 expr)}
		      {ea1 <+ (elim-exclusive-or a1)}
		      {ea2 <+ (elim-exclusive-or a2)}
		      `{{(not ,ea1) and ,ea2} or {,ea1 and (not ,ea2)}}))

      (else `(,(operator expr) ,(elim-exclusive-or (arg1 expr)) ,(elim-exclusive-or (arg2 expr))))))



;; Moving in the negation to the leaves of tree
;;
;; PHASE 2 : on fait rentrer les négations jusqu'aux feuilles de l’arbre
;; on ne s’occupe plus des implications !
;;
;; (move-in-negations '(not (not (not p)))) --> (not p)
;;
;; (move-in-negations '(not (or (not (and a b)) b))) --> '(and (and a b) (not b))



;; Moving in the negation to the leaves of tree
;;
;; PHASE 2 : on fait rentrer les négations jusqu'aux feuilles de l’arbre
;; on ne s’occupe plus des implications !
;;
;; (move-in-negations '(not (not (not p)))) --> (not p)
;;
;; (move-in-negations '(not (or (not (and a b)) b))) --> '(and (and a b) (not b))

(define (move-in-negations expr)
  (cond
   ((or (symbol? expr) ; symbol , ex: 'a
	(boolean? expr)) expr) ; boolean , ex: #f

   ;; this case is now studied in *
   ;;((and (isNOT? expr) (boolean? (arg expr))) expr)

   ((isNOT? expr)

    (let
	((p (arg expr))) ; expr = not (p)

      (cond

       ((or (symbol? p)
	    (boolean? p)) ;; *
	expr)

       ((isNOT? p) (move-in-negations (arg p))) ; expr = not p with p = not s, so expr = not(not (s)) = s

       ((isAND? p) ;; not(a and b) = not(a) or not(b)
	(let
	    ((a (arg1 p))
	     (b (arg2 p)))
	  `(or ,(move-in-negations `(not ,a)) ,(move-in-negations `(not ,b)))))

       ((isOR? p) ;; not(a or b) = not(a) and not(b)
	(let
	    ((a (arg1 p))
	     (b (arg2 p)))
	  `(and ,(move-in-negations `(not ,a)) ,(move-in-negations `(not ,b)))))

	(else (error  "Bad syntax (inner)" expr)))))

   ((isOR-AND? expr) ; (op p q)
    (let
	((op (operator expr))
	 (p (arg1 expr))
	 (q (arg2 expr)))
      `(,op ,(move-in-negations p) ,(move-in-negations q))))
    (else (error  "Bad syntax" expr))))



(define (distribute-and-over-or expr1 expr2)    ; expr1 et expr2 sont les arguments d'un 'and : ('and expr1 expr2)
  ;; remember we have (expr1 and expr2) to distribute over the "or"
  (cond
   ((isOR? expr1)               ; (expr1 expr2) <--> ( ('or p q) r )
    (let ((p (arg1 expr1))
	  (q (arg2 expr1))
	  (r expr2))
      `(or ,(distribute-and-over-or p r) ,(distribute-and-over-or q r)))) ; (p or q) and r = (p and r) or (q and r)
   ((isOR? expr2) ;  (expr1 expr2) <--> ( p ('or q r) )
    (let ((p expr1)
	  (q (arg1 expr2))
	  (r (arg2 expr2)))
      `(or ,(distribute-and-over-or p q) ,(distribute-and-over-or p r)))) ; p and (q or r) = (p and q) or (p and r)
   (else `(and ,expr1 ,expr2)))) ; else we create the expression ('and expr1 expr2)


;; we make the 'or going out by distributing them over the 'and
;;
;; PHASE 3 : on fait au contraire sortir les 'or en distribuant les 'and
;; on ne s'occupe plus des négations !

(define (phase3-dnf expr)

  ;;(debug-mode-off)
  (when debug-mode
    (display "phase3-dnf : ")
    (dv expr))


  (cond
   ((isOR? expr)
    (let ((p (arg1 expr))
	  (q (arg2 expr)))
      `(or ,(phase3-dnf p) ,(phase3-dnf q))))
   ((isAND? expr)
    (let ((p (arg1 expr))
	  (q (arg2 expr)))
      (distribute-and-over-or (phase3-dnf p) (phase3-dnf q))))
   (else expr))) ; else we leave it unchanged (could be atom, not(x),... )





;; simplify-negation in a classic recursive way
;; (simplify-negation '(not #t)) -> #f
;; (simplify-negation '(not (not #t))) -> #t
(define (simplify-negation expr)
  (cond
   ((symbol? expr) expr) ; symbol , ex: 'a
   ((boolean? expr) expr) ; boolean , ex: #f
   ((isNOT? expr) ; (not p)
    (let ((p (simplify-negation (arg1 expr)))) ; simplify p
      (cond
       ((is-True?  p) #f) ; !#t = #f
       ((is-False? p) #t)    ; !#f = #t
       (else `(not ,p))))) ; (not p)
					;; simplify (op p q)
   ((isOR-AND? expr) `(,(operator expr) ,(simplify-negation (arg1 expr)) ,(simplify-negation (arg2 expr))))))

;; simplify expression A ^ F or A ^ T or A v T ....

;; (prefix->infix-symb (simplify-logic (n-arity (simplify-OR (simplify-AND (phase3-dnf (simplify-negation (move-in-negations (elim-implications  '(or (and Cin (not (or (and A (not #t)) (and (not A) #t)))) (and (not Cin) (or (and A (not #t)) (and (not A) #t))))))))))))) -> '((A ^ Cin) v (!A ^ !Cin))
(define (simplify-OR expr)
  (cond
   ((symbol? expr) expr) ; symbol , ex: 'a
   ((boolean? expr) expr) ; boolean , ex: #f
   ((isNOT? expr) `(not ,(simplify-OR (arg expr)))) ; (not p)
   ((isAND? expr) `(and ,(simplify-OR (arg1 expr)) ,(simplify-OR (arg2 expr)))) ;  and
   ((isOR? expr) ; (or p q)
    (let ((p-simp (simplify-OR (arg1 expr)))) ; p simplified
      (if (is-True? p-simp) ; (or #t q)
	  #t ; tautology
	  (let ((q-simp (simplify-OR (arg2 expr)))) ; q simplified
	    (if (is-True? q-simp) ;  (or p #t)
		#t ; tautology
		(cond
		 ((is-False? p-simp) q-simp) ; (or #f q)
		 ((is-False? q-simp) p-simp) ; (or p #f)
		 (else `(or ,p-simp ,q-simp)))))))))) ; (or p q)


;; (prefix->infix-symb (simplify-logic (n-arity (simplify-OR (simplify-AND (phase3-dnf (simplify-negation (move-in-negations (elim-implications  '(or (and Cin (not (or (and A (not #t)) (and (not A) #t)))) (and (not Cin) (or (and A (not #t)) (and (not A) #t))))))))))))) -> '((A ^ Cin) v (!A ^ !Cin))
(define (simplify-AND expr)
  (cond
   ((symbol? expr) expr) ; symbol , ex: 'a
   ((boolean? expr) expr) ; boolean , ex: #f
   ((isNOT? expr) `(not ,(simplify-AND (arg expr)))) ; (not p)
   ((isOR? expr) `(or ,(simplify-AND (arg1 expr)) ,(simplify-AND (arg2 expr)))) ;  or
   ((isAND? expr) ; (and p q)
    (let ((p-simp (simplify-AND (arg1 expr)))) ; p simplified
      (if (is-False? p-simp) ; (and #f q)
	  #f ; antilogy
	  (let ((q-simp (simplify-AND (arg2 expr)))) ; q simplified
	    (if (is-False? q-simp) ;  (and p #f)
		#f ; antilogy
		(cond
		 ((is-True? p-simp) q-simp) ; (and #t q)
		 ((is-True? q-simp) p-simp) ; (and p #t)
		 (else `(and ,p-simp ,q-simp)))))))))) ; (and p q)


;; (dnf '(not (or (not (and a b)) b)))  --> '(and (and a b) (not b))
;; Warning : DNF takes binary expressions, NOT n-arity expressions
(define (dnf expr)    ; disjunctive normal form
  ;; (dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))
  ;; -> '(or (or (or (and c (and (not a) a)) (and c (and (not a) (not b)))) (or (and c (and b a)) (and c (and b (not b)))))
  ;;   (or (and (not c) (and a (not b))) (and (not c) (and (not a) b))))
  ;;
  ;; (dnf '(or (and Cin (not (or (and A (not #t)) (and (not A) #t)))) (and (not Cin) (or (and A (not #t)) (and (not A) #t)))))
  ;; -> '(or (or (or (and Cin (and (not A) A)) (and Cin (and (not A) (not #t)))) (or (and Cin (and #t A)) (and Cin (and #t (not #t)))))
  ;;         (or (and (not Cin) (and A (not #t))) (and (not Cin) (and (not A) #t))))

  (phase3-dnf (move-in-negations (elim-exclusive-or (elim-implications  (elim-equivalence expr))))))


;; simplify NF of forms :
;; ((a ^ b) v a) -> a
;; (a v b) ^ a -> a

(define (simplify-NF-by-unitary-reduction expr)

  (nodebug
   (display "simplify-NF-by-unitary-reduction : ")
   (dv expr))

  (declare result);; result-sorted)

  (cond
   ((null? expr) {result <- expr})
   ((is-simple-form? expr) {result <- expr})
   (else
    (let* ((oper (operator expr))
	   (sL (args expr)) ; extract the arguments of operation
	   (encaps (λ (expression)  ; put any remaining element in a set (list)
		     (if (symbol? expression)
			 (list expression)
			 expression)))
	   (encaps-args (map encaps sL)) ;; encapsulate any remaining isolated elements in the list in another list
	   (reducted-args (parse-args-by-unitary-reduction encaps-args encaps-args)) ; do unitary reduction
	   (decaps (λ (expression) ; extract any element from a list
		     (if (singleton? expression)
			 (first expression)
			 expression)))
	   (decaps-args (map decaps reducted-args)))

      (if (only-one? decaps-args)
	  {result <- (first decaps-args)}
	  {result <- (cons oper decaps-args)})))) ;; reconstruct operation expression with simplified arguments list

  (nodebug
   (display "simplify-NF-by-unitary-reduction : ")
   (dv result))

  ;;{result-sorted <- (sort-expressions-in-operation result)} ;; REMOVE

  (nodebug
   (display "simplify-NF-by-unitary-reduction : ")
   (dv result-sorted))

  ;;result-sorted)
  result)



;; simplify DNF of form ((a ^ b) v a) -> a
;; DEPRECATED,replaced by simplify-NF-by-unitary-reduction
;;
;; simplify a prefixed n-arity expression
;;
;;
;; (simplify-DNF-by-unitary-reduction '(or (and a (not b)) (and a b) (and b d) (and a e (not b)) (and a (not b) c)))
;;  -> (or (and b d) (and a b) (and a (not b)))
;;
;; (simplify-DNF-by-unitary-reduction '(or (and a (not b)) (and a b) a (and b d) (and a e (not b)) (and a (not b) c)))
;;  -> '(or (and b d) a)
;;
;; (simplify-DNF-by-unitary-reduction '(or (a (not b)) (a b) (b d) (a e (not b)) (a (not b) c))) -> (or (b d) (a b) (a (not b)))
;; (simplify-DNF-by-unitary-reduction '(or a (and a (not b)) (and a b) (and b d) (and a e (not b)) (and a (not b) c)))
;;  -> '(or (and b d) a)
;;
;; (simplify-DNF-by-unitary-reduction '(or (and a b) a)) -> 'a
;;
(define (simplify-DNF-by-unitary-reduction expr)
  (cond
   ((null? expr) expr)
   ((is-simple-form? expr) expr)
   (else
    (let* ((sL (args expr)) ; extract the arguments of 'or
	   (encaps (λ (expression)  ; put any remaining element in a set (list)
		     (if (symbol? expression)
			 (list expression)
			 expression)))
	   (encaps-args (map encaps sL))
	   (reducted-args (parse-args-by-unitary-reduction encaps-args encaps-args)) ; do unitary reduction
	   (decaps (λ (expression) ; extract any element from a list
		     (if (singleton? expression)
			 (first expression)
			 expression)))
	   (decaps-args (map decaps reducted-args)))
      (if (only-one? decaps-args)
	  (first decaps-args)
	  (cons 'or decaps-args)))))) ;; reconstruct 'or expression with simplified arguments list




;; parse-args-by-unitary-reduction = G
;;
;; algo :
;;
;; (C- means "is element of", !C- means "is not element of ")
;;
;; start with G(sL,sL)
;;
;; G(sL,W) :
;; s C- W  -> G(L,s.F(s,W))
;; s !C- W -> G(L,W)
;; G(0,W)  -> W
;;
;; meaning: for an OR expression it is useless to keep AND expressions that are already satisfied:
;; example : (and a (not b)) , (and a e (not b)) if (and a (not b)) is satisfied then (and a e (not b)) is useless in expression
;; whatever the value of e is
;; the same apply with ANDed of ORed expressions too !!!
;;
;; (parse-args-by-unitary-reduction '((and a (not b)) (and a b) (and b d) (and a e (not b)) (and a (not b) c)) '((and a (not b)) (and a b) (and b d) (and a e (not b)) (and a (not b) c)))
;;   -> '((and b d) (and a b) (and a (not b)))
;;
;; (parse-args-by-unitary-reduction '((and a (not b)) (and a b) (a) (and b d) (and a e (not b)) (and a (not b) c)) '((and a (not b)) (and a b) (a) (and b d) (and a e (not b)) (and a (not b) c))) -> '((and b d) (a))
;;
;;  (parse-args-by-unitary-reduction '((a) (and a (not b)) (and a b) (and b d) (and a e (not b)) (and a (not b) c)) '((a) (and a (not b)) (and a b) (and b d) (and a e (not b)) (and a (not b) c))) -> ((and b d) (a))
;;
;; (parse-args-by-unitary-reduction '((a) (a (not b)) (a b) (b d) (a e (not b)) (a (not b) c)) '((a) (a (not b)) (a b) (b d) (a e (not b)) (a (not b) c))) -> ((b d) (a))
(define (parse-args-by-unitary-reduction sL W)
  (if (null? sL)
      W ;; G(0,W)  -> W
      (let* ((s (first sL))
	     (L (rest sL))
	     (element (member s W))) ;; s C-? W
	(if element ;; s C-? W
	    (let ((F (unitary-reduction s W)))
	      (parse-args-by-unitary-reduction L (cons s F))) ;; s C- W  : G(sL,W) -> G(L,s.F(s,W))
	    (parse-args-by-unitary-reduction L W)))))         ;; s !C- W : G(sL,W) -> G(L,W)

;; (define (parse-args-by-unitary-reduction sL W)
;;   (if (null? sL)
;;       W ; G(0,W)  -> W
;;       (let* ((s (first sL))
;; 	    (L (rest sL))
;; 	    (element (member s W))) ; s C-? W
;; 	(begin
;; 	  (display "s=") (display s) (display "\n")
;; 	  (display "W=") (display W) (display "\n")
;; 	  (display "element=") (display element) (display "\n")
;; 	  (if element ; s C-? W
;; 	      (let ((F (unitary-reduction s W)))
;; 		(begin
;; 		  (display "F =") (display F) (display "\n")
;; 		  (parse-args-by-unitary-reduction L (cons s F)))) ; s C- W  : G(sL,W) -> G(L,s.F(s,W))
;; 	      (begin
;; 		(display "L=") (display L) (display "\n")
;; 		(parse-args-by-unitary-reduction L W)))))))           ; s !C- W : G(sL,W) -> G(L,W)




;; unitary-reduction of sL by k
;; will remove all element of sL containing k
;;
;; (unitary-reduction '(a) '((a b) (b d) (a b c))) -> ((b d))
;; (unitary-reduction '(a (not b)) '((a b) (b d) (a e (not b)) (a (not b) c))) -> ((a b) (b d))
;; (unitary-reduction '(and a (not b)) '((and a b) (and b d) (and a e (not b)) (and a (not b) c))) -> ((and a b) (and b d))
;; (unitary-reduction '(a) '((a b) (b d) (a b c) (a))) -> ((b d))
;; (unitary-reduction '(a) '((a) (a (not b)) (a b) (b d) (a e (not b)) (a (not b) c))) -> ((b d))
;; (unitary-reduction '(z) '((a b) (b d) (a b c))) -> ((a b) (b d) (a b c))
;;
;; algo:
;; sL = {} -> {}
;; k C s  -> F(k,L)
;; k !C s -> s.F(k,L)
(define (unitary-reduction k sL) ;;; unitary-reduction = F
  (if (null? sL) ;;  sL = 0
      sL
      (let* ((s (first sL))
	    (L (rest sL))
	    (F (unitary-reduction k L)))
	(if (include? k s) ;;       C means "include in", !C means "not include in"
	    F  ;; k C s -> F(k,L)
	    (cons s F))))) ;; k !C s -> s.F(k,L)




;; (enlight-dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))) -> (a^b^c)v(!a^!b^c)v(!a^b^!c)v(a^!b^!c)
(define (enlight-dnf expr)
  ;;(compact-display-bracket (prefix->infix-symb  (simplify-DNF-by-unitary-reduction (simplify-logic (n-arity (simplify-OR (simplify-AND (dnf expr)))))))))
  (compact-display-bracket (prefix->infix-symb  (simplify-NF-by-unitary-reduction (simplify-logic (n-arity (simplify-OR (simplify-AND (dnf expr)))))))))




;; put an expression in DNF in infix with symbols and all the simplifications
;;
;; with DrRacket:
;;  (dnf-infix-symb '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))) -> '((!a ^ !b ^ c) v (a ^ b ^ c) v (a ^ !b ^ !c) v (!a ^ b ^ !c))
;;
;; with MIT Scheme:
;; (dnf-infix-symb '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))) -> ((!a ^ b ^ !ci) v (a ^ !b ^ !ci) v (a ^ b ^ ci) v (!a ^ !b ^ ci))
;;
;; (dnf-infix-symb '(((p . and . q) . => . r) . and . ((not (p . and . q)) . => . r))) -> 'r
(define (dnf-infix-symb expr)
  (prefix->infix-symb (dnf-n-arity-simp expr)))

(define (dnf-infix expr)
  (prefix->infix (dnf-n-arity-simp expr)))

;; put expression in DNF in n-arity and simplified
;;
;;  (dnf-n-arity-simp '(or (and (and a b) (not (and c (or (and a (not b)) (and (not a) b))))) (and (not (and a b)) (and c (or (and a (not b)) (and (not a) b)))))) -> '(or (and a b) (and (not a) b c) (and a (not b) c))
;;
;; (dnf-n-arity-simp '(and (or b c d) (or a c (not d)) (or (not b) (not c))))
;; '(or (and b (not c) (not d)) (and (not b) c) (and a b (not c)) (and a (not b) d) (and a (not c) d))
(define (dnf-n-arity-simp expr)
  ;;(simplify-DNF-by-unitary-reduction (simplify-logic (n-arity (simplify-OR (simplify-AND (dnf (n-arity-operation->binary-operation expr))))))))
  (simplify-NF-by-unitary-reduction (simplify-logic (n-arity (simplify-OR (simplify-AND (dnf (n-arity-operation->binary-operation expr))))))))

;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and a b c (not d))))
;; '((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))
(define (infix-symb-min-dnf expr)
  (prefix->infix-symb  (minimal-dnf expr)))

;; (infix-symb-bool-min-dnf '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}} )
;; ((b̅ · c̅) ➕ (c · d̅) ➕ (a̅ · b · d))
(define (infix-symb-bool-min-dnf expr)
  (prefix->infix-symb-bool  (minimal-dnf expr)))


;; (infix-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) c (not d)) (and c d) (and a c (not d)) (and a b c d) (and a  (not c))))
;; '((!b and !c and !d) or (c and d) or (b and d) or a)
(define (infix-min-dnf expr)
  (prefix->infix  (minimal-dnf expr)))



;; put an expression in CNF in infix with symbols and all the simplifications
;;  (cnf-infix-symb '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))) -> '((a v b v c) ^ (!a v !b v c) ^ (!a v b v !c) ^ (a v !b v !c))


(define (cnf-infix-symb expr)
  (prefix->infix-symb (cnf-n-arity-simp expr)))

;;(cnf-infix '(or (and b (not c) (not d)) (and (not b) c) (and a b (not c)) (and a (not b) d) (and a (not c) d)))
;;'((b or c or d) and (!b or !c) and (a or b or c) and (a or !b or !d) and (a or c or !d))
;; TODO: minimize the CNF above
(define (cnf-infix expr)
  (prefix->infix (cnf-n-arity-simp expr)))

(define (cnf-n-arity-simp expr)
   ;;  added sorting
  (sort-expressions-in-operation-var-index (simplify-NF-by-unitary-reduction (simplify-logic (n-arity (simplify-AND (simplify-OR (cnf (n-arity-operation->binary-operation expr)))))))))

;; (define (cnf-prefix expr)
;;   (phase3-cnf (move-in-negations (elim-exclusive-or (elim-implications  (elim-equivalence  (n-arity-operation->binary-operation expr)))))))





;; return the literal of expression of type (not 'a) , 'a
;; in case of boolean return : #f -> F , #t -> T
(define (get-literal expr)
  (if (isNOT? expr)
      (first (rest expr)) ;; WARNING: we should test and return T or F perheaps , not #t #f
      (if (boolean? expr)
	  (if expr 'T 'F)
	  expr)))

;; return the literal of expression of type (not 'a) , 'a
;; or return the first literal of a binary expression (OR / AND)
;; (get-first-literal '(or a b)) -> 'a
;; (get-first-literal '(or (not a) b)) -> 'a
(define (get-first-literal expr)
  (if (isOR-AND? expr)
      (get-literal (first (rest expr)))
      (get-literal expr)))


;; search-not-lit
;; test if we have (not x)
;; lit : literal example: 'x 'b
;; lep : list of expressions ,example : '(a b (not x) c x (not a))
;;
;; (search-not-lit? 'c '(a b (not x) c x (not a))) -> #f
;;
;; (search-not-lit? 'x '(a b (not x) c x (not a))) -> '((not x) c x (not a))
;;
(define (search-not-lit? lit lep) ;; inputs are a literal and a list of expressions
  (member (list 'not lit) lep))

;; input : get an AND example: (AND x (not x) y z)
;; will check all operands of AND to see if there is an antilogy in it
;;
;; > (is-AND-antilogy? '(and c (not a) a)) -> #t
;; > (is-AND-antilogy? '(and (not c) a (not b))) -> #f
;; (define (is-AND-antilogy? expr)
;;   (letrec ((lep (args expr)) ;; list of expressions which could be literals (ex 'b , 'x ) or negations (not (b))
;;            (detect-antilogy (λ (listExpr)
;;                               (if (null? listExpr)
;;                                   #f
;;                                   (if (symbol? (first listExpr)) ;; we search for a literal ex: 'x
;;                                       (if (search-not-lit? (first listExpr) lep) ;; search antilogy with literal and the whole operands of AND
;;                                           #t
;;                                           (detect-antilogy (rest listExpr))) ;; check the rest of the list with another literal
;; 				      (detect-antilogy (rest listExpr))))))) ;; check the rest of the list to find a literal
;;     (detect-antilogy lep)))





;; input : get an AND example: (AND x (not x) y z)
;; will check all operands of AND to see if there is an antilogy in it
;;
;; > (is-AND-antilogy? '(and c (not a) a)) -> #t
;; > (is-AND-antilogy? '(and (not c) a (not b))) -> #f
;; (is-AND-antilogy?  '(and (not c) F))
(define (is-AND-antilogy? expr)

  {lep <+ (args expr)}  ;; list of expressions which could be literals (ex 'b , 'x ) or negations (not (b))
  {detect-antilogy <+ (λ (listExpr)
			(condx ((null? listExpr) #f)
			       (exec {fst <+ (first listExpr)})
			       ((is-False? fst) #t)
			       ;; now we search for a literal ex: 'x
			       ((symbol? fst) (if (search-not-lit? fst lep) ;; search antilogy with literal in the whole operands of AND
						  #t
						  (detect-antilogy (rest listExpr)))) ;; check the rest of the list with another literal
			       (else (detect-antilogy (rest listExpr)))))} ;; check the rest of the list to find a literal
  (detect-antilogy lep))


;; remove the antilogies out of a list of ANDed expressions and also remove useless 'T.
;; (remove-antilogies '((and c (not a) a) (and c (not a) (not b)))) -> '((and c (not a) (not b)))
;;  (remove-antilogies '()) -> '()
;; (remove-antilogies '(c (and a b (not b)))) -> '(c)
;;
;; (define (remove-antilogies andList) ;; argument is a list of ANDed expressions
;;   (cond
;;    ((null? andList) '())
;;    ({(isAND? (first andList)) and (is-AND-antilogy? (first andList))} (remove-antilogies (rest andList)))
;;    (else (cons (first andList) (remove-antilogies (rest andList))))))

(def (remove-antilogies-and-useless-true andList) ;; argument is a list of ANDed expressions

     (when (null? andList) (return '()))

     {fst <+ (first andList)}

     (when (isAND? fst)

       {fst <- (filter (lambda (x) (not (is-True? x))) fst)} ;; remove useless 'True

       (case (length fst)
	 ((0) (error "List of null length found in first element of argument of function." andList))
	 ((1) (return (cons 'T (remove-antilogies-and-useless-true (rest andList))))) ;; (and) remaining ! keep a 'T and continue with the rest
	 ((2) {fst <- (second fst)}))) ;; (and x) remaining !


     (if {(isAND? fst) and (is-AND-antilogy? fst)}
	 (remove-antilogies-and-useless-true (rest andList))
	 (cons fst (remove-antilogies-and-useless-true (rest andList)))))





;; input : get an OR example: (OR x (not x) y z)
;; will check all operands of OR to see if there is a tautology in it
;;
;; >  (is-OR-tautology? '(or a (not b) (not b) (not a))) -> #t
;; >  (is-OR-tautology? '(or a (not b) (not c))) -> #f
;; (define (is-OR-tautology? expr)
;;   (letrec ((lep (args expr)) ;; list of expressions which could be literals (ex 'b , 'x ) or negations (not (b))
;;            (detect-tautology (λ (listExpr)
;;                                (if (null? listExpr)
;;                                   #f
;;                                   (if (symbol? (first listExpr)) ;; we search for a literal ex: 'x
;;                                      (if (search-not-lit? (first listExpr) lep) ;; search tautology with literal and the whole operands of OR
;;                                         #t
;;                                         (detect-tautology (rest listExpr))) ;; check the rest of the list with another literal
;;                                      (detect-tautology (rest listExpr))))))) ;; check the rest of the list to find a literal
;;     (detect-tautology lep)))


(define (is-OR-tautology? expr)

  {lep <+ (args expr)}  ;; list of expressions which could be literals (ex 'b , 'x ) or negations (not (b))
  {detect-tautology <+ (λ (listExpr)
			(condx ((null? listExpr) #f)
			       (exec {fst <+ (first listExpr)})
			       ((is-True? fst) #t)
			       ;; now we search for a literal ex: 'x
			       ((symbol? fst) (if (search-not-lit? fst lep) ;; search tautology with literal in the whole operands of OR
						  #t
						  (detect-tautology (rest listExpr)))) ;; check the rest of the list with another literal
			       (else (detect-tautology (rest listExpr)))))} ;; check the rest of the list to find a literal
  (detect-tautology lep))


;; remove the tautologies out of a list of ORed expressions...
;; (remove-tautologies
;;   '((or c (not c))
;;     (or c a (not a))
;;     (or c a b)
;;     (or c (not b) (not a))
;;     (or c (not b) b)
;;     (or (not a) b (not c))
;;     (or (not a) b a (not a))
;;     (or (not a) b a b)
;;     (or (not a) b (not b) (not a))
;;     (or (not a) b (not b) b)
;;     (or a (not b) (not c))
;;     (or a (not b) a (not a))
;;     (or a (not b) a b)
;;     (or a (not b) (not b) (not a))
;;     (or a (not b) (not b) b)))
;; ->  '((or c a b) (or c (not b) (not a)) (or (not a) b (not c)) (or a (not b) (not c)))
;;
;;  (remove-tautologies '()) -> '()
;; (define (remove-tautologies orList) ;; argument is a list of ORed expressions
;;   (cond
;;    ((null? orList) '())
;;    ((and (isOR? (first orList)) (is-OR-tautology? (first orList))) (remove-tautologies (rest orList)))
;;    (else (cons (first orList) (remove-tautologies (rest orList))))))


(def (remove-tautologies-and-useless-false orList) ;; argument is a list of ORed expressions

     (when (null? orList) (return '()))

     {fst <+ (first orList)}

     (when (isOR? fst)

       {fst <- (filter (lambda (x) (not (is-False? x))) fst)} ;; remove useless 'False

       (case (length fst)
	 ((0) (error "List of null length found in first element of argument of function." orList))
	 ((1) (return (cons 'F (remove-tautologies-and-useless-false (rest orList))))) ;; (or) remaining ! keep a 'F and continue with the rest
	 ((2) {fst <- (second fst)}))) ;; (or x) remaining !

     (if {(isOR? fst) and (is-OR-tautology? fst)}
	 (remove-tautologies-and-useless-false (rest orList)) ;; drop the tautologie and continue checking the rest
	 (cons fst (remove-tautologies-and-useless-false (rest orList)))))




;; simplify the expressions
;; (or e1 e2 .... eN)
;; if eI is an antilogy then remove it
;;
;; (simplify-DNF (n-arity (dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))))
;; -> '(or
;;         (and c (not a) (not b))
;;         (and c b a)
;;         (and (not c) a (not b))
;;         (and (not c) (not a) b))
;;
;; (simplify-DNF '(or (and c b)  (and a b (not b)))) -> '(and c b)
;;
;;;;  (simplify-DNF '(or c (and a b (not b)))) -> c
;;
(define (simplify-DNF dnfExpr)
  ;;(debug-mode-on)
  (when debug-mode
    (display "simplify-DNF : ")
    (dv dnfExpr))
  (if (is-OR-tautology? dnfExpr)
      #t
      (let* ((operandList0 (remove-antilogies-and-useless-true (rest dnfExpr)))  ;; first we remove antilogies in the operands
	     (operandList (filter (lambda (x) (not (is-False? x)))
				  operandList0))) ;; we remove the useless False if any from the 'or expression
	(when debug-mode
	  (dv operandList))
	(cond ((null? operandList) #f) ;; dnfExpr is composed of antilogies ,so it is an antilogie too
	      ((null? (rest operandList)) ;; if we have only one element in the result list
	       (first operandList)) ;; we can forget the or operator
	      (else (cons 'or operandList))))))




;; simplify the expressions
;; (and e1 e2 .... eN)
;; if eI is a tautology then remove it
;;
;; (simplify-CNF (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))))
;; -> '(and (or c a b) (or c (not b) (not a)) (or (not a) b (not c)) (or a (not b) (not c)))
;;
(define (simplify-CNF cnfExpr)
  (if (is-AND-antilogy? cnfExpr)
      #f
      (let* ((operandList0 (remove-tautologies-and-useless-false (rest cnfExpr))) ;; first we remove tautologies in the operands
	     (operandList (filter (lambda (x) (not (is-True? x)))
				  operandList0))) ;; we remove the useless True if any from the 'and expression
	(cond ((null? operandList) #t) ;; cnfExpr is composed of tautologies ,so it is a tautologie too
	      ((null? (rest operandList)) ;; if we have only one element in the result list
	       (first operandList)) ;; we can forget the and operator
	      (else (cons 'and operandList))))))

;; TODO faire un evaluateur booleen
;; nota : le fait d'inclure des T ou F fais deja cela partiellement

;; simplify the DNF or CNF expressions
;;
;; (simplify-*NF (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))))
;;    -> '(and (or a b c) (or (not a) (not b) c) (or (not a) b (not c)) (or a (not b) (not c)))
;;
;; (simplify-*NF '(and (or a (not a) a) (or c b c))) -> '(or b c)
;;
;;;; (simplify-*NF '(or (and c c)  (and a b (not b)))) -> 'c
;;
(define (simplify-*NF norm-form)
  ;;(debug-mode-off)
  (when debug-mode
    (display "simplify-*NF : ")
    (dv norm-form))

  ;; first we will remove duplicates and sort arguments
  (let* ((arg-lst (args norm-form)) ;; define arguments list
	 (oper (operator norm-form)) ;; define operator
	 (arg-list-no-dup (map remove-duplicates-in-operation arg-lst)) ;; argument list without duplicate elements
	 ;;(arg-list-no-dup-sorted (map sort-arguments-in-operation arg-list-no-dup));; argument list sorted REMOVE
	 (expr-no-dup-sorted (cons oper arg-list-no-dup)));(cons oper arg-list-no-dup-sorted)))

    (when debug-mode
      (dv expr-no-dup-sorted)
      )

    (if (isAND? expr-no-dup-sorted) ;;(equal? (first expr-no-dup-sorted) 'and)
	(simplify-CNF expr-no-dup-sorted)
	(simplify-DNF expr-no-dup-sorted))))

;; simplify logical expressions by searching antilogies and tautologies in sub-expressions
;; parameter : a normal form expression
;;
;; (simplify-logic (n-arity (cnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))))
;;    -> '(and (or a b c) (or (not a) (not b) c) (or (not a) b (not c)) (or a (not b) (not c)))
;;
;; (simplify-logic '(and b a b (not b))) -> #f
;;
;; (simplify-logic '(and (or ci b) a (or a a))) -> '(and a (or b ci))
;;
;;  (simplify-logic '(and (or (not ci) b) d (not d) (or a a))) -> #f
;;
;; (simplify-logic '(and (or (not ci) b) (or d  d)  d (or (not d) (not d)) (or a a))) -> #f
(define (simplify-logic expr)
  ;;(debug-mode-on)
  (when debug-mode
    (display "simplify-logic : ")
    (dv expr)
    )
  (cond
   ((null? expr) expr)
   ((symbol? expr) expr)
   ((is-simple-form? expr) (simplify-SF expr))
   ;;(else (sort-arguments-in-operation (remove-duplicates-in-operation (simplify-*NF expr)))))) ;; REMOVE sort
   (else (remove-duplicates-in-operation (simplify-*NF expr)))))


;; simplify Single Form
;; (simplify-SF  '(and b a b (not b))) -> #f
(def (simplify-SF expr)
     (when {(boolean? expr) or (symbol? expr)}
	   (return expr))
     (let* ((oper (operator expr)) ;; define operator
	    (expr-no-dup (remove-duplicates-in-operation expr)) ;; remove duplicate symbols
	    )
	    ;; (expr-no-dup-sorted (sort-arguments-in-operation expr-no-dup))) ;; sort variables REMOVE
       ;; (if (AND-op? oper) ;;(equal? oper 'and) ;; AND => search for antilogies
       ;; 	   (if (is-AND-antilogy? expr-no-dup-sorted) #f expr-no-dup-sorted)
       ;; 	   (if (is-OR-tautology? expr-no-dup-sorted) #t expr-no-dup-sorted)))) ;;  OR => search for tautologies


       (if (AND-op? oper) ;;(equal? oper 'and) ;; AND => search for antilogies
	   (if (is-AND-antilogy? expr-no-dup) #f expr-no-dup)
	   (if (is-OR-tautology? expr-no-dup) #t expr-no-dup)))) ;;  OR => search for tautologies



(define lower-literal-symbol
  (λ (s)
    (string-downcase (expression->string (get-first-literal s))))) ;; 'Ci -> "ci", '(not A) -> "a"

;; calls diagram: sort-arguments-in-operation -> sort-arguments -> expression<?
;; sort-arguments used by minimal-dnf, Quine-Mc-Cluskey
;; expression<? is used indirecly in a lot of procedures
(define expression<?
  (λ (x y)
    (string<? (lower-literal-symbol x) (lower-literal-symbol y))))


(define expression-var-index<?
  (λ (x y)
    (var-index<? (lower-literal-symbol x) (lower-literal-symbol y)))) ;; breaks things cnf example do not seems to finish

;; DEPRECATED (no call)
;; (define expression-literal-first<?
;;   (λ (x y)
;;     (cond ((isOR-AND? x) #f)
;; 	  ((isOR-AND? y) #t)
;; 	  ;;(else (string<? (lower-literal-symbol x) (lower-literal-symbol y))))))
;; 	  (else (var-index<? (lower-literal-symbol x) (lower-literal-symbol y))))))

(define-syntax debug-var-index<?-name
  (syntax-rules ()
    ((_) (nodebug
	  (display-nl "var-index<? :")))))


;; scheme@(guile-user)> (var-index<? "c1" "c2")
;; #t
;; scheme@(guile-user)> (var-index<? "c12" "c2")
;; #f
;; scheme@(guile-user)> (var-index<? "c12" "c1")
;; #f
;; (var-index<? "C10" "C9")
;; #f

;; (var-index<? "C9" "C10")
;;  #t

(def (var-index<? v1 v2)

     ;;(debug-var-index<?-name)

     ;; TODO: modify v1 and v2 to translate subscript symbols and numbers in normal ones

     {re <+ "^([A-Za-z]+)([-+₋₊]?[0123456789₀₁₂₃₄₅₆₇₈₉]+)$"}

     ;; note that the one below works too in Guile but not in Racket (does not recognize subscript numbers)
     ;; "^([A-Za-z]+)([-₋]?[0-9]+)$"} ;; "^([A-Za-z]+)([0-9]+)$"}

     {v1m <+ (regexp-match re v1)}

     ;; when not a form like 'c1 we deal the normal way with string<?
     (unless v1m
       (return (string<? v1 v2)))

     {v2m <+ (regexp-match re v2)}

     (unless v2m
       (return (string<? v1 v2)))

     (nodebug
      (dv v1m)
      (dv v2m))

     {var1 <+ (second v1m)}
     {var2 <+ (second v2m)}

     (nodebug
      (dv var1)
      (dv var2))

     {str-equal <+ (string=? var1 var2)}

     (nodebug
      (dv str-equal))

     (unless str-equal (return (string<? var1 var2)))

     ;; possible subscript characters
     {str-index-subscript1 <+ (third v1m)}
     {str-index-subscript2 <+ (third v2m)}

     {str-index1 <+ (string-subscript-number->string-number str-index-subscript1)}
     {str-index2 <+ (string-subscript-number->string-number str-index-subscript2)}

     {index1 <+ (string->number str-index1)}
     {index2 <+ (string->number str-index2)}

     (nodebug
      (dv index1)
      (dv index2))

     {index1 < index2})


(define (symbol-var-index<? symb1 symb2)
  (var-index<? (symbol->string symb1)
	       (symbol->string symb2)))


(define expression->string
	  (λ (expr2)
	    (cond ((symbol? expr2) (symbol->string expr2))
		  ((boolean? expr2) (if expr2 "T" "F")) ;; #t -> "T", #f -> "F"
		  (else (error "expression->string: do not know how to handle this expression" expr2)))))


;; compare logical expressions args
;; warning : works with presorted lists!!!
;; scheme@(guile-user)> (compare-list-args<? '(A B C) '(B C))
;; #t
;; scheme@(guile-user)> (compare-list-args<? '(A B C) '(A B C))
;; #t
;; scheme@(guile-user)> (compare-list-args<? '(A B C) '(A B C D))
;; #t
;; scheme@(guile-user)> (compare-list-args<? '(A B C D E) '(A B C D))
;; #f
;; scheme@(guile-user)> (compare-list-args<? '(B C D E F) '(A B C D))
;; #f
;; scheme@(guile-user)> (compare-list-args<? '(B C D E F) '((not A) B C D))
;; #f
;; scheme@(guile-user)> (compare-list-args<? '(B C D E F) '((not A) (not B) C D))
;; #f
;; scheme@(guile-user)> (compare-list-args<? '(B C D E F) '((not B) C D))
;; #t

;;  call diagram : sort-expressions -> expression-literal-first-negation-tested<? -> compare-list-args<?
;;  multiple calls by other procedures

(define (compare-list-args<? L1 L2)
  (cond ((null? L1) #t)
	((null? L2) #f)
	(else (if (equal? (first L1) (first L2))
		  (compare-list-args<? (rest L1) (rest L2))
		  ($+> ;; something is not equal (not ...) ?
		   {fl1 <+ (first L1)}
		   {fl2 <+ (first L2)}
		   {lit1 <+ (expression->string (get-first-literal fl1))}
		   {lit2 <+ (expression->string (get-first-literal fl2))}
		   (if (equal? lit1 lit2)
		       (isNOT? fl2) ;; 'a '(not a)
		       ;;(string<? lit1 lit2)))))))
		       (var-index<? lit1 lit2))))))) ;; possibly BUG


;; calls diagram : Petrick -> sort-arguments-in-operation-most-little-literal-first -> expression-most-little-literal-first<?
(define expression-most-little-literal-first<?
  (λ (x y)
    (cond ((isOR-AND? x) #f)
	  ((isOR-AND? y) #t)
	  (else (literal<? (get-first-literal x) (get-first-literal y))))))


;; compare expressions
;; expression-literal-first-negation-tested<? -> compare-list-args<?
;;                                            -> expression-negation-tested<?

;; calls diagram : minimal-dnf -> sort-expressions-in-operation -> sort-expressions -> sort with expression-literal-first-negation-tested<?
;;                                                              -> sort-arguments-in-operation -> sort-arguments
(define expression-literal-first-negation-tested<?
  (λ (x y)
    (nodebug
     (display "expression-literal-first-negation-tested<? : ")
     (dv x)
     (dv y))
    (cond ({(isOR-AND? x) and (isOR-AND? y)} (compare-list-args<? (rest x) (rest y)))
	  ((isOR-AND? x) #f)
	  ((isOR-AND? y) #t)
	  (else (expression-negation-tested<? x y)))))


(define (expression-negation-tested<? a b)

  (nodebug
   (display  "expression-negation-tested<? : ")
   (dv a)
   (dv b))

  (if {(not (isNOT? a)) and (isNOT? b) and (equal? a (get-first-literal b))} ;; 'a '(not a)
      #t ;; we choose the litteral, not the negation
      ;;(string<?
      (var-index<? ;; possibly BUG
       (expression->string (get-first-literal a))
       (expression->string (get-first-literal b))))) ;; 'a '(not a)


;; DEPRECATED
(define (literal-negation-tested<? a b)

  (nodebug
   (display  "literal-negation-tested<? : ")
   (dv a)
   (dv b))

  (if {(not (isNOT? a)) and (isNOT? b) and (equal? a (get-first-literal b))} ;; 'a '(not a)
      #t
      (literal<? (get-first-literal a) (get-first-literal b)))) ;; '(not a) 'a ....


;; (literal<? 'V0x10x11 'V1x11x10) -> #f
(define (literal<? a b)
  (let* ((ax 0)
	 (bx 0)
	 (a1 0)
	 (b1 0)
	 ;;(a0 0)
	 ;;(b0 0)
	 (as (expression->string a)) ;;(symbol->string a))
	 (bs (expression->string b))) ;;(symbol->string b)))

    (for-basic (i 1 (- (string-length as) 1))

	 (case {as[i]} ;;(string-ref as i)

	   ((#\x) (incf ax))
	   ((#\1) (incf a1))
	   ;;((#\0) (incf a0))
	   )

	 (case {bs[i]} ;;(string-ref bs i)
	   ((#\x) (incf bx))
	   ((#\1) (incf b1))
	   ;;((#\0) (incf b0))
	   ))

    (cond ((> ax bx) #t)
	  ((< ax bx) #f)
	  (else ;; ax = bx
	   (> a1 b1)))))




;; sort operands in a logic expression
;; (sort-arguments-in-operation '(or c a b)) -> '(or a b c)
;; (sort-arguments-in-operation '(or c (not a) b)) -> '(or (not a) b c)
;; (sort-arguments-in-operation '(or c (not a) b (or c d))) -> '(or (not a) b (or c d) c)
(define (sort-arguments-in-operation expr)

  (if (isOR-AND? expr)

      (let* ((args-list (args expr)) ;;'(or c a b) -> '(c a b)

	     (sorted-args (sort-arguments args-list))
	     (oper (operator expr))) ;; define operator : (or Ci a b) -> or

	(cons oper sorted-args))

      expr)) ;; we have not a binary operator but a literal or negation of literal

(define (sort-arguments-in-operation-var-index expr)

  (if (isOR-AND? expr)

      (let* ((args-list (args expr)) ;;'(or c a b) -> '(c a b)

	     (sorted-args (sort-arguments-var-index args-list))
	     (oper (operator expr))) ;; define operator : (or Ci a b) -> or

	(cons oper sorted-args))

      expr)) ;; we have not a binary operator but a literal or negation of literal


;; sort this:
;; (sort-expressions-in-operation (or (and A Ci) (and A B) (and B Ci)))
;; (or (and A B) (and A Ci) (and B Ci))
;; (sort-expressions-in-operation '(and (or A Ci) (or A B) (or B Ci)))
;; (and (or A B) (or A Ci) (or B Ci))

;; (sort-expressions-in-operation -> sort-expressions
;;                                -> sort-arguments-in-operation

;;  (sort-expressions-in-operation '(or (and B2 B3) (and B2 B4) (and (not B12) B3)))
;; $2 = (or (and B2 B3) (and B2 B4) (and (not B12) B3))
(define (sort-expressions-in-operation expr)

  (if (isOR-AND? expr)

      ($+> {exprs-list <+ (args expr)} ;;'(or c a b) -> '(c a b)
	 (nodebug (display "sort-expressions-in-operation : ")
		(dv exprs-list))
	 {sorted-exprs <+ (sort-expressions exprs-list)}
	 (nodebug
	  (dv sorted-exprs))
	 {oper <+ (operator expr)} ;; define operator : (or Ci a b) -> or
	 (cons oper sorted-exprs))

      ;;(sort-arguments-in-operation expr))) ;; we have not an expression composed of expressions but a single expression
      expr))



;; (sort-expressions-in-operation-var-index '(or (and B2 B3) (and B2 B4) (and (not B12) B3)))
;; (or (and B2 B3) (and B2 B4) (and B3 (not B12)))
(define (sort-expressions-in-operation-var-index expr)

  (if (isOR-AND? expr)

      ($+> {exprs-list <+ (args expr)} ;;'(or c a b) -> '(c a b)
	 (nodebug (display "sort-expressions-in-operation-var-index : ")
		(dv exprs-list))
	 {sorted-exprs <+ (sort-expressions-var-index exprs-list)}
	 (nodebug
	  (dv sorted-exprs))
	 {oper <+ (operator expr)} ;; define operator : (or Ci a b) -> or
	 (cons oper sorted-exprs))

      expr))



;; calls diagram : minimal-dnf -> sort-expressions-in-operation -> sort-expressions -> sort with expression-literal-first-negation-tested<?
;;                                                              -> sort-arguments-in-operation -> sort-arguments
;; (sort-expressions '((and B2 B4) (and B2 B3) (and (not B12) B3)))
;; ((and B2 B3) (and B2 B4) (and (not B12) B3))
(define (sort-expressions exprs-list)

  (nodebug
   (display "sort-expressions : ")
   (dv exprs-list))

  {exprs-with-args-sorted <+ (map sort-arguments-in-operation exprs-list)}

  (sort exprs-with-args-sorted expression-literal-first-negation-tested<?)) ;; expression-negation-tested<?)) ;; expression<?))



;; scheme@(guile-user)> (sort-expressions-var-index '((and B2 B4) (and B2 B3) (and (not B12) B3)))
;; ((and B2 B3) (and B2 B4) (and B3 (not B12)))

(define (sort-expressions-var-index exprs-list)

  (nodebug
   (display "sort-expressions : ")
   (dv exprs-list))

  {exprs-with-args-sorted <+ (map sort-arguments-in-operation-var-index exprs-list)}

  (sort exprs-with-args-sorted expression-literal-first-negation-tested<?))



;;{cpt <+ 0}

;; (sort-arguments '(Ci c d (not a) b )) -> '((not a) b c Ci d)
;; (sort-arguments '( Ci (and c d) (not a) b )) -> '((not a) b (and c d) Ci)
;; (sort-arguments '(Ci c d c2 c12 c1 (not a) b ))
;;    ((not a) b c c1 c12 c2 Ci d)

;; calls diagram: sort-arguments-in-operation -> sort-arguments -> expression<?
;; used by minimal-dnf, Quine-Mc-Cluskey,maximal-dnf
(define (sort-arguments args-list)

  (nodebug
   (display-nl "sort-arguments :")
   (dv args-list))

  {res <+ (sort args-list  expression<?)} ;;expression-var-index<?)};BUG ;  expression-negation-tested<?))

  ;;{cpt <- {cpt + 1}}

  (nodebug
   ;;{cpt <- {cpt + 1}}
   (dv cpt)
   (dv res))
  res)


(define (sort-arguments-var-index args-list)

  (nodebug
   (display-nl "sort-arguments-var-index :")
   (dv args-list))

  {res <+ (sort args-list expression-var-index<?)}

  (nodebug
   (dv res))
  res)


;; (sort-arguments-in-operation-literal-first '(or (and V011x V0x01) V01x1 ))
;; '(or V01x1 (and V011x V0x01))
;; DEPRECATED (no call)
;; (define (sort-arguments-in-operation-literal-first expr)

;;   (if (isOR-AND? expr)

;;       (let* ((args-list (args expr)) ;;'(or Ci a b) -> '(Ci a b)

;; 	     (sorted-args (sort args-list expression-literal-first<?))
;; 	     (oper (operator expr))) ;; define operator : (or Ci a b) -> or

;; 	(cons oper sorted-args))

;;       expr)) ;; we have not a binary operator but a literal or negation of literal


;; (sort-arguments-in-operation-most-little-literal-first '(or (and V011x V0x01)  V01x1 V11x1))
;; -> '(or V11x1 V01x1 (and V011x V0x01))
(define (sort-arguments-in-operation-most-little-literal-first expr)

  (if (isOR-AND? expr)

      (let* ((args-list (args expr)) ;;'(or Ci a b) -> '(Ci a b)

	     (sorted-args (sort args-list expression-most-little-literal-first<?))
	     (oper (operator expr))) ;; define operator : (or Ci a b) -> or

	(cons oper sorted-args))

      expr)) ;; we have not a binary operator but a literal or negation of literal





;; remove duplicates operands in a logic expression
;; (remove-duplicates-in-operation '(or a (not a) a)) -> '(or a (not a))
;; (remove-duplicates-in-operation '(or a a)) -> 'a
(define (remove-duplicates-in-operation expr)
  (if (isOR-AND? expr)
      (let ((expr-unik (remove-duplicates (rest expr)))) ;; remove duplicates from operands '(or a b a) -> '(a b)
	(if (null? (rest expr-unik)) ;; test if we have only one element, example : '(a)
	    (first expr-unik) ;; return the single resting literal '(a) -> a
	    (cons (first expr) expr-unik))) ;; construct a list with operator and uniques operands, example '(a b) -> '(or a b)
      expr)) ;; we have not a binary operator but a literal or negation of literal






(define (distribute-or-over-and  expr1 expr2)  ;; expr1 et expr2 sont les arguments d'un 'or : ('or expr1 expr2)
  ;; remember we have (expr1 or expr2) to distribute over the "and"
  (cond
   ((isAND? expr1)            ;; (expr1 expr2) <--> ( ('and p q) r )
    (let ((p (arg1 expr1))
	  (q (arg2 expr1))
	  (r expr2))
      `(and ,(distribute-or-over-and p r) ,(distribute-or-over-and q r)))) ;; (p and q) or r = (p or r) and (q or r)
   ((isAND? expr2)            ;; (expr1 expr2) <--> ( p ('and q r) )
    (let ((p expr1)
	  (q (arg1 expr2))
	  (r (arg2 expr2)))
      `(and ,(distribute-or-over-and p q) ,(distribute-or-over-and p r)))) ;; p or (q and r) = (p or q) and (p or r)
   (else `(or ,expr1 ,expr2)))) ;; else we create the expression ('or expr1 expr2)



;; Conjunctive Normal Form
;; we make the 'and going out by distributing them over the 'or
;; PHASE 3 CNF: on fait au contraire sortir les 'and en distribuant les 'or
;; on ne s'occupe plus des négations !
(define (phase3-cnf expr)
  (debug-mode-off)
  (when debug-mode
    (display "phase3-cnf : ")
    (dv expr))

  (cond
   ((isAND? expr)
    (let ((p (arg1 expr))
	  (q (arg2 expr)))
      `(and ,(phase3-cnf p) ,(phase3-cnf q)))) ;; we do not distribute the 'and but apply phase3-cnf to arguments
   ((isOR? expr)
    (let ((p (arg1 expr))
	  (q (arg2 expr)))
      (distribute-or-over-and (phase3-cnf p) (phase3-cnf q)))) ;; apply distributivity to 'or
   (else expr))) ;; else we leave it unchanged (could be atom, not(x),... )



;; (cnf '(and (and a b) c)) -> '(and (and a b) c)
;; (cnf '(or (and a b) (and c d))) -> '(and (and (or a c) (or a d)) (and (or b c) (or b d)))
(define (cnf expr)    ;; conjunctive normal form
  (phase3-cnf (move-in-negations (elim-exclusive-or (elim-implications  (elim-equivalence expr))))))

;; a simplification package for DNF in n-arity form
;;
;; (prefix->infix (simplify-n-arity-dnf '(or (and (and a b) (not (and c (or (and a (not b)) (and (not a) b))))) (and (not (and a b)) (and c (or (and a (not b)) (and (not a) b)))))))
;;    -> '((a and b) or (a and b and !c) or (a and !b and c) or (!a and b and c))
;;
(define (simplify-n-arity-dnf expr)
  ;;(simplify-logic (n-arity (simplify-OR (simplify-AND (simplify-DNF-by-unitary-reduction (dnf expr)))))))
  (simplify-logic (n-arity (simplify-OR (simplify-AND (simplify-NF-by-unitary-reduction (dnf expr)))))))








;; disjunctive normal form to maximal disjunctive normal form
;;
;; (maximal-dnf (dnf-n-arity-simp '(or (and (and A B) (not (and C (or (and A (not B)) (and (not A) B))))) (and (not (and A B)) (and C (or (and A (not B)) (and (not A) B))))))) -> '(or (and A B C) (and A B (not C)) (and (not A) B C) (and A (not B) C))
;;
;; > (maximal-dnf '(or (and a b) (and b c)))
;; disj-norm-form = (or (and a b) (and b c))
;; var-list = (a b c)
;; and-terms = ((and a b) (and b c))
;; expanded-var-terms = ((c a b) ((not c) a b) (a b c) ((not a) b c))
;; sorted-expanded-var-terms = ((a b c) (a b (not c)) (a b c) ((not a) b c))
;; uniq-sorted-expanded-var-terms = ((a b (not c)) (a b c) ((not a) b c))
;; sorted-expanded-and-term = ((and a b (not c)) (and a b c) (and (not a) b c))
;; '(or (and a b (not c)) (and a b c) (and (not a) b c))
;;
;; > (maximal-dnf '(or (and a b) c))
;; disj-norm-form = (or (and a b) c)
;; var-list = (a b c)
;; and-terms = ((and a b) c)
;; expanded-var-terms = ((c a b) ((not c) a b) (b a c) (b (not a) c) ((not b) a c) ((not b) (not a) c))
;; sorted-expanded-var-terms = ((a b c) (a b (not c)) (a b c) ((not a) b c) (a (not b) c) ((not a) (not b) c))
;; uniq-sorted-expanded-var-terms = ((a b (not c)) (a b c) ((not a) b c) (a (not b) c) ((not a) (not b) c))
;; sorted-expanded-and-term = ((and a b (not c)) (and a b c) (and (not a) b c) (and a (not b) c) (and (not a) (not b) c))
;; '(or (and a b (not c)) (and a b c) (and (not a) b c) (and a (not b) c) (and (not a) (not b) c))
(define (maximal-dnf disj-norm-form)

  (cond
   ((symbol? disj-norm-form) disj-norm-form)
   ((isAND? disj-norm-form) disj-norm-form)
   (else
    (let* (
	   (var-list (collect-variables disj-norm-form)) ;; variable list changed with collect-var :
	   ;;(var-list (sort (remove-duplicates (collect-var disj-norm-form)) symbol-var-index<?))
	   (and-terms (args disj-norm-form)) ;; conjunctives minterms
	   ;; variable list of expanded minterms
	   (expanded-var-terms
	    (apply append (map

			   (λ
			       (min-term)
			     (expand-minterm var-list min-term))

			   and-terms)))

	   ;; (sorted-expanded-var-terms ;; (map sort-arguments expanded-var-terms)) ;; possible BUG : use only if needed :
	   ;;  (map sort-arguments-var-index expanded-var-terms)) ;; sorted variable list of expanded minterm

	   ;;(uniq-expanded-var-terms (remove-duplicates-sorted expanded-var-terms)) ;;sorted-expanded-var-terms))
	   (uniq-expanded-var-terms (remove-duplicates expanded-var-terms))

	   (expanded-and-term
	    (map

	     (λ
		 (literal-list)
	       (if (singleton-set? literal-list)
		   (first literal-list)
		   (cons 'and literal-list)))

	     uniq-expanded-var-terms))

	   (maximal-disj-norm-form (cons 'or expanded-and-term)))

      {maximal-disj-norm-form-sorted <+ (sort-expressions-in-operation-var-index maximal-disj-norm-form)}

      (nodebug
	(display "maximal-dnf:")
	(dv disj-norm-form)
	(dv var-list)
	(dv and-terms)
	(dv expanded-var-terms)
	;;(dv sorted-expanded-var-terms)
	(dv uniq-expanded-var-terms)
	;;(dv sorted-expanded-and-term)
	(dv maximal-disj-norm-form)
	)
      maximal-disj-norm-form-sorted))))





(define (pre-check-Quine-Mc-Cluskey expr)
  (not (is-simple-form? expr)))




;; (minimal-dnf '(or (and (not a) b (not c) d) (and (not a) b c d) (and a b (not c) (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d) (and a (not b) (not c) (not d)) (and a (not b) c (not d))))
;; ...
;; essential-prime-implicants = ((1 x x 0) (x 1 x 1))
;; function expressed by essential prime implicants ?
;; feepi = #t
;; '(or (and a (not d)) (and b d))
;;
;;
;; (dnf-infix-symb (minimal-dnf '(or (and (not a) b (not c) d) (and (not a) b c d) (and a b (not c) (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d) (and a (not b) (not c) (not d)) (and a (not b) c (not d)))))
;;
;; '((b ^ d) v (a ^ !d))
;;
;; (minimal-dnf '(or (and (and A B) (not (and C (or (and A (not B)) (and (not A) B))))) (and (not (and A B)) (and C (or (and A (not B)) (and (not A) B))))))
;; essential-prime-implicants = ((x 1 1) (1 x 1) (1 1 x))
;; function expressed by essential prime implicants ?
;; feepi = #t
;; '(or (and B C) (and A C) (and A B))
;;
;;
;; (dnf-infix-symb (minimal-dnf '(or (and (and A B) (not (and C (or (and A (not B)) (and (not A) B))))) (and (not (and A B)) (and C (or (and A (not B)) (and (not A) B))))))) -> '((B ^ C) v (A ^ C) v (A ^ B))
;;
;;
;;
;; (minimal-dnf '(and (=> (and p q) r) (=> (not (and p q)) r))) -> 'r
;;
;;
;;
;; (minimal-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) c (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d)))
;; ...
;; essential-prime-implicants = ((0 0 0 0) (x 1 x 1) (1 x 1 0))
;; function expressed by essential prime implicants ?
;; feepi = #t
;; '(or (and (not a) (not b) (not c) (not d)) (and b d) (and a c (not d)))
;;
;;
;;
;; (dnf-infix-symb (minimal-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) c (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d))))
;;
;; '((b ^ d) v (!a ^ !b ^ !c ^ !d) v (a ^ c ^ !d))
;;
;;
;;
;;
;;
;;  (minimal-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and a b c (not d))))
;; -> '(or (and (not b) (not c)) (and c (not d)) (and (not a) b d))
;;
(define (minimal-dnf expr)

  ;;(no-debug-region
  (declare min-expr-sorted
	   essential-prime-implicants
	   formula-find-with-Quine-Mc-Cluskey
	   non-essential-prime-implicants
	   min-expr
	   petrick-expr)

  ;;{collected-vars <+ (collect-var expr)}

  (nodebug
   (display "minimal-dnf : ")
   (dv collected-vars))

  ;;{var-list <+ (sort (remove-duplicates collected-vars) symbol-var-index<?)} ;; variable list , previously was : (collect-variables expr)
  ;; changed in maximal-dnf too
  {var-list <+  (collect-variables expr)}

  (nodebug
   (dv var-list))

  {disj-norm-form <+ (dnf-n-arity-simp expr)} ;; disjunctive form

  (nodebug
   (display "minimal-dnf : ")
   (dv disj-norm-form))

  {infix-disj-norm-form <+ (dnf-infix-symb disj-norm-form)} ;; infix only used for display, not for computation


  (if (not (pre-check-Quine-Mc-Cluskey disj-norm-form))

      (sort-arguments-in-operation disj-norm-form) ;; sorting here !

      (begin

	(set! essential-prime-implicants (Quine-Mc-Cluskey disj-norm-form var-list))

	(set! formula-find-with-Quine-Mc-Cluskey (essential-prime-implicants-list->formula essential-prime-implicants var-list))

	(nodebug
	 (dv disj-norm-form)
	 (dv var-list)
	 (dv essential-prime-implicants)
	 (dv infix-disj-norm-form)
	 (dv formula-find-with-Quine-Mc-Cluskey)
	 )

	(set! min-expr formula-find-with-Quine-Mc-Cluskey)

	(when (not feepi) ;; if Quine Mc Cluskey method did not worked completely to a minimal function

	      ;; we have to minimize again
	      (set! non-essential-prime-implicants
		    (set-difference prime-implicants-lst essential-prime-implicants))
	      (nodebug
	       (dv non-essential-prime-implicants)
	       (dv non-expressed-minterms))
	      (set! petrick-expr (Petrick non-essential-prime-implicants var-list))
	      ;;(dv petrick-expr)
	      ;;(dv min-expr)

	      (set! min-expr
		    (if (is-simple-form? min-expr)
			(if (is-simple-form? petrick-expr)
			    (list 'or min-expr petrick-expr)
			    `(,(operator petrick-expr) ,min-expr ,@(args petrick-expr))) ;; operator must be an OR
			(if (is-simple-form? petrick-expr)
			    `(,(operator min-expr) ,@(args min-expr) ,petrick-expr) ;; operator must be an OR
			    `(,(operator min-expr) ,@(args min-expr) ,@(args petrick-expr)))))) ;; operator must be an OR - end when

	{min-expr-sorted <- (sort-expressions-in-operation-var-index min-expr)} ;;(sort-expressions-in-operation min-expr)} ;; compare-list-args require presorted var (warning)

	(nodebug
	 (display-nl "minimal-dnf")
	 (dv min-expr)
	 (dv min-expr-sorted))

	min-expr-sorted)));;)




;; (essential-prime-implicants-list->formula '((0 0 0 0) (x 1 x 1) (1 x 1 0)) '(a b c d))
;;  -> '(or (and (not a) (not b) (not c) (not d)) (and b d) (and a c (not d)))
;; (essential-prime-implicants-list->formula '((x 0 0 0) (x x 1 1) (x 1 x 1) (x 1 x x)) '(a b c d))
;; -> '(or (and (not b) (not c) (not d)) (and c d) (and b d) b)
;;
;; (essential-prime-implicants-list->formula '((0 1 x 1)) '(a b c d)) -> '(and (not a) b d)
(define (essential-prime-implicants-list->formula essential-prime-implicants var-list)
  (if (singleton-list? essential-prime-implicants)
      (binary->min-term (first essential-prime-implicants) var-list)
      (insert 'or (map
		   (λ (epi) (binary->min-term epi var-list))
		   essential-prime-implicants))))




;; the hash table for minterms, better to be a top-level definition,it's nightmare otherwise...

(define minterms-ht (make-hash-table)) ;; SRFI 69
;;(define minterms-ht (make-hashtable)) ;; Bigloo





;;> (prime-implicants minterms-ht) -> '((x 1 x 1) (1 x x 0) (1 1 x x))
(define (prime-implicants mt-ht) ;; argument is a minterms hash table

  ;; return prime-implicant list when the value of hashtable key-value pair is #f

  ;; use srfi 69 hash-table->alist
  (map car ;; or car , works in Guile because first is a procedure (usable with map)
       (filter (λ (p)
		 (not (cdr p))) ;; when working with pair i prefer be using car and cdr rather than first an rest
	       (hash-table->alist mt-ht)))) ;; SRFI 69 , Guile

  ;;(map first (filter (λ (p) (not (cdr p))) (hash->list mt-ht)))) ;; DrRacket
  ;; (map car
  ;;      (filter (λ (p)
  ;; 		 (not (cdr p)))
  ;; 	       (hash->list mt-ht))))
;; DrRacket

;; (map
  ;;  first
  ;;  (filter (λ (p) (not (cdr p)))
  ;; 	   (map cons (hashtable-key-list mt-ht) (hashtable->list mt-ht)))))


;; diagram of calls:
;; funct-unify-minterms-set-of-sets-rec-tail --> funct-unify-minterms-set-1-unit  --> product-set-with-set-imperative
;;                                                                                    function-unify-minterms-list --> function-unify-two-minterms-and-tag
;;                                           <--

;; funct-unify-minterms-set-1-unit  --> product-set-with-set-imperative
;;                                      function-unify-minterms-list --> function-unify-two-minterms-and-tag --> unify-two-minterms

;; unify two sets of minterms separated by a weight distance of one unit (1 bit)
;;
;; (funct-unify-minterms-set-1-unit '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0))) -> '((1 0 x 0) (1 x 0 0))
;;  minterms-ht
;; '#hash(((1 1 0 0) . #t) ((1 0 1 0) . #t) ((1 0 0 0) . #t))
;;
;; (funct-unify-minterms-set-1-unit '((0 1 1) (1 0 1) (1 1 0)) '((1 1 1))) -> '((x 1 1) (1 x 1) (1 1 x))
;;
(define (funct-unify-minterms-set-1-unit set1 set2)

  (nodebug
   (display-nl "funct-unify-minterms-set-1-unit : begin"))

  (nodebug
   (dvs set1)
   (dvs set2))

  {function-unify-minterms-list <+ (λ (L) (apply function-unify-two-minterms-and-tag L))}

  ;; note : sorting is useless
  {minterms-set <+ (product-set-with-set-imperative set1 set2)} ;; (product-set-with-set-imperative-sorted set1 set2)}  ;;(product-set-with-set set1 set2)} ;;(associate-set-with-set set1 set2)} ;; set multiplication : create list of pair of minterms MODIF

  (nodebug
   ;;(display "after call of recursive function associate-set-with-set: ")
   (dvs minterms-set))

  (nodebug
   (display-nl "before (map function-unify-minterms-list minterms-set)")
   {minterms-set-length <+ (length minterms-set)}
   {minterms-set-first <+ (first minterms-set)}
   (dv minterms-set-length)
   (dv minterms-set-first))

  {unified-minterms-set-1 <+ (map function-unify-minterms-list minterms-set)}
  (nodebug
   (display-nl "after (map function-unify-minterms-list minterms-set)"))

  (nodebug
   (dvs unified-minterms-set-1))

  {unified-minterms-set-2 <+ (filter (λ (x) x) unified-minterms-set-1)} ;; remove #f results
  (nodebug
   {unified-minterms-set-length <+ (length unified-minterms-set-2)}
   (dv unified-minterms-set-length))

  {unified-minterms-set <+ (remove-duplicates unified-minterms-set-2)} ;;(remove-duplicates-sorted unified-minterms-set-2)} ;; uniq MODIF
  (nodebug
   {unified-minterms-set-uniq-length <+ (length unified-minterms-set)}
   (dv unified-minterms-set-uniq-length))

  (nodebug
   (dvs unified-minterms-set))

  (nodebug
   (display-nl "funct-unify-minterms-set-1-unit : end"))

  unified-minterms-set)






;; > (init-hash-table-with-set-and-value minterms-ht '((1 0 0 0) (0 1 0 1) (1 0 1 0) (1 1 0 0) (0 1 1 1) (1 1 0 1) (1 1 1 0) (1 1 1 1)) #f)
;; '(#<void> #<void> #<void> #<void> #<void> #<void> #<void> #<void>)
;; > minterms-ht
;; '#hash(((1 1 0 1) . #f)
;;        ((1 1 0 0) . #f)
;;        ((0 1 0 1) . #f)
;;        ((0 1 1 1) . #f)
;;        ((1 0 0 0) . #f)
;;        ((1 1 1 1) . #f)
;;        ((1 1 1 0) . #f)
;;        ((1 0 1 0) . #f))
;; > (funct-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; >
;; > minterms-ht
;; '#hash(((1 1 0 1) . #t)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t))
;;


;; funct-unify-minterms-set-of-sets-rec-tail --> funct-unify-minterms-set-1-unit
;;                                           <--

;; argument: a set of sets of minterms
;; this function advance of a level in unify minterms set of sets
;; when there is no more things to do it returns a set of empty set i.e : '(()) or an empty set '() (for this reason this function will be wrapped)
;;
;; (funct-unify-minterms-set-of-sets-rec '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;;   -> '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;;
;; (funct-unify-minterms-set-of-sets-rec '(((1 x x 0)) ((x 1 x 1) (1 1 x x)))) -> '(())
;;
;; (funct-unify-minterms-set-of-sets-rec '(((x 1 x 1)))) -> '()
;;
;; TODO:
;;
;; (funct-unify-minterms-set-of-sets-rec '(() ((1 0 x x) (1 x 0 x) (1 x x 0)) ((x x 1 1) (x 1 x 1) (1 x x 1) (1 x 1 x) (1 1 x x))))
;; sos = (() ((1 0 x x) (1 x 0 x) (1 x x 0)) ((x x 1 1) (x 1 x 1) (1 x x 1) (1 x 1 x) (1 1 x x)))
;; . . car: contract violation
;;   expected: pair?
;;   given: '()

;; see the tail recursive version after
(define (funct-unify-minterms-set-of-sets-rec-backup sos)

  (nodebug
   (display-nl "funct-unify-minterms-set-of-sets-rec")
   ;;(dvsos sos)
   )

  (debug-region-name "region inside funct-unify-minterms-set-of-sets-rec"
  (if (singleton-set? sos)

      ;; singleton
      ($> (nodebug ;; debug
	  (display-nl "funct-unify-minterms-set-of-sets-rec :: singleton-set? ")
	  (dvsos sos)
	  )

	 '() ) ;; return '()

      ;; at least 2 elements in set of sets
      ($+> {mt-set1 <+ (car sos)} ;; minterm set 1
	 {mt-set2 <+ (cadr sos)} ;; minterm set 2
	 {mt-set2-to-mt-setn <+ (cdr sos)} ;; minterm sets 2 to n
	 {weight-mt-set1 <+ (floor-bin-minterm-weight (car mt-set1))} ;; in a set all minterms have same weight
	 {weight-mt-set2 <+ (floor-bin-minterm-weight (car mt-set2))}
	 {delta-weight <+ {weight-mt-set2 - weight-mt-set1}}

	 (nodebug
	  (dvs mt-set1)
	  (newline)
	  (dvs mt-set2)
	  (newline)
	  (dv delta-weight))

	 ;; this was not original code! here we do first the computation from set 2 to set n and after set 1 and set 2! so no tail recursion optimisation
	 {unified-minterms-set2-to-setn <+ (funct-unify-minterms-set-of-sets-rec mt-set2-to-mt-setn)} ;; in any case we continue with sets from 2 to n

	 (if {delta-weight = 1} ;; if minterms set are neighbours

	     ($+> {unified-mt-set1-and-mt-set2 <+ (funct-unify-minterms-set-1-unit mt-set1 mt-set2)} ;; unify neighbours minterms sets

		(if (null? unified-mt-set1-and-mt-set2)
		    unified-minterms-set2-to-setn ;; the result will be the continuation with sets from 2 to n
		    (insert unified-mt-set1-and-mt-set2 unified-minterms-set2-to-setn))) ;; end &

	     unified-minterms-set2-to-setn))))) ;; continue with sets from 2 to n
       ;; this procedure returns a set of unified minterms of the current level and
       ;; and when there is no more minterms set to unify this procedure returns '() and perheaps
       ;; sort of '(()) or '(() () ...)


;; a tail recursive version
(define (funct-unify-minterms-set-of-sets-rec-tail sos acc) ;; with accumulator

  ;;(newline)
  ;;(display "(funct-unify-minterms-set-of-sets-rec-tail :")
  {zorglub <+ 1}
  {zorglub <- zorglub + 3 * 5 + 2}
  
  ;;(debug-region-name "region inside funct-unify-minterms-set-of-sets-rec-tail"
  (nodebug
   (display-nl "funct-unify-minterms-set-of-sets-rec-tail : begin"))
  (if (singleton-set? sos)

      ;; singleton
      ($> (nodebug ;; debug
	  (display-nl "funct-unify-minterms-set-of-sets-rec :: singleton-set? ")
	  (dvsos sos)
	  )

	 (reverse acc) )

      ;; at least 2 elements in set of sets
      ($+> {mt-set1 <+ (car sos)} ;; minterm set 1
	 {mt-set2 <+ (cadr sos)} ;; minterm set 2
	 {mt-set2-to-mt-setn <+ (cdr sos)} ;; minterm sets 2 to n
	 {weight-mt-set1 <+ (floor-bin-minterm-weight (car mt-set1))} ;; in a set all minterms have same weight
	 {weight-mt-set2 <+ (floor-bin-minterm-weight (car mt-set2))}
	 {delta-weight <+ {weight-mt-set2 - weight-mt-set1}}

	 (nodebug
	  (dvs mt-set1)
	  (newline)
	  (dvs mt-set2)
	  (newline)
	  (dv delta-weight))

	 (if {delta-weight = 1} ;; if minterms set are neighbours

	     ($+> {unified-mt-set1-and-mt-set2 <+  (funct-unify-minterms-set-1-unit-future mt-set1 mt-set2)}  ;;(funct-unify-minterms-set-1-unit-threads mt-set1 mt-set2)} ;; (funct-unify-minterms-set-1-unit-vector-1cpu mt-set1 mt-set2)} ;; (funct-unify-minterms-set-1-unit-par-for-each mt-set1 mt-set2)} ;;    (funct-unify-minterms-set-1-unit mt-set1 mt-set2)} ;;(funct-unify-minterms-set-1-unit-para mt-set1 mt-set2)} ;;  (funct-unify-minterms-set-1-unit-par-map mt-set1 mt-set2)} ;; ;; unify neighbours minterms sets

		(nodebug
		 (display-nl "funct-unify-minterms-set-of-sets-rec-tail : leaving this level..."))
		(if (null? unified-mt-set1-and-mt-set2)
		    (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn acc) ;; the result will be the continuation with sets from 2 to n
		    (funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn (insert unified-mt-set1-and-mt-set2 acc)))) ;; end &

	     ($> (nodebug
		 (display-nl "funct-unify-minterms-set-of-sets-rec-tail : leaving this level..."))
		(funct-unify-minterms-set-of-sets-rec-tail mt-set2-to-mt-setn acc)))))) ;; continue with sets from 2 to n

       ;; this procedure returns a set of unified minterms of the current level and
       ;; and when there is no more minterms set to unify this procedure returns '() and perheaps
       ;; sort of '(()) or '(() () ...)

;; a tail recursive version
(define (funct-unify-minterms-set-of-sets-rec sos)

  (nodebug
   (display-nl "funct-unify-minterms-set-of-sets-rec")
   {lgt-sos <+ (length-sos sos)}
   (dv lgt-sos)
   ;;(dvsos sos)
   (newline)
   )

  ;; now call the tail recursive version
  (funct-unify-minterms-set-of-sets-rec-tail sos '()))


;; funct-unify-minterms-set-of-sets-rec-wrap --> funct-unify-minterms-set-of-sets-rec

;; it's a wrap of previous function to return empty set
;; > (funct-unify-minterms-set-of-sets-rec-wrap '(((1 x x 0)) ((x 1 x 1) (1 1 x x))))
;; '()
;; > (funct-unify-minterms-set-of-sets-rec-wrap '(((x 1 x 1))))
;; '()
;; > (funct-unify-minterms-set-of-sets-rec-wrap '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; >
;; DEPRECATED (but keep in case of error on other data set to reuse it)

;; (define (funct-unify-minterms-set-of-sets-rec-wrap sos)

;;   (debug
;;    (newline)
;;    (newline)
;;    (display "funct-unify-minterms-set-of-sets-rec-wrap : ")
;;    (dvsos sos))

;;   (let ((rv (funct-unify-minterms-set-of-sets-rec sos)))

;;     (debug
;;      (newline)
;;      (newline)
;;      (display "funct-unify-minterms-set-of-sets-rec-wrap : ")
;;      (dv rv)
;;      (dvsos rv))

;;     (if (set-of-empty-set? rv)
;; 	'()
;; 	rv)))



;; recursive-unify-minterms-set-of-sets --> put-elements-of-set-of-sets-in-minterms-ht
;;                                          funct-unify-minterms-set-of-sets-rec
;;                                      <--

;; (recursive-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '((0 1 x 1)
;;   (1 x 0 0)
;;   (1 0 1 0)
;;   (1 1 1 0)
;;   (x 1 0 1)
;;   (1 1 x 1)
;;   (0 1 1 1)
;;   (1 0 x 0)
;;   (1 1 1 x)
;;   (1 1 0 1)
;;   (1 1 0 x)
;;   (1 1 x x)
;;   (1 x 1 0)
;;   (1 1 1 1)
;;   (1 0 0 0)
;;   (x 1 x 1)
;;   (0 1 0 1)
;;   (1 x x 0)
;;   (1 1 0 0)
;;   (1 1 x 0)
;;   (x 1 1 1))
;;
;; > (recursive-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; sos = (((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1)))
;; sos = (((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; sos = (((1 x x 0)) ((x 1 x 1) (1 1 x x)))
;; sos = ()
;; '((1 1 0 0)
;;   (x 1 x 1)
;;   (1 1 x 1)
;;   (0 1 x 1)
;;   (1 1 x 0)
;;   (1 x x 0)
;;   (x 1 0 1)
;;   (1 x 1 0)
;;   (1 1 x x)
;;   (1 0 0 0)
;;   (1 0 1 0)
;;   (1 1 1 1)
;;   (0 1 1 1)
;;   (0 1 0 1)
;;   (1 x 0 0)
;;   (1 0 x 0)
;;   (1 1 0 1)
;;   (1 1 1 x)
;;   (x 1 1 1)
;;   (1 1 1 0)
;;   (1 1 0 x))
;; (define (recursive-unify-minterms-set-of-sets sos)

;;   (debug
;;    (display-nl "recursive-unify-minterms-set-of-sets : "))

;;   (debug
;; 	(newline)
;; 	;;(display "recursive-unify-minterms-set-of-sets : ")
;; 	(dv sos)
;; 	(dvsos sos)
;; 	)

;;   (if (set-of-multiple-empty-sets? sos)

;;       (hash-table-keys minterms-ht)
;;       ;;(hashtable-key-list minterms-ht) ;; Bigloo

;;       (begin
;; 	(when debug-mode (display-msg-symb-nl "recursive-unify-minterms-set-of-sets ::" minterms-ht))

;; 	(put-elements-of-set-of-sets-in-minterms-ht sos)

;; 	(when debug-mode (display-msg-symb-nl "recursive-unify-minterms-set-of-sets :: after (put-elements-of-set-of-sets-in-minterms-ht sos)" minterms-ht))

;; 	(recursive-unify-minterms-set-of-sets (funct-unify-minterms-set-of-sets-rec-wrap sos)))))


(define (recursive-unify-minterms-set-of-sets sos)

  (nodebug
   (display-nl "recursive-unify-minterms-set-of-sets : "))

  (nodebug
	(newline)
	;;(display "recursive-unify-minterms-set-of-sets : ")
	(dv sos)
	(dvsos sos)
	)

  (if {(null? sos) or (set-of-multiple-empty-sets? sos)}

      (hash-table-keys minterms-ht)
      ;;(hashtable-key-list minterms-ht) ;; Bigloo

      (begin
	(when debug-mode (display-msg-symb-nl "recursive-unify-minterms-set-of-sets ::" minterms-ht))

	(put-elements-of-set-of-sets-in-minterms-ht sos)

	(when debug-mode (display-msg-symb-nl "recursive-unify-minterms-set-of-sets :: after (put-elements-of-set-of-sets-in-minterms-ht sos)" minterms-ht))

	(recursive-unify-minterms-set-of-sets (funct-unify-minterms-set-of-sets-rec sos)))))


;; (put-elements-of-set-of-sets-in-minterms-ht '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x))))
;; '((#<void> #<void>) (#<void> #<void> #<void> #<void> #<void>) (#<void> #<void> #<void>))
;; > minterms-ht
;; '#hash(((1 x 1 0) . #f)
;;        ((1 1 0 x) . #f)
;;        ((1 1 x 1) . #f)
;;        ((x 1 1 1) . #f)
;;        ((1 0 x 0) . #f)
;;        ((1 1 1 x) . #f)
;;        ((1 1 x 0) . #f)
;;        ((x 1 0 1) . #f)
;;        ((1 x 0 0) . #f)
;;        ((0 1 x 1) . #f))
;;
;;
;;
;; Bienvenue dans DrRacket, version 6.1.1 [3m].
;; Langage: racket [personnalisé]; memory limit: 256 MB.
;; > (init-hash-table-with-set-and-value minterms-ht '((1 0 0 0) (0 1 0 1) (1 0 1 0) (1 1 0 0) (0 1 1 1) (1 1 0 1) (1 1 1 0) (1 1 1 1)) #f)
;; '(#<void> #<void> #<void> #<void> #<void> #<void> #<void> #<void>)
;; > minterms-ht
;; '#hash(((1 1 0 1) . #f)
;;        ((1 1 0 0) . #f)
;;        ((0 1 0 1) . #f)
;;        ((0 1 1 1) . #f)
;;        ((1 0 0 0) . #f)
;;        ((1 1 1 1) . #f)
;;        ((1 1 1 0) . #f)
;;        ((1 0 1 0) . #f))
;; > (funct-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; > minterms-ht
;; '#hash(((1 1 0 1) . #t)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t))
;; > (put-elements-of-set-of-sets-in-minterms-ht '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x))))
;; '((#<void> #<void>) (#<void> #<void> #<void> #<void> #<void>) (#<void> #<void> #<void>))
;; > minterms-ht
;; '#hash(((x 1 1 1) . #f)
;;        ((1 1 x 0) . #f)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #f)
;;        ((1 1 0 x) . #f)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #f)
;;        ((1 0 x 0) . #f)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #f)
;;        ((x 1 0 1) . #f)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #f)
;;        ((0 1 x 1) . #f))
;; > (funct-unify-minterms-set-of-sets '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x))))
;; '(((1 x x 0)) ((x 1 x 1) (1 1 x x)))
;; > minterms-ht
;; '#hash(((x 1 1 1) . #t)
;;        ((1 1 x 0) . #t)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #t)
;;        ((1 1 0 x) . #t)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #t)
;;        ((1 0 x 0) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #t)
;;        ((x 1 0 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #t)
;;        ((0 1 x 1) . #t))
;; > (put-elements-of-set-of-sets-in-minterms-ht '(((1 x x 0)) ((x 1 x 1) (1 1 x x))))
;; '((#<void>) (#<void> #<void>))
;; > minterms-ht
;; '#hash(((x 1 1 1) . #t)
;;        ((1 1 x 0) . #t)
;;        ((1 1 0 0) . #t)
;;        ((1 x x 0) . #f)
;;        ((0 1 0 1) . #t)
;;        ((x 1 x 1) . #f)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #t)
;;        ((1 1 x x) . #f)
;;        ((1 1 0 x) . #t)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #t)
;;        ((1 0 x 0) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #t)
;;        ((x 1 0 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #t)
;;        ((0 1 x 1) . #t))
;; > (funct-unify-minterms-set-of-sets '(((1 x x 0)) ((x 1 x 1) (1 1 x x))))
;; '(())
;; > minterms-ht
;; '#hash(((x 1 1 1) . #t)
;;        ((1 1 x 0) . #t)
;;        ((1 1 0 0) . #t)
;;        ((1 x x 0) . #f)
;;        ((0 1 0 1) . #t)
;;        ((x 1 x 1) . #f)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #t)
;;        ((1 1 x x) . #f)
;;        ((1 1 0 x) . #t)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #t)
;;        ((1 0 x 0) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #t)
;;        ((x 1 0 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #t)
;;        ((0 1 x 1) . #t))
;; >

;; e element
;; s set
;; sos set of sets
(define (put-elements-of-set-of-sets-in-minterms-ht sos)
  (map ;; deal with sets of the 'set of sets'
   (λ (s)
     ;; deal with elements of a set
     (map    ( λ (e) {minterms-ht[e] <- #f} )     s))
   sos))



;; unify function for two minterms
;;
;; (function-unify-two-minterms-and-tag '(1 0 0 0) '(1 0 1 0)) -> '(1 0 x 0)
;;
;; (unify-minterms-set '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0)) function-unify-two-minterms-and-tag)
;;
;; '((1 0 x 0) (1 x 0 0))
;; >  minterms-ht
;; '#hash(((1 1 0 0) . #t) ((1 0 1 0) . #t) ((1 0 0 0) . #t))
;;
;;
(define (function-unify-two-minterms-and-tag mt1 mt2)

  (nodebug
   (display-nl "function-unify-two-minterms-and-tag : begin"))

  {res ⥆ (unify-two-minterms mt1 mt2)} ;; (unify-two-minterms-rec mt1 mt2)}
  (when res
    {minterms-ht[mt1] <- #t}
    {minterms-ht[mt2] <- #t})

  (nodebug
   (display-nl "function-unify-two-minterms-and-tag : end"))

  res) ;; result





;; (init-hash-table-with-set-and-value ht '((1 0 0 0) (0 1 0 1) (1 0 1 0) (1 1 0 0) (0 1 1 1) (1 1 0 1) (1 1 1 0) (1 1 1 1)) #f)
;; '(#<void> #<void> #<void> #<void> #<void> #<void> #<void> #<void>)
;; > ht
;; '#hash(((1 1 0 1) . #f)
;;        ((1 1 0 0) . #f)
;;        ((0 1 0 1) . #f)
;;        ((0 1 1 1) . #f)
;;        ((1 0 0 0) . #f)
;;        ((1 1 1 1) . #f)
;;        ((1 1 1 0) . #f)
;;        ((1 0 1 0) . #f))

;; used by Quine - Mc Cluskey
(define (init-hash-table-with-set-and-value ht s val)

  ;;(debug-mode-off)
  (when debug-mode
	(display "init-hash-table-with-set-and-value") (newline))

  ;;{ht ← (make-hash-table)} ;; attention semble ne pas marcher
  ;;(hash-clear! ht) ;; Guile built-in, will not work with SRFI 69 !
  (map (λ (e) {ht[e] <- val}) s)
  ;;(map (λ (e) (hash-table-set! ht e val)) s) ;; Guile , SRFI 69
  (when debug-mode
	(display "end of init-hash-table-with-set-and-value") (newline)))


;; list of non expressed minterms
{non-expressed-minterms ⥆ '()} ;; could also be done with (declare non-expressed-minterms)

;; iepi : identifying essential prime implicant array
;; first line : minterms
;; first row : prime-implicants
;; for now i do not know the array dimension
(declare iepi lgt-pi lgt-mt)



;; example part of output:

;; {iepi[1 2]} = 0
;; {iepi[1 2] ← 1} = 1
;; #(() (0 0 0 0) (0 0 0 1) (0 0 1 0) (1 0 0 0) (0 1 0 1) (0 1 1 0) (1 0 0 1) (1 0 1 0) (0 1 1 1) (1 1 1 0))
;; #(0 0 0 0 0 0 0 0 0 0 0)
;; #(0 1 0 0 0 0 0 0 0 0 0)
;; #(0 0 0 0 0 0 0 0 0 0 0)
;; #(0 0 0 0 0 0 0 0 0 0 0)
;; #(0 0 0 0 0 0 0 0 0 0 0)
;; #(0 0 0 0 0 0 0 0 0 0 0)

;; iepi =
;; #(() (0 0 0 0) (0 0 0 1) (0 0 1 0) (1 0 0 0) (0 1 0 1) (0 1 1 0) (1 0 0 1) (1 0 1 0) (0 1 1 1) (1 1 1 0))
;; #((0 x 0 1)      *           *                     )
;; #((0 1 x 1)                  *               *     )
;; #((0 1 1 x)                      *           *     )
;; #((x x 1 0)          *           *       *      (*))
;; #((x 0 0 x)  *   *       *          (*)            )
;; #((x 0 x 0)  *       *   *               *         )

(define (identify-essential-prime-implicants  prime-implicants minterms)

 
  {vct-prime-implicants ⥆ (list->vector prime-implicants)}
  {essential-prime-implicants-list ⥆ '()}
  {cpt-mt ⥆ 0} ;; counter of minterms
  {lin-pos-epi ⥆ 0} ;; position of essential prime implicant in colomn if there exists one
  {star-in-column ⥆ #f} ;; at the beginning

  {feepi ← #f} ;; at the beginning

  ;;(debug-mode-off)
  (when debug-mode
    (display-nl "identify-essential-prime-implicants ::")
    (dv prime-implicants)
    (dv minterms))

  {lgt-pi ← (length prime-implicants)}
  {lgt-mt ← (length minterms)}

  ;; identifying essential prime implicant array
  ;; first line : minterms
  ;; first row : prime-implicants
  {iepi ← (make-array-2d {lgt-pi + 1} {lgt-mt + 1} 0)} ;; two dimensions array
  (when debug-mode
    (dv-2d iepi))

  {iepi[0] ← (list->vector (cons '() minterms))}
  ;;(vector-set! iepi 0 (list->vector (cons '() minterms))) ;; set the title line

  (when debug-mode
    (dv-2d iepi)
    )

  ;; construction of the array
  ;; set the left column containing prime implicants
  (for-basic (lin 0 {lgt-pi - 1})

       {iepi[{lin + 1} 0] ← vct-prime-implicants[lin]})

  ;; identify prime implicants
  (for-basic (col 1 lgt-mt)

       {cpt-mt ← 0}

       (for-basic (lin 1 lgt-pi)

	    (if (compare-minterm-and-implicant {iepi[lin 0]}
					       {iepi[0 col]})
		;; then
		($>
		  (incf cpt-mt)
		  (when (= 1 cpt-mt)
			{lin-pos-epi ← lin}) ;; position of essential prime implicant
		  ;;{iepi[lin col] ← " * "})
		  {iepi[lin col] ← 1})

		;; else
		;;{iepi[lin col] ← "   "})) ;; end for lin
		{iepi[lin col] ← 3}))

       (when (= 1 cpt-mt) ;; essential prime implicant
	     ;;{iepi[lin-pos-epi col] ← "(*)"}
	     {iepi[lin-pos-epi col] ← 2}
	     ;; add essential prime implicant to list
	     {essential-prime-implicants-list ← (cons {iepi[lin-pos-epi 0]} essential-prime-implicants-list)})

     ) ;; end for col


  (when debug-mode
	(newline)
	(dv-2d iepi))



  {essential-prime-implicants-list ← (remove-duplicates essential-prime-implicants-list)}

  {feepi ← #t}

  ;; check if function is expressed by essential implicants
  (for-basic/break break-col (col 1 lgt-mt) ;; loop over minterms

	     (for-basic/break break-lin (lin 1 lgt-pi) ;; loop over prime implicants

			;; check wether prime implicant is an essential one?
			(when (member {iepi[lin 0]} essential-prime-implicants-list)

			      (nodebug
			       (de {iepi[lin 0]})
			       (de {iepi[lin col]}))

			      ;; is the essential prime implicant expressing this minterms?
			      ;; (when (or (string=?  {iepi[lin col]} "(*)")
			      ;; 		(string=?  {iepi[lin col]} " * "))
			      (when (or (= {iepi[lin col]} 2)
					(= {iepi[lin col]} 1))

				    (when debug-mode
					  (display-nl "star-in-column"))

				    {star-in-column ← #t}

				    (break-lin)))) ;; that's enought! we know the minterm is expressed.

	     ;; end for/break break-lin

	     (unless star-in-column
		     {feepi ← #f} ;; function not expressed by prime implicants
		     ;; add minterm to non expressed minterms list
		     {non-expressed-minterms ← (insert {iepi[0 col]} non-expressed-minterms)}
		     ;;(break-col) ;; removed break-col as we have to check all the minterms now
		     )

	     {star-in-column ← #f})  ;; set it for the next loop
  ;; end for/break break-col

  essential-prime-implicants-list)



(declare feepi) ;; function expressed by essential prime implicants



;; (compare-2-bits-symbolically 'x 1) -> #t
;; (compare-2-bits-symbolically 1 1) -> #t
;; (compare-2-bits-symbolically 0 1) -> #f
;; (compare-2-bits-symbolically 0 'x) -> #t
(define (compare-2-bits-symbolically a b)
  (or (equal? a 'x) (equal? a b) (equal? b 'x)))

;; (compare-minterm-and-implicant '(1 0 0 1) '(1 x 0 1)) -> #t
;; (compare-minterm-and-implicant '(1 0 1 1) '(1 x 0 1)) -> #f
;; arguments could be swapped
;; (compare-minterm-and-implicant '(1 x 0 1)  '(1 0 1 1) ) -> #f
(define (compare-minterm-and-implicant mt im)
  (andmap compare-2-bits-symbolically mt im))


;; Quine-Mc Cluskey method for minimizing function
;;
;; called by minimal-dnf
;;
;; (Quine-Mc-Cluskey '(or (and c (not d)) (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) (not c) (not d)) (and a (not b) (not c) d)) '(a b c d))
;;
;; '((x 0 0 x) (x x 1 0))
(def (Quine-Mc-Cluskey disj-norm-form var-list)

     (nodebug
      (display-nl "Quine-Mc-Cluskey:"))

     {and-terms ⥆ (args disj-norm-form)} ;; conjunctives minterms
     ;; variable list of expanded minterms
     {expanded-var-terms  ⥆ ($>
				(when debug-mode
				  (dv and-terms)) ;; dv:display value
				(apply append
				       (map (λ (min-term) (expand-minterm var-list min-term))
					    and-terms)))}

     (nodebug
      (dv expanded-var-terms))

     {sorted-expanded-var-terms  ⥆ (map sort-arguments expanded-var-terms)} ;; sorted variable list of expanded minterms

     ;; possible BUG :
     ;;{sorted-expanded-var-terms  ⥆ (map sort-arguments-var-index expanded-var-terms)} ;; sorted variable list of expanded minterms

     {sorted-minterms-list <+ (sort sorted-expanded-var-terms compare-list-args<?)} ;; sort expanded minterms list

     {uniq-sorted-minterms <+ (uniq sorted-minterms-list)} ;; (remove-duplicates-sorted sorted-minterms-list)}

     (nodebug
      ;; dv : display value
      (dv disj-norm-form)
      (dv var-list)
      (dv and-terms)
      (dv expanded-var-terms)
      (dv sorted-expanded-var-terms)
      (dv sorted-minterms-list)
      (dv uniq-sorted-minterms))

     {binary-minterms ⥆ (map var->binary uniq-sorted-minterms)} ;; minterms in binary form
     {sorted-binary-minterms ⥆ (sort binary-minterms minterm-binary-weight-number<?)} ;; sorted binary minterms
     {uniq-sorted-binary-minterms ⥆ (uniq sorted-binary-minterms)} ;;(remove-duplicates-sorted sorted-binary-minterms)}  ;; uniq? because there could be the same many times
     {minterms ⥆ uniq-sorted-binary-minterms}

     (nodebug
      ;; (dv binary-minterms)
      ;; (dv sorted-binary-minterms)
      (dv uniq-sorted-binary-minterms))

     {set-of-sets-of-minterms ⥆ (order-by-weight-minterms uniq-sorted-binary-minterms)} ;; set of sets of minterms ordered by weight
     ;; (begin
     ;;   (de (order-by-weight-basic uniq-sorted-binary-minterms)) ;; set of sets of minterms ordered by weight
     ;;   (error "escaping from Quine-Mc-Cluskey")))
     ;; (order-by-weight-basic uniq-sorted-binary-minterms))

     (nodebug
      (dvsos set-of-sets-of-minterms))

     {unified-minterms ⥆ ($>
			     (when debug-mode (display-nl "Quine-Mc-Cluskey:"))
			     (init-hash-table-with-set-and-value minterms-ht minterms #f)
			     (when debug-mode (dv minterms-ht))
			     (recursive-unify-minterms-set-of-sets  set-of-sets-of-minterms))}

     (nodebug
      (newline)
      (display-nl "Quine-Mc-Cluskey:")
      (dv unified-minterms)
      (newline))

     {essential-prime-implicants ⥆ ($> {prime-implicants-lst ← (prime-implicants minterms-ht)}
				      (identify-essential-prime-implicants prime-implicants-lst minterms))}


     (nodebug
      ;;(dvsos set-of-sets-of-minterms)
      ;;(dv unified-minterms)
      (dv minterms-ht)
      (dv prime-implicants-lst)
      (dv essential-prime-implicants)
      (display-nl "function expressed by essential prime implicants ?")
      (dv feepi))

     (hash-table-clear! minterms-ht) ;; to avoid GC Warning: Repeated allocation of very large block (appr. size 144117760): May lead to memory leak and poor performance (from Boehm garbage collector)

     essential-prime-implicants)


(define prime-implicants-lst '())


(def (Petrick non-essential-prime-implicants var-list)

  ;; create the conjunction of disjunction expression

  (declare mt conj-expr prim-imp colmn disj-expr disj-expr-sorted mt-var missing-term)

  ;;(display-nl "Entering Petrick...")

  (for-basic (col 1 lgt-mt) ;; loop over minterms

       {mt ← iepi[0 col]}

       (when (member mt non-expressed-minterms) ;; non expressed minterm

	 {colmn ← '()}

	 (for-basic (lin 1 lgt-pi) ;; loop over prime implicants

	      {prim-imp ← iepi[lin 0]} ;; prime implicant

	      ;; check wether prime implicant is a non essential one?
	      (when (member prim-imp non-essential-prime-implicants)

		    ;; is the non essential prime implicant expressing this minterms?
		    ;; (when (string=? {iepi[lin col]} " * ")
		    (when (= {iepi[lin col]} 1)
			  (insert-set! (minterm->var prim-imp) colmn))))

	 ;; end for lin

	 (if (singleton-set? colmn)
	     {colmn ← (car colmn)}  ;; ( V ) -> V
	     (insert-set! 'or colmn))  ;; (V1 V2 ...) -> (or V1 V2 ...)

	 (insert-set! colmn conj-expr)))

  ;; end for col

  (if (singleton-set? conj-expr)
      {conj-expr ← (car conj-expr)}  ;; ( conj-expr ) -> conj-expr
      (insert-set! 'and conj-expr))  ;; (e1 e2 ...) -> (and e1 e2 ...)

  ;;(dv conj-expr)

  ;; find the disjunctive form
  {disj-expr ← (dnf-n-arity-simp conj-expr)}

  ;;(dv disj-expr)

  ;; sorting terms
  ;; sort by x < 1 < 0
  {disj-expr-sorted ← (sort-arguments-in-operation-most-little-literal-first disj-expr)}
  ;;(dv disj-expr-sorted)

  ;; get the shortest minterm
  (if (isOR-AND? disj-expr-sorted)
      {mt-var ← (first (args disj-expr-sorted))}
      {mt-var ← disj-expr-sorted})

  ;;(dv mt-var)

  {mt ← (var->minterm mt-var)}

  ;;(dv mt)

  ;; TODO: possible bug missing term could be an expression ? (multiple terms)
  {missing-term ← (essential-prime-implicants-list->formula (list mt)
							    var-list)}

  ;;(dv missing-term)

  missing-term

)




;; scheme@(guile-user)> (unify-two-minterms-rec '(1 0 1 0 0 1 0 1 0 1) '(1 0 1 0 1 1 0 1 0 1))
;; $2 = (1 0 1 0 x 1 0 1 0 1)
;; (define (unify-two-minterms-rec mt1 mt2)

;;   {err <+ #f}

;;   (def (unify-two-lists-tolerant-one-mismatch mt1 mt2)

;;        (if {(null? mt1) and (null? mt2)}
;; 	   (return '()))

;;        (if {{(null? mt1) and (not (null? mt2))} or {(not (null? mt1)) and (null? mt2)}}
;; 	   (return-rec #f))


;;        {fst-mt1 <+ (first mt1)}
;;        {fst-mt2 <+ (first mt2)}

;;        (if (equal? fst-mt1 fst-mt2) (return (cons fst-mt1
;; 						  (unify-two-lists-tolerant-one-mismatch (rest mt1) (rest mt2)))))
;;        (if err (return-rec #f))

;;        {err <- #t}
;;        (cons 'x
;; 	     (unify-two-lists-tolerant-one-mismatch (rest mt1) (rest mt2))))

;;   (unify-two-lists-tolerant-one-mismatch mt1 mt2))





;; test case procedure
(def (logic-test)

     (declare expr res-expr res-expr-exact)

     ;; test 1
     (display-nl "test 1")
     {expr <- '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}} }
     (display expr)
     (display " = ")
     {res-expr <- (infix-symb-min-dnf expr)}
     (display-nl res-expr)
     {res-expr-exact <- '((¬a ∧ b ∧ d) ∨ (¬b ∧ ¬c) ∨ (c ∧ ¬d))}
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 1 ******* DIFFER *******")
	  (return #f)))
     (newline)

     ;; test 2
     (display-nl "test 2")
     {expr <- '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and c (not d)))}
     (display expr)
     (display " = ")
     {res-expr <- (infix-symb-min-dnf expr)}
     (display-nl res-expr)
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 2 ******* DIFFER *******")
	  (return #f)))
     (newline)

     ;; test 3
     (display-nl "test 3")
     {expr <- '(or (and (and A B) (not (and C (or (and A (not B)) (and (not A) B))))) (and (not (and A B)) (and C (or (and A (not B)) (and (not A) B)))))}
     (display expr)
     (display " = ")
     {res-expr <- (infix-symb-min-dnf expr)}
     (display-nl res-expr)
     {res-expr-exact <- '((A ∧ B) ∨ (A ∧ C) ∨ (B ∧ C))}
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 3 ******* DIFFER *******")
	  (return #f)))
     (newline)

     ;; test 4
     (display-nl "test 4")
     {expr <- '{{B1 · B0} ⊕ {C1 · {B1 ⊕ B0}}}}
     (display expr)
     (display " = ")
     {res-expr <- (infix-symb-min-dnf expr)}
     (display-nl res-expr)
     {res-expr-exact <- '((B0 ∧ B1) ∨ (B0 ∧ C1) ∨ (B1 ∧ C1))}
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 4 ******* DIFFER *******")
	  (return #f)))
     (newline)

     ;; test 5
     (display-nl "test 5")
     {expr <- '(or (and B3 (or B2 (and (not B12) B3))) (and B4 (or B2 (and (not B12) B3))))}
     (display expr)
     (display " = ")
     {res-expr <- (minimal-dnf expr)}
     (display-nl res-expr)
     {res-expr-exact <- '(or (and B2 B3) (and B2 B4) (and B3 (not B12)))} ;;'(or (and B2 B3) (and B2 B4) (and (not B12) B3))}
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 5 ******* DIFFER *******")
	  (return #f)))
     (newline)

     ;; test 6
     (display-nl "test 6")
     {expr <- '{{(not a) and (not b) and (not c) and (not d)} or {(not a) and (not b) and (not c) and d} or {(not a) and (not b) and c and (not d)} or {(not a) and b and (not c) and d} or {(not a) and b and c and (not d)} or {(not a) and b and c and d} or {a and (not b) and (not c) and (not d)} or {a and (not b) and (not c) and d} or {a and (not b) and c and (not d)} or {c and (not d)}}}
     (display expr)
     (display " = ")
     {res-expr <- (cnf-infix-symb expr)}
     (display-nl res-expr)
     {res-expr-exact <- '((¬a ∨ ¬b ∨ c) ∧ (¬a ∨ ¬b ∨ ¬d) ∧ (¬a ∨ ¬c ∨ ¬d) ∧ (b ∨ ¬c ∨ ¬d) ∧ (¬b ∨ c ∨ d))}
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 6 ******* DIFFER *******")
	  (return #f)))
     (newline)

     ;; test 7
     (display-nl "test 7")
     {expr <- '(or (and (and A B) (not (and C (or (and A (not B)) (and (not A) B))))) (and (not (and A B)) (and C (or (and A (not B)) (and (not A) B)))))}
     (display expr)
     (display " = ")
     {res-expr <- (maximal-dnf (dnf-n-arity-simp expr))}
     (display-nl res-expr)
     {res-expr-exact <- '(or (and A B C) (and A B (not C)) (and A (not B) C) (and (not A) B C))}
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 7 ******* DIFFER *******")
	  (return #f)))
     (newline)

     ;; test 8
     (display-nl "test 8")
     {expr <- '(or (and B₃ (or B₂ (and (not B₁₂) B₃))) (and B₄ (or B₂ (and (not B₁₂) B₃))))}
     (display expr)
     (display " = ")
     {res-expr <- (minimal-dnf expr)}
     (display-nl res-expr)
     {res-expr-exact <- '(or (and B₂ B₃) (and B₂ B₄) (and B₃ (not B₁₂)))} ;;'(or (and B2 B3) (and B2 B4) (and (not B12) B3))}
     (if (equal? res-expr res-expr-exact)
	 (display-nl "EXACT")
	 ($>
	  (display-nl "test 8 ******* DIFFER *******")
	  (return #f)))
     (newline)

     #t)





;; Parallelisation

;; code from Olivier Dion
;; (define (par-map-vector proc input) ;; was define*
;;                          ;; #:optional
;;                          ;; (max-thread (current-processor-count)))

;;   {max-thread <+ (processor-count)} ;;(current-processor-count)}
  
;;   (if (< (vector-length input) max-thread) 
      
;;       (list->vector (map proc (vector->list input))) ;; less data than threads or CPUs
      
;;       (let* ((block-size (quotient (vector-length input) max-thread))
;; 	     (rest (remainder (vector-length input) max-thread))
;; 	     (output (make-vector (vector-length input) #f)))
	
;; 	(when (not (zero? block-size))

;; 	  (let ((mtx (make-mutex))
;; 		(cnd (make-condition-variable))
;; 		(n 0))
	    
;; 	    (fold

;; 	     (lambda (scale output)
	       
;; 	       (begin-thread
		
;; 		(let lp ((i 0))
;; 		  (when (< i block-size)
;; 		    (let ((i (+ i (* scale block-size))))
;; 		      (vector-set! output i (proc (vector-ref input i))))
;; 		    (lp (1+ i))))
		
;; 		(with-mutex mtx
;; 			    (set! n (1+ n))
;; 			    (signal-condition-variable cnd)))
;; 	       output)
	     
;; 	     output
;; 	     (iota max-thread))
	    
;; 	    (with-mutex mtx
;; 			(while (not (< n max-thread))
;; 			       (wait-condition-variable cnd mtx)))))
	  
;; 	  (let ((base (- (vector-length input) rest)))
;; 	    (let lp ((i 0))
;; 	      (when (< i rest)
;; 	  	(let ((i (+ i base)))
;; 	  	  (vector-set! output i (proc (vector-ref input i))))
;; 	  	(lp (1+ i)))))
	  
;; 	  output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; code courtesy of Zelphir Kaltstahl
(define make-segment
  (λ (start end)
    (cons start end)))

(define segment-start
  (λ (seg)
    (car seg)))

(define segment-end
  (λ (seg)
    (cdr seg)))

(define segment 
  (lambda (start ;; was lambda*
	    end
	    segment-count
	    ;; #:key
	    ;; (next (λ (num) (+ num 1))))
	    )
	   "Make segments of mostly equal length/size. A
segment's starting point is based on the previous segment's
ending point. Segments do not necessarily connect with no
gap in between. The NEXT argument is a function, which is
used to calculate the start of the starting point of the
following segment from the ending point of the previous
segment."

	   {next <+ (λ (num) (+ num 1))}
	   
	   (let ([segment-size
		  (ceiling
		   (/ (- end start)
		      segment-count))])
	     (let loop ([pos start])
	       (cond
		[(>= (+ pos segment-size) end)
		 (list (make-segment pos end))]
		[else
		 (cons (make-segment pos (+ pos segment-size))
		       (loop (next (+ pos segment-size))))])))))



(define run-in-parallel
  (λ (segments map-proc) ;;reduce-proc reduce-init)
    "Use futures to run a procedure in parallel, if
multiple cores are available. Take a list of SEGMENTS as
input, which are ranges of values to work on. MAP-PROC is
applied to the SEGMENTS using map. When the MAP-PROC calls
for all segments finished and returned values, the
REDUCE-PROC is applied to the map result using reduce and
the REDUCE-INIT argument."
    (let ([futures
	   (map (λ (seg)
		  ;;(display-nl "run-in-parallel : making future")
		  (future ;; make-future
		   ;; Need to wrap in a thunk, to not
		   ;; immediately start evaluating.
		   (λ () (map-proc seg))))
		segments)])
      (let ([segment-results (map
			      ;; (lambda (f)
			      ;; 	    (display-nl "run-in-parallel : touching future")
			      ;; 	    (touch f))
			      touch
			      futures)])
	segment-results
	;; (reduce reduce-proc
	;; 	reduce-init
	;; 	segment-results)
	))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define run-in-parallel-and-reduce
  (λ (segments map-proc reduce-proc reduce-init)
    
    "Use futures to run a procedure in parallel, if
multiple cores are available. Take a list of SEGMENTS as
input, which are ranges of values to work on. MAP-PROC is
applied to the SEGMENTS using map. When the MAP-PROC calls
for all segments finished and returned values, the
REDUCE-PROC is applied to the map result using reduce and
the REDUCE-INIT argument."
    
    (define reduce
      (λ (f init ls)
	(if (empty? ls)
	    init
	    (reduce f (f init (first ls)) (rest ls)))))
    
    (let ([futures
	   (map (λ (seg)
		  (future ;; make-future
		   ;; Need to wrap in a thunk, to not
		   ;; immediately start evaluating.
		   (λ () (map-proc seg))))
		segments)])
      (let ([segment-results (map touch futures)])
	segment-results
	(reduce reduce-proc
		reduce-init
		segment-results)))))



(declare minterms-vector unified-minterms-vector-1)


;; call sequentially in post processing after the // region
(define (tag-minterms i umt)
  (when umt
	{mt1 <+ (first minterms-vector[i])}
	{mt2 <+ (second minterms-vector[i])}
	{minterms-ht[mt1] <- #t}
	{minterms-ht[mt2] <- #t}))


;; proc to be called with futures
(define (proc-unify-minterms-seg-and-tag seg)

  {function-unify-minterms-list <+ (λ (L) (apply function-unify-two-minterms-and-tag L))}
   
  {start <+ (segment-start seg)}
  {end <+ (segment-end seg)}
  (for ({i <+ start} {i <= end} {i <- i + 1})
       {mtL <+ minterms-vector[i]}
       ;; (nodebug
       ;; 	(dv mtL))
       {unified-minterms-vector-1[i] <- (function-unify-minterms-list mtL)}
       )
  )


;; proc to be called with futures
(define (proc-unify-minterms-seg seg)

  {function-unify-minterms-list <+ (λ (L) (apply unify-two-minterms L))}
   
  {start <+ (segment-start seg)}
  {end <+ (segment-end seg)}
  (for ({i <+ start} {i <= end} {i <- i + 1})
       {mtL <+ minterms-vector[i]}
       ;; (nodebug
       ;; 	(dv mtL))
       {unified-minterms-vector-1[i] <- (function-unify-minterms-list mtL)}
       )
  )



;; put below because of . period bug in SRFI 105 reader

;; proc to be called with //
(define (proc-unify-minterms-seg-inner-definitions seg)

  (define (function-map-with-escaping-by-kontinuation2 clozure list1 . more-lists) ;; ERROR: . period not supported in curly infix reader
    (call/cc (lambda (kontinuation)
	       (let ((lists (cons list1 more-lists))
		     (funct-continu ;; this function have the kontinuation in his environment 
		      (lambda (arg1 . more-args)
			(let ((args (cons arg1 more-args)))
			  (apply clozure kontinuation args))))) ;; a tester: (apply clozure (cons conti args))
		 
		 (apply map funct-continu lists)))))

  ;; compare two list of bits until we got more than one difference
  (define-syntax macro-function-compare-2-bits-with-continuation ;; continuation version of macro-compare-2-bits
    ;; i need a macro because of external function to the clozure
    (syntax-rules ()
      ((_) (let ((cnt 0)) ;; counter
	     (lambda (continuation b1 b2) (if (equal? b1 b2)
					      b1
					      (begin
						(set! cnt (add1 cnt))
						(when (> cnt 1) (continuation #f)) ;; escaping with the continuation
						'x)))))))

  
  (define (unify-two-minterms mt1 mt2)

    (nodebug
     (display-nl "unify-two-minterms : ")
     (dv mt1)
     (dv mt2))
    
    (function-map-with-escaping-by-kontinuation2  (macro-function-compare-2-bits-with-continuation) mt1 mt2))

  
  (nodebug
   (display "proc-unify-minterms-seg : ")
   (dv seg))
  
  (define function-unify-minterms-list (λ (L) (apply unify-two-minterms L))) ;; (apply unify-two-minterms-rec L))} ;; 
   
  (define start (segment-start seg))
  (define end (segment-end seg))
  (for ((define i start) (<= i end) (set! i (+ i 1)))
       (define mtL (vector-ref minterms-vector i))
       (nodebug
	(dv mtL))
       (vector-set! unified-minterms-vector-1 i (function-unify-minterms-list mtL))))





(define (funct-unify-minterms-set-1-unit-threads set1 set2)

  (nodebug
   (display-nl "funct-unify-minterms-set-1-unit-thread : begin"))
  
  (nodebug
   {set1-length <+ (length set1)}
   {set2-length <+ (length set2)}
   (dv set1-length)
   (dv set2-length)
   (display-nl "before Cartesian product set"))
  
  (nodebug
   (dvs set1)
   (dvs set2))

  ;; note : sorting is useless

  {minterms-set <+ (product-set-with-set-imperative set1 set2)} ;;(product-set-with-set-imperative-sorted set1 set2)} ;;(product-set-with-set set1 set2)} ;;(associate-set-with-set set1 set2)} ;; set multiplication : create list of pair of minterms - pair is a 2 element list MODIF

  (nodebug
   (dvs minterms-set))

  (nodebug
   (display-nl "after Cartesian product set")
   {minterms-set-length <+ (length minterms-set)}
   {minterms-set-first <+ (first minterms-set)}
   (dv minterms-set-length)
   (dv minterms-set-first))

  {minterms-vector <- (list->vector minterms-set)} ;; vector of pair (mathematic) of minterms - pair (mathematic) is a 2 element list, not a pair (Lisp)

  (nodebug
   (dv minterms-vector))

  {minterms-vector-length <+ (vector-length minterms-vector)}

  (nodebug
   (dv minterms-vector-length))

  ;; warning : // gives almost no better result
  ;; it (// procedures) uses Vectors instead of Lists, with Guile it is faster than the sequential procedures written initially in Lists 
  {nb-procs <+ 8} ;; 16} ;;(processor-count)} ;; 4};; C12 :1'25" with processor-count

  (when {minterms-vector-length < 500000} ;; 1'21" C12 with 16 threads on Mac OS M1 , 1'57" on Linux intel, 1' 52" with 8 threads
	{nb-procs <- 1})
  
  {segmts <+ (segment 0 {minterms-vector-length - 1} nb-procs)} ;; compute the segments

  (nodebug
   (dv segmts))

  {unified-minterms-vector-1 <- (make-vector minterms-vector-length #f)}

  (if {nb-procs = 1}
      (proc-unify-minterms-seg-and-tag (first segmts)) ;;(proc-unify-minterms-seg (first segmts))
      ($+>

       (nodebug
	(display-nl "before //"))
       
       ;; run the parallel code
       {threads <+ (map (λ (seg)
			  ;;(display "initialising thread ")
			  ;;(dv seg)
			  (thread
			   (λ ()
			     ;;(display "starting thread ")
			     ;;(dv seg)
			     (proc-unify-minterms-seg-and-tag seg)))) ;; (proc-unify-minterms-seg-inner-definitions seg))));; (proc-unify-minterms-seg seg))))
			segmts)}

       (nodebug
	(display-nl "waiting for threads to finish..."))
  
       ;; wait for threads to finish
       (map (λ (thread)
	      ;;(display "waiting thread ")
	      ;;(dv thread)
	      (thread-wait thread)) ;;(+ start-time max-sleep)))
	    threads)
       
       (nodebug
	(display-nl "after //"))))

  (nodebug
   {unified-minterms-vector-1-length <+ (vector-length unified-minterms-vector-1)}
   (dv unified-minterms-vector-1-length)
   (newline))

  ;; (unless {nb-procs = 1}
  ;; 	  (vector-for-each tag-minterms unified-minterms-vector-1))
  ;; tag the minterms in the hash table
  
  {unified-minterms-set-1 <+ (vector->list unified-minterms-vector-1)}
  
  (nodebug
   (dvs unified-minterms-set-1))
  
  {unified-minterms-set-2 <+ (filter (λ (x) x) unified-minterms-set-1)} ;; remove #f results
  (nodebug
   {unified-minterms-set-2-length <+ (length unified-minterms-set-2)}
   (dv unified-minterms-set-2-length))

  {unified-minterms-set <+ (remove-duplicates unified-minterms-set-2)} ;;(remove-duplicates-sorted unified-minterms-set-2)} ;; uniq MODIF
  (nodebug
   {unified-minterms-set-uniq-length <+ (length unified-minterms-set)}
   (dv unified-minterms-set-uniq-length))
  
  (nodebug
   (dvs unified-minterms-set))

  (nodebug
   (display-nl "funct-unify-minterms-set-1-unit-thread : end"))
      
  unified-minterms-set)




(define (funct-unify-minterms-set-1-unit-future set1 set2)

  (nodebug
   (display-nl "funct-unify-minterms-set-1-unit-future : begin"))
  
  (nodebug
   (dvs set1)
   (dvs set2))

  ;; note : sorting is useless

  {minterms-set <+ (product-set-with-set-imperative set1 set2)} ;;(product-set-with-set-imperative-sorted set1 set2)} ;;(product-set-with-set set1 set2)} ;;(associate-set-with-set set1 set2)} ;; set multiplication : create list of pair of minterms - pair is a 2 element list      MODIF

  (nodebug
   (dvs minterms-set))

  (nodebug
   {minterms-set-length <+ (length minterms-set)}
   {minterms-set-first <+ (first minterms-set)}
   (dv minterms-set-length)
   (dv minterms-set-first))

  {minterms-vector <- (list->vector minterms-set)} ;; vector of pair of minterms - pair is a 2 element list

  (nodebug
   (dv minterms-vector))

  {minterms-vector-length <+ (vector-length minterms-vector)}

  {nb-procs <+ 1}  ;; 32} ;; (processor-count)} ;; 32 : 1'25" for C12 on mac os M1 , 1' 42" on intel linux
  ;; 32" for C12 in Terminal mode with MacOS Ventura M1 and 31" with transducers

  (nodebug
   (dv nb-procs))
  
  {segmts <+ (segment 0 {minterms-vector-length - 1} nb-procs)} ;; compute the segments

  (nodebug
   (dv segmts))

  {unified-minterms-vector-1 <- (make-vector minterms-vector-length #f)}

  (if {nb-procs = 1}
      (proc-unify-minterms-seg-and-tag (first segmts)) ;;(proc-unify-minterms-seg (first segmts))
      ($+>

       (nodebug
	(display-nl "before //"))
       
       (run-in-parallel segmts  proc-unify-minterms-seg-and-tag) ;;proc-unify-minterms-seg) ;;proc-unify-minterms-seg-inner-definitions) ;; run the parallel code
       
       (nodebug
	(display-nl "after //"))

       ))
  
  ;;(vector-for-each tag-minterms unified-minterms-vector-1) ;; tag the minterms in the hash table
  
  {unified-minterms-set-1 <+ (vector->list unified-minterms-vector-1)}
  
  
  (nodebug
   (dvs unified-minterms-set-1))
  
  ;;{unified-minterms-set-2 <+ (filter (λ (x) x) unified-minterms-set-1)} ;; remove #f results
  ;; (nodebug
  ;;  {unified-minterms-set-2-length <+ (length unified-minterms-set-2)}
  ;;  (dv unified-minterms-set-2-length))

  ;;{unified-minterms-set <+ (remove-duplicates unified-minterms-set-2)} ;; (remove-duplicates-sorted unified-minterms-set-2)} ;; uniq MODIF
  ;; C12 in Terminal mode with MacOS Ventura M1 and 32"
  
  ;; (nodebug
  ;;  {unified-minterms-set-uniq-length <+ (length unified-minterms-set)}
  ;;  (dv unified-minterms-set-uniq-length))

  {unified-minterms-set <+ (remove-duplicates (filter (λ (x) x) unified-minterms-set-1))} ;; C12 in Terminal mode with MacOS Ventura M1 and 32"
  
  ;;{unified-minterms-set <+ (list-transduce (compose (tfilter (λ (x) x)) (tdelete-duplicates)) rcons unified-minterms-set-1)} ;; C12 in Terminal mode with MacOS Ventura M1 and 31"
  
  (nodebug
   (dvs unified-minterms-set))

  (nodebug
   (display-nl "funct-unify-minterms-set-1-unit-future : end"))
      
  unified-minterms-set)


(define (funct-unify-minterms-set-1-unit-vector-1cpu set1 set2)

  ;; (nodebug
  ;;  (display-nl "funct-unify-minterms-set-1-unit-vector-1cpu : begin"))
  
  ;; (nodebug
  ;;  (dvs set1)
  ;;  (dvs set2))

  ;;{function-unify-minterms-list <+ (λ (L) (apply function-unify-two-minterms-and-tag L))}
  
  ;; note : sorting is useless

  {minterms-set <+ (product-set-with-set-imperative set1 set2)} ;;(product-set-with-set-imperative-sorted set1 set2)} ;;(product-set-with-set set1 set2)} ;;(associate-set-with-set set1 set2)} ;; set multiplication : create list of pair of minterms - pair is a 2 element list      MODIF

  ;; (nodebug
  ;;  (dvs minterms-set))

  ;; (nodebug
  ;;  {minterms-set-length <+ (length minterms-set)}
  ;;  {minterms-set-first <+ (first minterms-set)}
  ;;  (dv minterms-set-length)
  ;;  (dv minterms-set-first))

  {minterms-vector <- (list->vector minterms-set)} ;; vector of pair of minterms - pair is a 2 element list

  ;; (nodebug
  ;;  (dv minterms-vector))

  {minterms-vector-length <+ (vector-length minterms-vector)}

  ;; (nodebug
  ;;  (dv minterms-vector-length))

  {nb-procs <+ 1} ;; (processor-count)} ;; 

  ;; (nodebug
  ;;  (dv nb-procs))
  
  {segmts <+ (segment 0 {minterms-vector-length - 1} nb-procs)} ;; compute the segment (only one !)

  ;; (nodebug
  ;;  (dv segmts))

  {unified-minterms-vector-1 <- (make-vector minterms-vector-length #f)}

  ;; (nodebug
  ;;  (display-nl "before proc-unify-minterms-seg-and-tag"))
  
  (proc-unify-minterms-seg-and-tag (first segmts))

  ;; (nodebug
  ;;  (display-nl "after proc-unify-minterms-seg-and-tag")
  ;;  (newline))
  
  ;;(vector-for-each tag-minterms unified-minterms-vector-1) ;; tag the minterms in the hash table
  
  {unified-minterms-set-1 <+ (vector->list unified-minterms-vector-1)}
  
  ;; (nodebug
  ;;  (dvs unified-minterms-set-1))
  
  {unified-minterms-set-2 <+ (filter (λ (x) x) unified-minterms-set-1)} ;; remove #f results
  ;; (nodebug
  ;;  {unified-minterms-set-2-length <+ (length unified-minterms-set-2)}
  ;;  (dv unified-minterms-set-2-length))

  {unified-minterms-set <+ (remove-duplicates unified-minterms-set-2)} ;; (remove-duplicates-sorted unified-minterms-set-2)} ;; uniq MODIF
  ;; (nodebug
  ;;  {unified-minterms-set-uniq-length <+ (length unified-minterms-set)}
  ;;  (dv unified-minterms-set-uniq-length))
  
  ;; (nodebug
  ;;  (dvs unified-minterms-set))

  ;; (nodebug
  ;;  (display-nl "funct-unify-minterms-set-1-unit-future : end"))
      
  unified-minterms-set)



;; par-map-vector version
;; (define (funct-unify-minterms-set-1-unit-para set1 set2)

;;   (nodebug
;;    (display-nl "funct-unify-minterms-set-1-unit-para : begin"))
  
;;   (nodebug
;;    (dvs set1)
;;    (dvs set2))
  
;;   {function-unify-minterms-list <+ (λ (L) (apply unify-two-minterms L))}

;;   ;; note : sorting is useless
;;   {minterms-set <+ (product-set-with-set-imperative-sorted set1 set2)} ;;(product-set-with-set-imperative set1 set2)} ;;(product-set-with-set set1 set2)} ;;(associate-set-with-set set1 set2)} ;; set multiplication : create list of pair of minterms

;;   (nodebug
;;    (dvs minterms-set))

;;   (debug
;;    (display-nl "before (par-map-vector function-unify-minterms-list minterms-vector)")
;;    {minterms-set-length <+ (length minterms-set)}
;;    {minterms-set-first <+ (first minterms-set)}
;;    (dv minterms-set-length)
;;    (dv minterms-set-first))

;;   {minterms-vector <- (list->vector minterms-set)}

;;   (nodebug
;;    (dv minterms-vector))
  
;;   {unified-minterms-vector-1 <- (par-map-vector function-unify-minterms-list minterms-vector)} ;; // code

;;   (debug
;;    (display-nl "after (par-map-vector function-unify-minterms-list minterms-vector)"))

;;   (vector-for-each tag-minterms unified-minterms-vector-1) ;; tag the minterms in the hash table
  
;;   {unified-minterms-set-1 <+ (vector->list unified-minterms-vector-1)}
  
  

;;   (nodebug
;;    (dvs unified-minterms-set-1))
  
;;   {unified-minterms-set-2 <+ (filter (λ (x) x) unified-minterms-set-1)} ;; remove #f results
;;   (nodebug
;;    {unified-minterms-set-2-length <+ (length unified-minterms-set-2)}
;;    (dv unified-minterms-set-2-length))

;;   {unified-minterms-set <+ (remove-duplicates-sorted unified-minterms-set-2)} ;; uniq
;;   (nodebug
;;    {unified-minterms-set-uniq-length <+ (length unified-minterms-set)}
;;    (dv unified-minterms-set-uniq-length))
  
;;   (nodebug
;;    (dvs unified-minterms-set))

;;   (nodebug
;;    (display-nl "funct-unify-minterms-set-1-unit-para : end"))
      
;;   unified-minterms-set)





;; par-map Guile version
;; (define (funct-unify-minterms-set-1-unit-par-map set1 set2)

;;   ;; call sequentially in post processing after the // region
;;   (define (extract-unified-minterm-and-tag-minterms umt-res-lst)
;;     {umt <+ (first umt-res-lst)}
;;     {mtL <+ (second umt-res-lst)}
;;     {mt1 <+ (first mtL)}
;;     {mt2 <+ (second mtL)}
;;     {minterms-ht[mt1] <- #t}
;;     {minterms-ht[mt2] <- #t}
;;     umt)
  
;;   (nodebug
;;    (display-nl "funct-unify-minterms-set-1-unit-par-map : begin"))
  
;;   (nodebug
;;    (dvs set1)
;;    (dvs set2))
  
;;   {function-unify-minterms-list <+ (λ (L)
;; 				     {res <+ (apply unify-two-minterms L)}
;; 				     (if res
;; 					 (list res L)
;; 					 res))} ;; return (unified-minterm (minterm1 minterm2)) or #f 

;;   ;; note : sorting is useless
;;   {minterms-set <+ (product-set-with-set-imperative-sorted set1 set2)} ;;(product-set-with-set-imperative set1 set2)} ;;(product-set-with-set set1 set2)} ;;(associate-set-with-set set1 set2)} ;; set multiplication : create list of pair of minterms

;;   (nodebug
;;    ;;(display "after call of recursive function associate-set-with-set: ")
;;    (dvs minterms-set))

;;   (debug
;;    (display-nl "before (par-map function-unify-minterms-list minterms-set)")
;;    {minterms-set-length <+ (length minterms-set)}
;;    {minterms-set-first <+ (first minterms-set)}
;;    (dv minterms-set-length)
;;    (dv minterms-set-first))
  
;;   {unified-minterms-set-1 <+ (par-map function-unify-minterms-list minterms-set)} ;; // code
;;   (debug
;;    (display-nl "after (par-map function-unify-minterms-list minterms-set)"))

;;   (nodebug
;;    (dvs unified-minterms-set-1))

;;   {unified-minterms-set-2 <+ (filter (λ (x) x) unified-minterms-set-1)} ;; remove #f results
;;   (nodebug
;;    {unified-minterms-set-length <+ (length unified-minterms-set-2)}
;;    (dv unified-minterms-set-length))

;;   {unified-minterms-set-3 <+ (map extract-unified-minterm-and-tag-minterms unified-minterms-set-2)} ;;  tag minterms and construct a list of unified minterms
  
;;   {unified-minterms-set <+ (remove-duplicates-sorted unified-minterms-set-3)} ;; uniq
;;   (nodebug
;;    {unified-minterms-set-uniq-length <+ (length unified-minterms-set)}
;;    (dv unified-minterms-set-uniq-length))
  
;;   (nodebug
;;    (dvs unified-minterms-set))

;;   (nodebug
;;    (display-nl "funct-unify-minterms-set-1-unit-par-map : end"))
      
;;   unified-minterms-set)


{ztest <+ 1}
(display "ztest=") (display ztest) (newline)
{3 * 5 + ztest}

(declare x)
{x <- 3 * 5 + ztest}

(define (area-square x) (* x x))

;; (define-overload-procedure area)

;; (if #t
;;     (overload area area-square (number?))
;;     #f)

;;(define vector? 3)


{#(1 2 3 4 5 6 7)[2 * 3 - 4 + 2]}


(overload-existing-n-arity-operator + add-n-lists (list? list?))



;; > {'(1 2 3) - '(4 5 6) - '(7 8 9)}
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = ((1 2 3) (4 5 6) (7 8 9))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (1 4 7)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (2 5 8)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (3 6 9)
;; '(-10 -11 -12)
;; > (+ '(1 2 3) '(4 5 6))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = ((1 2 3) (4 5 6))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (1 4)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (2 5)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (3 6)
;; '(5 7 9)
;; > {'(1 2 3) - '(4 5 6)}
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = ((1 2 3) (4 5 6))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (1 4)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (2 5)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (3 6)
;; '(-3 -3 -3)
;; > (- '(1 2 3) '(4 5 6))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = ((1 2 3) (4 5 6))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (1 4)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (2 5)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (3 6)
;; '(-3 -3 -3)
;; > (- '(1 2 3))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = ((1 2 3))
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (1)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (2)
;; check-arguments-for-n-arity : type = #<procedure:list?>
;; check-arguments-for-n-arity : args = (3)
;; '(-1 -2 -3)
(overload-existing-n-arity-operator - sub-n-lists (list? list?))

(display "+ =") (display +) (newline)

(+ '(1 2) '(3 4))

(display "before mult-num-list") (newline)
(define (mult-num-list k v) (map (λ (x) (* k x)) v))

(overload-existing-operator * mult-num-list (number? list?))



{t <+ {3 * '(1 2 3) + '(4 5 6) + '(7 8 9)}}
(display t) (newline)

;; ../../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/overload.scm:600:7: require: not at module level or top level in: (require (rename-in racket/base (* orig-proc)))
;; (define (foo) ;; ko
;;   ;;(declare x)
;;   (define x 23)
;;   (display "before define mult-num-list") (newline)
;;   (define (mult-num-list k v) (map (λ (x) (* k x)) v))
;;   (display "before overload *") (newline)
;;   (define-overload-existing-operator *)
;;   (overload * mult-num-list (number? list?))
;;   {t <+ {3 * '(1 2 3) + '(4 5 6) + '(7 8 9)}}
;;   {x <- 1 + x + 4 * 5}
;;   t)



(overload-existing-procedure length vector-length (vector?))
(overload-existing-procedure length string-length (string?))

(length #(1 2 3 4))
(length '(1 2 3))
(length "abcde")
