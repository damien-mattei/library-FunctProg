;;#lang racket
;; depending if you use LOGIKI as a main program or an include file you will have to comment or uncomment the above line
;;
;;#lang r5rs
;; #!r6rs
;; (import (rnrs))
;;#lang racket
;;

;; uncomment above for DrRacket Scheme, leave commented for other Schemes
;;
;;
;;                    λογικι
;;
;;                    LOGIKI
;;
;;
;; a program to compute logic symbolically
;;
;; Copyright (C) 2014-2019  Damien MATTEI
;;
;;
;; e-mail: damien.mattei@gmail.com 
;;         (damien.mattei@univ-cotedazur.fr, damien.mattei@unice.fr, damien.mattei@oca.eu)
;; 
;;
;;
;;
;; version 2.8
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
;; scheme version (tested with MIT-scheme, Bigloo, Dr Racket)
;;
;; windows : (load "c:/Users/mattei/Google Drive/info/logiki.scm")
;;
;; apple : (load "/Users/mattei/Google Drive/info/logiki.scm")
;;         (load "/Users/mattei/Dropbox/info/logiki.scm")
;;
;; linux : (load "/home/mattei/Dropbox/info/logiki-DrRacket.scm")
;;
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
;;  (dnf-infix-symb '(and (=> (and p q) r) (=> (not (and p q)) r))) -> not yet tested !
;;
;; the same in CNF:
;;
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
;;  with others Schemes:
;;
;;  (cnf-infix-symb '(and (=> (and p q) r) (=> (not (and p q)) r))) ->  ((p v r) ^ (!p v !q v r) ^ (q v r))
;;
;;  (enlight-dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b))))) -> (a^b^c)v(!a^!b^c)v(!a^b^!c)v(a^!b^!c)
;;
;; (enlight-dnf '(or (and a b) a)) -> a
;;
;; (enlight-dnf  '(and (=> (and p q) r) (=> (not (and p q)) r))) -> r



;; TODO: verifier rapidité en remplaçant les chaines de caracteres " * " et "(*)" par des nombres entiers

;; macros and functions definitions are included in files






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
       ((equal? p #t) #f) ; !#t = #f
       ((equal? p #f) #t)    ; !#f = #t
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
      (if (equal? #t p-simp) ; (or #t q)
	  #t ; tautology
	  (let ((q-simp (simplify-OR (arg2 expr)))) ; q simplified
	    (if (equal? #t q-simp) ;  (or p #t)
		#t ; tautology
		(cond
		 ((equal? p-simp #f) q-simp) ; (or #f q)
		 ((equal? q-simp #f) p-simp) ; (or p #f)
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
      (if (equal? #f p-simp) ; (and #f q)
	  #f ; antilogy
	  (let ((q-simp (simplify-AND (arg2 expr)))) ; q simplified
	    (if (equal? #f q-simp) ;  (and p #f)
		#f ; antilogy
		(cond
		 ((equal? p-simp #t) q-simp) ; (and #t q)
		 ((equal? q-simp #t) p-simp) ; (and p #t)
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
  
  (phase3-dnf (move-in-negations (elim-implications expr))))

;; simplify NF of forms :
;; ((a ^ b) v a) -> a
;; (a v b) ^ a -> a

(define (simplify-NF-by-unitary-reduction expr)
  (cond
   ((null? expr) expr)
   ((is-single-form? expr) expr)
   (else 
    (let* ((oper (operator expr))
	   (sL (args expr)) ; extract the arguments of operation
	   (encaps (lambda (expression)  ; put any remaining element in a set (list)
		     (if (symbol? expression)
			 (list expression)
			 expression)))
	   (encaps-args (map encaps sL))
	   (reducted-args (parse-args-by-unitary-reduction encaps-args encaps-args)) ; do unitary reduction
	   (decaps (lambda (expression) ; extract any element from a list
		     (if (singleton? expression)
			 (first expression)
			 expression)))
	   (decaps-args (map decaps reducted-args)))
      (if (only-one? decaps-args)
	  (first decaps-args)
	  (cons oper decaps-args)))))) ;; reconstruct operation expression with simplified arguments list


;; todo : sort arguments by size 

;; simplify DNF of form ((a ^ b) v a) -> a
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
   ((is-single-form? expr) expr)
   (else 
    (let* ((sL (args expr)) ; extract the arguments of 'or
	   (encaps (lambda (expression)  ; put any remaining element in a set (list)
		     (if (symbol? expression)
			 (list expression)
			 expression)))
	   (encaps-args (map encaps sL))
	   (reducted-args (parse-args-by-unitary-reduction encaps-args encaps-args)) ; do unitary reduction
	   (decaps (lambda (expression) ; extract any element from a list
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
	    (parse-args-by-unitary-reduction L W)))))           ;; s !C- W : G(sL,W) -> G(L,W)

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
  (compact-display-bracket (prefix->infix-symb  (simplify-DNF-by-unitary-reduction (simplify-logic (n-arity (simplify-OR (simplify-AND (dnf expr)))))))))





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
  (simplify-DNF-by-unitary-reduction (simplify-logic (n-arity (simplify-OR (simplify-AND (dnf (n-arity-operation->binary-operation expr))))))))


;; (infix-symb-min-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) (not b) (not c) d) (and (not a) (not b) c (not d)) (and (not a) b (not c) d)  (and (not a) b c (not d))  (and (not a) b c d)  (and a (not b) (not c) (not d)) (and a (not b) (not c) d)  (and a (not b) c (not d))   (and a b c (not d))))
;; '((!b ^ !c) v (c ^ !d) v (!a ^ b ^ d))
(define (infix-symb-min-dnf expr)
  (prefix->infix-symb  (minimal-dnf expr)))

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
(define (cnf-infix expr)
  (prefix->infix (cnf-n-arity-simp expr)))

(define (cnf-n-arity-simp expr)
  (simplify-NF-by-unitary-reduction (simplify-logic (n-arity (simplify-AND (simplify-OR (cnf (n-arity-operation->binary-operation expr))))))))




;; return the literal of expression of type (not 'a) , 'a
;; in case of boolean return : #f -> F , #t -> T
(define (get-literal expr)
  (if (isNOT? expr)
      (first (rest expr))
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
(define (is-AND-antilogy? expr)
  (letrec ((lep (args expr)) ;; list of expressions which could be literals (ex 'b , 'x ) or negations (not (b))
           (detect-antilogy (lambda (listExpr)
                              (if (null? listExpr) 
                                  #f
                                  (if (symbol? (first listExpr)) ;; we search for a literal ex: 'x    
                                      (if (search-not-lit? (first listExpr) lep) ;; search antilogy with literal and the whole operands of AND
                                          #t
                                          (detect-antilogy (rest listExpr))) ;; check the rest of the list with another literal
                                  (detect-antilogy (rest listExpr))))))) ;; check the rest of the list to find a literal                              
    (detect-antilogy lep)))

;; remove the antilogies out of a list of ANDed expressions... whaooo j'aime cette phrase...
;; (remove-antilogies '((and c (not a) a) (and c (not a) (not b)))) -> '((and c (not a) (not b)))
;;  (remove-antilogies '()) -> '()
;; (remove-antilogies '(c (and a b (not b)))) -> '(c)
(define (remove-antilogies andList) ;; argument is a list of ANDed expressions
  (cond
   ((null? andList) '())
   ((and (isAND? (first andList)) (is-AND-antilogy? (first andList))) (remove-antilogies (rest andList)))
   ;;[((isAND? (first andList)) . and . (is-AND-antilogy? (first andList))) (remove-antilogies (rest andList))]
   (else (cons (first andList) (remove-antilogies (rest andList))))))


;; input : get an OR example: (OR x (not x) y z)
;; will check all operands of OR to see if there is a tautology in it 
;;
;; >  (is-OR-tautology? '(or a (not b) (not b) (not a))) -> #t
;; >  (is-OR-tautology? '(or a (not b) (not c))) -> #f
(define (is-OR-tautology? expr)
  (letrec ((lep (args expr)) ;; list of expressions which could be literals (ex 'b , 'x ) or negations (not (b))
           (detect-tautology (lambda (listExpr)
                               (if (null? listExpr) 
                                  #f
                                  (if (symbol? (first listExpr)) ;; we search for a literal ex: 'x    
                                     (if (search-not-lit? (first listExpr) lep) ;; search tautology with literal and the whole operands of OR
                                        #t
                                        (detect-tautology (rest listExpr))) ;; check the rest of the list with another literal
                                     (detect-tautology (rest listExpr))))))) ;; check the rest of the list to find a literal                              
    (detect-tautology lep)))

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
(define (remove-tautologies orList) ;; argument is a list of ORed expressions
  (cond
   ((null? orList) '())
   ;;[((isOR? (first orList)) . and . (is-OR-tautology? (first orList))) (remove-tautologies (rest orList))]
   ((and (isOR? (first orList)) (is-OR-tautology? (first orList))) (remove-tautologies (rest orList)))
   (else (cons (first orList) (remove-tautologies (rest orList))))))

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
  (if (is-OR-tautology? dnfExpr)
      #t
      (let ((operandList (remove-antilogies (rest dnfExpr)))) ;; first we remove antilogies in the operands
	(if (null? (rest operandList)) ;; if we have only one element in the result list
	    (first operandList) ;; we can forget the or operator
	    (cons 'or operandList)))))
 

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
      (let ((operandList (remove-tautologies (rest cnfExpr)))) ;; first we remove tautologies in the operands
	(if (null? (rest operandList)) ;; if we have only one element in the result list
	    (first operandList) ;; we can forget the or operator
	    (cons 'and operandList)))))

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
  ;; first we will remove duplicates and sort arguments
  (let* ((arg-lst (args norm-form)) ;; define arguments list
	 (oper (operator norm-form)) ;; define operator
	 (arg-list-no-dup (map remove-duplicates-in-operation arg-lst)) ;; argument list without duplicate elements
	 (arg-list-no-dup-sorted (map sort-arguments-in-operation arg-list-no-dup));; argument list sorted
	 ;;(expr-no-dup (cons oper arg-list-no-dup)) ;; reconstruct expression with removed duplicate in operands
	 (expr-no-dup-sorted (cons oper arg-list-no-dup-sorted)))
    (if (equal? (first expr-no-dup-sorted) 'and)
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
  (cond 
   ((null? expr) expr)
   ((symbol? expr) expr) 
   ((is-single-form? expr) (simplify-SF expr)) 
   (else (sort-arguments-in-operation (remove-duplicates-in-operation (simplify-*NF expr))))))

;; simplify Single Form
;; (simplify-SF  '(and b a b (not b))) -> #f
(define (simplify-SF expr)
  (let* ((oper (operator expr)) ;; define operator
	 (expr-no-dup (remove-duplicates-in-operation expr)) ;; remove duplicate symbols
	 (expr-no-dup-sorted (sort-arguments-in-operation expr-no-dup))) ;; sort variables
    (if (equal? oper 'and) ;; AND => search for antilogies
	(if (is-AND-antilogy? expr-no-dup-sorted) #f expr-no-dup-sorted)
	(if (is-OR-tautology? expr-no-dup-sorted) #t expr-no-dup-sorted)))) ;;  OR => search for tautologies


(define expression<?
  (lambda (x y)
    (string<? (lower-literal-symbol x) (lower-literal-symbol y))))


(define expression-literal-first<?
  (lambda (x y)
    (cond ((isOR-AND? x) #f)
	  ((isOR-AND? y) #t)
	  (string<? (lower-literal-symbol x) (lower-literal-symbol y)))))

(define lower-literal-symbol
  (lambda (s)
    (string-downcase (expression->string (get-first-literal s))))) ;; 'Ci -> "ci", '(not A) -> "a"

(define expression->string
	  (lambda (expr2)
	    (cond ((symbol? expr2) (symbol->string expr2)) 
		  ((boolean? expr2) (if expr2 "T" "F")) ;; #t -> "T", #f -> "F"
		  (else (error "expression->string: do not know how to handle this expression" expr2)))))

(define expression-most-little-literal-first<?
  (lambda (x y)
    (cond ((isOR-AND? x) #f)
	  ((isOR-AND? y) #t)
	  (literal<? (get-first-literal x) (get-first-literal y)))))

;; (literal<? 'V0x10x11 'V1x11x10) -> #f
(define (literal<? a b)
  (let* ((ax 0)
	 (bx 0)
	 (a1 0)
	 (b1 0) 
	 ;;(a0 0)
	 ;;(b0 0)
	 (as (symbol->string a))
	 (bs (symbol->string b)))

    (for (i 1 (- (string-length as) 1))
	 
	 (case (string-ref as i)

	   ((#\x) (incf ax))
	   ((#\1) (incf a1))
	   #;((#\0) (incf a0)))

	 (case (string-ref bs i)
	   ((#\x) (incf bx))
	   ((#\1) (incf b1))
	   #;((#\0) (incf b0))))
    
    (cond ((> ax bx) #t)
	  ((< ax bx) #f)
	  (else ;; ax = bx
	   (> a1 b1)))))
		   

			     
    
;; (define (error2)
;;   (car '()))

;; sort operands in a logic expression
;; (sort-arguments-in-operation '(or c a b)) -> '(or a b c)
;; (sort-arguments-in-operation '(or c (not a) b)) -> '(or (not a) b c)
;; (sort-arguments-in-operation '(or c (not a) b (or c d))) -> '(or (not a) b (or c d) c)
(define (sort-arguments-in-operation expr)

  (if (isOR-AND? expr)

      (let* ((args-list (args expr)) ;;'(or c a b) -> '(c a b)
	    
	     ;;(sorted-args (sort args-list #:key lower-literal-symbol string<?)) ;; symbol<?)) ;; '(Ci a b) -> '(a b Ci)
	     (sorted-args (sort-arguments args-list))
	     (oper (operator expr))) ;; define operator : (or Ci a b) -> or
	
	(cons oper sorted-args))
      
      expr)) ;; we have not a binary operator but a literal or negation of literal


;; (sort-arguments '(Ci c d (not a) b )) -> '((not a) b c Ci d)
;; (sort-arguments '( Ci (and c d) (not a) b )) -> '((not a) b (and c d) Ci)
(define (sort-arguments args-list)

	 ;;(sort args-list #:key lower-literal-symbol string<?) ;; symbol<?)) ;; '(Ci a b) -> '(a b Ci)
	 (sort args-list expression<?)

  )


;; (sort-arguments-in-operation-literal-first '(or (and V011x V0x01) V01x1 ))
;; '(or V01x1 (and V011x V0x01))
(define (sort-arguments-in-operation-literal-first expr)

  (if (isOR-AND? expr)

      (let* ((args-list (args expr)) ;;'(or Ci a b) -> '(Ci a b)
	    
	     ;;(sorted-args (sort args-list #:key lower-literal-symbol string<?)) ;; symbol<?)) ;; '(Ci a b) -> '(a b Ci)
	     (sorted-args (sort args-list expression-literal-first<?))
	     (oper (operator expr))) ;; define operator : (or Ci a b) -> or
	
	(cons oper sorted-args))
      
      expr)) ;; we have not a binary operator but a literal or negation of literal


;; (sort-arguments-in-operation-most-little-literal-first '(or (and V011x V0x01)  V01x1 V11x1))
;; -> '(or V11x1 V01x1 (and V011x V0x01))
(define (sort-arguments-in-operation-most-little-literal-first expr)

  (if (isOR-AND? expr)

      (let* ((args-list (args expr)) ;;'(or Ci a b) -> '(Ci a b)
	    
	     ;;(sorted-args (sort args-list #:key lower-literal-symbol string<?)) ;; symbol<?)) ;; '(Ci a b) -> '(a b Ci)
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

 ;; test for a monomial negation
;; (is-monomial-NOT? '(not x)) -> #t
;; (is-monomial-NOT? '(not (not x))) -> #f
(define (is-monomial-NOT? expr)
  (and (isNOT? expr) (symbol? (car (cdr expr)))))

;; predicate to know if an expression is a single form 
;;
;; examples of single forms: (not x) , (or x y z) , x
;; counter examples: (and (or x y))
;;
;; (is-single-form? 'x) -> #t
;; (is-single-form? '(not x)) -> #t
;; (is-single-form? '(not (not x))) -> #f
;; (is-single-form? '(and (or x y))) -> #f
;; (is-single-form? '(or x y z)) -> #t
;; (is-single-form?  '(and a b (not b))) -> #t
;;
(define (is-single-form? expr)
  (cond
   ((symbol? expr) #t) ;; test for a single literal
   ((is-monomial-NOT? expr) #t) ;; test for a monomial negation
   ((isNOT? expr) #f)
   (else ;; we have a OR or AND
    ;; we check that all the arguments are literals or single negations
    (andmap
     (lambda (q)
       (or (symbol? q) (is-monomial-NOT? q)))
     (args expr)))))






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
  (phase3-cnf (move-in-negations (elim-implications expr))))

;; a simplification package for DNF in n-arity form
;;
;; (prefix->infix (simplify-n-arity-dnf '(or (and (and a b) (not (and c (or (and a (not b)) (and (not a) b))))) (and (not (and a b)) (and c (or (and a (not b)) (and (not a) b)))))))
;;    -> '((a and b) or (a and b and !c) or (a and !b and c) or (!a and b and c))
;;
(define (simplify-n-arity-dnf expr)
  (simplify-logic (n-arity (simplify-OR (simplify-AND (simplify-DNF-by-unitary-reduction (dnf expr)))))))









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
	   (var-list (collect-variables disj-norm-form)) ;; variable list
	   (and-terms (args disj-norm-form)) ;; conjunctives minterms
	   ;; variable list of expanded minterms 
	   (expanded-var-terms
	    (apply append (map
			   
			   (lambda
			       (min-term)
			     (expand-minterm var-list min-term))
			   
			   and-terms)))
	   
	   (sorted-expanded-var-terms (map sort-arguments expanded-var-terms)) ;; sorted variable list of expanded minterm
	   (uniq-sorted-expanded-var-terms (remove-duplicates-sorted sorted-expanded-var-terms))
	   (sorted-expanded-and-term
	    (map
	     
	     (lambda
		 (literal-list)
	       (if (singleton-set? literal-list)
		   (first literal-list)
		   (cons 'and literal-list)))
	     
	     uniq-sorted-expanded-var-terms))
	   
	   (maximal-disj-norm-form (cons 'or sorted-expanded-and-term)))

      (debug-mode-on)
      (when debug-mode
	    (dv disj-norm-form)
	    (dv var-list)
	    (dv and-terms)
	    (dv expanded-var-terms)
	    (dv sorted-expanded-var-terms)
	    (dv uniq-sorted-expanded-var-terms)
	    (dv sorted-expanded-and-term)
	    (debug-mode-reload))
      maximal-disj-norm-form))))




(define (is-simple-form? expr)
  (is-single-form? expr))

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

  (let* (
	 (var-list (collect-variables expr)) ;; variable list
	 (disj-norm-form (dnf-n-arity-simp expr)) ;; disjunctive form
	 (essential-prime-implicants '())
	 (formula-find-with-Quine-Mc-Cluskey '())
	 (infix-disj-norm-form (dnf-infix-symb disj-norm-form))
	 (non-essential-prime-implicants '())
	 (min-expr '())
	 (petrick-expr '())
	 )
    
    (debug-mode-on)
    (when debug-mode
	  (dv disj-norm-form)
	  (dv var-list))
    (debug-mode-reload)
    
    (if (not (pre-check-Quine-Mc-Cluskey disj-norm-form))

	disj-norm-form

	(begin

	  (set! essential-prime-implicants (Quine-Mc-Cluskey disj-norm-form var-list))

	  (set! formula-find-with-Quine-Mc-Cluskey (essential-prime-implicants-list->formula essential-prime-implicants var-list))
	  (debug-mode-on)
	  (when debug-mode
		(dv disj-norm-form)
		(dv var-list)
		(dv essential-prime-implicants)
		(dv infix-disj-norm-form)
		(dv formula-find-with-Quine-Mc-Cluskey)
		(debug-mode-reload))

	  (set! min-expr formula-find-with-Quine-Mc-Cluskey)
	  
	  (when (not feepi) ;; if Quine Mc Cluskey method did not worked completely to a minimal function

		;; we have to minimize again
		(set! non-essential-prime-implicants
		      (set-difference prime-implicants-lst essential-prime-implicants))
		(debug
		 (dv non-essential-prime-implicants)
		 (dv non-expressed-minterms))
		(set! petrick-expr (Petrick non-essential-prime-implicants var-list))
		(dv petrick-expr)
		(dv min-expr)

		(set! min-expr
		      (if (is-single-form? min-expr)
			  (if (is-single-form? petrick-expr)
			      (list 'or min-expr petrick-expr)
			      `(,(operator petrick-expr) ,min-expr ,@(args petrick-expr))) ;; operator must be an OR
			  (if (is-single-form? petrick-expr)
			      `(,(operator min-expr) ,@(args min-expr) ,petrick-expr) ;; operator must be an OR
			      `(,(operator min-expr) ,@(args min-expr) ,@(args petrick-expr)))))) ;; operator must be an OR
	

	  (dv min-expr)

	  min-expr))))
		
  


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
		   (lambda (epi) (binary->min-term epi var-list))
		   essential-prime-implicants))))









;;> (prime-implicants minterms-ht) -> '((x 1 x 1) (1 x x 0) (1 1 x x))
(define (prime-implicants mt-ht) ;; argument is a minterms hash table
  ;;(map first (filter (lambda (p) (not (cdr p))) (hash->list mt-ht)))) ;; DrRacket
  (map car (filter (lambda (p) (not (cdr p))) (hash->list mt-ht)))) ;; DrRacket
  ;; (map
  ;;  first
  ;;  (filter (lambda (p) (not (cdr p)))
  ;; 	   (map cons (hashtable-key-list mt-ht) (hashtable->list mt-ht)))))








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
  (hash-clear! ht)
  (map (lambda (e) (hash-set! ht e val)) s)) ;; DrRacket
  ;;(map (lambda (e) (hashtable-put! ht e val)) s)) 



;; list of non expressed minterms
(define non-expressed-minterms '())


;; identifying essential prime implicant array
;; first line : minterms
;; first row : prime-implicants
(define iepi '()) ;; for now i do not know the array dimension

(define lgt-pi '())
(define lgt-mt '())

(define (identify-essential-prime-implicants  prime-implicants minterms)
  (let* (;;(lgt-pi (length prime-implicants))
	 ;;(lgt-mt (length minterms))
	 ;; identifying essential prime implicant array
	 ;; first line : minterms
	 ;; first row : prime-implicants
	 ;;(iepi (make-array-2d (+ lgt-mt 1) (+ lgt-pi 1) 0)) ;; two dimensions array
	 ;;(array-iepi (lambda (x y) (vector-ref (vector-ref iepi y) x)))
	 (vct-prime-implicants (list->vector prime-implicants))
	 (essential-prime-implicants-list '())
	 (cpt-mt 0) ;; counter of minterms
	 (y-pos-epi 0) ;; position of essential prime implicant in colomn if there exists one
	 (star-in-column #f)) ;; at the beginning

    
    (debug-mode-on)
    (when debug-mode
	  (display-nl "identify-essential-prime-implicants ::")
	  (dv prime-implicants)
	  (dv minterms))
    
    (set! lgt-pi (length prime-implicants))
    (set! lgt-mt (length minterms))
    
    ;; identifying essential prime implicant array
    ;; first line : minterms
    ;; first row : prime-implicants
    (set! iepi (make-array-2d (+ lgt-mt 1) (+ lgt-pi 1) 0)) ;; two dimensions array
    (when debug-mode
	  (dv-2d iepi))
    
    (vector-set! iepi 0 (list->vector (cons '() minterms))) ;; set the title line

    (when debug-mode
	  (dv-2d iepi)
	  (debug-mode-reload))
	  

    (when debug-mode
	  
	  ;; (newline)
	  ;; (display "(array-iepi 1 2) =")
	  ;; (display (array-iepi 1 2))
	  ;; (newline)
	  
	  (display "(array-2d-ref iepi 1 2) =")
	  (display (array-2d-ref iepi 1 2))
	  (newline)
	  
	  (display "(array-2d-set! iepi 1 2 1) =")
	  (display (array-2d-set! iepi 1 2 1))
	  (newline)
	  
	  (display-array-2d iepi)
	  
	  (display "(array-2d-ref iepi 1 2) =")
	  (display (array-2d-ref iepi 1 2))
	  (newline)
	  
	  (display "(funct-array-2d-set! iepi 1 2 2) =")
	  (display (funct-array-2d-set! iepi 1 2 2))
	  (newline)

	  (display "(array-2d-ref iepi 1 2) =")
	  (display (array-2d-ref iepi 1 2))
	  (newline))

    (debug-mode-on)
    (when debug-mode
	  (display vct-prime-implicants)
	  (newline)
	  (dv-2d vct-prime-implicants))

    ;; construction of the array
    ;; set the left column containing prime implicants 
    (for (y 0 (- lgt-pi 1))
	 (begin
	   ;;(display-expr-nl  (vector-ref vct-prime-implicants y))
	   ;;(display-symb-nl y)
	   (array-2d-set!
	    iepi
	    0
	    (+ y 1)
	    (vector-ref vct-prime-implicants y)))
	    ;;(display-symb-nl iepi)))
	 )

    (when debug-mode
	  (newline)
	  (dv-2d iepi))

    ;; identify prime implicants
    (for (x 1 lgt-mt)
	 (set! cpt-mt 0)
	 (for (y 1 lgt-pi)

	      (if (compare-minterm-and-implicant
		   (array-2d-ref iepi 0 y)
		   (array-2d-ref iepi x 0))
		  ;; then
		  (begin
		    (incf cpt-mt)
		    (when (= 1 cpt-mt)
			  (set! y-pos-epi y)) ;; position of essential prime implicant
		    (array-2d-set! iepi x y " * "))
		  ;; else
		  (array-2d-set! iepi x y "   "))) ;; end for y
	 
	 (when (= 1 cpt-mt) ;; essential prime implicant
	       (array-2d-set! iepi x y-pos-epi "(*)")
	       ;; add essential prime implicant to list
	       (set! essential-prime-implicants-list (cons (array-2d-ref iepi 0 y-pos-epi) essential-prime-implicants-list)))) ;; end for x


    (when debug-mode
	  (newline)
	  (dv-2d iepi))

    (debug-mode-reload)
    
    (set! essential-prime-implicants-list (remove-duplicates essential-prime-implicants-list))

    (set! feepi #t)
    
    ;; check if function is expressed by essential implicants
    (for/break break-x (x 1 lgt-mt) ;; loop over minterms
	 
	       (for/break break-y (y 1 lgt-pi) ;; loop over prime implicants

			  ;; check wether prime implicant is an essential one?
			  (when (member (array-2d-ref iepi 0 y) essential-prime-implicants-list)
				
				(when debug-mode
				      (de (array-2d-ref iepi 0 y))
				      (de (array-2d-ref iepi x y)))
				
				;; is the essential prime implicant expressing this minterms?
				(when (or (string=?  (array-2d-ref iepi x y) "(*)")
					  (string=?  (array-2d-ref iepi x y) " * "))
				      (when debug-mode
					    (display-nl "star-in-column"))
				      (set! star-in-column #t)
				      (break-y)))) ;; that's enought! we know the minterm is expressed.

	       ;; end for/break break-y
	       
	       (unless star-in-column
		       (set! feepi #f) ;; function non expressed by prime implicants
		       ;; add minterm to non expressed minterms list
		       (set! non-expressed-minterms
			     (insert (array-2d-ref iepi x 0) non-expressed-minterms))
		       ;;(break-x) ;; remove break-x as we have to check all the minterms now
		       )
	       
	       (set! star-in-column #f))  ;; set it for the next loop
    ;; end for/break break-x

    essential-prime-implicants-list))


(define feepi #f) ;; function expressed by essential prime implicants


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
(define (Quine-Mc-Cluskey disj-norm-form var-list)
  (display-nl "Entering Quine-Mc-Cluskey")
  (let* (
	 (and-terms (args disj-norm-form)) ;; conjunctives minterms
	 ;; variable list of expanded minterms 
	 (expanded-var-terms
	  (begin
	    (set! debug-mode-save debug-mode)
	    (set! debug-mode #t)
	    (when debug-mode
		  (dv and-terms))
	    (set! debug-mode debug-mode-save)
		  (apply append
			 (map (lambda (min-term)
				(expand-minterm var-list min-term))
			      and-terms))))
	 (sorted-expanded-var-terms (map sort-arguments expanded-var-terms)) ;; sorted variable list of expanded minterms
	 (binary-minterms (map var->binary sorted-expanded-var-terms)) ;; minterms in binary form
	 (sorted-binary-minterms (sort binary-minterms minterm-binary-weight-number<?)) ;; sorted binary minterms
	 (uniq-sorted-binary-minterms (remove-duplicates-sorted sorted-binary-minterms))  ;; prevoir uniq pourquoi???? sais plus !
	 (minterms uniq-sorted-binary-minterms)
	 (set-of-sets-of-minterms
	  ;; (begin
	  ;;   (de (order-by-weight-basic uniq-sorted-binary-minterms)) ;; set of sets of minterms ordered by weight
	  ;;   (error "escaping from Quine-Mc-Cluskey")))
	  ;; (order-by-weight-basic uniq-sorted-binary-minterms))
	  (order-by-weight-minterms uniq-sorted-binary-minterms)) ;; set of sets of minterms ordered by weight
	 (unified-minterms 
	  (begin
	    (set! debug-mode-save debug-mode)
	    (set! debug-mode #t)
	    (when debug-mode (display-nl "Quine-Mc-Cluskey:"))
	    (init-hash-table-with-set-and-value minterms-ht minterms #f)
	    (dv minterms-ht)
	    (set! debug-mode debug-mode-save)
	    (recursive-unify-minterms-set-of-sets  set-of-sets-of-minterms)))
	 (essential-prime-implicants
	  (begin
	    (set! prime-implicants-lst (begin
					 (set! debug-mode debug-mode-save)
					 (prime-implicants minterms-ht)))
	    (identify-essential-prime-implicants prime-implicants-lst minterms)))
	 )

    (dv disj-norm-form)
    (dv var-list)
    (dv and-terms)
    (dv expanded-var-terms)
    (dv sorted-expanded-var-terms)
    (dv binary-minterms)
    (dv sorted-binary-minterms)
    (dv uniq-sorted-binary-minterms)
    (dvsos set-of-sets-of-minterms)
    (dv unified-minterms)
    (dv minterms-ht)
    (dv prime-implicants-lst)
    (dv essential-prime-implicants)
    (display-nl "function expressed by essential prime implicants ?")
    (dv feepi)
    essential-prime-implicants))


(define prime-implicants-lst '())
  

(define (Petrick non-essential-prime-implicants var-list)

  ;; create the conjunction of disjunction expression

  (let ((mt '())
	(conj-expr '())
	(prim-imp '())
	(col '())
	(disj-expr '())
	(disj-expr-sorted '())
	(mt-var '())
	(missing-term '()))

    (display-nl "Entering Petrick...")
    
    (for (x 1 lgt-mt) ;; loop over minterms
	       
	 (set! mt (array-2d-ref iepi x 0))
	 
	 (when (member mt non-expressed-minterms) ;; non expressed minterm
	       
	       (set! col '())
	       
	       (for (y 1 lgt-pi) ;; loop over prime implicants
		    
		    (set! prim-imp (array-2d-ref iepi 0 y)) ;; prime implicant
		    
		    ;; check wether prime implicant is a non essential one?
		    (when (member prim-imp non-essential-prime-implicants)
				
			  ;; is the non essential prime implicant expressing this minterms?
			  (when (string=? (array-2d-ref iepi x y) " * ")
				(insert-set! (minterm->var prim-imp) col))))
		    
	       ;; end for y
	       
	       (if (singleton-set? col)
		   (set! col (car col))  ;; ( V ) -> V
		   (insert-set! 'or col))  ;; (V1 V2 ...) -> (or V1 V2 ...)
	       
	       (insert-set! col conj-expr)))
    
    ;; end for x

    (if (singleton-set? conj-expr)
	(set! conj-expr (car conj-expr))  ;; ( conj-expr ) -> conj-expr
	(insert-set! 'and conj-expr))  ;; (e1 e2 ...) -> (and e1 e2 ...)

    (dv conj-expr)

    ;; find the disjunctive form
    (set! disj-expr (dnf-n-arity-simp conj-expr))
  
    (dv disj-expr)

    ;; sorting terms
    ;; sort by x < 1 < 0
    (set! disj-expr-sorted (sort-arguments-in-operation-most-little-literal-first disj-expr))
    (dv disj-expr-sorted)
    
    ;; get the shortest minterm
    (if (isOR-AND? disj-expr-sorted)
	(set! mt-var (first (args disj-expr-sorted)))
	(set! mt-var disj-expr-sorted))

    (dv mt-var)

    (set! mt (var->minterm mt-var))

    (dv mt)

    ;; TODO: possible bug missing term could be an expression ? (multiple terms)
    (set! missing-term (essential-prime-implicants-list->formula
			(list mt)
			var-list))

    (dv missing-term)

    missing-term
    
    ) ;; end let
      
)




