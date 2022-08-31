#lang reader "racket/SRFI-105.rkt"

(require (rename-in racket/base [for for-rack])) ;; backup original Racket 'for'

(require "../Scheme-PLUS-for-Racket/main/Scheme-PLUS-for-Racket/Scheme+.rkt")

;; init and start the syracuse program

(include "syntactic-sugar.scm")
(include "debug.scm")
(include "list.scm")
(include "operation.scm")
(include "symbolic.scm")
(include "simplify.scm")
(include "racket/escape-char-racket-scheme.scm")
(include "display.scm")
(include "increment.scm")
(include "binary-arithmetic.scm")
(include "for-next-step.scm")
(include "pair.scm")
(include "number.scm")
(include "stat.scm")
(include "cycle.scm")

(require racket/format)

(include "syracuse.scm")

;; compute probability of Ck knowing Bk-1 for Collatz function
;;
;; definition of Sk: a bit computed with two operands and a carry, see calculus below:
;;
;;  C        C                       C
;;   n+2      k                       0
;;   1.......b......................10    2b + C0 = a with a = b
;;            k-1                                           k   k-1
;;    1......bb......................1    b is odd
;;            k k-1
;;  ----------------------------------
;;  ..S......SS........S.............0    S
;;     n      k k-1     i            0    index
;;
;;
;;
;; > (collatz-proba-stat #b1000000000 1)
;; alea = 512
;; omega-universe = 256
;; Bk-1-true = 128
;; Probability of Bk-1 = Bk-1-true / omega-universe = 128 / 256 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 86 / 128 = 43/64 = 0.671875
;; Probability of Ck = Ck-true / omega-universe = 129 / 256 = 129/256 = 0.50390625
;;
;; > (collatz-proba-stat #b1000000000 0)
;; alea = 512
;; omega-universe = 256
;; Bk-1-true = 128
;; Probability of Bk-1 = Bk-1-true / omega-universe = 128 / 256 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 85 / 128 = 85/128 = 0.6640625
;; Probability of Ck = Ck-true / omega-universe = 128 / 256 = 1/2 = 0.5
;; > ; seems to converge to 2/3
(define (collatz-proba-stat Ck C0)

  {Bk-1 <+ (shift-right Ck)}
  {alea <+ Ck} ;; (shift-left Ck 8)} ;; 8 is arbitrary
  {omega-universe <+ 0}
  {Bk-1-true <+ 0}
  {Ck-true <+ 0}
  {Ck-true-knowing-Bk-1 <+ 0}
  {display-enabled <+ #f}
  {mask <+ {Ck - 1}} ;; example: Ck = #b100000 , Ck - 1 = #b11111

  (dv alea)
  
  (for-basic (b alea)
       ;;(incf omega-universe)
       
       (when (flag-set? #b1 b) ; only for odd numbers
	 
	 (incf omega-universe)
	 {S <+ {(bitwise-ior C0 (shift-left b)) + b}} ; shift to left and set lowest significant bit and finally add b, i.e compute 2b+1+b = 3b+1
	 {Bk-1-set <+ (flag-set? Bk-1 b)} ;; Bk-1 flag

	 
	 
	 {S-masked <+ {(bitwise-and (bitwise-ior C0 (shift-left b)) mask) + (bitwise-and b mask)}} ; shift to left and set lowest significant bit and mask the upper partial result and finally add b masked too, i.e compute 2b+1+b = 3b+1 but not some high bits that will be the carry
	 {Ck-set <+ (flag-set? Ck S-masked)} ;; Ck flag
	     
	 (when display-enabled
	   (display (padding (bitwise-ior C0 (shift-left b)))) (newline)
	   (display (padding b))
	   (when Bk-1-set
	     (display " Bk-1 set"))
	   (newline)
	   (display "------------------------") (newline) 
	   (display (padding S))
	   (when Ck-set ; test if carry set
	     (display " C set"))
	   (newline)
	   (newline))
	 
	 (when Ck-set ; test if carry set
	   (incf Ck-true))
	 
	 (when Bk-1-set
	   (incf Bk-1-true)
	   (when Ck-set
	     (incf Ck-true-knowing-Bk-1))))) ; end for
  
  ;; display results
  {pBk-1 <+ {Bk-1-true / omega-universe}}  ;; pBk-1 probability of Bk-1
  {pCkkBk-1 <+ {Ck-true-knowing-Bk-1 / Bk-1-true}} ;; pCkkBk-1 probability Ck knowing Bk-1
  {pCk <+ {Ck-true / omega-universe}}

  (display "omega-universe = ")
  (display omega-universe)
  (newline)
  
  (display "Bk-1-true = ")
  (display Bk-1-true)
  (newline)
  
  (display "Probability of Bk-1 = Bk-1-true / omega-universe = ")
  (display Bk-1-true) (display " / ") (display omega-universe) (display " = ")
  (display pBk-1) (display " = ") (display (exact->inexact pBk-1))
  (newline)
  
  (display "Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = ")
  (display Ck-true-knowing-Bk-1) (display " / ") (display Bk-1-true) (display " = ")
  (display pCkkBk-1)  (display " = ") (display (exact->inexact pCkkBk-1))
  (newline)
  
  (display "Probability of Ck = Ck-true / omega-universe = ")
  (display Ck-true) (display " / ") (display omega-universe) (display " = ")
  (display pCk)  (display " = ") (display (exact->inexact pCk))
  (newline)

  )


;; compute probability of Sk,Ck, Ck knowing Bk-1 ,etc for Collatz function
;;
;; definition of Sk: a bit computed with two operands and a carry, see calculus below:
;;
;;  ⅓½½       ...                   ½    probabilities  (approximate)
;; Ckmax
;;   Ckm
;;   Cn
;;  C        C                       C
;;   n+1      k                       0
;;   1.......b......................10    2b + C0 = a with a = b
;;            k-1                                           k   k-1
;;    1......bbk-1...................1    b is odd
;;    bn-1    k 
;;  ----------------------------------
;;  S.Sn-1...SSk-1.....S.............0    S
;;   n+1      k         i            0    index
;;  ⅓⅓½               ....         ½     probabilities (approximate)
;;
;; (collatz-proba-stat-nmask #b100000000000000 1)
;; ...
;; omega-universe = 4096
;; Bk-1-true = 2048
;; Probability of Bk-1 = Bk-1-true / omega-universe = 2048 / 4096 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 1366 / 2048 = 683/1024 = 0.6669921875
;; Probability of Ck = Ck-true / omega-universe = 2049 / 4096 = 2049/4096 = 0.500244140625
;; alea = 16384
;; omega-universe = 8192
;; Bk-1-true = 4096
;; Probability of Bk-1 = Bk-1-true / omega-universe = 4096 / 8192 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 2731 / 4096 = 2731/4096 = 0.666748046875
;; Probability of Ck = Ck-true / omega-universe = 4097 / 8192 = 4097/8192 = 0.5001220703125
;; >
;; (collatz-proba-stat-nmask #b1000000000000 0)
;; T-pCk =#(0 0.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5)
;; exact 1/2
;;
;; (collatz-proba-stat-nmask #b1000000000000 1)
;; T-pCk =(0.0 0.33349609375 0.50048828125 0.5009765625 0.501953125 0.50390625 0.5078125 0.515625 0.53125 0.5625 0.625 0.75 1.0 1.0 1)
;; T-pSk =(not-computed 0.33349609375 0.33349609375 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.0)
;; T-pBk =(not-computed 0.0 0.0 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 1.0)
;; T-pCkkBk-1 =(not-computed not-computed 0.6669921875 0.66796875 0.66796875 0.671875 0.671875 0.6875 0.6875 0.75 0.75 1.0 1.0 1.0 not-computed)
;; todo : calculer proba 0 sachant que on a un 1 à gauche sur le MSB
(define (collatz-proba-stat-nmask Ckm C0) ;; Ckm is a position, C0 a value

  {display-enabled <+ #f}

  {Ck <+ #b10} ;; MSB of Ck is the position of the carry
  {Ckmax <+ (shift-left Ckm 2)} ;; mandatory for computing Sn+1
  {sn <+ (size-bit Ckmax)}
  {T-pCk <+ (make-vector sn 'not-computed)}
  {T-pCk[0] <- C0}
  {k <+ 1}

  {T-pSk-1 <+ (make-vector sn 'not-computed)}
  {T-pBk-1 <+ (make-vector sn 'not-computed)}
  {T-pCkkBk-1 <+ (make-vector sn 'not-computed)}
  {T-pCkkBk-and-Bk-1 <+ (make-vector sn 'not-computed)}
  
  (while {Ck <= Ckmax}
	 
	 {mask <+ {Ck - 1}} ;; example: Ck = #b100000 , Ck - 1 = #b11111
	 {Bk-1 <+ (shift-right Ck)} ;; position
	 {Sk-1 <+ Bk-1} ;; it is the same bit position
	 {Bk <+ Ck} ;; same bit position as Ck
	 
	 {alea <+ (min Ck Ckm)} ;; (shift-left Ck) allow computation of pCkkBk-and-Bk-1 ;; (shift-left Ck 8)} ;; 8 is arbitrary
	 
	 ;;
	 ;; 100000000000000 Ck , p(Ck) -> 1/2
	 ;;      carries
	 ;; aaaaaaaaaaaaaa1
	 ;;  aaaaaaaaaaaaaa alea
	 ;; 011111111111111 mask
	 ;;   S masked
	 ;;
	 ;;1000000000000000 Ck , p(Ck) -> 1/3
	 ;;     carries
	 ;; aaaaaaaaaaaaaa1
	 ;;  aaaaaaaaaaaaaa alea
	 ;;0111111111111111 mask
	 ;;   S masked
	 ;;
	 {omega-universe <+ 0}
	 {Bk-1-true <+ 0}
	 {Ck-true <+ 0}
	 {Ck-true-knowing-Bk-1 <+ 0}

	 {Sk-1-true <+ 0}
	 {Bk-true <+ 0}
	 {Bk-and-Bk-1-true <+ 0}
	 {Ck-true-knowing-Bk-and-Bk-1 <+ 0}

	 (dv alea)
	 (dv k)
	 
	 (display "Ck=") (display (padding Ck)) (newline)
	 	 
	 (for-basic (b alea)
	      
	      (when (flag-set? #b1 b) ; only for odd numbers
	 
		(incf omega-universe)

		{S <+ {(bitwise-ior C0 (shift-left b)) + b}} ; shift to left and set lowest significant bit and finally add b, i.e compute 2b+1+b = 3b+1

		{Sk-1-set <+ (flag-set? Bk-1 S)}

		(when Sk-1-set
		  (incf Sk-1-true))
		
		{Bk-1-set <+ (flag-set? Bk-1 b)} ;; Bk-1 flag

		{S-masked <+ {(bitwise-and (bitwise-ior C0 (shift-left b)) mask) + (bitwise-and b mask)}} ; shift to left and set lowest significant bit and mask the upper partial result and finally add b masked too, i.e compute 2b+1+b = 3b+1 but not some high bits that will be the carry
		{Ck-set <+ (flag-set? Ck S-masked)} ;; Ck flag

		{Bk-set <+ (flag-set? Bk b)} ;; Bk flag, warning always 0 because alea
	     
		(when display-enabled
		  (display (padding (bitwise-ior C0 (shift-left b)))) (newline)
		  (display (padding b))
		  (when Bk-1-set
		    (display " Bk-1 set"))
		  (newline)
		  (display "------------------------") (newline) 
		  (display (padding S))
		  (when Ck-set ; test if carry set
		    (display " C set"))
		  (newline)
		  (newline))
	 
		(when Ck-set ; test if carry set
		  (incf Ck-true))
	 
		(when Bk-1-set
		  (incf Bk-1-true)
		  (when Ck-set
		    (incf Ck-true-knowing-Bk-1)))

		(when Bk-set
		  (incf Bk-true)
		  (when Bk-1-set
		    (incf Bk-and-Bk-1-true)
		    (when Ck-set
		      (incf Ck-true-knowing-Bk-and-Bk-1)))))) ; end for
  
	 ;; display results
	 {pBk-1 <+ {Bk-1-true / omega-universe}}  ;; pBk-1 : probability of Bk-1
	 {T-pBk-1[{k - 1}] <- (exact->inexact pBk-1)}
	 
	 (declare pCkkBk-1) ;; pCkkBk-1 : probability Ck knowing Bk-1

	 (if {Bk-1-true <> 0}
	     ($
	      {pCkkBk-1 <- {Ck-true-knowing-Bk-1 / Bk-1-true}} ;; pCkkBk-1 : probability Ck knowing Bk-1
	      {T-pCkkBk-1[k] <- (exact->inexact pCkkBk-1)})
	     ($ ;; this only happens when Ck is greater than the max alea
	      (display "Bk-1-true = ") (display Bk-1-true) (newline)
	      (display "Ck-true-knowing-Bk-1 = ") (display Ck-true-knowing-Bk-1) (newline)
	      {pCkkBk-1 <- {T-pCkkBk-1[k]}})) ;; not computed ! (Bk-1 does not appearin Ck computation for this k)

	 (declare pCkkBk-and-Bk-1)
	 
	 (if {Bk-and-Bk-1-true <> 0}
	     ($
	      {pCkkBk-and-Bk-1 <- {Ck-true-knowing-Bk-and-Bk-1 / Bk-and-Bk-1-true}} 
	      {T-pCkkBk-and-Bk-1[k] <- (exact->inexact pCkkBk-and-Bk-1)})
	     {pCkkBk-and-Bk-1 <- {T-pCkkBk-and-Bk-1[k]}}) ;; not computed ! (probably Bk does not appear in Ck computation for this k)
	 
	 {pCk <+ {Ck-true / omega-universe}}
	 {T-pCk[k] <- (exact->inexact pCk)}

	 {pSk-1 <+ {Sk-1-true / omega-universe}}
	 {T-pSk-1[{k - 1}] <- (exact->inexact pSk-1)}

	 (display "omega-universe = ")
	 (display omega-universe)
	 (newline)
  
	 (display "Bk-1-true = ")
	 (display Bk-1-true)
	 (newline)

	 (display "Bk-true = ")
	 (display Bk-true)
	 (newline)
  
	 (display "Probability of Bk-1 = Bk-1-true / omega-universe = ")
	 (display Bk-1-true) (display " / ") (display omega-universe) (display " = ")
	 (display pBk-1) (display " = ") (display (exact->inexact pBk-1))
	 (newline)
	 

	 
	 (display "Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = ")
	 (display Ck-true-knowing-Bk-1) (display " / ") (display Bk-1-true) (display " = ")
	 (display pCkkBk-1) 
	 (when (not (equal? pCkkBk-1  'not-computed))
	    (display " = ") 
	    (display (exact->inexact pCkkBk-1)))	     
	 (newline)

	 
	 (display "Probability of Ck = Ck-true / omega-universe = ")
	 (display Ck-true) (display " / ") (display omega-universe) (display " = ")
	 (display pCk)  (display " = ") (display (exact->inexact pCk))
	 (newline)


	 (display "Probability of Sk-1 = Sk-1-true / omega-universe = ")
	 (display Sk-1-true) (display " / ") (display omega-universe) (display " = ")
	 (display pSk-1) (display " = ") (display (exact->inexact pSk-1))
	 (newline)
	 

	 (display "Probability of Ck knowing Bk and Bk-1 = Ck-true-knowing-Bk-and-Bk-1 / Bk-and-Bk-1-true = ")
	 (display Ck-true-knowing-Bk-and-Bk-1) (display " / ") (display Bk-and-Bk-1-true) (display " = ")
	 (display pCkkBk-and-Bk-1) 
	 (when (not (equal? pCkkBk-and-Bk-1  'not-computed))
	    (display " = ") 
	    (display (exact->inexact pCkkBk-and-Bk-1)))	     
	 (newline)
	 (newline)
	 
	 ;; shift to the next bit
	 {Ck <- (shift-left Ck)}
	 ;;{Sk-1 <- (shift-left Sk-1)}
	 
	 {k <- {k + 1}} ;; increment index for array storing
	 
	 ) ;; end while

  (display "T-pCk =")
  (display (reverse (vector->list T-pCk)))
  (newline)

  (display "T-pSk =")
  (display (reverse (vector->list T-pSk-1)))
  (newline)

  (display "T-pBk =")
  (display (reverse (vector->list T-pBk-1)))
  (newline)

  (display "T-pCkkBk-1 =")
  (display (reverse (vector->list T-pCkkBk-1)))
  (newline)

  (display "T-pCkkBk-and-Bk-1 =")
  (display (reverse (vector->list T-pCkkBk-and-Bk-1)))
  (newline)
  
  )


;; TODO : tester pour 1 sur S1 (apres 3x+1,x/2,....) quels sont les valeurs des bk...
