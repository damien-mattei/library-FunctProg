#lang reader "racket/SRFI-105.rkt"


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
(include "for.scm")
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
;;    1.......b......................1    b
;;             k-1
;;  ----------------------------------
;;  ..S......S.........S.............0    S
;;     n      k         i            0    index
;;
;;
;;
;; > (collatz-proba-stat #b1000000000 1)
;; alea = 131072
;; omega-universe = 65536
;; Bk-1-true = 32768
;; Probability of Bk-1 = Bk-1-true / omega-universe = 32768 / 65536 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 22016 / 32768 = 43/64 = 0.671875
;; Probability of Ck = Ck-true / omega-universe = 33024 / 65536 = 129/256 = 0.50390625
;;
;; > (collatz-proba-stat #b1000000000 0)
;; alea = 131072
;; omega-universe = 65536
;; Bk-1-true = 32768
;; Probability of Bk-1 = Bk-1-true / omega-universe = 32768 / 65536 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 21760 / 32768 = 85/128 = 0.6640625
;; Probability of Ck = Ck-true / omega-universe = 32768 / 65536 = 1/2 = 0.5
;; > ; seems to converge to 2/3
(define (collatz-proba-stat Ck C0)

  {Bk-1 <+ (shift-right Ck)}
  {alea <+ (shift-left Ck 8)}
  {omega-universe <+ 0}
  {Bk-1-true <+ 0}
  {Ck-true <+ 0}
  {Ck-true-knowing-Bk-1 <+ 0}
  {display-enabled <+ #f}
  {mask <+ {Ck - 1}}

  (display "alea = ")
  (display alea)
  (newline)
  
  (for ((b alea))
       ;;(incf omega-universe)
       
       (when (flag-set? #b1 b) ; only for odd numbers

	     (incf omega-universe)
	     {S-masked <+ {(bitwise-and (bitwise-ior C0 (shift-left b)) mask) + (bitwise-and b mask)}} ; shift to left and set lowest significant bit and mask the upper partial result and finally add b masked too, i.e compute 2b+incfb = 3b+1 but not some high bits that will be the carry
	     {Ck-set <+ (flag-set? Ck S-masked)} ;; Ck flag
	     {S <+ {(bitwise-ior C0 (shift-left b)) + b}} ; shift to left and set lowest significant bit and finally add b, i.e compute 2b+incfb = 3b+1
	     {Bk-1-set <+ (flag-set? Bk-1 b)} ;; Bk-1 flag
	       	       
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

