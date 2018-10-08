;; decrement symbolic polynomial

;; Damien Mattei

;; updated 7/10/2018


;; **********************************************************************************************************************

;; Mathematical abstract:

;; h : P -> P - 1

;; polynomial P(b) = Cn.b^n + Cn-1.b^(n-1) .... + Ck.b^k



;; **********************************************************************************************************************

;; Computer Science (& Mathematical) abstract:

;; h = rec-atomic-symbolic-polynomial-1
;; h : P -> P - 1
;;
;; polynomial P = (+ e_n e_n-1 e_n-2 ... e_k) = Cn.b^n + Cn-1.b^(n-1) .... + Ck.b^k
;; at left is LisP/Scheme prefix expression, at right is mathematical expression
;; note: e_n are monomials , e_n is a monomial of degree n, in this list monomials are sorted from highest to lowest degree
;;
;;
;; Polynomial case:
;;
;; -GET THE MONOMIAL OF LOWER DEGREE: reverse polynomial list to get e_k the first monomial of lowest degree 
;;  P_rev = (e_k .... e_n-2 e_n-1 e_n +) = Ck.b^k + ... + Cn-1.b^(n-1) + Cn.b^n
;;
;; note that the list expression P_rev is now a postfix notation with operator at end, this does not change the mathematical meaning. 
;;
;; get lower degree monomial : m = e_k = Ck.b^k
;;
;; -KEEP THE REST :
;;   P_rev_rest = (e_k+1 .... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
;;
;; -COMPUTE m' = MONOMIAL - 1 = m - 1 :
;;  m' = f(m)
;;  compute m' = Ck.b^k - 1 in hereditary base by calling recursive function f = hereditary-base-monomial-1
;;
;; NOW MONOMIAL-1 IS COMPUTED WE SET IT BACK IN POLYNOMIAL:
;;
;; number:
;;  m' = 0 :
;;      P_rev_rest = ( e_n + ) = Cn.b^n + 0 = Cn.b^n => P = e_n = Cn.b^n
;;      P_rev_rest = ( e_k+1.... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
;;            => h(P) = h_P = (+ e_n e_n-1 e_n-2 ... e_k+1) = Cn.b^n + Cn-1.b^(n-1) .... + Ck+1.b^(k+1) 
;;
;;  m' # 0 (not zero) :
;;
;;    reconstruct the polynomial:
;;      P_rev_rest = (e_k+1 .... e_n-2 e_n-1 e_n +)
;;            => h(P) = h_P = (+ e_n e_n-1  ....  e_k+1 m') = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + m'
;;
;;
;; not number:
;;
;; monomial-1 = m' = (+ e'_n' e'_n'-1 .... e'_k') = Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
;;          (higher to lower monomial degrees)
;;
;; P_rev_rest = ( e_k+1.... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
;;    => h(P) = h_P = (+ e_n e_n-1  ....  e_k+1 e'_n' e'_n'-1 .... e'_k')
;;            = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
;;
;;
;; case Polynomial is a simple monomial:
;;
;; m = P
;; m' = f(m)
;; h_P = m'
;;
;; return h(P) = h_P

;; > (prefix->infix (expt->^ (atomic-symbolic-polynomial-1 '(* 5 (expt 6 4)))))
;; '((4 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;;
;; > (prefix->infix (expt->^ (atomic-symbolic-polynomial-1 '(+ (* 5 (expt 6 4)) 1))))
;; '(5 * (6 ^ 4))
;; > 
(define (rec-atomic-symbolic-polynomial-1 P) ;; P : hereditary base polynomial, h = rec-atomic-symbolic-polynomial-1 function

  ;; h = rec-atomic-symbolic-polynomial-1
  ;; h : P -> P - 1
  

  (let ((monomial '())
	(monomial-1 '())
	(P_rev '())  ;; P reverse
	(P_rev_rest '()) ;; P reverse rest
	(h_P '())) ;; result : h(P)
	
       ;;
     ;; polynomial : (+ e_n e_n-1 e_n-2 ... e_k) = Cn.b^n + Cn-1.b^(n-1) + .... + Ck.b^k
     ;;
     
     (if (and    ;; not number and addition of monomials
	  (not (number? P)) 
	  (is+? P))
	 
	 (then-block
	  ;;
	  ;; GET THE MONOMIAL OF LOWER DEGREE
	  ;;
	  (set! P_rev (reverse P)) ;; reverse polynomial list to get e_k the first monomial of lower degree 
	  
	  ;; P_rev = (e_k .... e_n-2 e_n-1 e_n +) = Ck.b^k + ... + Cn-1.b^(n-1) + Cn.b^n
	  
	  
	  ;;(dv P_rev)
	  (set! monomial (first P_rev)) ;; get lower degree monomial : m = e_k = Ck.b^k 
	  ;;(dv monomial)
	  ;;(display (prefix->infix (expt->^ monomial)))
	  ;;(display " - 1 = ")
	  
	  (set! P_rev_rest (rest P_rev)) ;; keep the rest :
	  ;; P_rev_rest = (e_k+1 .... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
	  ;;
	  ;;(dv P_rev_rest)
	  ) ;; end then-block
     
     ;; else
	 (set! monomial P)) ;; end if then else

     ;; COMPUTE m' = MONOMIAL - 1
     ;; m' = f(m)
     (set! monomial-1 ;;  compute Ck.b^k - 1 in hereditary base
	   (rec-atomic-hereditary-base-monomial-1 monomial)) ;; substract one
	     
     ;;(dv monomial-1)
     ;;(display (prefix->infix (expt->^ monomial-1)))
     ;;(newline)

     (if (and    ;; not number and addition of monomials
	  (not (number? P)) 
	  (is+? P))
	 
	 
	 ;;
	 ;; NOW MONOMIAL-1 IS COMPUTED WE SET IT BACK IN POLYNOMIAL
	 ;;
	 (cond (
		;;
		;; number
		;;
		(number? monomial-1) ;; number = m'
		
		(if (zero? monomial-1) 
		    
		    ;; zero
		    (if (pair-list? P_rev_rest) 
			;; then
			;; P_rev_rest = ( e_n + ) = Cn.b^n + 0 = Cn.b^n
			(set! h_P (first P_rev_rest)) ;; remove the no more usefull operator : h(P) = h_P = e_n = Cn.b^n
			;; else
			;; P_rev_rest = ( e_k+1.... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
			(set! h_P (reverse P_rev_rest))) ;; h(P) = h_P = (+ e_n e_n-1 e_n-2 ... e_k+1) = Cn.b^n + Cn-1.b^(n-1) .... + Ck+1.b^(k+1) 
		 
		    ;; not zero
		    (set! h_P ;; reconstruct the polynomial
			  (reverse
			   (insert monomial-1 P_rev_rest))))) ;; (m' e_k+1 .... e_n-2 e_n-1 e_n +)
	       ;; h(P) = h_P = (+ e_n e_n-1  ....  e_k+1 m') = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + m'
	       
	       #|((is*? monomial-1) ;; multiplication : monomial-1 = m' = c.b^s, ce cas semble ne jamais avoir lieu car m'(b)=m(b)-1 n'est jamais de cette forme  
	       (set! h_P
	       (reverse (insert monomial-1 P_rev_rest))))|# ;; reconstruct the polynomial

	    ;; not number:
	       (else ;; monomial-1 = m' = (+ e'_n' e'_n'-1 .... e'_k') = Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
		;; (higher to lower monomial degrees)
		
		(set! h_P
		      
		      ;; P_rev_rest = ( e_k+1.... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
		      ;;    => h(P) = h_P = (+ e_n e_n-1  ....  e_k+1 e'_n' e'_n'-1 .... e'_k')
		      ;;            = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
		      
		      (reverse ;; higher to lower monomial degrees
		       (append ;; construct the new polynomial with monomial and rest of polynomial
			(reverse (args monomial-1)) ;; lower to higher monomial degrees
			P_rev_rest))))) ;; end if

     
	 ;;
	 ;; polynomial = e = monomial  , polynomial is a monomial
	 ;;
	 ;; m = P
	 ;; m' = f(m)
	 ;; h(P) = h_P = m'
	 (set! h_P monomial-1))

    h_P)) ;; return h(P) = h_P












;; in recursive form:

;; take a monomial c.b^n and substract 1 

;; f = rec-atomic-hereditary-base-monomial-1

;; f: M -> M - 1

;; M is a monomial, M = (* c (^ b n)) = c.b^n or M = (^ b n) = b^n or M = b

;; important property of f : f( M + Q ) = M + Q - 1 = M + f(Q)     (*)

;; M is a number:
;;   M = b => f(M) = b-1

;; M is a product:
;;   M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f(M) = f( (c-1).b^n + b^n ) = (c-1).b^n + f(b^n) by (*)
;;       note: n could be equal to 1, so M could be equal to c.b

;; M is a power:
;;   M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) => f(M) = f( (b-1).b^(n-1) + b^(n-1) )
;;                                                         = (b-1).b^(n-1) + f(b^(n-1))  by  (*)
;;                                                         = (b-1).b^h(n)  + f(b^h(n))
;;     with n-1 computed with h as n could be a polynomial , h = atomic-symbolic-polynomial-1




;; (prefix->infix (expt->^ (simplify (rec-atomic-hereditary-base-monomial-1 '(expt 4 5)))))
;;   ->  '((3 * (4 ^ 4)) + ((3 * (4 ^ 3)) + ((3 * (4 ^ 2)) + ((3 * 4) + 3))))
;;
;;(prefix->infix (n-arity (expt->^ (simplify (rec-atomic-hereditary-base-monomial-1 '(expt 4 7))))))
;; -> '((3 * (4 ^ 6)) + (3 * (4 ^ 5)) + (3 * (4 ^ 4)) + (3 * (4 ^ 3)) + (3 * (4 ^ 2)) + (3 * 4) + 3)
;;
;; >  (prefix->infix (expt->^ (simplify  (rec-atomic-hereditary-base-monomial-1 '(* 4 (expt 6 7))))))
;; '((3 * (6 ^ 7)) + (5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; >  (simplify  (rec-atomic-hereditary-base-monomial-1 '(* 4 (expt 6 7))))
;; '(+ (* 3 (expt 6 7)) (* 5 (expt 6 6)) (* 5 (expt 6 5)) (* 5 (expt 6 4)) (* 5 (expt 6 3)) (* 5 (expt 6 2)) (* 5 6) 5)
;; > (* 4 (expt 6 7))
;; 1119744
;; > (+ (* 3 (expt 6 7)) (* 5 (expt 6 6)) (* 5 (expt 6 5)) (* 5 (expt 6 4)) (* 5 (expt 6 3)) (* 5 (expt 6 2)) (* 5 6) 5)
;; 1119743
;; >
;;
;; > (simplify  (rec-atomic-hereditary-base-monomial-1 '(expt 5 (+ 5 1))))
;; '(+ (* 4 (expt 5 5)) (* 4 (expt 5 4)) (* 4 (expt 5 3)) (* 4 (expt 5 2)) (* 4 5) 4)
;; > (prefix->infix (expt->^ (simplify  (rec-atomic-hereditary-base-monomial-1 '(expt 5 (+ 5 1))))))
;; '((4 * (5 ^ 5)) + (4 * (5 ^ 4)) + (4 * (5 ^ 3)) + (4 * (5 ^ 2)) + (4 * 5) + 4)
;; > (expt 5 (+ 5 1))
;; 15625
;; > (+ (* 4 (expt 5 5)) (* 4 (expt 5 4)) (* 4 (expt 5 3)) (* 4 (expt 5 2)) (* 4 5) 4)
;; 15624
;; > 
;; 
(define (rec-atomic-hereditary-base-monomial-1 M) ;; f = rec-atomic-hereditary-base-monomial-1

  ;; f: M -> M - 1

  ;; M is a monomial, M = (* c (^ b n)) = c.b^n or M = (^ b n) = b^n or M = b

  ;; important property of f : f( M + Q ) = M + Q - 1 = M + f(Q)     (*)
  
  ;; (dv M)



  ;;(let ((f_M '()))
    
  ;;     M is a number:
  (cond ((number? M) ;; M = b => f(M) = b-1
	 
	 ;; call function monomial-1-number
	 (monomial-1-number M))
	;; (set! f_M (- M 1))
	
	;; M is a product:
	;; call function monomial-1-product    
	((is*? M) ;; M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f(M) = f( (c-1).b^n + b^n ) = (c-1).b^n + f(b^n)  by (*)
	 ;; note: n could be equal to 1, so M could be equal to c.b
	 
	 (monomial-1-product M))
	
	
	;; M is a power:
	;; call function rec-monomial-1-power
	(else ;; M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) => f(M) = f( (b-1).b^(n-1) + b^(n-1) )
	 ;;                                                            = (b-1).b^(n-1) + f(b^(n-1))  by (*)
	 ;;                                                            = (b-1).b^h(n)  + f(b^h(n))
	 ;;  with n-1 computed with h(n) as n could be a polynomial,h=atomic-symbolic-polynomial-1
	 
	 (rec-monomial-1-power M))))




(define (monomial-1-number M)
  (- M 1))



;; (prefix->infix (expt->^ (rec-monomial-1-power '(expt 6 4))))
;; '((5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)

;; 
(define (rec-monomial-1-power M) ;; f" = rec-monomial-1-power
  
  ;; property of x^n:

  ;; x^n = x.x^(n-1) = (x-1+1).x^(n-1) = (x-1).x^(n-1) + x^(n-1)
  ;;                                   = (x-1).x^(n-1) + (x-1).x^(n-2) + x^(n-2)
  ;;                                   = (x-1).x^(n-1) + (x-1).x^(n-2) + (x-1).x^(n-3) + x^(n-3)
  ;; x^n = (x-1).x^(n-1) + (x-1).x^(n-2) + (x-1).x^(n-3) + ... + (x-1).x^3 + (x-1).x^2 + x^2
  ;; x^n = (x-1).x^(n-1) + (x-1).x^(n-2) + (x-1).x^(n-3) + ... + (x-1).x^3 + (x-1).x^2 + (x-1).x + x

  ;; general case:
  ;; M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) => f"(M) = f"[(b-1).b^(n-1) + b^(n-1)]
  ;;                                                        = (b-1).b^(n-1) + f"(b^(n-1)) 
  ;;                                                        = (b-1).b^h(n)  + f"(b^h(n))

  ;;  with n-1 computed with h(n) as n could be a polynomial,h=atomic-symbolic-polynomial-1

  ;; we recursively go to h(n)=1 which occurs when computing for n=2 : f"(M) = f"(b²) = (b-1).b^h(2) + f"(b^h(2)) , at this point we have:
  ;; f"(M) = f"(b²) = (b-1).b^h(n)  + f"(b^h(n)) = (b-1).b^1  + f"(b^1) = (b-1).b  + f"(b) = (b-1).b + (b-1)

  ;; constant case:
  ;; M = b => f"(M) = f"(b) = b - 1

  (if (number? M)
      
      (- M 1)
      
      (let* ((b (arg1 M))
	     (n (arg2 M))
	     (b-1 (- b 1))
	     (n-1 (rec-atomic-symbolic-polynomial-1 n))) ;; n-1 = h(n)
	
	(simplify
	 (n-arity ;; put in n-arity the expression
	  (if (unity-symb? n-1) ;; if n-1 = 1

	      (then-block
	       (when debug-mode
		     (display-nl "rec-monomial-1-power : unity symbol !"))
	       `(+ ;; (b-1).b^h(n) + f"(b^(n-1)) with n-1 = 1 and so (b-1).b^h(n) + f"(b^(n-1)) = (b-1).b^h(n) + f"(b)
		 (* ,b-1 (expt ,b ,n-1))   ;; (b-1).b^(n-1)
		 ;;,(rec-monomial-1-power b)) ;; f"(b)
		 ;;,(- b 1)) ;; f"(b)
		 ,b-1) ;; f"(b)
	       )
	      
	      `(+  ;; (b-1).b^h(n) + f"(b^h(n))
		(* ,b-1 (expt ,b ,n-1))    ;; (b-1).b^(n-1)
		,(rec-monomial-1-power `(expt ,b ,n-1)))))))))  ;; f"(b^(n-1))



;; (prefix->infix (expt->^ (monomial-1-product '(* 5 (expt 6 4)))))
;; '((4 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;;
;; (prefix->infix (expt->^ (monomial-1-product '(* 5 6)))) -> '((4 * 6) + 5)
;;
;; note: after product we call power (in general)
(define (monomial-1-product M) ;; f' = monomial-1-product

  ;; f" = rec-monomial-1-power
  
  ;; M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f'(M) = f'( (c-1).b^n + b^n ) = (c-1).b^n + f"(b^n)

  ;; note: n could be equal to 1, so M could be equal to c.b
	  
  (let* ((c (arg1 M))
	 (c-1 (- c 1))
	 (b^n (arg2 M)))

    (simplify
     (n-arity ;; put in n-arity the expression
      (list (quote +)
	    (list (quote *) c-1 b^n)
	    (rec-monomial-1-power b^n)))))) ;; (+ (* c-1 b^n) (f" b^n)) = (c-1).b^n + f"(b^n)








