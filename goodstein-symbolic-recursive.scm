;; below is an atomic (package) set of functions (recursive version)


;; goodstein-init-atomic-rec : start-up function
;;
;;    - set a few variables
;;    - define Goodstein function recursively
;;    - compute the start number in hereditary base
;;    - call the Goodstein recursive function with the start number as argument
;;        
;;        the Goodstein recursive function do:
;;            - check if we have reached zero 
;;            - display polynomial at each step
;;            - bump the base
;;            - decrement polynomial by calling symbolic-polynomial-1 function (also called h)
;;            - call (recursively) goodstein-rec

;; >  (goodstein-init-atomic-rec 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2) = xn = x2
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω) = αn = α2 = fn[xn]
;;                                                               = f2[((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)]
;;                                                               = f3[x3 + 1]
;;                                                               = f3[((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2) + 1]
;;                                                               = f3[((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 3]
;;                                                               = ((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)
;;                                                               =
;;
;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2) = x(n+1) = x3
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2) = α(n+1) = α3 = f(n+1)[x(n+1)]
;;                                                                   = f3[((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)]
;;
;; f(n+1)[x(n+1)+1] = f3[((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2) + 1] = f3[((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 3]
;;                                                                      = ((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)
;;                                                                      = fn[xn]
;;
;; G(266)(3)=((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)
;; P(266)(3)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 1)
;; G(266)(4)=((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))
;; P(266)(4)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)))

;; G(266)(5)=((6 ^ (6 ^ (6 + 1))) + (5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; P(266)(5)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 5)

;; G(266)(6)=((7 ^ (7 ^ (7 + 1))) + (5 * (7 ^ 7)) + (5 * (7 ^ 5)) + (5 * (7 ^ 4)) + (5 * (7 ^ 3)) + (5 * (7 ^ 2)) + (5 * 7) + 4)
;; P(266)(6)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 4)
;; G(266)(7)=((8 ^ (8 ^ (8 + 1))) + (5 * (8 ^ 8)) + (5 * (8 ^ 5)) + (5 * (8 ^ 4)) + (5 * (8 ^ 3)) + (5 * (8 ^ 2)) + (5 * 8) + 3)
;; P(266)(7)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 3)
;; G(266)(8)=((9 ^ (9 ^ (9 + 1))) + (5 * (9 ^ 9)) + (5 * (9 ^ 5)) + (5 * (9 ^ 4)) + (5 * (9 ^ 3)) + (5 * (9 ^ 2)) + (5 * 9) + 2)
;; P(266)(8)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 2)
;; G(266)(9)=((10 ^ (10 ^ (10 + 1))) + (5 * (10 ^ 10)) + (5 * (10 ^ 5)) + (5 * (10 ^ 4)) + (5 * (10 ^ 3)) + (5 * (10 ^ 2)) + (5 * 10) + 1)
;; P(266)(9)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 1)
;; G(266)(10)=((11 ^ (11 ^ (11 + 1))) + (5 * (11 ^ 11)) + (5 * (11 ^ 5)) + (5 * (11 ^ 4)) + (5 * (11 ^ 3)) + (5 * (11 ^ 2)) + (5 * 11))
;; P(266)(10)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω))
;; G(266)(11)=((12 ^ (12 ^ (12 + 1))) + (5 * (12 ^ 12)) + (5 * (12 ^ 5)) + (5 * (12 ^ 4)) + (5 * (12 ^ 3)) + (5 * (12 ^ 2)) + (4 * 12) + 11)
;; P(266)(11)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 11)
;; G(266)(12)=((13 ^ (13 ^ (13 + 1))) + (5 * (13 ^ 13)) + (5 * (13 ^ 5)) + (5 * (13 ^ 4)) + (5 * (13 ^ 3)) + (5 * (13 ^ 2)) + (4 * 13) + 10)
;; P(266)(12)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 10)
;; G(266)(13)=((14 ^ (14 ^ (14 + 1))) + (5 * (14 ^ 14)) + (5 * (14 ^ 5)) + (5 * (14 ^ 4)) + (5 * (14 ^ 3)) + (5 * (14 ^ 2)) + (4 * 14) + 9)
;; P(266)(13)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 9)
;; G(266)(14)=((15 ^ (15 ^ (15 + 1))) + (5 * (15 ^ 15)) + (5 * (15 ^ 5)) + (5 * (15 ^ 4)) + (5 * (15 ^ 3)) + (5 * (15 ^ 2)) + (4 * 15) + 8)
;; P(266)(14)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 8)
;; G(266)(15)=((16 ^ (16 ^ (16 + 1))) + (5 * (16 ^ 16)) + (5 * (16 ^ 5)) + (5 * (16 ^ 4)) + (5 * (16 ^ 3)) + (5 * (16 ^ 2)) + (4 * 16) + 7)
;; P(266)(15)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 7)
;; G(266)(16)=((17 ^ (17 ^ (17 + 1))) + (5 * (17 ^ 17)) + (5 * (17 ^ 5)) + (5 * (17 ^ 4)) + (5 * (17 ^ 3)) + (5 * (17 ^ 2)) + (4 * 17) + 6)
;; P(266)(16)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 6)
;; G(266)(17)=((18 ^ (18 ^ (18 + 1))) + (5 * (18 ^ 18)) + (5 * (18 ^ 5)) + (5 * (18 ^ 4)) + (5 * (18 ^ 3)) + (5 * (18 ^ 2)) + (4 * 18) + 5)
;; P(266)(17)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (4 * ω) + 5)
(define (goodstein-init-atomic-rec n)

  ;; constants and variables
  (let ((n-start n) ;; n at start
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	(b 2))
    
    (letrec ((goodstein-rec

	      ;; the Goodstein recursive function do:
	      ;;   - check if we have reached zero 
	      ;;   - display polynomial at each step
	      ;;   - bump the base (+1)
	      ;;   - decrement polynomial by calling symbolic-polynomial-1 function
	      ;;   - call (recursively) goodstein-rec

	      ;; the Goodstein recursive function do:
	      ;; .> - check if we have reached zero 
	      ;; |  - display polynomial at each step
	      ;; |  - bump the base (+1)
	      ;; |  - decrement polynomial and rewrite it in hereditary base by calling
	      ;; |
	      ;; |    -> h: rec-atomic-symbolic-polynomial-1 function which FIND the _lowest_ _degree_ _monomial_ M and call f(M)
	      ;; |        (despite his name it's not a recursive function but she calls the recursive function f which
	      ;; |         makes recursive calls by f" which calls again h, so there is some recursion)
	      ;; |
	      ;; |       -> f:rec-atomic-hereditary-base-monomial-1 which depending monomial M=c.b^n call one of those functions:
	      ;; |            (again not a true recursive function but calls f' which is recursive)
	      ;; |          
	      ;; |          -> M number => monomial-1-number : f"' : M -> M-1
	      ;; |                                             (not recursive ,it's a "leaf" function)
	      ;; |          -> c#1 => monomial-1-product: f'
	      ;; |                     \        f'(c.b^n) = f'( (c-1).b^n + b^n ) = (c-1).b^n + f"(b^n) which always call the function f":
	      ;; |                      \       (f' not recursive and not "leaf" function, it's a "path" function : f->f'->f") 
	      ;; |                       \
	      ;; |          -> c=1 => -->-`-->  f" : rec-monomial-1-power, compute recursively
	      ;; |                               if M number then return M-1 
	      ;; |                               else : f"(b^n) = f"(b.b^h(n)) =f"([b-1+1].b^h(n)) = f"([b-1].b^h(n) + b^h(n))= (b-1).b^h(n) + f"(b^h(n))
	      ;; |
	      ;; |                                      note that: (h^k)(n)=n-k
	      ;; |                    f"(b^n) = (b-1).b^h(n) + (b-1).b^h(h(n)) + (b-1).b^h(h(h(n))) + (b-1).b^h⁴(n) + ... + (b-1).b  + (b-1)
	      ;; |                                 h(n)=n-1         h²(n)=n-2         h³(n)=n-3         h⁴(n)=n-4         h^(n-1)(n)=n-(n-1)=1 
	      ;; |                    f"(b^n) = (b-1).b^h(n) + (b-1).b^h(h(n)) + (b-1).b^h(h(h(n))) + (b-1).b^h⁴(n) + ... + (b-1).b^[h^(n-1)](n)  + (b-1)
	      ;; |                numerically:
	      ;; |                    f"(b^n) = (b-1).b^(n-1) + (b-1).b^(n-2) + (b-1).b^(n-3) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1
	      ;; |            
	      ;; |                    h is a "lift" function that go up in the exponential level, it guarantees that we will reach the highest levels of exponentiation, and by consequence that the function h always erodes the degrees of the upper polynomial,causing the fall of the goodstein function by the reduction of the exponentiation level. Note h is applied on all the monomials of lower degree and so their exponentiation will be recursively reduced.
	      ;; |             note:
	      ;; |                f'(c.b^n) = (c-1).b^n + (b-1).b^h(n) + (b-1).b^h(h(n)) + (b-1).b^h(h(h(n))) + (b-1).b^h⁴(n) + ... + (b-1).b^[h^(n-1)](n)  + (b-1)
	      ;; |       as we are in hereditary base b, n is in fact a polynomial of variable b so n=P(b), and here is the expression of h:
	      ;; |         h(n) = h(P(b)) = Pu(b) + (c-1).b^n + (b-1).b^h(n) + (b-1).b^h(h(n)) + (b-1).b^h(h(h(n))) + (b-1).b^h⁴(n) + ... + (b-1).b^[h^(n-1)](n)  + (b-1)
	      ;; |       keeping in mind that n is a function of b, we have:
	      ;; |       h(P(b)) = Pu(b) + (c-1).b^n(b) + (b-1).b^h(n(b)) + (b-1).b^h(h(n(b))) + (b-1).b^h(h(h(n(b)))) + (b-1).b^h⁴(n(b)) + ... + (b-1).b^[h^(n-1)](n(b))  + (b-1)
	      ;; |       h(Pu(b) + c.b^n(b)) = Pu(b) + (c-1).b^n(b) + (b-1).b^h(n(b)) + (b-1).b^h(h(n(b))) + (b-1).b^h(h(h(n(b)))) + (b-1).b^h⁴(n(b)) + ...
	      ;; |                               ... + (b-1).b^[h^(n-1)](n(b))  + (b-1)
	      ;; |       h(Pu(b) + c.b^n(b)) = Pu(b) + (c-1).b^n(b) + (b-1).b^(n-1) + (b-1).b^(b-2) + (b-1).b^(n-3) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1
	      ;; |       h(Pu(b) + c.b^n(b)) = Pu(b) + (c-1).b^n(b) + (b-1).b^h(n(b)) + (b-1).b^h(h(n(b))) + (b-1).b^h³(n(b)) + ....
	      ;; |                               ... + (b-1).b^b + (b-1).b^h(b) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1
	      ;; |       h(Pu(b) + c.b^n(b)) = Pu(b) + (c-1).b^n(b) + (b-1).b^h(n(b)) + (b-1).b^h(h(n(b))) + (b-1).b^h³(n(b)) + ....
	      ;; |                               ... + (b-1).b^b + (b-1).b^(b-1) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1
	      ;; |       knowing that (b-1).b^(b-1) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1 is going to 0 by goodstein function
	      ;; |       we have to proove that this is also going to 0 by goodstein function:
	      ;; |       h(Pu(b) + c.b^n(b)) = Pu(b) + (c-1).b^n(b) + (b-1).b^h(n(b)) + (b-1).b^h(h(n(b))) + (b-1).b^h³(n(b)) + ....
	      ;; |                               ... + (b-1).b^b
	      ;; |         (b-1).b^b = (b-1-1).b^b + b^b = (b-2).b^b + b^b
	      ;; |         with :b^b = (b-1).b^(b-1) + (b-1).b^(b-2) + (b-1).b^(b-3) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b
	      ;; |         (b-1).b^b = (b-2).b^b + (b-1).b^(b-1) + (b-1).b^(b-2) + (b-1).b^(b-3) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b

	      ;; |         (b-1).b^b = (b-1)².b^(b-1) + (b-1)².b^(b-2) + (b-1)².b^(b-3) + ... + (b-1)².b^3 + (b-1)².b^2 + (b-1)².b + (b-1).b
	      ;; |         (b-1).b^b = (b-1).b.b^(b-1) = (b-1).(b-1+1).b^(b-1) = (b-1).[(b-1).b^(b-1) + b^(b-1)]
	      
	      ;; |_ - recursively call itself
    

	      
	      (lambda (P) ;; polynomial hereditary base b expression

		(if (and (number? P)
			 (= P 0))

		    0 ;; finito
	
		    (let ((Pi '()) ;; infix expression
			  (Ps '()) ;; hereditary base b+1 expression, historically was P successor
			  (Psi '()) ;; infix expression of Ps
			  (Pi-omega '())) ;; hereditary infix omega expression of Ps
			  
		      
		      ;; convertir, ne pas recalculer
		      (set! Pi (prefix->infix P))
		      (set! Pi
			    (replace Pi 'expt '^)) ;; expt))
	  	   
		      (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
		      (display-nl Pi)
		      
		      (set! Pi-omega 
			    (replace Pi b omega))
		      (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
		      (display-nl Pi-omega)

		      (set! Ps ;; bump the base
			    (replace P b (+ 1 b)))
	   
		      (set! b (+ 1 b))

		      (goodstein-rec (rec-atomic-symbolic-polynomial-1 Ps)))))))

      (goodstein-rec (number->hereditary-base-k-expt n b)))))


;; summary of upper comments:

;; the Goodstein recursive function do:
;;   - check if we have reached zero 
;;   - display polynomial at each step
;;   - bump the base (+1)
;;   - decrement polynomial by calling symbolic-polynomial-1 function
;;   - call (recursively) goodstein-rec

;; the Goodstein recursive function do:
;; .> - check if we have reached zero 
;; |  - display polynomial at each step
;; |  - bump the base (+1)
;; |  - decrement polynomial by calling
;; |
;; |    -> h: rec-atomic-symbolic-polynomial-1 function which find the _lower_ _degree_ _monomial_ and call
;; |
;; |       -> rec-atomic-hereditary-base-monomial-1 which depending monomial call one of those functions:
;; |          
;; |          -> monomial-1-number (do M-1)
;; |          -> monomial-1-product: f'( (c-1).b^n + b^n ) = (c-1).b^n + f"(b^n) which always call this function: 
;; |                  \
;; |          ->--->---`-> f" : rec-monomial-1-power
;; |                         f"(b^n) =  (b-1).b^h(n)  + f"(b^h(n))
;; |        
;; |_ - recursively call itself










;; goodstein-init-atomic-rec-depth : start-up function
;;
;;    - set a few variables
;;    - define Goodstein function recursively
;;    - compute the start number in hereditary base
;;    - call the Goodstein recursive function with the start number as argument
;;        
;;        the Goodstein recursive function do:
;;            - check if we have reached zero 
;;            - display polynomial at each step
;;            - bump the base
;;            - decrement polynomial by calling symbolic-polynomial-1 function (also called h)
;;            - call (recursively) goodstein-rec

;; 
;;  (goodstein-init-atomic-rec-depth (expt 2 4))
;; depth of P,  d = 2
;; G(16)(1)=(2 ^ (2 ^ 2))
;; P(16)(1)=(ω ^ (ω ^ ω))
;; depth of P,  d = 2
;; G(16)(2)=((2 * (3 ^ ((2 * (3 ^ 2)) + (2 * 3) + 2))) + (2 * (3 ^ ((2 * (3 ^ 2)) + (2 * 3) + 1))) + (2 * (3 ^ ((2 * (3 ^ 2)) + (2 * 3)))) + (2 * (3 ^ ((2 * (3 ^ 2)) + 3 + 2))) + (2 * (3 ^ ((2 * (3 ^ 2)) + 3 + 1))) + (2 * (3 ^ ((2 * (3 ^ 2)) + 3))) + (2 * (3 ^ ((2 * (3 ^ 2)) + 2))) + (2 * (3 ^ ((2 * (3 ^ 2)) + 1))) + (2 * (3 ^ (2 * (3 ^ 2)))) + (2 * (3 ^ ((3 ^ 2) + (2 * 3) + 2))) + (2 * (3 ^ ((3 ^ 2) + (2 * 3) + 1))) + (2 * (3 ^ ((3 ^ 2) + (2 * 3)))) + (2 * (3 ^ ((3 ^ 2) + 3 + 2))) + (2 * (3 ^ ((3 ^ 2) + 3 + 1))) + (2 * (3 ^ ((3 ^ 2) + 3))) + (2 * (3 ^ ((3 ^ 2) + 2))) + (2 * (3 ^ ((3 ^ 2) + 1))) + (2 * (3 ^ (3 ^ 2))) + (2 * (3 ^ ((2 * 3) + 2))) + (2 * (3 ^ ((2 * 3) + 1))) + (2 * (3 ^ (2 * 3))) + (2 * (3 ^ (3 + 2))) + (2 * (3 ^ (3 + 1))) + (2 * (3 ^ 3)) + (2 * (3 ^ 2)) + (2 * 3) + 2)
;; P(16)(2)=((2 * (ω ^ ((2 * (ω ^ 2)) + (2 * ω) + 2))) + (2 * (ω ^ ((2 * (ω ^ 2)) + (2 * ω) + 1))) + (2 * (ω ^ ((2 * (ω ^ 2)) + (2 * ω)))) + (2 * (ω ^ ((2 * (ω ^ 2)) + ω + 2))) + (2 * (ω ^ ((2 * (ω ^ 2)) + ω + 1))) + (2 * (ω ^ ((2 * (ω ^ 2)) + ω))) + (2 * (ω ^ ((2 * (ω ^ 2)) + 2))) + (2 * (ω ^ ((2 * (ω ^ 2)) + 1))) + (2 * (ω ^ (2 * (ω ^ 2)))) + (2 * (ω ^ ((ω ^ 2) + (2 * ω) + 2))) + (2 * (ω ^ ((ω ^ 2) + (2 * ω) + 1))) + (2 * (ω ^ ((ω ^ 2) + (2 * ω)))) + (2 * (ω ^ ((ω ^ 2) + ω + 2))) + (2 * (ω ^ ((ω ^ 2) + ω + 1))) + (2 * (ω ^ ((ω ^ 2) + ω))) + (2 * (ω ^ ((ω ^ 2) + 2))) + (2 * (ω ^ ((ω ^ 2) + 1))) + (2 * (ω ^ (ω ^ 2))) + (2 * (ω ^ ((2 * ω) + 2))) + (2 * (ω ^ ((2 * ω) + 1))) + (2 * (ω ^ (2 * ω))) + (2 * (ω ^ (ω + 2))) + (2 * (ω ^ (ω + 1))) + (2 * (ω ^ ω)) + (2 * (ω ^ 2)) + (2 * ω) + 2)
;; depth of P,  d = 2
(define (goodstein-init-atomic-rec-depth n)

  ;; constants and variables
  (let ((n-start n) ;; n at start
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	(b 2))
    
    (letrec ((goodstein-rec

	      ;; the Goodstein recursive function do:
	      ;;   - check if we have reached zero 
	      ;;   - display polynomial at each step
	      ;;   - bump the base (+1)
	      ;;   - decrement polynomial by calling symbolic-polynomial-1 function
	      ;;   - call (recursively) goodstein-rec
	      
	      (lambda (P) ;; polynomial hereditary base b expression

		(if (and (number? P)
			 (= P 0))

		    0 ;; finito
	
		    (let ((Pi '()) ;; infix expression
			  (Ps '()) ;; hereditary base b+1 expression, historically was P successor
			  (Psi '()) ;; infix expression of Ps
			  (Pi-omega '()) ;; hereditary infix omega expression of Ps
			  (d 0) ;; depth of expression tree
			  )
		      
		      ;; display depth
		      (set! d (depth-select P 'expt))
		      (display-msg-symb-nl "depth of P, " d)
			      
		      ;; convertir, ne pas recalculer
		      (set! Pi (prefix->infix P))
		      (set! Pi
			    (replace Pi 'expt '^)) ;; expt))
	  	   
		      (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
		      (display-nl Pi)
		      
		      (set! Pi-omega 
			    (replace Pi b omega))
		      (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
		      (display-nl Pi-omega)

		      (set! Ps ;; bump the base
			    (replace P b (+ 1 b)))
	   
		      (set! b (+ 1 b))

		      (goodstein-rec (rec-atomic-symbolic-polynomial-1 Ps)))))))

      (goodstein-rec (number->hereditary-base-k-expt n b)))))
