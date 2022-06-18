;; below is an atomic (package) set of functions (iterative version)



;; goodstein-init-atomic-rec2 : start-up function
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
;;            - call (recursively) goodstein-rec2

;; >  (goodstein-init-atomic-rec2 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)
;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2)
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
;;
;; > (goodstein-init-atomic-rec2 3)
;; G(3)(1)=(2 + 1)
;; P(3)(1)=(ω + 1)
;; G(3)(2)=3
;; P(3)(2)=ω
;; G(3)(3)=3
;; P(3)(3)=3
;; G(3)(4)=2
;; P(3)(4)=2
;; G(3)(5)=1
;; P(3)(5)=1
;; 0
(define (goodstein-init-atomic-rec2 n)

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
	
		    (let ((Pi '()) ;; Polynomial infix expression
			  (Ps '()) ;; hereditary base b+1 expression, historically was Polynomial  successor
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

		      (goodstein-rec (iter-atomic-symbolic-polynomial-1 Ps)))))))

      (goodstein-rec (number->hereditary-base-k-expt n b)))))





;; goodstein-init-atomic-iter : start-up function
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
;;            - call (recursively) goodstein-iter

;; >  (goodstein-init-atomic-iter 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)
;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2)
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
;;
;; > (goodstein-init-atomic-iter 3)
;; G(3)(1)=(2 + 1)
;; P(3)(1)=(ω + 1)
;; G(3)(2)=3
;; P(3)(2)=ω
;; G(3)(3)=3
;; P(3)(3)=3
;; G(3)(4)=2
;; P(3)(4)=2
;; G(3)(5)=1
;; P(3)(5)=1
;; G(3)(6)=0
;; P(3)(6)=0
;;
(define (goodstein-init-atomic-iter n)

  ;; constants and variables
  (let* ((n-start n) ;; n at start
	 (omega (string->symbol (string #\u03C9))) ;; omega symbol
	 (b 2)
	 (P  (number->hereditary-base-k-expt n b)) ;; polynomial hereditary base b expression
	 )
    
    
    
    ;; the Goodstein iterative function do:
    ;; .> - check if we have reached zero 
    ;; |  - display polynomial at each step
    ;; |  - bump the base (+1)
    ;; |  - decrement polynomial by calling *-symbolic-polynomial-1 function:
    ;; |     -> find the lower degree monomial and call *-hereditary-base-monomial-1:
    ;; |        depending monomial call one of those functions:
    ;; |        -> monomial-1-number
    ;; |        -> *-monomial-1-product which always call this function:
    ;; |           -> *-monomial-1-power
    ;; |        -> *-monomial-1-power
    ;; |_ - iterate to first step
    
    

    (while (not (and (number? P)
		     (= P 0)))
	   
	   (let ((Pi '()) ;; Polynomial infix expression
		 (Ps '()) ;; hereditary base b+1 expression, historically was Polynomial  successor
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

	     (set! P (iter-atomic-symbolic-polynomial-1 Ps))))
    
    ;; finito
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl P)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")))




;; TODO: check bug: exposants ne devraient pas etre calculé (26,25....)

;; (number->hereditary-base-k-infix (expt 3 (expt 3 3)) 3) -> '(3 ^ (3 ^ 3))

;; > (prefix->infix (expt->^ (iter-atomic-hereditary-base-monomial-1 '(expt 3 (expt 3 3)))))
;; '((2 * (3 ^ 26))
;;   +
;;   (2 * (3 ^ 25))
;;   +
;;   (2 * (3 ^ 24))
;;   +
;;   (2 * (3 ^ 23))
;;   +
;;   (2 * (3 ^ 22))
;;   +
;;   (2 * (3 ^ 21))
;;   +
;;   (2 * (3 ^ 20))
;;   +
;;   (2 * (3 ^ 19))
;;   +
;;   (2 * (3 ^ 18))
;;   +
;;   (2 * (3 ^ 17))
;;   +
;;   (2 * (3 ^ 16))
;;   +
;;   (2 * (3 ^ 15))
;;   +
;;   (2 * (3 ^ 14))
;;   +
;;   (2 * (3 ^ 13))
;;   +
;;   (2 * (3 ^ 12))
;;   +
;;   (2 * (3 ^ 11))
;;   +
;;   (2 * (3 ^ 10))
;;   +
;;   (2 * (3 ^ 9))
;;   +
;;   (2 * (3 ^ 8))
;;   +
;;   (2 * (3 ^ 7))
;;   +
;;   (2 * (3 ^ 6))
;;   +
;;   (2 * (3 ^ 5))
;;   +
;;   (2 * (3 ^ 4))
;;   +
;;   (2 * (3 ^ 3))
;;   +
;;   (2 * (3 ^ 2))
;;   +
;;   (2 * 3)
;;   +
;;   2)
;; > (prefix->infix (expt->^ (rec-atomic-hereditary-base-monomial-1 '(expt 3 (expt 3 3)))))
;; '((2 * (3 ^ ((2 * (3 ^ 2)) + (2 * 3) + 2)))
;;   +
;;   (2 * (3 ^ ((2 * (3 ^ 2)) + (2 * 3) + 1)))
;;   +
;;   (2 * (3 ^ ((2 * (3 ^ 2)) + (2 * 3))))
;;   +
;;   (2 * (3 ^ ((2 * (3 ^ 2)) + 3 + 2)))
;;   +
;;   (2 * (3 ^ ((2 * (3 ^ 2)) + 3 + 1)))
;;   +
;;   (2 * (3 ^ ((2 * (3 ^ 2)) + 3)))
;;   +
;;   (2 * (3 ^ ((2 * (3 ^ 2)) + 2)))
;;   +
;;   (2 * (3 ^ ((2 * (3 ^ 2)) + 1)))
;;   +
;;   (2 * (3 ^ (2 * (3 ^ 2))))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + (2 * 3) + 2)))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + (2 * 3) + 1)))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + (2 * 3))))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + 3 + 2)))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + 3 + 1)))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + 3)))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + 2)))
;;   +
;;   (2 * (3 ^ ((3 ^ 2) + 1)))
;;   +
;;   (2 * (3 ^ (3 ^ 2)))
;;   +
;;   (2 * (3 ^ ((2 * 3) + 2)))
;;   +
;;   (2 * (3 ^ ((2 * 3) + 1)))
;;   +
;;   (2 * (3 ^ (2 * 3)))
;;   +
;;   (2 * (3 ^ (3 + 2)))
;;   +
;;   (2 * (3 ^ (3 + 1)))
;;   +
;;   (2 * (3 ^ 3))
;;   +
;;   (2 * (3 ^ 2))
;;   +
;;   (2 * 3)
;;   +
;;   2)
;; > 
;; G(16)(27)=((2 * (28 ^ ((2 * (28 ^ 2)) + (2 * 28) + 2))) + (2 * (28 ^ ((2 * (28 ^ 2)) + (2 * 28) + 1))) + (2 * (28 ^ ((2 * (28 ^ 2)) + (2 * 28)))) + (2 * (28 ^ ((2 * (28 ^ 2)) + 28 + 2))) + (2 * (28 ^ ((2 * (28 ^ 2)) + 28 + 1))) + (2 * (28 ^ ((2 * (28 ^ 2)) + 28))) + (2 * (28 ^ ((2 * (28 ^ 2)) + 2))) + (2 * (28 ^ ((2 * (28 ^ 2)) + 1))) + (2 * (28 ^ (2 * (28 ^ 2)))) + (2 * (28 ^ ((28 ^ 2) + (2 * 28) + 2))) + (2 * (28 ^ ((28 ^ 2) + (2 * 28) + 1))) + (2 * (28 ^ ((28 ^ 2) + (2 * 28)))) + (2 * (28 ^ ((28 ^ 2) + 28 + 2))) + (2 * (28 ^ ((28 ^ 2) + 28 + 1))) + (2 * (28 ^ ((28 ^ 2) + 28))) + (2 * (28 ^ ((28 ^ 2) + 2))) + (2 * (28 ^ ((28 ^ 2) + 1))) + (2 * (28 ^ (28 ^ 2))) + (2 * (28 ^ ((2 * 28) + 2))) + (2 * (28 ^ ((2 * 28) + 1))) + (2 * (28 ^ (2 * 28))) + (2 * (28 ^ (28 + 2))) + (2 * (28 ^ (28 + 1))) + (2 * (28 ^ 28)) + (28 ^ 2) + (23 * 28) + 19)

;; G(16)(27)=((2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (2 * (28 ^ 28)) + (28 ^ 2) + (23 * 28) + 19)

;; TODO : debug iter-monomial-1-power






;; decrement symbolic polynomial (iterative version)

;; **********************************************************************************************************************

;; Mathematical abstract:

;; h : P -> P - 1

;; polynomial P(b) = Cn.b^n + Cn-1.b^(n-1) .... + Ck.b^k



;; **********************************************************************************************************************

;; Computer Science (& Mathematical) abstract:

;; h = iter-atomic-symbolic-polynomial-1
;; h : P -> P - 1
;;
;; polynomial P = (+ e_n e_n-1 e_n-2 ... e_k) = Cn.b^n + Cn-1.b^(n-1) .... + Ck.b^k
;; at left is LisP/Scheme prefix expression, at right is mathematical expression
;; note: e_n are monomials , e_n is a monomial of degree n, in this list monomials are sorted from higher to lower degree
;;
;;
;; Polynomial case:
;;
;; -GET THE MONOMIAL OF LOWER DEGREE: reverse polynomial list to get e_k the first monomial of lower degree 
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
(define (iter-atomic-symbolic-polynomial-1 P) ;; P : hereditary base polynomial, h = symbolic-polynomial-1 function

  ;; h = symbolic-polynomial-1
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
	   (iter-atomic-hereditary-base-monomial-1 monomial)) ;; substract one
	     
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





;; iterative version

;; in recursive form (documentation only):

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

;; M is a power: (note this doc is recursive version but in this part of code i will use iteration)
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
(define (iter-atomic-hereditary-base-monomial-1 M) ;; f = iter-atomic-hereditary-base-monomial-1

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
	 
	 (iter-monomial-1-product M))
	
	
	;; M is a power:
	;; call function rec-monomial-1-power
	(else ;; M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) => f(M) = f( (b-1).b^(n-1) + b^(n-1) )
	 ;;                                                            = (b-1).b^(n-1) + f(b^(n-1))  by (*)
	 ;;                                                            = (b-1).b^h(n)  + f(b^h(n))
	 ;;  with n-1 computed with h(n) as n could be a polynomial,h=atomic-symbolic-polynomial-1
	 
	 (iter-monomial-1-power M))))





;; (prefix->infix (expt->^ (iter-monomial-1-power '(expt 6 4))))
;; '((5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;;
;; > (prefix->infix (expt->^ (iter-monomial-1-power '(expt 6 4))))
;; '((5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * (6 ^ 1)) + (5 * (6 ^ 0)))
;;
;; > (prefix->infix (expt->^ (rec-monomial-1-power '(expt 6 4))))
;; '((5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; > (prefix->infix (expt->^ (iter-monomial-1-power '(expt 6 4))))
;; '((5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; > (prefix->infix (expt->^ (iter-monomial-1-power '(expt 6 2))))
;; '((5 * 6) + 5)
;; > (prefix->infix (expt->^ (iter-monomial-1-power '(expt 6 1))))
;; 5
;; > (prefix->infix (expt->^ (iter-monomial-1-power '(expt 6 0))))
;; 0
;;
;; iterative form
(define (iter-monomial-1-power M) ;; f"' = iter-monomial-1-power
  
  ;; property of x^n:

  ;; x^n = x.x^(n-1) = (x-1+1).x^(n-1) = (x-1).x^(n-1) + x^(n-1)
  ;;                                   = (x-1).x^(n-1) + (x-1).x^(n-2) + x^(n-2)
  ;;                                   = (x-1).x^(n-1) + (x-1).x^(n-2) + (x-1).x^(n-3) + x^(n-3)
  ;; x^n = (x-1).x^(n-1) + (x-1).x^(n-2) + (x-1).x^(n-3) + ... + (x-1).x^3 + (x-1).x^2 + x^2
  ;; x^n = (x-1).x^(n-1) + (x-1).x^(n-2) + (x-1).x^(n-3) + ... + (x-1).x^3 + (x-1).x^2 + (x-1).x + x

  ;; M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) => f"'(M) = f"'( (b-1).b^(n-1) + b^(n-1) )
  ;;                                                         = f"'( (b-1).b^h(n)  + b^h(n))
  ;; f"'(M) = (b-1).b^(n-1) + (b-1).b^(n-2) + (b-1).b^(n-3) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1
  
  ;;  with n-1 computed with h(n) as n could be a polynomial,h=atomic-symbolic-polynomial-1

  ;; M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f'(M) = f'( (c-1).b^n + b^n ) = (c-1).b^n + f"(b^n)

  ;; with f" _= f"'  f" is recursive, f"' iterative

  ;; f'(M) = f'(c.b^n) = (c-1).b^n + f"(b^n) = (c-1).b^n + f"'(b^n)
  ;; f'(c.b^n) = (c-1).b^n + (b-1).b^(n-1) + (b-1).b^(n-2) + (b-1).b^(n-3) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1

  (if (number? M)
      
      (- M 1)
      
      (let* ((b (arg1 M))
	     (n (arg2 M))
	     (b-1 (- b 1))
	     (n-1 (atomic-symbolic-polynomial-1 n)) ;; n-1 = h(n)
	     (term-lst '()))

	(if (unity-symb? n-1) ;; if n-1 = 1
	      
	    (simplify
	     `(+ ;; (b-1).b^h(n) + f"(b)
	       (* ,b-1 (expt ,b ,n-1))   ;; (b-1).b^(n-1)
	       ;;,(rec-monomial-1-power b)) ;; f"(b)
	       ;;,(- b 1))) ;; f"(b)
	       ,b-1))
	    
	    (begin

	      ;; init list
	      ;; (set! term-lst
	      ;; 	    (list `(* ,b-1 ,b) b-1)) ;; '( (b-1).b  b-1 )

	      ;; iterate 
	      (for (k 0 (eval n-1)) ;; eval correct an awful bug: n-1 is symbolically computed!
		   ;; but we need the numeric value to compute the "for" loop
		   ;;(dv term-lst)
		   (set! term-lst
			 (cons `(* ,b-1 (expt ,b ,k)) ;; (b-1).b^k
			       term-lst))) ;; ( (b-1).b^(k-1) (b-1).b^(k-2) (b-1).b^(k-3) ... (b-1).b^3 (b-1).b^2 (b-1).b b-1 )

	      (simplify ;; formal simplification algorithm
	       ;; add the + symbol
	       (cons '+ term-lst))))))) ;; (b-1).b^k + (b-1).b^(k-1) + (b-1).b^(k-2) + (b-1).b^(k-3) + ... + (b-1).b^3 + (b-1).b^2 + (b-1).b + b-1
	      



;; (prefix->infix (expt->^ (monomial-1-product '(* 5 (expt 6 4)))))
;; '((4 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;;
;; (prefix->infix (expt->^ (monomial-1-product '(* 5 6)))) -> '((4 * 6) + 5)
;;
;; note: after product we call power (in general)
(define (iter-monomial-1-product M) ;; f' = monomial-1-product
  
  ;; M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f'(M) = f'( (c-1).b^n + b^n ) = (c-1).b^n + f"(b^n)

  ;; note: n could be equal to 1, so M could be equal to c.b
	  
  (let* ((c (arg1 M))
	 (c-1 (- c 1))
	 (b^n (arg2 M)))

    (simplify
     (n-arity ;; put in n-arity the expression
      (list (quote +)
	    (list (quote *) c-1 b^n)
	    (iter-monomial-1-power b^n)))))) ;; (+ (* c-1 b^n) (f" b^n)) = (c-1).b^n + f"(b^n)
