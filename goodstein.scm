;; goodstein.scm
;;
;; Copyright (C) 2016-2017  Damien MATTEI
;;
;; e-mail: damien.mattei@gmail.com 
;;         (damien.mattei@unice.fr , damien.mattei@oca.eu)
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

;; > (goodstein 3)
;; b = 2
;; hi = (2 + 1)
;; n = 3

;; b = 3
;; hi = 3
;; n = 3

;; b = 4
;; hi = 3
;; n = 2

;; b = 5
;; hi = 2
;; n = 1

;; b = 6
;; hi = 1
;; n = 0
;;
;;
;; > (goodstein 266)
;; b = 2
;; hi = ((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)

;; b = 3
;; hi = ((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)

;; b = 4
;; hi = ((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)

;; b = 5
;; hi = ((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))

;; b = 6
;; . . ../git/LOGIKI/lib/binary_arithmetic.scm:330:21: user break
;;
;; G(13)(280)=(+ (expt 281 (+ 281 1)) (* 3 (expt 281 3)) (* 2 (expt 281 2)) (* 61 281) 230)
;;
;; test with 257
;; > (goodstein 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)

;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2)

;; G(266)(3)=((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)
;; P(266)(3)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 1)

;; G(266)(4)=((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))
;; P(266)(4)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)))

(define (goodstein n)

  (let ((n-start n)
	(h '()) ;; hereditary base b
	(hi '()) ;; infix
	(hs '()) ;; hereditary base b+1
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2))
    
    (while (not (= n 0))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (set! h (number->hereditary-base-k-expt n b))
	   ;;(display-nl h)
	   ;;(display-nl (eval h))

	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi)
	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)
	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;;(dv hs)
	   (set! b (+ 1 b))
	   (set! n (- (eval hs) 1)) ;; substract 1
	   ;;(dv n)
	   (newline))
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl n)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")))

    
;; > (goodstein-optim 266)
;; G(266)(1)=((2 ^ (2 ^ (2 + 1))) + (2 ^ (2 + 1)) + 2)
;; P(266)(1)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + ω)
;; 3 - 1 = 2

;; G(266)(2)=((3 ^ (3 ^ (3 + 1))) + (3 ^ (3 + 1)) + 2)
;; P(266)(2)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 2)
;; 2 - 1 = 1

;; G(266)(3)=((4 ^ (4 ^ (4 + 1))) + (4 ^ (4 + 1)) + 1)
;; P(266)(3)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)) + 1)
;; 1 - 1 = 0

;; G(266)(4)=((5 ^ (5 ^ (5 + 1))) + (5 ^ (5 + 1)))
;; P(266)(4)=((ω ^ (ω ^ (ω + 1))) + (ω ^ (ω + 1)))
;; (6 ^ (6 + 1)) - 1 = ((5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)

;; G(266)(5)=((6 ^ (6 ^ (6 + 1))) + (5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; P(266)(5)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 5)
;; 5 - 1 = 4

;; G(266)(6)=((7 ^ (7 ^ (7 + 1))) + (5 * (7 ^ 7)) + (5 * (7 ^ 5)) + (5 * (7 ^ 4)) + (5 * (7 ^ 3)) + (5 * (7 ^ 2)) + (5 * 7) + 4)
;; P(266)(6)=((ω ^ (ω ^ (ω + 1))) + (5 * (ω ^ ω)) + (5 * (ω ^ 5)) + (5 * (ω ^ 4)) + (5 * (ω ^ 3)) + (5 * (ω ^ 2)) + (5 * ω) + 4)
;; 4 - 1 = 3
;;
(define (goodstein-optim n)

  (let ((n-start n) ;;  remove n-start
	(h '()) ;; hereditary base b expression
	(hi '()) ;; infix expression
	(hs '()) ;; hereditary base b+1 expression
	(hsi '()) ;; infix expression
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2)
	(hs-rev '()) ;; reverse of base b+1 hereditary expression
	(monomial '())
	(monomial-1 '())
	(hs-rev-rest '())
	)
    
    (while (not 
	    (and (number? h)
		 (= h 0)))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (when (= 2 b) ;; only at the beginning
		 (set! h (number->hereditary-base-k-expt n b)))

	   ;;(display-nl h)
	   
	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (display-nl hi)
	   ;;(display-nl (eval h))

	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)

	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;; convertir ,ne pas recalculer
	   ;; (set! hsi (prefix->infix hs))
	   ;; (set! hsi
	   ;; 	 (replace hsi 'expt '^)) ;; expt))
	   ;; (dv hsi)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   (cond ((number? hs) ;; sometimes it's a number , not a list
		  (set! h (- hs 1)))
	       
		 ((is+? hs)
		  (set! hs-rev (reverse hs)) ;; reverse to get the monomial first (low polynomial degrees firsts)
		  ;;(dv hs-rev)
		  (set! monomial (first hs-rev)) ;; get lower degree monomial
		  ;;(dv monomial)
		  ;;(display (prefix->infix (expt->^ monomial)))
		  ;;(display " - 1 = ")
		  (set! hs-rev-rest (rest hs-rev)) ;; keep the rest
		  ;;(dv hs-rev-rest)
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
		  ;;(display (prefix->infix (expt->^ monomial-1)))
		  ;;(newline)
		  (cond ((number? monomial-1)
			 (if (zero? monomial-1)
			     (if (pair-list? hs-rev-rest) ;; ex : hs-rev-rest = ( e1 + )
				 (set! h (first hs-rev-rest)) ;; remove the no more usefull operator
				 (set! h (reverse hs-rev-rest))) ;; hs-rev-rest = (e1 e2 ... +)
			     (set! h 
				   (reverse (insert monomial-1 hs-rev-rest)))))
			((is*? monomial-1) ;; monomial-1 = ( c * b^s)
			 (set! h
			       (reverse (insert monomial-1 hs-rev-rest))))
			(else ;; monomial-1 = (+ e1 e2 ....)
			 (set! h
			       (reverse
				(append
				 (reverse (args monomial-1)) ;; lower to higher monomial degrees
				 hs-rev-rest))))))
		 (else ;; e
		  (set! monomial hs)
		  ;;(dv monomial)
		  (display (prefix->infix (expt->^ monomial)))
		  (display " - 1 = ")
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
		  (display (prefix->infix (expt->^ monomial-1)))
		  (newline)
		  (set! h monomial-1)))
	   ;;(dv h)
	   (newline)) ;; end WHILE
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    ))




;; this version defines only 2 cases : addition of monomial and others (monomial or number)
(define (goodstein-optim-enhanced n)

  (let ((n-start n) ;; remove n-start
	(h '()) ;; hereditary base b expression
	(hi '()) ;; infix expression
	(hs '()) ;; hereditary base b+1 expression
	(hsi '()) ;; infix expression
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2)
	(hs-rev '()) ;; reverse of base b+1 hereditary expression
	(monomial '())
	(monomial-1 '())
	(hs-rev-rest '())
	)
    
    (while (not 
	    (and (number? h)
		 (= h 0)))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (when (= 2 b) ;; only at the beginning
		 (set! h (number->hereditary-base-k-expt n b)))

	   ;;(display-nl h)
	   
	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (display-nl hi)
	   ;;(display-nl (eval h))

	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)

	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;; convertir ,ne pas recalculer
	   ;; (set! hsi (prefix->infix hs))
	   ;; (set! hsi
	   ;; 	 (replace hsi 'expt '^)) ;; expt))
	   ;; (dv hsi)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   (cond ;;((number? hs) ;; sometimes it's a number , not a list
		 ;; (set! h (- hs 1)))
	       
		 ((and
		   (not (number? hs))
		   (is+? hs))

		  (set! hs-rev (reverse hs)) ;; reverse to get the monomial first (low polynomial degrees firsts)
		  ;;(dv hs-rev)
		  (set! monomial (first hs-rev)) ;; get lower degree monomial
		  ;;(dv monomial)
		  (display (prefix->infix (expt->^ monomial)))
		  (display " - 1 = ")
		  (set! hs-rev-rest (rest hs-rev)) ;; keep the rest
		  ;;(dv hs-rev-rest)
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
		  (display (prefix->infix (expt->^ monomial-1)))
		  (newline)
		  (cond ((number? monomial-1)
			 (if (zero? monomial-1)
			     (if (pair-list? hs-rev-rest) ;; ex : hs-rev-rest = ( e1 + )
				 (set! h (first hs-rev-rest)) ;; remove the no more usefull operator
				 (set! h (reverse hs-rev-rest))) ;; hs-rev-rest = (e1 e2 ... +)
			     (set! h 
				   (reverse (insert monomial-1 hs-rev-rest)))))
			((is*? monomial-1) ;; monomial-1 = ( c * b^s)
			 (set! h
			       (reverse (insert monomial-1 hs-rev-rest))))
			(else ;; monomial-1 = (+ e1 e2 ....)
			 (set! h
			       (reverse
				(append
				 (reverse (args monomial-1)) ;; lower to higher monomial degrees
				 hs-rev-rest))))))

		 (else ;; e
		  (set! monomial hs)
		  ;;(dv monomial)
		  (display (prefix->infix (expt->^ monomial)))
		  (display " - 1 = ")
		  (set! monomial-1
			(number->hereditary-base-k-expt 
			 (- (eval monomial) 1) ;; substract one
			 b))
		  ;;(dv monomial-1)
		  (display (prefix->infix (expt->^ monomial-1)))
		  (newline)
		  (set! h monomial-1)))
	   ;;(dv h)
	   (newline)) ;; end WHILE
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    ))



;; take a monomial c*b^n and substract 1 ( with simplification)
;;
;; DEPRECATED function
;;
;;(hereditary-base-monomial-1 '(* 4 (expt 6 7))) -> 
(define (hereditary-base-monomial-1-simp P) ;; f = hereditary-base-monomial-1
  
  (cond ((number? P)
	 (- P 1))
	((is*? P) ;; c*b^n = (c-1)*b^n + b^n -> (c-1)*b^n + f(b^n)
	 (let* ((c (simplify (arg1 P)))
		(c-1 (simplify (- c 1)))
		;;(hereditary-base-monomial-1 c))) ;; just a hack : we do the -1 by calling again (hereditary-base-monomial-1 but it will return a NUMBER, nothing else ,i could have returned (- (arg1 P) 1)
		(b^n (simplify (arg2 P))))

	   (list (quote +)
		 (list (quote *) c-1 b^n)
		 (hereditary-base-monomial-1-simp b^n))))

	(else ;; b^n = b*b^(n-1) = (b-1)*b^(n-1) + b^(n-1) -> (b-1)*b^(n-1) + f(b^(n-1))
	 (let* ((b (simplify (arg1 P)))
		(n (simplify (arg2 P)))
		(b-1 (simplify (- b 1)))
		(n-1 (simplify (- n 1))))

	   (if (= (- n 1) 1)
	       `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1-simp b))
	       `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1-simp `(expt ,b ,n-1))))))))


;; (2 * (24 ^ 2)) - 1 = ((24 ^ 2) + (23 * 24) + 23)







(define (goodstein-symbolic n)

  (let ((n-start n) ;; 
	(h '()) ;; hereditary base b expression
	(hi '()) ;; infix expression
	(hs '()) ;; hereditary base b+1 expression
	(hsi '()) ;; infix expression
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2)
	(hs-rev '()) ;; reverse of base b+1 hereditary expression
	(monomial '())
	(monomial-1 '())
	(hs-rev-rest '())
	)
    
    (while ;; not zero
     (not (and
	   (number? h)
	   (= h 0)))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (when (= 2 b) ;; only at the beginning
		 (set! h (number->hereditary-base-k-expt n b)))

	   ;;(display-nl h)
	   
	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (display-nl hi)
	   ;;(display-nl (eval h))

	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)

	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;; convertir ,ne pas recalculer
	   ;; (set! hsi (prefix->infix hs))
	   ;; (set! hsi
	   ;; 	 (replace hsi 'expt '^)) ;; expt))
	   ;; (dv hsi)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   (cond ((number? hs) ;; sometimes it's a number , not a list
		  (set! h (- hs 1)))
	       
		 ((is+? hs)
		  (dv hs)
		  (set! hs-rev (reverse hs)) ;; reverse to get the monomial first (low polynomial degrees firsts)
		  ;;(dv hs-rev)
		  (set! monomial (first hs-rev)) ;; get lower degree monomial
		  ;;(dv monomial)
		  (display-nl "is+?")
		  (display (prefix->infix (expt->^ monomial)))
		  (display " - 1 = ")
		  (set! hs-rev-rest (rest hs-rev)) ;; keep the rest
		  ;;(dv hs-rev-rest)
		  (set! monomial-1
			(hereditary-base-monomial-1 monomial)) ;; substract one
    
		  ;;(dv monomial-1)
		  (display (prefix->infix (expt->^ monomial-1)))
		  (newline)
		  (cond ((number? monomial-1)
			 (if (zero? monomial-1)
			     (if (pair-list? hs-rev-rest) ;; ex : hs-rev-rest = ( e1 + )
				 (set! h (first hs-rev-rest)) ;; remove the no more usefull operator
				 (set! h (reverse hs-rev-rest))) ;; hs-rev-rest = (e1 e2 ... +)
			     (set! h 
				   (reverse (insert monomial-1 hs-rev-rest)))))
			((is*? monomial-1) ;; monomial-1 = ( c * b^s)
			 (set! h
			       (reverse (insert monomial-1 hs-rev-rest))))
			(else ;; monomial-1 = (+ e1 e2 ....)
			 (set! h
			       (reverse
				(append
				 (reverse (args monomial-1)) ;; lower to higher monomial degrees
				 hs-rev-rest))))))
		 (else ;; e
		  (set! monomial hs)
		  (display-nl "else")
		  ;;(dv monomial)
		  (display (prefix->infix (expt->^ monomial)))
		  (display " - 1 = ")
		  (set! monomial-1
			(hereditary-base-monomial-1 monomial)) ;; substract one
		       
		  ;;(dv monomial-1)
		  (display (prefix->infix (expt->^ monomial-1)))
		  (newline)
		  (set! h monomial-1)))
	   ;;(dv h)
	   (newline)) ;; end WHILE
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    ))


;; decrement polynomial numerically
(define (numeric-polynomial-1 hs b) ;; hs : h successor , b base

  (let ((monomial '())
	(monomial-1 '())
	(hs-rev '())  ;; hs reverse
	(hs-rev-rest '()) ;; hs reverse rest
	(h '())) ;; result 
    
    (cond
     
     ((and    ;; polynomial
       (not (number? hs)) 
       (is+? hs))
      
      (set! hs-rev (reverse hs)) ;; reverse to get the monomial first (low polynomial degrees firsts)
      ;;(dv hs-rev)
      (set! monomial (first hs-rev)) ;; get lower degree monomial
      ;;(dv monomial)
      (display (prefix->infix (expt->^ monomial)))
      (display " - 1 = ")
      (set! hs-rev-rest (rest hs-rev)) ;; keep the rest
      ;;(dv hs-rev-rest)
      (set! monomial-1
	    (number->hereditary-base-k-expt 
	     (- (eval monomial) 1) ;; substract one
	     b))
      ;;(dv monomial-1)
      (display (prefix->infix (expt->^ monomial-1)))
      (newline)


      (cond ((number? monomial-1)

	     (if (zero? monomial-1) 
		 ;; zero
		 (if (pair-list? hs-rev-rest) ;; ex : hs-rev-rest = ( e1 + )
		     (set! h (first hs-rev-rest)) ;; remove the no more usefull operator
		     (set! h (reverse hs-rev-rest))) ;; hs-rev-rest = (e1 e2 ... +)
		 ;; not zero
		 (set! h 
		       (reverse (insert monomial-1 hs-rev-rest))))) ;; reconstruct the polynomial

	    ((is*? monomial-1) ;; monomial-1 = ( c * b^s)
	     (set! h
		   (reverse (insert monomial-1 hs-rev-rest)))) ;; reconstruct the polynomial

	    (else ;; monomial-1 = (+ e1 e2 ....)
	     (set! h
		   (reverse
		    (append ;; reconstruct the polynomial
		     (reverse (args monomial-1)) ;; lower to higher monomial degrees
		     hs-rev-rest)))))) ;; end case polynomial


     (else ;; e = monomial

      (set! monomial hs)
      ;;(dv monomial)
      (display (prefix->infix (expt->^ monomial)))
      (display " - 1 = ")
      (set! monomial-1
	    (number->hereditary-base-k-expt 
	     (- (eval monomial) 1) ;; substract one
	     b))
      ;;(dv monomial-1)
      (display (prefix->infix (expt->^ monomial-1)))
      (newline)
      (set! h monomial-1)))

    h))



;; this version call a subroutine for computing the decremented polynomial
;; this version defines only 2 cases : polynomial (addition of monomial) and others (monomial or number)
(define (goodstein-optim-enhanced-funct n)

  (let ((n-start n) ;; 
	(h '()) ;; hereditary base b expression
	(hi '()) ;; infix expression
	(hs '()) ;; hereditary base b+1 expression
	(hsi '()) ;; infix expression
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2)
	(hs-rev '()) ;; reverse of base b+1 hereditary expression
	(monomial '())
	(monomial-1 '())
	(hs-rev-rest '())
	)
    
    (while (not 
	    (and (number? h)
		 (= h 0)))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (when (= 2 b) ;; only at the beginning
		 (set! h (number->hereditary-base-k-expt n b)))

	   ;;(display-nl h)
	   
	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (display-nl hi)
	   ;;(display-nl (eval h))

	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)

	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;; convertir ,ne pas recalculer
	   ;; (set! hsi (prefix->infix hs))
	   ;; (set! hsi
	   ;; 	 (replace hsi 'expt '^)) ;; expt))
	   ;; (dv hsi)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   (dv hs)

	   (set! h (numeric-polynomial-1 hs b))
	   
	   (dv h)

	   (newline)) ;; end WHILE
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    ))


;; this version call a subroutine for computing the decremented polynomial
;; this version defines only 2 cases : polynomial (addition of monomial) and others (monomial or number)
;; 
;; > (symbolic-goodstein-optim-enhanced-funct 266)
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
(define (symbolic-goodstein-optim-enhanced-funct n)

  (let ((n-start n) 
	(h '()) ;; hereditary base b expression
	(hi '()) ;; infix expression
	(hs '()) ;; hereditary base b+1 expression
	(hsi '()) ;; infix expression
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	
	(hi-omega '()) ;; hereditary infix omega 
	(b 2)
	(hs-rev '()) ;; reverse of base b+1 hereditary expression
	(monomial '())
	(monomial-1 '())
	(hs-rev-rest '())
	)
    
    (while (not 
	    (and (number? h)
		 (= h 0)))

	   ;; dv : display variable
	   ;;(display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (when (= 2 b) ;; only at the beginning
		 (set! h (number->hereditary-base-k-expt n b)))	   ;;(display-nl h)
	   
	   ;; convertir ,ne pas recalculer
	   (set! hi (prefix->infix h))
	   (set! hi
		 (replace hi 'expt '^)) ;; expt))
	   ;;(set! hi (number->hereditary-base-k-infix n b))
	   
	   (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   
	   (display-nl hi)
	   ;;(display-nl (eval h))

	   (set! hi-omega 
		 (replace hi b omega))
	   (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
	   (display-nl hi-omega)

	   (set! hs ;; bump the base
		 (replace h b (+ 1 b)))
	   ;; convertir ,ne pas recalculer
	   ;; (set! hsi (prefix->infix hs))
	   ;; (set! hsi
	   ;; 	 (replace hsi 'expt '^)) ;; expt))
	   ;; (dv hsi)

	   (set! b (+ 1 b))

	   ;;(set! n (- (eval hs) 1)) ;; substract 1
	   
	   ;;(dv hs)

	   (set! h (symbolic-polynomial-1 hs))
	   
	   ;;(dv h)

	   #;(newline)) ;; end WHILE
    
    (display "G(") (display n-start) (display ")(") (display (- b 1)) (display ")=")
    (display-nl h)
    (display "P(") (display n-start) (display ")(") (display (- b 1)) (display ")=0")
    
    ))






;; decrement symbolic polynomial

;; **********************************************************************************************************************

;; Mathematical abstract:

;; h : P -> P - 1

;; polynomial P(b) = Cn.b^n + Cn-1.b^(n-1) .... + Ck.b^k



;; **********************************************************************************************************************

;; Computer Science (& Mathematical) abstract:

;; h = symbolic-polynomial-1
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
;; -keep the rest :
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
;;            => P = (+ e_n e_n-1 e_n-2 ... e_k+1) = Cn.b^n + Cn-1.b^(n-1) .... + Ck+1.b^(k+1) 
;;
;;  m' # 0 (not zero) :
;;
;;    reconstruct the polynomial:
;;      P_rev_rest = (e_k+1 .... e_n-2 e_n-1 e_n +)
;;            => P = (+ e_n e_n-1  ....  e_k+1 m') = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + m'
;;
;;
;; not number:
;;
;; monomial-1 = m' = (+ e'_n' e'_n'-1 .... e'_k') = Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
;;          (higher to lower monomial degrees)
;;
;; P_rev_rest = ( e_k+1.... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
;;    => P = (+ e_n e_n-1  ....  e_k+1 e'_n' e'_n'-1 .... e'_k')
;;         = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
;;
;;
;; case Polynomial is a simple monomial:
;;
;; m = P
;; m' = f(m)
;; P = m'
;;
;; return the new P
(define (symbolic-polynomial-1 P) ;; P : hereditary base polynomial, h = symbolic-polynomial-1

  ;; h = symbolic-polynomial-1
  ;; h : P -> P - 1
  
  (let ((monomial '())
	(monomial-1 '())
	(P_rev '())  ;; P reverse
	(P_rev_rest '())) ;; P reverse rest
	
    
    (cond
     
     ;;
     ;; polynomial : (+ e_n e_n-1 e_n-2 ... e_k) = Cn.b^n + Cn-1.b^(n-1) + .... + Ck.b^k
     ;;
     
     ((and    ;; not number and addition of monomials
       (not (number? P)) 
       (is+? P))
      
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
      
      ;; COMPUTE m' = MONOMIAL - 1
      ;; m' = f(m)
      (set! monomial-1 ;;  compute Ck.b^k - 1 in hereditary base
	    (hereditary-base-monomial-1 monomial)) ;; substract one
	     
      ;;(dv monomial-1)
      ;;(display (prefix->infix (expt->^ monomial-1)))
      ;;(newline)

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
		     (set! P (first P_rev_rest)) ;; remove the no more usefull operator : P = e_n = Cn.b^n
		     ;; else
		     ;; P_rev_rest = ( e_k+1.... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
		     (set! P (reverse P_rev_rest))) ;; P = (+ e_n e_n-1 e_n-2 ... e_k+1) = Cn.b^n + Cn-1.b^(n-1) .... + Ck+1.b^(k+1) 
		 
		 ;; not zero
		 (set! P ;; reconstruct the polynomial
		       (reverse
			(insert monomial-1 P_rev_rest))))) ;; (m' e_k+1 .... e_n-2 e_n-1 e_n +)
	    ;; P = (+ e_n e_n-1  ....  e_k+1 m') = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + m'

	    #|((is*? monomial-1) ;; multiplication : monomial-1 = m' = c.b^s, ce cas semble ne jamais avoir lieu car P(b)-1 n'est jamais de cette forme  
	     (set! P
		   (reverse (insert monomial-1 P_rev_rest))))|# ;; reconstruct the polynomial

	    ;; not number:
	    (else ;; monomial-1 = m' = (+ e'_n' e'_n'-1 .... e'_k') = Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
	     ;; (higher to lower monomial degrees)
	     
	     (set! P
		   
		   ;; P_rev_rest = ( e_k+1.... e_n-2 e_n-1 e_n +) = Ck+1.b^(k+1) + ... + Cn-1.b^(n-1) + Cn.b^n
		   ;;    => P = (+ e_n e_n-1  ....  e_k+1 e'_n' e'_n'-1 .... e'_k')
		   ;;         = Cn.b^n + Cn-1.b^(n-1) .... Ck+1.b^(k+1) + Cn'.b^n' + Cn'-1.b^(n'-1) + ... + Ck'.b^k'
		   
		   (reverse ;; higher to lower monomial degrees
		    (append ;; construct the new polynomial with monomial and rest of polynomial
		     (reverse (args monomial-1)) ;; lower to higher monomial degrees
		     P_rev_rest)))))) ;; end case polynomial

     
     ;;
     ;; polynomial = e = monomial  , polynomial is a monomial
     ;;
     (else 
      ;; m = P
      ;; m' = f(m)
      ;; P = m'
      (set! monomial P)
      ;;(dv monomial)
      ;;(display (prefix->infix (expt->^ monomial)))
      ;;(display " - 1 = ")

      (set! monomial-1
	    (hereditary-base-monomial-1
	     monomial)) ;; substract one
	    
      ;;(dv monomial-1)
      ;;(display (prefix->infix (expt->^ monomial-1)))
      ;;(newline)
      (set! P monomial-1)))

    P))






;; take a monomial c.b^n and substract 1 

;; f = hereditary-base-monomial-1

;; f: M -> M - 1

;; M is a monomial, M = (* c (^ b n)) = c.b^n or M = (^ b n) = b^n or M = b

;; propertie : f( M + Q ) = M + Q - 1 = M + f(Q)

;; M is a number:
;;   M = b => f(M) = b-1

;; M is a product:
;;   M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f(M) = f( (c-1).b^n + b^n ) = (c-1).b^n + f(b^n)
;;       note: n could be equal to 1, so M could be equal to c.b

;; M is a power:
;;   M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) => f(M) = f( (b-1).b^(n-1) + b^(n-1) )
;;                                                         = (b-1).b^(n-1) + f(b^(n-1))
;;                                                         = (b-1).b^h(n)  + f(b^h(n))
;;     with n-1 computed with h as n could be a polynomial , h = symbolic-polynomial-1




;; (prefix->infix (expt->^ (simplify (hereditary-base-monomial-1 '(expt 4 5)))))
;;   ->  '((3 * (4 ^ 4)) + ((3 * (4 ^ 3)) + ((3 * (4 ^ 2)) + ((3 * 4) + 3))))
;;
;;(prefix->infix (n-arity (expt->^ (simplify (hereditary-base-monomial-1 '(expt 4 7))))))
;; -> '((3 * (4 ^ 6)) + (3 * (4 ^ 5)) + (3 * (4 ^ 4)) + (3 * (4 ^ 3)) + (3 * (4 ^ 2)) + (3 * 4) + 3)
;;
;; >  (prefix->infix (expt->^ (simplify  (hereditary-base-monomial-1 '(* 4 (expt 6 7))))))
;; '((3 * (6 ^ 7)) + (5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; >  (simplify  (hereditary-base-monomial-1 '(* 4 (expt 6 7))))
;; '(+ (* 3 (expt 6 7)) (* 5 (expt 6 6)) (* 5 (expt 6 5)) (* 5 (expt 6 4)) (* 5 (expt 6 3)) (* 5 (expt 6 2)) (* 5 6) 5)
;; > (* 4 (expt 6 7))
;; 1119744
;; > (+ (* 3 (expt 6 7)) (* 5 (expt 6 6)) (* 5 (expt 6 5)) (* 5 (expt 6 4)) (* 5 (expt 6 3)) (* 5 (expt 6 2)) (* 5 6) 5)
;; 1119743
;; >
;;
;; > (simplify  (hereditary-base-monomial-1 '(expt 5 (+ 5 1))))
;; '(+ (* 4 (expt 5 5)) (* 4 (expt 5 4)) (* 4 (expt 5 3)) (* 4 (expt 5 2)) (* 4 5) 4)
;; > (prefix->infix (expt->^ (simplify  (hereditary-base-monomial-1 '(expt 5 (+ 5 1))))))
;; '((4 * (5 ^ 5)) + (4 * (5 ^ 4)) + (4 * (5 ^ 3)) + (4 * (5 ^ 2)) + (4 * 5) + 4)
;; > (expt 5 (+ 5 1))
;; 15625
;; > (+ (* 4 (expt 5 5)) (* 4 (expt 5 4)) (* 4 (expt 5 3)) (* 4 (expt 5 2)) (* 4 5) 4)
;; 15624
;; > 
;; 
(define (hereditary-base-monomial-1 M) ;; f = hereditary-base-monomial-1

  ;; f: M -> M - 1

  ;; M is a monomial, M = (* c (^ b n)) = c.b^n or M = (^ b n) = b^n or M = b

  ;; propertie : f( M + Q ) = M + Q - 1 = M + f(Q)
  
  ;; (dv M)

  
  ;;     M is a number:
  (cond ((number? M) ;; M = b => f(M) = b-1

	 (- M 1))

	;; M is a product:
	((is*? M) ;; M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f(M) = f( (c-1).b^n + b^n ) = (c-1).b^n + f(b^n)
	 ;; note: n could be equal to 1, so M could be equal to c.b

	 (let* ((c (arg1 M))
		(c-1 (- c 1))
		(b^n (arg2 M)))

	   ;;(display-nl "case c*b^n")
	   ;;(dv c)
	   ;;(dv b^n)
	   
	   (simplify
	    (n-arity ;; put in n-arity the expression
	     (list (quote +)
		   (list (quote *) c-1 b^n)
		   (hereditary-base-monomial-1 b^n)))))) ;; (+ (* c-1 b^n) (f b^n)) = (c-1).b^n + f(b^n)

	
	;; to be continued
	
	;;

	;; M is a power:
	(else ;; M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) -> f(M) = f( (b-1).b^(n-1) + b^(n-1) )
	 ;;                                                            = (b-1).b^(n-1) + f(b^(n-1))
	 ;;                                                            = (b-1).b^h(n)  + f(b^h(n))
	 ;;  with n-1 computed with h(n) as n could be a polynomial, h = symbolic-polynomial-1
	 
	 (let* ((b (arg1 M))
		(n (arg2 M))
		(b-1 (- b 1))
		(n-1 (symbolic-polynomial-1 #;hereditary-base-monomial-1 n))) ;; n-1 = h(n)

	   ;;(display-nl "case b^n")
	   ;;(dv b)
	   ;;(dv n)
	   
	   (simplify
	    (n-arity ;; put in n-arity the expression
	     (if (unity-symb? n-1) ;; if n-1 = 1
		 `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1 b))
		 `(+ (* ,b-1 (expt ,b ,n-1)) ,(hereditary-base-monomial-1 `(expt ,b ,n-1))))))))))







;; goodstein-init : start-up function
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



;; > (goodstein-init 266)
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


(define (goodstein-init n)

  ;; constants and variables
  (let ((n-start n) ;; n at start
	(omega (string->symbol (string #\u03C9))) ;; omega symbol
	(b 2))
    
    (letrec ((goodstein-rec

	      ;; the Goodstein recursive function do:
	      ;;   - check if we have reached zero 
	      ;;   - display polynomial at each step
	      ;;   - bump the base
	      ;;   - decrement polynomial by calling symbolic-polynomial-1 function
	      ;;   - call (recursively) goodstein-rec
	      
	      (lambda (P) ;; polynomial hereditary base b expression

		(if (and (number? P)
			 (= P 0))

		    0
	
		    (let ((Pi '()) ;; infix expression
			  (Ps '()) ;; hereditary base b+1 expression
			  (Psi '()) ;; infix expression
			  (Pi-omega '())) ;; hereditary infix omega expression
			  
		      
		      ;; convertir ,ne pas recalculer
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

		      (set! P (symbolic-polynomial-1 Ps))

		      (goodstein-rec P))))))

      (goodstein-rec (number->hereditary-base-k-expt n b)))))
		    



   
;;   be h and f melted in a single function????



;; decrement symbolic polynomial

;; **********************************************************************************************************************

;; Mathematical abstract:

;; h : P -> P - 1

;; polynomial P(b) = Cn.b^n + Cn-1.b^(n-1) .... + Ck.b^k



;; **********************************************************************************************************************

;; Computer Science (& Mathematical) abstract:

;; h = symbolic-polynomial-1
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
(define (atomic-symbolic-polynomial-1 P) ;; P : hereditary base polynomial, h = symbolic-polynomial-1 function

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
	   (atomic-hereditary-base-monomial-1 monomial)) ;; substract one
	     
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



;; take a monomial c.b^n and substract 1 

;; f = atomic-hereditary-base-monomial-1

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




;; (prefix->infix (expt->^ (simplify (atomic-hereditary-base-monomial-1 '(expt 4 5)))))
;;   ->  '((3 * (4 ^ 4)) + ((3 * (4 ^ 3)) + ((3 * (4 ^ 2)) + ((3 * 4) + 3))))
;;
;;(prefix->infix (n-arity (expt->^ (simplify (atomic-hereditary-base-monomial-1 '(expt 4 7))))))
;; -> '((3 * (4 ^ 6)) + (3 * (4 ^ 5)) + (3 * (4 ^ 4)) + (3 * (4 ^ 3)) + (3 * (4 ^ 2)) + (3 * 4) + 3)
;;
;; >  (prefix->infix (expt->^ (simplify  (atomic-hereditary-base-monomial-1 '(* 4 (expt 6 7))))))
;; '((3 * (6 ^ 7)) + (5 * (6 ^ 6)) + (5 * (6 ^ 5)) + (5 * (6 ^ 4)) + (5 * (6 ^ 3)) + (5 * (6 ^ 2)) + (5 * 6) + 5)
;; >  (simplify  (atomic-hereditary-base-monomial-1 '(* 4 (expt 6 7))))
;; '(+ (* 3 (expt 6 7)) (* 5 (expt 6 6)) (* 5 (expt 6 5)) (* 5 (expt 6 4)) (* 5 (expt 6 3)) (* 5 (expt 6 2)) (* 5 6) 5)
;; > (* 4 (expt 6 7))
;; 1119744
;; > (+ (* 3 (expt 6 7)) (* 5 (expt 6 6)) (* 5 (expt 6 5)) (* 5 (expt 6 4)) (* 5 (expt 6 3)) (* 5 (expt 6 2)) (* 5 6) 5)
;; 1119743
;; >
;;
;; > (simplify  (atomic-hereditary-base-monomial-1 '(expt 5 (+ 5 1))))
;; '(+ (* 4 (expt 5 5)) (* 4 (expt 5 4)) (* 4 (expt 5 3)) (* 4 (expt 5 2)) (* 4 5) 4)
;; > (prefix->infix (expt->^ (simplify  (atomic-hereditary-base-monomial-1 '(expt 5 (+ 5 1))))))
;; '((4 * (5 ^ 5)) + (4 * (5 ^ 4)) + (4 * (5 ^ 3)) + (4 * (5 ^ 2)) + (4 * 5) + 4)
;; > (expt 5 (+ 5 1))
;; 15625
;; > (+ (* 4 (expt 5 5)) (* 4 (expt 5 4)) (* 4 (expt 5 3)) (* 4 (expt 5 2)) (* 4 5) 4)
;; 15624
;; > 
;; 
(define (atomic-hereditary-base-monomial-1 M) ;; f = atomic-hereditary-base-monomial-1

  ;; f: M -> M - 1

  ;; M is a monomial, M = (* c (^ b n)) = c.b^n or M = (^ b n) = b^n or M = b

  ;; important property of f : f( M + Q ) = M + Q - 1 = M + f(Q)     (*)
  
  ;; (dv M)

  
  ;;     M is a number:
  (cond ((number? M) ;; M = b => f(M) = b-1

	 (- M 1))

	;; M is a product:
	((is*? M) ;; M = (* c (^ b n)) = c.b^n = (c-1).b^n + b^n => f(M) = f( (c-1).b^n + b^n ) = (c-1).b^n + f(b^n)  by (*)
	 ;; note: n could be equal to 1, so M could be equal to c.b

	 (let* ((c (arg1 M))
		(c-1 (- c 1))
		(b^n (arg2 M)))

	   ;;(display-nl "case c*b^n")
	   ;;(dv c)
	   ;;(dv b^n)
	   
	   (simplify
	    (n-arity ;; put in n-arity the expression
	     (list (quote +)
		   (list (quote *) c-1 b^n)
		   (atomic-hereditary-base-monomial-1 b^n)))))) ;; (+ (* c-1 b^n) (f b^n)) = (c-1).b^n + f(b^n)

	
	;; to be continued
	
	;;

	;; M is a power:
	(else ;; M = b^n = b.b^(n-1) = (b-1).b^(n-1) + b^(n-1) => f(M) = f( (b-1).b^(n-1) + b^(n-1) )
	 ;;                                                            = (b-1).b^(n-1) + f(b^(n-1))  by (*)
	 ;;                                                            = (b-1).b^h(n)  + f(b^h(n))
	 ;;  with n-1 computed with h(n) as n could be a polynomial,h=atomic-symbolic-polynomial-1
	 
	 (let* ((b (arg1 M))
		(n (arg2 M))
		(b-1 (- b 1))
		(n-1 (atomic-symbolic-polynomial-1 #;atomic-hereditary-base-monomial-1 n))) ;; n-1 = h(n)

	   ;;(display-nl "case b^n")
	   ;;(dv b)
	   ;;(dv n)
	   
	   (simplify
	    (n-arity ;; put in n-arity the expression
	     (if (unity-symb? n-1) ;; if n-1 = 1
		 `(+
		   (* ,b-1 (expt ,b ,n-1))   ;; (b-1).b^(n-1)
		   ,(atomic-hereditary-base-monomial-1 b))
		 `(+
		   (* ,b-1 (expt ,b ,n-1))    ;; (b-1).b^(n-1)
		   ,(atomic-hereditary-base-monomial-1 `(expt ,b ,n-1))))))))))   ;; f(b^(n-1))




;; goodstein-init : start-up function
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



;; > (goodstein-init 266)
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


(define (goodstein-init-atomic n)

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

		    0
	
		    (let ((Pi '()) ;; infix expression
			  (Ps '()) ;; hereditary base b+1 expression, historically was P successor
			  (Psi '()) ;; infix expression of Ps
			  (Pi-omega '())) ;; hereditary infix omega expression of Ps
			  
		      
		      ;; convertir ,ne pas recalculer
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

		      (goodstein-rec (atomic-symbolic-polynomial-1 Ps)))))))

      (goodstein-rec (number->hereditary-base-k-expt n b)))))


















































