;; (number->hereditary-base-2 #b10110)
;; -> '(+ (^ 2 (^ 2 (^ 2 (^ 2 0)))) (^ 2 (^ 2 (^ 2 0))) (^ 2 (^ 2 0)))
;;
;; (number->hereditary-base-2 35) -> (+ (^ 2 (+ (^ 2 2) 1)) 2 1)
(define (number->hereditary-base-2 n)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->hereditary-base-2-rec

	  (lambda (n) 

	    ;;(dv deg)
	    (cond 
	     
	     ((zero? n) '(0))
	     
	     ;;((= 1 n) (quasiquote  ((^ 2 ,(number->hereditary-base-2 deg)))))
	     
	     ((= (modulo n 2) 1)
	      (let ((monomial
		     (quasiquote ((^ 2 ,(number->hereditary-base-2 deg))))))

		(set! deg (+ 1 deg))
		(append
		 (number->hereditary-base-2-rec (quotient n 2))
		 monomial)))
		
	     (else
		(set! deg (+ 1 deg))
		(append (number->hereditary-base-2-rec (quotient n 2)) (list 0)))))))
	 
      (set! poly-prefix (cons '+ (number->hereditary-base-2-rec n)))
      (dv poly-prefix)
      (set! poly-prefix-simp (simplify poly-prefix))
      (dv poly-prefix-simp)
      
      poly-prefix-simp)))
      


;; (number->hereditary-base-k 100 3) -> '(+ (^ 3 (+ 3 1)) (* 2 (^ 3 2)) 1)
(define (number->hereditary-base-k n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->hereditary-base-k-rec

	  (lambda (n k) 

	    (if (zero? n)
		'(0)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      ;;(dv deg)

	      (if (not (zero? r))
		
		  (let ((monomial
			 (quasiquote ((* ,r (^ ,k ,(number->hereditary-base-k deg k))))))
			(q (quotient n k)))
		  
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec q k)
		     monomial))
		
		  (begin
		    
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec (quotient n k) k)
		     (list 0)))))))))
      
      (set! poly-prefix (cons '+ (number->hereditary-base-k-rec n k)))
      ;;(dv poly-prefix)
      (set! poly-prefix-simp (simplify poly-prefix))
      ;;(dv poly-prefix-simp)
      
      poly-prefix-simp)))


;; (number->hereditary-base-k-expt 100 3) -> '(+ (expt 3 (+ 3 1)) (* 2 (expt 3 2)) 1)
;;
;; (number->hereditary-base-k-expt  22876792454964 3) -> '(+ (expt 3 (+ (expt 3 3) 1)) 3)
;;
;; (number->hereditary-base-k-expt  7625597484990 3) -> '(+ (expt 3 (expt 3 3)) 3)
;;
;;  (number->hereditary-base-k-expt   173646739038364 5)
;;
;; '(+
;;   (expt 5 (* 4 5))
;;   (* 4 (expt 5 (+ (* 3 5) 4)))
;;   (* 2 (expt 5 (+ (* 3 5) 2)))
;;   (* 3 (expt 5 (+ (* 3 5) 1)))
;;   (expt 5 (+ (* 2 5) 3))
;;   (* 2 (expt 5 (+ (* 2 5) 2)))
;;   (expt 5 (* 2 5))
;;   (expt 5 (+ 5 3))
;;   (* 4 (expt 5 (+ 5 2)))
;;   (* 3 (expt 5 (+ 5 1)))
;;   (* 2 (expt 5 5))
;;   (expt 5 4)
;;   (expt 5 3)
;;   (* 4 (expt 5 2))
;;   (* 2 5)
;;   4)
(define (number->hereditary-base-k-expt n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->hereditary-base-k-rec-expt

	  (lambda (n k) 

	    (if (zero? n)
		'(0)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      ;;(dv deg)

	      (if (not (zero? r))
		
		  (let ((monomial
			 (quasiquote ((* ,r (expt ,k ,(number->hereditary-base-k-expt deg k))))))
			(q (quotient n k)))
		  
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec-expt q k)
		     monomial))
		
		  (begin
		    
		    (set! deg (+ 1 deg))
		    (append
		     (number->hereditary-base-k-rec-expt (quotient n k) k)
		     (list 0)))))))))
      
      (set! poly-prefix (cons '+ (number->hereditary-base-k-rec-expt n k)))
      ;;(dv poly-prefix)
      (set! poly-prefix-simp (simplify poly-prefix))
      ;;(dv poly-prefix-simp)
      
      poly-prefix-simp)))



;; (number->hereditary-base-2-infix #b10110)
;; -> '((2 ^ (2 ^ (2 ^ (2 ^ 0)))) + (2 ^ (2 ^ (2 ^ 0))) + (2 ^ (2 ^ 0)))
;;
;; (number->hereditary-base-2-infix 26)
;; -> '((2 ^ (2 ^ 2)) + (2 ^ (2 + 1)) + 2)
;; previous -> '((2 ^ (2 ^ (2 ^ (2 ^ 0)))) + (2 ^ ((2 ^ (2 ^ 0)) + (2 ^ 0))) + (2 ^ (2 ^ 0)))
;;
(define (number->hereditary-base-2-infix n)
  (prefix->infix (number->hereditary-base-2 n)))


;; (number->hereditary-base-k-infix  22876792454964 3)
;; -> '((3 ^ ((3 ^ 3) + 1)) + 3)
;;
;; (number->hereditary-base-k-infix  109 7) -> '((2 * (7 ^ 2)) + 7 + 4)
;;
;; (number->hereditary-base-k-infix  299 12) -> '((2 * (12 ^ 2)) + 11)
;;
(define (number->hereditary-base-k-infix n k)
  (prefix->infix (number->hereditary-base-k n k)))
