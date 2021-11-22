;; useful binary and arithmetic functions





;; binary list to true-false list
;; (binList2TrueFalseList '(1 1 0 1 0 0)) -> (#t #t #f #t #f #f)
(define (binList2TrueFalseList L)
  (if (null? L)
      L
      (if (= (car L) 1)
          (cons #t (binList2TrueFalseList (cdr L)))
          (cons #f (binList2TrueFalseList (cdr L))))))

;; true-false list to binary list
;; >  (trueFalseList2binList '(#t #t #f #f #t #t #t #t)) -> '(1 1 0 0 1 1 1 1)
(define (trueFalseList2binList L)
  (map boolean->binary L))



;; number2binlist : convert a number in a list containing its binary number conversion
;; (number2binlist #b10110) --> (1 0 1 1 0)
(define (number2binlist n)

  (cond 

   ((zero? n) '(0))

   ((= 1 n) '(1))

   ((= (modulo n 2) 1) (append (number2binlist (quotient n 2)) (list 1)))

   (else (append (number2binlist (quotient n 2)) (list 0)))))


;; (number->poly-base-2 #b10110) -> '(+ (^ 2 4) (^ 2 2) (^ 2 1))
;;
(define (number->poly-base-2 n)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->poly-base-2-rec

	  (lambda (n) 

	    ;;(dv deg)
	    (cond 
	     
	     ((zero? n) '(0))
	     
	     ;;((= 1 n) (quasiquote #;((2 ^ ,deg)) ((^ 2 ,deg))))
	     
	     ((= (modulo n 2) 1)
	      (let ((monomial (quasiquote #;((2 ^ ,deg)) ((^ 2 ,deg)))))
		(set! deg (+ 1 deg))
		(append
		 (number->poly-base-2-rec (quotient n 2))
		 monomial)))
		
	     (else
		(set! deg (+ 1 deg))
		(append (number->poly-base-2-rec (quotient n 2)) (list 0)))))))
	 
      (set! poly-prefix (cons '+ (number->poly-base-2-rec n)))
      ;;(dv poly-prefix)
      (set! poly-prefix-simp (simplify poly-prefix))
      ;;(dv poly-prefix-simp)
           
      poly-prefix-simp)))


(define (number->poly-base-k n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->poly-base-k-rec

	  (lambda (n k) 

	    (dv deg)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      (cond 
	     
	       ((zero? n) '(0))
	       
	       ;;((= 1 r) (quasiquote  ((^ ,k ,deg))))
	     
	       ((not (zero? r))
		(let ((monomial (quasiquote  ((* ,r (^ ,k ,deg)))))
		      (q (quotient n k)))
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec q k)
		   monomial)))
	       
	       (else
		
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec (quotient n k) k)
		   (list 0))))))))
	 
      (set! poly-prefix (cons '+ (number->poly-base-k-rec n k)))
      (dv poly-prefix)
      (set! poly-prefix-simp (simplify poly-prefix))
      (dv poly-prefix-simp)
           
      poly-prefix-simp)))



(define (number->poly-base-k-expt n k)
  
  (let ((deg 0)  ;; polynom degree
	(poly-prefix '())
	(poly-prefix-simp '()))

    (letrec

	((number->poly-base-k-rec-expt

	  (lambda (n k) 

	    ;;(dv deg)
	    
	    (let ((r (modulo n k))) ;; remainder
	      
	      (cond 
	     
	       ((zero? n) '(0))
	       
	       ((not (zero? r))
		(let ((monomial (quasiquote  ((* ,r (expt ,k ,deg)))))
		      (q (quotient n k)))
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec-expt q k)
		   monomial)))
	       
	       (else
		
		  (set! deg (+ 1 deg))
		  (append
		   (number->poly-base-k-rec-expt (quotient n k) k)
		   (list 0))))))))
	 
      (set! poly-prefix (cons '+ (number->poly-base-k-rec-expt n k)))
      (dv poly-prefix)
      (set! poly-prefix-simp (simplify poly-prefix))
      (dv poly-prefix-simp)
           
      poly-prefix-simp)))





;; (number->poly-base-2-infix #b10110) -> '((2 ^ 4) + (2 ^ 2) + (2 ^ 1))
;;
(define (number->poly-base-2-infix n)
  (prefix->infix (number->poly-base-2 n)))

(define (number->poly-base-k-infix n k)
  (prefix->infix (number->poly-base-k n k)))




;; binlist2number : convert a binary list to a number
;;>  (binlist2number '(1 1 0 0 1 1 1 1)) -> 207
(define (binlist2number L)
  (letrec ((revL (reverse L)) ; reversed list
	   (reverseBinList->number

	    (lambda (revBinLst expo) ; starting exposant 

	      (when debug-mode
		    (display "calling reverseBinList") (newline)
		    (display "... revBinLst : ") (display revBinLst) (newline)
		    (display "... expo : ") (display expo) (newline) (newline))

	      (if (null? revBinLst)

		  0 ;; zero is not only represented by an empty list but it will do the job of ending computation
		  
		  (let* ((Bk (first revBinLst))
			 (BkX2Pk (* Bk (expt 2 expo)))) ;; Bk * 2^k
		    (+ 
		     (begin
		       (when debug-mode
			   (display "...... Bk : ") (display Bk) (newline)
			   (display "...... Bk * (2 ^ expo) : ") (display BkX2Pk) (newline))
		       BkX2Pk)
		     (let ((restRBL2n (reverseBinList->number (rest revBinLst) (1+ expo))))
		       (begin
			 (when debug-mode
			       (display "...... restRBL2n : ") (display restRBL2n) (newline))
			 restRBL2n ))))))))

    (reverseBinList->number revL 0))) ; expo set to 0 at beginning


    
(define (boolean->binary b)
  (if b 1 0))

;; convert binary string number to carries string
;; (binary-string->carries "   101101110" ) -> "   1 11 111 "
(define (binary-string->carries s)
  (string-replace s "0" " "))

;; convert binary string number to carries string
(define (binary-string->carries-c s)
  (string-replace (string-replace s "0" " ") "1" "C"))

;; display binary numbers with padding
;;  (padding #b10110) -> "0000000000010110"
(define (padding x)
  ;;(~r x #:base 2 #:min-width 24 #:pad-string "0"))
  (format #f "~24,'0,':b" x))

;; display binary numbers with padding
;;  (padding-spc #b10110) -> "                   10110"
;;
;; WARNING : use monospace font in DrRacket to get constant spacing
;;
(define (padding-spc x)
  ;;(~r x #:base 2 #:min-width 24 #:pad-string " "))
   (format #f "~24,' ,':b" x))

(define (display-binary-pad x)
  (display (padding-spc x)))


;; (flag-set? #b10 #b11) -> #t
;; (flag-set? #b100 #b11) -> #f
(define (flag-set? f x)
  (= (bitwise-and f x) f))


;; test the bit of k position
;; (bit-test? #b101 2) -> #t
;; (bit-test? #b101 1) -> #f
(define (bit-test? x k)
  (flag-set? (expt 2 k) x))

;;  (bit-value #b10110 2) -> 1
;; > (bit-value #b10110 0) -> 0
;; > (bit-value #b10110 3) -> 0
(define (bit-value n pos)
  (bitwise-and 1
	       (arithmetic-shift n (- pos))))

;; logarithme binaire
(define (lb x)
  (/ (log x) (log 2)))

;; > (size-bit #b1000) -> 4
;; > (size-bit #b1001) -> 4
;; > (size-bit #b001) -> 1
;; > (size-bit #b0) -> 1
(define (size-bit x)
  (if (= 0 x) ;; to avoid error with lb
      1
      (inexact->exact
       (+ 1
	  (floor (lb x))))))

;; increment variable
;; nota: DrRacket Scheme has it own add1 function 
(define-syntax 1+
  (syntax-rules ()
    ((_ x)   (begin (set! x (+ x 1)) x))))


;; shift left a binary number
(define-syntax shift-left
  (syntax-rules ()
    ((_ x) (arithmetic-shift x 1))
    ((_ x n) (arithmetic-shift x n))))

;; shift right a binary number
(define-syntax shift-right
  (syntax-rules ()
    ((_ x) (arithmetic-shift x -1))
    ((_ x n) (arithmetic-shift x (- n)))))


;; non symbolic functions
;;

;; compute Cout, the 'carry out' of the result of Cin + A + B
(define (compute-carry Cin A B)
  ;; (compute-carry #t #f #t) -> #t
  (xor (and A B) (and Cin (xor A B))))
 
(define (compute-sum Cin A B)
  ;; (compute-sum #f #t #t) -> #f
  ;; (compute-sum #t #t #t) -> #t
  (xor Cin (xor A B)))


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

(define-syntax macro-return-function-compare-2-bits-with-kontinuation ;; continuation version of macro-compare-2-bits
  ;; i need a macro because of external function to the clozure
  (syntax-rules (kontinuation)
    ((_) (let ((cnt 0)) ;; counter
			(lambda (b1 b2) (if (equal? b1 b2)
					  b1
					  (begin
					    (set! cnt (add1 cnt))
					    (when (> cnt 1) (kontinuation #f)) ;; escaping with the continuation
					    'x)))))))



(define-syntax macro-compare-2-bits ;; i need a macro because of external variable to the clozure
  (syntax-rules ()
    ((_ condition) (let ((cnt 0)) ;; counter
		     (lambda (b1 b2) (if (equal? b1 b2)
				       b1
				       (begin
					 (set! cnt (add1 cnt))
					 (when (> cnt 1) (set! condition #t))
					 'x)))))))


(define-syntax macro-compare-2-bits-with-continuation ;; continuation version of macro-compare-2-bits
  ;; i need a macro because of external function to the clozure
  (syntax-rules ()
    ((_ continuation) (let ((cnt 0)) ;; counter
			(lambda (b1 b2) (if (equal? b1 b2)
					  b1
					  (begin
					    (set! cnt (add1 cnt))
					    (when (> cnt 1) (continuation #f)) ;; escaping with the continuation
					    'x)))))))



(define-syntax macro-compare-2-bits-with-kontinuation ;; continuation version of macro-compare-2-bits
  ;; i need a macro because of external function to the clozure
  (syntax-rules (#;kontinuation)
    ((_) (let ((cnt 0)) ;; counter
			(lambda (b1 b2) (if (equal? b1 b2)
					  b1
					  (begin
					    (set! cnt (add1 cnt))
					    (when (> cnt 1) (kontinuation #f)) ;; escaping with the continuation
					    'x)))))))




;; function  version of macro-compare-2-bits-with-continuation
;;
;; > (call/cc (lambda (k) (function-compare-2-bits-with-continuation k 1 0)))
;; cnt = 1

;; 'x
;; > (call/cc (lambda (k) (function-compare-2-bits-with-continuation k 1 0)))
;; cnt = 2

;; #f
(define function-compare-2-bits-with-continuation 
 
  (let ((cnt 0)) ;; counter
    (lambda (continuation x y) (if (equal? x y)
				   x
				   (begin
				     (set! cnt (add1 cnt))
				     ;;(dv cnt)
				     ;;(newline)
				     (when (> cnt 1) (continuation #f)) ;; escaping with the continuation
				     'x)))))


;; exclusive or
(define (xor p q) 
  ;; (xor #f #f) -> #f
  ;; (xor #t #f) -> #t
  (or (and p (not q)) (and (not p) q)))

;; (xor 0 0) -> 0
;; (xor 1 0) -> 1
;;
;; for DrRacket Scheme
;;(bitwise-xor p q))
;; (if (and (equal? p 1) (equal? q 1))
;;     0
;;     1))

  
;; symbolic exclusive or
(define (symb-xor p q) 
  ;; (symb-xor 'p 'q) -> '(or (and p (not q)) (and (not p) q))
  `(or (and ,p (not ,q)) (and (not ,p) ,q)))



;; adder circuit functions

;; symbolic functions
;;

;; compute Sum symbolically
;; return result of Cin + A + B (Cin being 'carry in')
(define (symb-compute-sum  Cin A B)
  ;; (symb-compute-sum 'Ci 'a 'b) -> '(or (and Ci (not (or (and a (not b)) (and (not a) b)))) (and (not Ci) (or (and a (not b)) (and (not a) b))))
  ;; (enlight-dnf (symb-compute-sum 'Ci 'a 'b)) -> (a^b^Ci)v(!a^!b^Ci)v(!a^b^!Ci)v(a^!b^!Ci)
  (symb-xor Cin (symb-xor A B)))

(define (symb-compute-carry Cin A B)
  ;; (symb-compute-carry 'Ci 'a 'b)
  ;; -> '(or (and (and a b) (not (and Ci (or (and a (not b)) (and (not a) b)))))
  ;;   (and (not (and a b)) (and Ci (or (and a (not b)) (and (not a) b)))))
  ;;
  ;; (enlight-dnf (symb-compute-carry 'Ci 'a 'b)) -> (a^b)v(a^b^!Ci)v(a^!b^Ci)v(!a^b^Ci)
  ;; (prefix->infix (simplify (n-arity (simplify-OR (simplify-AND (dnf (symb-compute-carry 'C 'a 'b)))))))
  ;;  -> '((a and !b and C) or (!a and b and C) or (a and b) or (a and b and !C))
  ;; todo: mettre sous forme disjunctive minimale
  (symb-xor `(and ,A ,B) `(and ,Cin  ,(symb-xor A B))))

;; redondant avec size-bit
;; (define (binary-length n)
;;   (if (= 0 n)
;;       1
;;       (integer-length n)))


(define (last-bit-position n) (- (size-bit n) 1))


;;(count-ones #b11001010) -> 4
(define (count-ones n)
  (define c 0)
  (for (i 0 (size-bit n))
       (when (bit-test? n i)
	     (incf c)))
  c)


(define (mult3 x)
  (+ (shift-left x) x)) ;; return 2x+x

(define-syntax mac-mult3
  (syntax-rules ()

    ((_ x)
     (+ (shift-left x) x))))
