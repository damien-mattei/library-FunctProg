;;#lang racket

;; for DrRacket Scheme uncomment above

;; syracuse.scm

;; Damien Mattei



;; compute carries recursively
;;
;; start recursively the computation for all bk 
;; with k>=2 to the end of the Bk list, i.e to the HSB
;;
;; Arguments:
;;
;; rBk : the reverse Bk list is the bits of B from the k-th bit to the last bit (HSB)
;; bk-2 is the bit of B of index k-2
;; bk-1 is the bit of B of index k-1
;; ck-1 is the carry at the top of the column of index k-1 based on Bn indexing
;; in the addition described below :
;;
+;;  computing 3*B + c = A + B  with Carry set to c at start of computation
;;
;;      A + B + c = S
;;
;;  2 * B + B + c = S
;;
;;
;;    ....Cn....c = 000...00c at start of computation
;;      Bn.....10 = A = 2 * B 
;;  +     Bn....1 = B
;;  -------------
;;    ....Sn....s = S
;;
;;
;; Return:
;; the list of the computed carries of indexes starting from Bk (usually k=2)
;; for k=2 we start with b0, b1,c1 and we compute c2 at first and after c3,...,cn,cn+1 
;; in the others recursive calls, in the general case for k:
;; we compute ck,ck+1,ck+2,....,cn,cn+1
;; when bk is used from the list rBk we compute ck+1
;; for bn we compute cn+1 ,if cn+1 is a carry set 
;; depending if cn+1 is a carry set an extra carry is reported 
;; and had to be appended to the result list of carries
;; this list is oriented as this: the carries of the LSB are at the tail 
;; the carries of the MSB are at the beginning of the list - it is not a reversed list.
;; (cc-rec '() #f #f #t) -> (#f)
;; (cc-rec '(#f) #t #f #t) ->  (#f #t)
;; (cc-rec '() #t #f #f) -> (#f)
;; (cc-rec '() #t #f #t) -> '(#t)
;; (cc-rec '(#f) #f #f #t) -> (#f #f)
;; (cc-rec '(#t) #t #f #f) -> (#f #f)
;; (cc-rec '(#t) #f #t #f) -> (#t #f)
;; details:
;; (#t #f) is 1 0 in 1 1 0 0
;; 1100
;;  110
;;   11
;; (cc-rec '(#t) #t #f #t) -> '(#t #t)
;; details:
;; (#t #t) is 1 1 in the middle of 1 1 1 1
;; 1111
;;  110
;;   11

;;
;;    ....Cn..CkCk-1...c = 000...00c at start of computation
;;      Bn......Bk-2..10 = A = 2 * B 
;;  +     Bn..BkBk-1...1 = B
;;  --------------------
;;    ....Sn..Sk.......s = S
;;
;; rBk means "reverse Bk list"
(define (cc-rec rBk bk-2 bk-1 ck-1)

  (if-t debug-mode
	(display "calling cc-rec") (newline)
	(display "...") (display "rBk :") (display rBk) (newline)
	(display "...") (display "bk-2 :") (display bk-2) (newline)
	(display "...") (display "bk-1 :") (display bk-1) (newline)
	(display "...") (display "ck-1 :") (display ck-1) (newline))

  (cond
   ((null? rBk) (list (compute-carry ck-1 bk-2 bk-1)))
   
   (else
    
    (let* ((ck (compute-carry ck-1 bk-2 bk-1)) ;; compute the next carry (Carry Out)
	   (bk (car rBk))   ;; get Bk
	   (rBk+1 (cdr rBk))  ;; set rBk+1
	   (carries-out-list (cc-rec rBk+1 bk-1 bk ck))) ;; recursive call to cc-rec that return a list
      
      (append carries-out-list (list ck))))))
					; the extra carry have to be reported with an append call


;; compute-carries 
;;
;; initiate the call to cc-rec
;;
;; B is the list of bits, starting with HSB to LSB
;;
;; (compute-carries (binList2TrueFalseList (number2binlist #b110011))) -> '(#t #t #f #f #t #t #t #t)
;;
;;> (compute-carries (binList2TrueFalseList '(1))) -> '(#t #t #t)
;;  111
;;  010
;;   01
;;
;; (compute-carries (binList2TrueFalseList '(1 0 1))) -> '(#t #t #t #t #t)
;; details:
;; 11111
;;  1010
;;   101
;;
;; (compute-carries (binList2TrueFalseList '(1 0))) ->  b0 not equal to 1 (#t), b0: #f 
;; (compute-carries (binList2TrueFalseList '(1 0 0 1))) -> '(#f #f #t #t #t)
;; verification:
;; 00111
;; 10010
;;  1001
;;
;; > (compute-carries (binList2TrueFalseList '(1 1)))
;; calling cc-rec
;; ...rBk :()
;; ...bk-2 :#t
;; ...bk-1 :#t
;; ...ck-1 :#t
;; reverseB2 :()
;; b0 :#t
;; b1 :#t
;; c1 :#t
;; C-c2 :(#t)
;; '(#t #t #t #t)
;;
;;   1111
;;    110
;;     11
;;
;; (compute-carries (binList2TrueFalseList '(1 1 0 1)))
;; calling cc-rec
;; ...rBk :(#t #t)
;; ...bk-2 :#t
;; ...bk-1 :#f
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#t)
;; ...bk-2 :#f
;; ...bk-1 :#t
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :()
;; ...bk-2 :#t
;; ...bk-1 :#t
;; ...ck-1 :#t
;; reverseB2 :(#t #t)
;; b0 :#t
;; b1 :#f
;; c1 :#t
;; C-c2 :(#t #t #t)
;; '(#t #t #t #t #t #t)
;;
;; checking:
;;
;;  111111
;;   11010
;;    1101
;;
;; Good !!!
;;
;; another test:
;;
;; > (compute-carries (binList2TrueFalseList '(1 0 1 0 1)))
;; calling cc-rec
;; ...rBk :(#t #f #t)
;; ...bk-2 :#t
;; ...bk-1 :#f
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#f #t)
;; ...bk-2 :#f
;; ...bk-1 :#t
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#t)
;; ...bk-2 :#t
;; ...bk-1 :#f
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :()
;; ...bk-2 :#f
;; ...bk-1 :#t
;; ...ck-1 :#t
;; reverseB2 :(#t #f #t)
;; b0 :#t
;; b1 :#f
;; c1 :#t
;; C-c2 :(#t #t #t #t)
;; '(#t #t #t #t #t #t #t)
;;
;; checking:
;;
;;    1111111
;;     101010
;;      10101
;;
;; > (compute-carries (binList2TrueFalseList '(1 1 0 1 1)))
;; calling cc-rec
;; ...rBk :(#f #t #t)
;; ...bk-2 :#t
;; ...bk-1 :#t
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#t #t)
;; ...bk-2 :#t
;; ...bk-1 :#f
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#t)
;; ...bk-2 :#f
;; ...bk-1 :#t
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :()
;; ...bk-2 :#t
;; ...bk-1 :#t
;; ...ck-1 :#t
;; reverseB2 :(#f #t #t)
;; b0 :#t
;; b1 :#t
;; c1 :#t
;; C-c2 :(#t #t #t #t)
;; '(#t #t #t #t #t #t #t)
;;
;; checking now:
;;
;;       1111111
;;        110110
;;         11011
;;
;; > (compute-carries (binList2TrueFalseList '(1 1 0 0 1 1)))
;; calling cc-rec
;; ...rBk :(#f #f #t #t)
;; ...bk-2 :#t
;; ...bk-1 :#t
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#f #t #t)
;; ...bk-2 :#t
;; ...bk-1 :#f
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#t #t)
;; ...bk-2 :#f
;; ...bk-1 :#f
;; ...ck-1 :#t
;; calling cc-rec
;; ...rBk :(#t)
;; ...bk-2 :#f
;; ...bk-1 :#t
;; ...ck-1 :#f
;; calling cc-rec
;; ...rBk :()
;; ...bk-2 :#t
;; ...bk-1 :#t
;; ...ck-1 :#f
;; reverseB2 :(#f #f #t #t)
;; b0 :#t
;; b1 :#t
;; c1 :#t
;; C-c2 :(#t #f #f #t #t)
;; '(#t #t #f #f #t #t #t #t)
;;
;; checking:
;;
;;    11001111
;;     1100110
;;      110011
;;
;; 11001111 ~ '(#t #t #f #f #t #t #t #t)
;;
;; very good ! 
;;
;;> (compute-carries (binList2TrueFalseList '(1 1 0 0 1 1)) #t) -> '(#t #t #f #f #t #t #t #t)
;;
;; > (compute-carries (binList2TrueFalseList '(1)) #t) -> '(#t #t #t)
;; > (compute-carries (binList2TrueFalseList '(0)) #t) -> . . b0 not equal to 1 (#t)
;; > (compute-carries (binList2TrueFalseList '(1)) #f) -> '(#f #f #f)
(define (compute-carries B c0)  ;; B = (....,b2,b1,b0)

  (cond ((equal? B '(#t)) ;; (null? (cdr B)) ;; if B ~ '(1)

	 (if c0 ; carry set
	     ;;    111 C  start with c0 = 1
	     ;;     10 2B
	     ;;      1 B
	     '(#t #t #t)
	     ;;    000 C  start with c0 = 0
	     ;;     10 2B
	     ;;      1 B
	     '(#f #f #f)))
	  
	 
	 ((equal? B '(#f)) ;; if B ~ '(0)
	  (error "b0 not equal to 1 (#t)"))

	 (else 
	  (let* ((reverseB (reverse B)) ;; reverse the list, so we can access the lower bits easily : reverseB = (b0,b1,b2,.....)
		 (b0 (car reverseB)) ;; get the first element that is b0
		 (b1 (cadr reverseB)) ;; get the second element that is b1
		 (c1 c0) ;; c1 = c0, see computation above ('then' code)
		 (reverseB2 (cddr reverseB)) ;; get the rest of the list : (b2,.....)
		 ;; the reverse Bk list is the bits of B from the k-th bit to the last bit (HSB)
		 
		 ;; then call the recursive computation
		 ;; compute the list (Cn Cn-1 Cn-2 ... C2) 
		 (C-c2 (cc-rec reverseB2 b0 b1 c1)) ;; (c1 = c0) (cc-rec rBk bk-2 bk-1 ck-1)
		 (tailResult (append C-c2 `(,c1 ,c0))))  ;; (c1 c0) = (1 1) ~ (#t #t) or (c1 c0) = (0 0) ~ (#f #f)
	    
	    ;;       ....Cn..CkCk-1..1c = C : Carries , 000...00c at start of computation, C : c=C0, C1=C0
	    ;;         Bn......Bk-2..10 = A = 2 * B 
	    ;;     +     Bn....Bk-1...1 = B
	    ;;     --------------------
	    ;;       ....Sn..Sk.......s = S
	    
	    (if-t debug-mode
		  (display "reverseB2 :") (display reverseB2) (newline)
		  (display "b0 :") (display b0) (newline)
		  (display "b1 :") (display b1) (newline)
		  (display "c0 :") (display c0) (newline)
		  (display "c1 :") (display c1) (newline)
		  (display "C-c2 :") (display C-c2) (newline))
	    
	    (if (car tailResult) ; having a carry set onto the HSB will cause an extra carry
		(cons #t tailResult) ; the extra carry is set
		tailResult)))))



;; get a (binary) number and compute the carries (boolean list result)
;; (compute-carries-bin #b1001) -> '(#f #f #t #t #t)
(define (compute-carries-bin B)
  (compute-carries (binList2TrueFalseList (number2binlist B)) #t))


;; get a (binary) number and compute the carries, result is a number
;; (compute-carries-bin-numeric #b1001) -> 7
(define (compute-carries-bin-numeric B)
  (binlist2number (trueFalseList2binList (compute-carries-bin B))))


;; get a (binary) number and compute the carries (boolean list result)
;; (compute-carries-bin-3b #b1001) -> '(#f #f #f #f #f)
(define (compute-carries-bin-3b B)
  (compute-carries (binList2TrueFalseList (number2binlist B)) #f))


;; get a (binary) number and compute the carries, result is a number
;; (compute-carries-bin-numeric-3b #b1001) -> 0
(define (compute-carries-bin-numeric-3b B)
  (binlist2number (trueFalseList2binList (compute-carries-bin-3b B))))


;; compute Collatz until it reach 1
(define (collatz n)
  (display (padding-spc n))
  (newline)
  ;;(printf "~B\n" n)
  (cond ((eq? n 1) 1)
	((zero? (modulo n 2)) (collatz (quotient n 2)))
	(else (collatz (+ (* 3 n) 1)))))


(define (collatz-stat n)
  (define sum01 (scan-and-stat-bits-no-HSB n))
  (define sum0 (car sum01))
  (define sum1 (cdr sum01))
  
  (display (padding-spc n))
  (display "   (#0 . #1) = ")
  (display sum01)
  (display "   ratio 1 over 0 = ")
  (if (zero? sum0)
      (display "infinity")
      (display (exact->inexact (/ sum1 sum0))))
  (newline)

  (cond ((eq? n 1) 1)
	((zero? (modulo n 2)) (collatz-stat (quotient n 2)))
	(else (collatz-stat (+ (* 3 n) 1)))))


;; compute Collatz compressed until it reach 1
(define (collatz-compressed n)
  (display (padding-spc n))
  (newline)
  (cond ((eq? n 1) 1)
	((zero? (modulo n 2)) (collatz-compressed (quotient n 2)))
	(else (collatz-compressed (/ (+ (* 3 n) 1) 2)))))


;; compute Collatz compressed until it reach 1 and compute statistics over 0 and 1
(define (collatz-compressed-stat n)
  (define sum01 (scan-and-stat-bits-no-HSB n))
  (define sum0 (car sum01))
  (define sum1 (cdr sum01))
  
  (display (padding-spc n))
  (display "   (#0 . #1) = ")
  (display sum01)
  (display "   ratio 1 over 0 = ")
  (if (zero? sum0)
      (display "infinity")
      (display (exact->inexact (/ sum1 sum0))))
  (newline)
  (cond ((eq? n 1) 1)
	((zero? (modulo n 2)) (collatz-compressed-stat (quotient n 2)))
	(else (collatz-compressed-stat (/ (+ (* 3 n) 1) 2)))))


	
      
 
;; compute Collatz until it reach 1
(define (collatz-verbose n)

  (let ((orig (size-bit n))
	(display-enabled #t))

    (letrec ((collatz-verbose-rec
	      (lambda (n)
			    
		(cond ((eq? n 1) 1) ;; end of game
		      
		      ((zero? (modulo n 2)) ;; even number
		       (when (> orig 0)
			     (set! orig (- orig 1)))
		       (collatz-verbose-rec (quotient n 2)))
		      
		      (else ; only for odd numbers
		       (let* ((b n)
			      (2b (shift-left b))    ; 2*b
			      (a (bitwise-ior 1 2b)) ; a = 2b + 1
			      (S (+ a b)) ; compute a + b = 2b + 1 + b = 3b + 1
			      (S_3b (+  2b b))  ; compute 3b
			      (C (compute-carries-bin-numeric b)) ; compute Carries
			      (C_3b (compute-carries-bin-numeric-3b b))
			      )
			 
			 ;; displaying
			 (if-t display-enabled
			       (display "              3b+1                   ")
			       (display "             ")
			       (display "     3b")
			       (newline)
			       
			       (display (binary-string->carries (padding-spc C)))
			       (display "             ")
			       (display (binary-string->carries (padding-spc C_3b)))
			       (display-nl "     Carries") ;; carries
			       
			       ;;(dv orig)
			       (if (or (equal? (current-command-line-arguments) '#())
				       (= orig 0))
				   
				   (then-block
				    (display (padding-spc 2b))
				    (display "             ")
				    (display (padding-spc 2b))
				    (display-nl "     2b") ;; 2b and 2b
				    (display (padding-spc b))
				    (display "             ")
				    (display (padding-spc b))
				    (display-nl "      b") ;; b
				    (display "------------------------")
				    
				    (display "       ") (display "       ")
				    (display "------------------------") (newline) 
				    (display (padding-spc S))
				    (display "             ")
				    (display (padding-spc S_3b))
				    (display-nl "     Sum") ;; 3b+1 and 3b		       
				    (newline))
				   
				   (else-block
				    (display (dtirfe (padding-spc 2b) (+ orig 1)))
				    (display "             ")
				    (display (dtirfe (padding-spc 2b) (+ orig 1)))
				    (display-nl "     2b") ;; 2b and 2b
				    (display (dtirfe (padding-spc b) orig))
				    (display "             ")
				    (display (dtirfe (padding-spc b) orig))
				    (display-nl "      b") ;; b
				    (display "------------------------")
				    
				    (display "       ") (display "       ")
				    (display "------------------------") (newline) 
				    (display (dtirfe (padding-spc S) orig))
				    (display "             ")
				    (display (dtirfe (padding-spc S_3b) orig))
				    (display-nl "     Sum") ;; 3b+1 and 3b		       
				    (newline))) ;; end if
			       
			       ) ;; end if display enabled
			 
			 (collatz-verbose-rec S)

			 ) ;; end let*
		       ) ;; end else du cond
		      ) ;; end cond
		) ;; end lambda
	      ) ;; end (last) var of letrec
	     ) ;; end list of var of letrec
      
      ;; body of letrec
      (collatz-verbose-rec n) ;; initial call of recursion

      ) ;; end of letrec
    ) ;; end body of let
  ) ;; end of definition


;; Collatz function
;; compute a single step
;;
;; (fc 0) -> 0
;; (fc 5) -> 16
;;
(define (fc n)
  (if (zero? (modulo n 2))
      (quotient n 2)
      (+ (* 3 n) 1)))

(define (f-3x-1 n)
  (if (zero? (modulo n 2))
      (quotient n 2)
      (- (* 3 n) 1)))



;; compressed collatz function
;; the same as collatz function but we divide (3x+1)/2 for odd numbers
;;
(define (fc-comp n)
  (if (zero? (modulo n 2))
      (quotient n 2)
      (quotient (+ (* 3 n) 1) 2)))

;; stop on 1
(define (fc-comp-stop n)
  (if (= 1 n)
      (begin
	(display-nl "STOP because 1")
	1)
      (if (zero? (modulo n 2))
	  (quotient n 2)
	  (quotient (+ (* 3 n) 1) 2))))

;; compute fc x times
(define (fc-x n x)
  (if (equal? x 1)
      (fc n)
      (fc-x (fc n) (- x 1))))

;; compute fc compressed x times
;; (fc-comp-x 7 5) -> 20
(define (fc-comp-x n x)
  ;; (display-nl "fc-comp-x")
  ;; (display-symb-nl n)
  ;; (display-symb-nl x)
  ;;(if (equal? x 1)
  (if (= x 1)
      (fc-comp n)
      (fc-comp-x
       (fc-comp n) (- x 1))))

(define (fc-comp-x-stop n x)
  ;; (display-nl "fc-comp-x")
  ;; (display-symb-nl n)
  ;; (display-symb-nl x)
  ;;(if (equal? x 1)
  (if (= x 1)
      (fc-comp-stop n)
      (fc-comp-x-stop
       (fc-comp-stop n) (- x 1))))




(define (fc-comp-size-times n)
  ;; (display-nl "fc-comp-size-times")
  ;; (display-symb-nl n) 
  (fc-comp-x-stop #;fc-comp-x
   n
   (size-bit n)))

(define (fc-comp-half-size-times n)

  (let ((x (quotient
	    (size-bit n) 2)))
    
    ;;(display-nl "fc-comp-half-size-times")
    ;;(display-symb-nl n)
    ;;(display-symb-nl x)
    (if (= x 0) ;; was to correct a bug, no more useful now
	(fc-comp-x n 1)
	(fc-comp-x n x))))

(define (fc-comp-half-size-times--1 n)
  ;; (display-nl "fc-comp-half-size-times")
  ;; (display-symb-nl n) 
  (fc-comp-x
   n
   (quotient
    (- (size-bit n) 1)
    2)))


(define (fc-comp-size-times--1 n)
  ;; (display-nl "fc-comp-half-size-times")
  ;; (display-symb-nl n)
  (define sbn (size-bit n))
  (when debug-mode
	(display " size:")
	(display sbn)
	(display " "))
  (fc-comp-x
   n
   (- sbn 1)))

(define (fc-comp-size-times--2 n)
  ;; (display-nl "fc-comp-half-size-times")
  ;; (display-symb-nl n)
  (define sbn (size-bit n))
  (when debug-mode
	(display " size:")
	(display sbn)
	(display " "))
  (fc-comp-x
   n
   (- sbn 2)))


;; (compute-fc-size-times-over-numbers 16)
;;                       10
;;                       10
;;                     1000
;;                       10
;;                       10
;;                     1000
;;                    11010
;;                       10
;;                    10001
;;                       10
;;                    10100
;;                     1000
;;                     1000
;;                    11010
;;                  1010000
;;                       10
(define (compute-fc-size-times-over-numbers n-end)
  (for (n 1 n-end)
       (let ((res (fc-comp-size-times n)))
	 (display (padding-spc res))
	 (newline))))


;; LSB0 et LSB1: 8 one and 8 zero
;; > (compute-fc-size-times--1-over-numbers-gap 16 31)
;; 16 -- compressed Collatz size of n-1 times ->                      100
;; 17 -- compressed Collatz size of n-1 times ->                     1101
;; 18 -- compressed Collatz size of n-1 times ->                     1110
;; 19 -- compressed Collatz size of n-1 times ->                   101100
;; 20 -- compressed Collatz size of n-1 times ->                      101
;; 21 -- compressed Collatz size of n-1 times ->                    10000
;; 22 -- compressed Collatz size of n-1 times ->                    10001
;; 23 -- compressed Collatz size of n-1 times ->                   110101
;; 24 -- compressed Collatz size of n-1 times ->                      110
;; 25 -- compressed Collatz size of n-1 times ->                    10011
;; 26 -- compressed Collatz size of n-1 times ->                    10100
;; 27 -- compressed Collatz size of n-1 times ->                   111110
;; 28 -- compressed Collatz size of n-1 times ->                      111
;; 29 -- compressed Collatz size of n-1 times ->                    10110
;; 30 -- compressed Collatz size of n-1 times ->                    10111
;; 31 -- compressed Collatz size of n-1 times ->                  1000111
;; > (compute-fc-size-times--1-over-numbers-gap 3 16)
;; . . user break
;; > (compute-fc-size-times--1-over-numbers-gap 4 16)
;; 4 -- compressed Collatz size of n-1 times ->                       10
;; 5 -- compressed Collatz size of n-1 times ->                     1000
;; 6 -- compressed Collatz size of n-1 times ->                       11
;; 7 -- compressed Collatz size of n-1 times ->                     1011
;; 8 -- compressed Collatz size of n-1 times ->                      100
;; 9 -- compressed Collatz size of n-1 times ->                     1110
;; 10 -- compressed Collatz size of n-1 times ->                      101
;; 11 -- compressed Collatz size of n-1 times ->                    10001
;; 12 -- compressed Collatz size of n-1 times ->                      110
;; 13 -- compressed Collatz size of n-1 times ->                    10100
;; 14 -- compressed Collatz size of n-1 times ->                      111
;; 15 -- compressed Collatz size of n-1 times ->                    10111
;; 16 -- compressed Collatz size of n-1 times ->                      100
;; >
;; 7 zero 6 one


;; (compute-fc-size-times--1-over-numbers-gap 16 31)
;; 16 -- compressed Collatz size of n-1 times ->                        1
;; 17 -- compressed Collatz size of n-1 times ->                     1010
;; 18 -- compressed Collatz size of n-1 times ->                     1011
;; 19 -- compressed Collatz size of n-1 times ->                     1011
;; 20 -- compressed Collatz size of n-1 times ->                      100
;; 21 -- compressed Collatz size of n-1 times ->                      100
;; 22 -- compressed Collatz size of n-1 times ->                     1101
;; 23 -- compressed Collatz size of n-1 times ->                   101000
;; 24 -- compressed Collatz size of n-1 times ->                      101
;; 25 -- compressed Collatz size of n-1 times ->                   101100
;; 26 -- compressed Collatz size of n-1 times ->                      101
;; 27 -- compressed Collatz size of n-1 times ->                   101111
;; 28 -- compressed Collatz size of n-1 times ->                    10001
;; 29 -- compressed Collatz size of n-1 times ->                    10001
;; 30 -- compressed Collatz size of n-1 times ->                   110101
;; 31 -- compressed Collatz size of n-1 times ->                 10100001

;; 65 one and 66 zero without counting HSB one
;; > (compute-fc-size-times--1-over-numbers-gap 32 63)
;; 32 -- compressed Collatz size of n-1 times ->                        1
;; 33 -- compressed Collatz size of n-1 times ->                    11101
;; 34 -- compressed Collatz size of n-1 times ->                     1010
;; 35 -- compressed Collatz size of n-1 times ->                     1010
;; 36 -- compressed Collatz size of n-1 times ->                     1011
;; 37 -- compressed Collatz size of n-1 times ->                     1011
;; 38 -- compressed Collatz size of n-1 times ->                     1011
;; 39 -- compressed Collatz size of n-1 times ->                  1100101
;; 40 -- compressed Collatz size of n-1 times ->                      100
;; 41 -- compressed Collatz size of n-1 times ->                  1101011
;; 42 -- compressed Collatz size of n-1 times ->                      100
;; 43 -- compressed Collatz size of n-1 times ->                   100101
;; 44 -- compressed Collatz size of n-1 times ->                     1101
;; 45 -- compressed Collatz size of n-1 times ->                     1101
;; 46 -- compressed Collatz size of n-1 times ->                   101000
;; 47 -- compressed Collatz size of n-1 times ->                  1111001
;; 48 -- compressed Collatz size of n-1 times ->                      101
;; 49 -- compressed Collatz size of n-1 times ->                     1110
;; 50 -- compressed Collatz size of n-1 times ->                   101100
;; 51 -- compressed Collatz size of n-1 times ->                   101100
;; 52 -- compressed Collatz size of n-1 times ->                      101
;; 53 -- compressed Collatz size of n-1 times ->                      101
;; 54 -- compressed Collatz size of n-1 times ->                   101111
;; 55 -- compressed Collatz size of n-1 times ->                   101111
;; 56 -- compressed Collatz size of n-1 times ->                    10001
;; 57 -- compressed Collatz size of n-1 times ->                   110001
;; 58 -- compressed Collatz size of n-1 times ->                    10001
;; 59 -- compressed Collatz size of n-1 times ->                 10011000
;; 60 -- compressed Collatz size of n-1 times ->                   110101
;; 61 -- compressed Collatz size of n-1 times ->                   110101
;; 62 -- compressed Collatz size of n-1 times ->                 10100001
;; 63 -- compressed Collatz size of n-1 times ->                111100101
;; >

;;  (compute-fc-comp-size-times--1-over-numbers-gap 16 31)
;;  size:5.0 16 :                    10000 -- compressed Collatz size of n-1 times ->                        1
;;  size:5.0 17 :                    10001 -- compressed Collatz size of n-1 times ->                     1010
;;  size:5.0 18 :                    10010 -- compressed Collatz size of n-1 times ->                     1011
;;  size:5.0 19 :                    10011 -- compressed Collatz size of n-1 times ->                     1011
;;  size:5.0 20 :                    10100 -- compressed Collatz size of n-1 times ->                      100
;;  size:5.0 21 :                    10101 -- compressed Collatz size of n-1 times ->                      100
;;  size:5.0 22 :                    10110 -- compressed Collatz size of n-1 times ->                     1101
;;  size:5.0 23 :                    10111 -- compressed Collatz size of n-1 times ->                   101000
;;  size:5.0 24 :                    11000 -- compressed Collatz size of n-1 times ->                      101
;;  size:5.0 25 :                    11001 -- compressed Collatz size of n-1 times ->                   101100
;;  size:5.0 26 :                    11010 -- compressed Collatz size of n-1 times ->                      101
;;  size:5.0 27 :                    11011 -- compressed Collatz size of n-1 times ->                   101111
;;  size:5.0 28 :                    11100 -- compressed Collatz size of n-1 times ->                    10001
;;  size:5.0 29 :                    11101 -- compressed Collatz size of n-1 times ->                    10001
;;  size:5.0 30 :                    11110 -- compressed Collatz size of n-1 times ->                   110101
;;  size:5.0 31 :                    11111 -- compressed Collatz size of n-1 times ->                 10100001
;; '(1 10 11 11 4 4 13 40 5 44 5 47 17 17 53 161)


;; > (compute-fc-comp-size-times--1-over-numbers-gap 32 63
;;                                                   )
;;  size:6.0 32 :                   100000 -- compressed Collatz size of n-1 times ->                        1
;;  size:6.0 33 :                   100001 -- compressed Collatz size of n-1 times ->                    11101
;;  size:6.0 34 :                   100010 -- compressed Collatz size of n-1 times ->                     1010
;;  size:6.0 35 :                   100011 -- compressed Collatz size of n-1 times ->                     1010
;;  size:6.0 36 :                   100100 -- compressed Collatz size of n-1 times ->                     1011
;;  size:6.0 37 :                   100101 -- compressed Collatz size of n-1 times ->                     1011
;;  size:6.0 38 :                   100110 -- compressed Collatz size of n-1 times ->                     1011
;;  size:6.0 39 :                   100111 -- compressed Collatz size of n-1 times ->                  1100101
;;  size:6.0 40 :                   101000 -- compressed Collatz size of n-1 times ->                      100
;;  size:6.0 41 :                   101001 -- compressed Collatz size of n-1 times ->                  1101011
;;  size:6.0 42 :                   101010 -- compressed Collatz size of n-1 times ->                      100
;;  size:6.0 43 :                   101011 -- compressed Collatz size of n-1 times ->                   100101
;;  size:6.0 44 :                   101100 -- compressed Collatz size of n-1 times ->                     1101
;;  size:6.0 45 :                   101101 -- compressed Collatz size of n-1 times ->                     1101
;;  size:6.0 46 :                   101110 -- compressed Collatz size of n-1 times ->                   101000
;;  size:6.0 47 :                   101111 -- compressed Collatz size of n-1 times ->                  1111001
;;  size:6.0 48 :                   110000 -- compressed Collatz size of n-1 times ->                      101
;;  size:6.0 49 :                   110001 -- compressed Collatz size of n-1 times ->                     1110
;;  size:6.0 50 :                   110010 -- compressed Collatz size of n-1 times ->                   101100
;;  size:6.0 51 :                   110011 -- compressed Collatz size of n-1 times ->                   101100
;;  size:6.0 52 :                   110100 -- compressed Collatz size of n-1 times ->                      101
;;  size:6.0 53 :                   110101 -- compressed Collatz size of n-1 times ->                      101
;;  size:6.0 54 :                   110110 -- compressed Collatz size of n-1 times ->                   101111
;;  size:6.0 55 :                   110111 -- compressed Collatz size of n-1 times ->                   101111
;;  size:6.0 56 :                   111000 -- compressed Collatz size of n-1 times ->                    10001
;;  size:6.0 57 :                   111001 -- compressed Collatz size of n-1 times ->                   110001
;;  size:6.0 58 :                   111010 -- compressed Collatz size of n-1 times ->                    10001
;;  size:6.0 59 :                   111011 -- compressed Collatz size of n-1 times ->                 10011000
;;  size:6.0 60 :                   111100 -- compressed Collatz size of n-1 times ->                   110101
;;  size:6.0 61 :                   111101 -- compressed Collatz size of n-1 times ->                   110101
;;  size:6.0 62 :                   111110 -- compressed Collatz size of n-1 times ->                 10100001
;;  size:6.0 63 :                   111111 -- compressed Collatz size of n-1 times ->                111100101
;; '(1 29 10 10 11 11 11 101 4 107 4 37 13 13 40 121 5 14 44 44 5 5 47 47 17 49 17 152 53 53 161 485)
;; >
;; 22 ones and 9 zeros for 31 numbers at LSB
;; 8 ones and 22 zeros for 30 numbers at LSB+1
;; 10 ones and 8 zeros for 18 numbers at LSB+2

;; > (stat '(1 29 10 10 11 11 11 101 4 107 4 37 13 13 40 121 5 14 44 44 5 5 47 47 17 49 17 152 53 53 161 485))
;; mx = 9
;; v-zero-sum = #(9 22 13 9 10 1 2 0 0)
;; v-one-sum = #(22 9 13 9 5 5 1 1 0)
;; P-one = #(0.7096774193548387 0.2903225806451613 0.5 0.5 0.3333333333333333 0.8333333333333334 0.3333333333333333 1.0 0.5)
;; '(#(9 22 13 9 10 1 2 0 0) . #(22 9 13 9 5 5 1 1 0))
(define (compute-fc-comp-size-times--1-over-numbers-gap n-start n-end)
  (define L '())
  (for (n n-start n-end)
       (let ((res (fc-comp-size-times--1 n)))
	 (insert-set! res L)
	 (when debug-mode
	       (display n)
	       (display " : ")
	       (display (padding-spc n))
	       (display " -- compressed Collatz size of n-1 times -> ")
	       (display (padding-spc res))
	       (newline))
	 ))
  (reverse L))


(define (compute-fc-comp-size-times--2-over-numbers-gap n-start n-end)
  (define L '())
  (for (n n-start n-end)
       (let ((res (fc-comp-size-times--2 n)))
	 (insert-set! res L)
	 (when debug-mode
	       (display n)
	       (display " : ")
	       (display (padding-spc n))
	       (display " -- compressed Collatz size of n-2 times -> ")
	       (display (padding-spc res))
	       (newline))
	 ))
  (reverse L))

;; > (compute-fc-comp-size--2-times-stop-over-numbers-gap 16 31)
;; 16 =                    10000 -- compressed Collatz 3.0 X ->                       10
;; 17 =                    10001 -- compressed Collatz 3.0 X ->                    10100
;; 18 =                    10010 -- compressed Collatz 3.0 X ->                      111
;; 19 =                    10011 -- compressed Collatz 3.0 X ->                    10110
;; 20 =                    10100 -- compressed Collatz 3.0 X ->                     1000
;; 21 =                    10101 -- compressed Collatz 3.0 X ->                     1000
;; 22 =                    10110 -- compressed Collatz 3.0 X ->                    11010
;; 23 =                    10111 -- compressed Collatz 3.0 X ->                  1010000
;; 24 =                    11000 -- compressed Collatz 3.0 X ->                       11
;; 25 =                    11001 -- compressed Collatz 3.0 X ->                    11101
;; 26 =                    11010 -- compressed Collatz 3.0 X ->                     1010
;; 27 =                    11011 -- compressed Collatz 3.0 X ->                    11111
;; 28 =                    11100 -- compressed Collatz 3.0 X ->                     1011
;; 29 =                    11101 -- compressed Collatz 3.0 X ->                     1011
;; 30 =                    11110 -- compressed Collatz 3.0 X ->                   100011
;; 31 =                    11111 -- compressed Collatz 3.0 X ->                  1101011
;; lsb1 = 8
;; lsb0 = 8
(define (compute-fc-comp-size--2-times-stop-over-numbers-gap n-start n-end)
  (define lsb1 0)
  (define lsb0 0)
  (for (n n-start n-end)
       (let* ((x (- (size-bit n) 2))
	      (res (fc-comp-x-stop n x)))
	 (if (bit-test? res 0)
	     (incf lsb1)
	     (incf lsb0))
	 (display n) (display " = ")
	 (display (padding-spc n))
	 (display " -- compressed Collatz ") (display x) (display " X -> ")
	 (display (padding-spc res))
	 (newline)))
  (display-symb-nl lsb1)
  (display-symb-nl lsb0))


;; > (stat-01 32 63)
;; 32 -- compressed Collatz size of n-1 times ->                        1
;; 33 -- compressed Collatz size of n-1 times ->                    11101
;; 34 -- compressed Collatz size of n-1 times ->                     1010
;; 35 -- compressed Collatz size of n-1 times ->                     1010
;; 36 -- compressed Collatz size of n-1 times ->                     1011
;; 37 -- compressed Collatz size of n-1 times ->                     1011
;; 38 -- compressed Collatz size of n-1 times ->                     1011
;; 39 -- compressed Collatz size of n-1 times ->                  1100101
;; 40 -- compressed Collatz size of n-1 times ->                      100
;; 41 -- compressed Collatz size of n-1 times ->                  1101011
;; 42 -- compressed Collatz size of n-1 times ->                      100
;; 43 -- compressed Collatz size of n-1 times ->                   100101
;; 44 -- compressed Collatz size of n-1 times ->                     1101
;; 45 -- compressed Collatz size of n-1 times ->                     1101
;; 46 -- compressed Collatz size of n-1 times ->                   101000
;; 47 -- compressed Collatz size of n-1 times ->                  1111001
;; 48 -- compressed Collatz size of n-1 times ->                      101
;; 49 -- compressed Collatz size of n-1 times ->                     1110
;; 50 -- compressed Collatz size of n-1 times ->                   101100
;; 51 -- compressed Collatz size of n-1 times ->                   101100
;; 52 -- compressed Collatz size of n-1 times ->                      101
;; 53 -- compressed Collatz size of n-1 times ->                      101
;; 54 -- compressed Collatz size of n-1 times ->                   101111
;; 55 -- compressed Collatz size of n-1 times ->                   101111
;; 56 -- compressed Collatz size of n-1 times ->                    10001
;; 57 -- compressed Collatz size of n-1 times ->                   110001
;; 58 -- compressed Collatz size of n-1 times ->                    10001
;; 59 -- compressed Collatz size of n-1 times ->                 10011000
;; 60 -- compressed Collatz size of n-1 times ->                   110101
;; 61 -- compressed Collatz size of n-1 times ->                   110101
;; 62 -- compressed Collatz size of n-1 times ->                 10100001
;; 63 -- compressed Collatz size of n-1 times ->                111100101
;; mx = 9
;; v-zero-sum = #(9 22 13 9 10 1 2 0 0)
;; v-one-sum = #(22 9 13 9 5 5 1 1 0)
;; P-one = #(0.7096774193548387 0.2903225806451613 0.5 0.5 0.3333333333333333 0.8333333333333334 0.3333333333333333 1.0 0.5)
;; n-zero = 66
;; n-one = 65
;; '(66 . 65)


;; > (stat-01 128 255)
;; mx = 13
;; v-zero-sum = #(47 72 56 46 54 34 28 12 1 5 1 1 0)
;; v-one-sum = #(80 55 64 59 39 30 14 17 7 0 0 0 0)
;; P-one = #(0.6299212598425197 0.4330708661417323 0.5333333333333333 0.5619047619047619 0.41935483870967744 0.46875 0.3333333333333333 0.5862068965517241 0.875 0.0 0.0 0.0 0.5)
;; n-zero = 357
;; n-one = 365
;; '(357 . 365)
;; > (stat-01 16 31)
;; mx = 8
;; v-zero-sum = #(5 11 7 4 4 0 1 0)
;; v-one-sum = #(10 4 4 3 1 1 0 0)
;; P-one = #(0.6666666666666666 0.26666666666666666 0.36363636363636365 0.42857142857142855 0.2 1.0 0.0 0.5)
;; n-zero = 32
;; n-one = 23
;; '(32 . 23)
;; > (stat-01 32 63)
;; mx = 9
;; v-zero-sum = #(9 22 13 9 10 1 2 0 0)
;; v-one-sum = #(22 9 13 9 5 5 1 1 0)
;; P-one = #(0.7096774193548387 0.2903225806451613 0.5 0.5 0.3333333333333333 0.8333333333333334 0.3333333333333333 1.0 0.5)
;; n-zero = 66
;; n-one = 65
;; '(66 . 65)
;; > (stat-01 32 255)
;; mx = 13
;; v-zero-sum = #(79 136 99 75 88 45 41 14 1 6 1 1 0)
;; v-one-sum = #(142 85 104 94 59 47 18 23 8 0 0 0 0)
;; P-one = #(0.6425339366515838 0.38461538461538464 0.5123152709359606 0.5562130177514792 0.4013605442176871 0.5108695652173914 0.3050847457627119 0.6216216216216216 0.8888888888888888 0.0 0.0 0.0 0.5)
;; n-zero = 586
;; n-one = 580
;; '(586 . 580)
;; > (stat-01 32 1023)
;; mx = 16
;; v-zero-sum = #(431 533 463 416 437 333 291 196 65 111 32 19 7 1 1 0)
;; v-one-sum = #(556 454 489 458 369 304 195 181 111 23 24 2 4 0 0 0)
;; P-one = #(0.563323201621074 0.45997973657548125 0.5136554621848739 0.5240274599542334 0.45781637717121587 0.4772370486656201 0.4012345679012346 0.48010610079575594 0.6306818181818182 0.17164179104477612 0.42857142857142855 0.09523809523809523 0.36363636363636365 0.0 0.0 0.5)
;; n-zero = 3336
;; n-one = 3170
;; '(3336 . 3170)
;; > (stat-01 32 1024)
;; mx = 16
;; v-zero-sum = #(431 533 463 416 437 333 291 196 65 111 32 19 7 1 1 0)
;; v-one-sum = #(556 454 489 458 369 304 195 181 111 23 24 2 4 0 0 0)
;; P-one = #(0.563323201621074 0.45997973657548125 0.5136554621848739 0.5240274599542334 0.45781637717121587 0.4772370486656201 0.4012345679012346 0.48010610079575594 0.6306818181818182 0.17164179104477612 0.42857142857142855 0.09523809523809523 0.36363636363636365 0.0 0.0 0.5)
;; n-zero = 3336
;; n-one = 3170
;; '(3336 . 3170)
;; > (stat-01 32 8192)
;; mx = 21
;; v-zero-sum = #(3968 4175 4008 3923 3873 3664 3477 2992 1881 2288 1284 728 623 217 160 50 6 11 1 1 0)
;; v-one-sum = #(4184 3977 4076 3967 3802 3434 2874 2723 2215 1148 1096 559 294 161 34 35 8 0 0 0 0)
;; P-one = #(0.5132482826300294 0.487855740922473 0.5042058386937159 0.502788339670469 0.49537459283387625 0.48379825302902224 0.45252716107699575 0.4764654418197725 0.540771484375 0.3341094295692666 0.46050420168067224 0.43434343434343436 0.32061068702290074 0.42592592592592593 0.17525773195876287 0.4117647058823529 0.5714285714285714 0.0 0.0 0.0 0.5)
;; n-zero = 37330
;; n-one = 34587
;; '(37330 . 34587)

;; > (stat-01 1 8191)
;; . . user break
;; > (stat-01 2 8191)
;; mx = 21
;; v-zero-sum = #(3975 4196 4018 3930 3877 3664 3478 2992 1881 2288 1284 728 623 217 160 50 6 11 1 1 0)
;; v-one-sum = #(4203 3982 4082 3970 3804 3435 2874 2723 2215 1148 1096 559 294 161 34 35 8 0 0 0 0)
;; P-one = #(0.5139398385913426 0.48691611640988014 0.5039506172839506 0.5025316455696203 0.4952480145814347 0.4838709677419355 0.452455919395466 0.4764654418197725 0.540771484375 0.3341094295692666 0.46050420168067224 0.43434343434343436 0.32061068702290074 0.42592592592592593 0.17525773195876287 0.4117647058823529 0.5714285714285714 0.0 0.0 0.0 0.5)
;; n-zero = 37380
;; n-one = 34623
;; '(37380 . 34623)

;;(stat-01 128 255)
;; ...
;; mx = 12
;; v-zero-sum = #(63 47 58 44 45 22 16 8 1 1 0 0)
;; v-one-sum = #(63 76 56 46 34 22 12 6 1 1 0 0)
;; P-one = #(0.5 0.6178861788617886 0.49122807017543857 0.5111111111111111 0.43037974683544306 0.5 0.42857142857142855 0.42857142857142855 0.5 0.5 0.5 0.5)
;; n-zero = 305
;; n-one = 317
;; '(305 . 317)
(define (stat-01 n-start n-end)
  (define Lnum (compute-fc-comp-size-times--2-over-numbers-gap n-start n-end))
  (define Lstat (stat Lnum))
  ;;(define n-zero (foldl + 0 (vector->list (car Lstat))))
  (define n-zero (apply + (vector->list (car Lstat))))
  ;;(define n-one (foldl + 0 (vector->list (cdr Lstat))))
  (define  n-one (apply + (vector->list (cdr Lstat))))
  (dv n-zero)
  (dv n-one)
  (cons n-zero n-one))
 
;; autant de 1 que de 0 sur le LSB (low significant bit):
;; > (compute-fc-X-times-over-numbers-gap 3 16 31)
;; 16 -- compressed Collatz X times ->                       10
;; 17 -- compressed Collatz X times ->                    10100
;; 18 -- compressed Collatz X times ->                      111
;; 19 -- compressed Collatz X times ->                    10110
;; 20 -- compressed Collatz X times ->                     1000
;; 21 -- compressed Collatz X times ->                     1000
;; 22 -- compressed Collatz X times ->                    11010
;; 23 -- compressed Collatz X times ->                  1010000
;; 24 -- compressed Collatz X times ->                       11
;; 25 -- compressed Collatz X times ->                    11101
;; 26 -- compressed Collatz X times ->                     1010
;; 27 -- compressed Collatz X times ->                    11111
;; 28 -- compressed Collatz X times ->                     1011
;; 29 -- compressed Collatz X times ->                     1011
;; 30 -- compressed Collatz X times ->                   100011
;; 31 -- compressed Collatz X times ->                  1101011

;; > (compute-fc-X-times-over-numbers-gap 2 16 31)
;; 16 -- compressed Collatz X times ->                      100
;; 17 -- compressed Collatz X times ->                     1101
;; 18 -- compressed Collatz X times ->                     1110
;; 19 -- compressed Collatz X times ->                   101100
;; 20 -- compressed Collatz X times ->                      101
;; 21 -- compressed Collatz X times ->                    10000
;; 22 -- compressed Collatz X times ->                    10001
;; 23 -- compressed Collatz X times ->                   110101
;; 24 -- compressed Collatz X times ->                      110
;; 25 -- compressed Collatz X times ->                    10011
;; 26 -- compressed Collatz X times ->                    10100
;; 27 -- compressed Collatz X times ->                   111110
;; 28 -- compressed Collatz X times ->                      111
;; 29 -- compressed Collatz X times ->                    10110
;; 30 -- compressed Collatz X times ->                    10111
;; 31 -- compressed Collatz X times ->                  1000111
(define (compute-fc-X-times-over-numbers-gap x n-start n-end)
  (for (n n-start n-end)
       (let ((res (fc-comp-x n x)))
	 (display n) (display " = ")
	 (display (padding-spc n))
	 (display " -- compressed Collatz ") (display x) (display " X -> ")
	 (display (padding-spc res))
	 (newline))))

;; > (compute-fc-X-times-stop-over-numbers-gap 2 16 31)
;; 16 =                    10000 -- compressed Collatz 2 X ->                      100
;; 17 =                    10001 -- compressed Collatz 2 X ->                     1101
;; 18 =                    10010 -- compressed Collatz 2 X ->                     1110
;; 19 =                    10011 -- compressed Collatz 2 X ->                   101100
;; 20 =                    10100 -- compressed Collatz 2 X ->                      101
;; 21 =                    10101 -- compressed Collatz 2 X ->                    10000
;; 22 =                    10110 -- compressed Collatz 2 X ->                    10001
;; 23 =                    10111 -- compressed Collatz 2 X ->                   110101
;; 24 =                    11000 -- compressed Collatz 2 X ->                      110
;; 25 =                    11001 -- compressed Collatz 2 X ->                    10011
;; 26 =                    11010 -- compressed Collatz 2 X ->                    10100
;; 27 =                    11011 -- compressed Collatz 2 X ->                   111110
;; 28 =                    11100 -- compressed Collatz 2 X ->                      111
;; 29 =                    11101 -- compressed Collatz 2 X ->                    10110
;; 30 =                    11110 -- compressed Collatz 2 X ->                    10111
;; 31 =                    11111 -- compressed Collatz 2 X ->                  1000111
;; lsb1 = 8
;; lsb0 = 8
;; > (compute-fc-X-times-stop-over-numbers-gap 2 16 31)
;; 16 =                    10000 -- compressed Collatz 2 X ->                      100
;; 17 =                    10001 -- compressed Collatz 2 X ->                     1101
;; 18 =                    10010 -- compressed Collatz 2 X ->                     1110
;; 19 =                    10011 -- compressed Collatz 2 X ->                   101100
;; 20 =                    10100 -- compressed Collatz 2 X ->                      101
;; 21 =                    10101 -- compressed Collatz 2 X ->                    10000
;; 22 =                    10110 -- compressed Collatz 2 X ->                    10001
;; 23 =                    10111 -- compressed Collatz 2 X ->                   110101
;; 24 =                    11000 -- compressed Collatz 2 X ->                      110
;; 25 =                    11001 -- compressed Collatz 2 X ->                    10011
;; 26 =                    11010 -- compressed Collatz 2 X ->                    10100
;; 27 =                    11011 -- compressed Collatz 2 X ->                   111110
;; 28 =                    11100 -- compressed Collatz 2 X ->                      111
;; 29 =                    11101 -- compressed Collatz 2 X ->                    10110
;; 30 =                    11110 -- compressed Collatz 2 X ->                    10111
;; 31 =                    11111 -- compressed Collatz 2 X ->                  1000111
;; > (compute-fc-X-times-stop-over-numbers-gap 3 16 31)
;; 16 =                    10000 -- compressed Collatz 3 X ->                       10
;; 17 =                    10001 -- compressed Collatz 3 X ->                    10100
;; 18 =                    10010 -- compressed Collatz 3 X ->                      111
;; 19 =                    10011 -- compressed Collatz 3 X ->                    10110
;; 20 =                    10100 -- compressed Collatz 3 X ->                     1000
;; 21 =                    10101 -- compressed Collatz 3 X ->                     1000
;; 22 =                    10110 -- compressed Collatz 3 X ->                    11010
;; 23 =                    10111 -- compressed Collatz 3 X ->                  1010000
;; 24 =                    11000 -- compressed Collatz 3 X ->                       11
;; 25 =                    11001 -- compressed Collatz 3 X ->                    11101
;; 26 =                    11010 -- compressed Collatz 3 X ->                     1010
;; 27 =                    11011 -- compressed Collatz 3 X ->                    11111
;; 28 =                    11100 -- compressed Collatz 3 X ->                     1011
;; 29 =                    11101 -- compressed Collatz 3 X ->                     1011
;; 30 =                    11110 -- compressed Collatz 3 X ->                   100011
;; 31 =                    11111 -- compressed Collatz 3 X ->                  1101011
;; > (compute-fc-X-times-stop-over-numbers-gap 4 16 31)
;; 16 =                    10000 -- compressed Collatz 4 X ->                        1
;; 17 =                    10001 -- compressed Collatz 4 X ->                     1010
;; 18 =                    10010 -- compressed Collatz 4 X ->                     1011
;; 19 =                    10011 -- compressed Collatz 4 X ->                     1011
;; 20 =                    10100 -- compressed Collatz 4 X ->                      100
;; 21 =                    10101 -- compressed Collatz 4 X ->                      100
;; 22 =                    10110 -- compressed Collatz 4 X ->                     1101
;; 23 =                    10111 -- compressed Collatz 4 X ->                   101000
;; 24 =                    11000 -- compressed Collatz 4 X ->                      101
;; 25 =                    11001 -- compressed Collatz 4 X ->                   101100
;; 26 =                    11010 -- compressed Collatz 4 X ->                      101
;; 27 =                    11011 -- compressed Collatz 4 X ->                   101111
;; 28 =                    11100 -- compressed Collatz 4 X ->                    10001
;; 29 =                    11101 -- compressed Collatz 4 X ->                    10001
;; 30 =                    11110 -- compressed Collatz 4 X ->                   110101
;; 31 =                    11111 -- compressed Collatz 4 X ->                 10100001
;; > (compute-fc-X-times-stop-over-numbers-gap 5 16 31)
;; STOP because 1
;; 16 =                    10000 -- compressed Collatz 5 X ->                        1
;; 17 =                    10001 -- compressed Collatz 5 X ->                      101
;; 18 =                    10010 -- compressed Collatz 5 X ->                    10001
;; 19 =                    10011 -- compressed Collatz 5 X ->                    10001
;; 20 =                    10100 -- compressed Collatz 5 X ->                       10
;; 21 =                    10101 -- compressed Collatz 5 X ->                       10
;; 22 =                    10110 -- compressed Collatz 5 X ->                    10100
;; 23 =                    10111 -- compressed Collatz 5 X ->                    10100
;; 24 =                    11000 -- compressed Collatz 5 X ->                     1000
;; 25 =                    11001 -- compressed Collatz 5 X ->                    10110
;; 26 =                    11010 -- compressed Collatz 5 X ->                     1000
;; 27 =                    11011 -- compressed Collatz 5 X ->                  1000111
;; 28 =                    11100 -- compressed Collatz 5 X ->                    11010
;; 29 =                    11101 -- compressed Collatz 5 X ->                    11010
;; 30 =                    11110 -- compressed Collatz 5 X ->                  1010000
;; 31 =                    11111 -- compressed Collatz 5 X ->                 11110010
;; > (compute-fc-X-times-stop-over-numbers-gap 6 16 31)
;; STOP because 1
;; STOP because 1
;; 16 =                    10000 -- compressed Collatz 6 X ->                        1
;; 17 =                    10001 -- compressed Collatz 6 X ->                     1000
;; 18 =                    10010 -- compressed Collatz 6 X ->                    11010
;; 19 =                    10011 -- compressed Collatz 6 X ->                    11010
;; 20 =                    10100 -- compressed Collatz 6 X ->                        1
;; 21 =                    10101 -- compressed Collatz 6 X ->                        1
;; 22 =                    10110 -- compressed Collatz 6 X ->                     1010
;; 23 =                    10111 -- compressed Collatz 6 X ->                     1010
;; 24 =                    11000 -- compressed Collatz 6 X ->                      100
;; 25 =                    11001 -- compressed Collatz 6 X ->                     1011
;; 26 =                    11010 -- compressed Collatz 6 X ->                      100
;; 27 =                    11011 -- compressed Collatz 6 X ->                  1101011
;; 28 =                    11100 -- compressed Collatz 6 X ->                     1101
;; 29 =                    11101 -- compressed Collatz 6 X ->                     1101
;; 30 =                    11110 -- compressed Collatz 6 X ->                   101000
;; 31 =                    11111 -- compressed Collatz 6 X ->                  1111001
;; > (compute-fc-X-times-stop-over-numbers-gap 7 16 31)
;; STOP because 1
;; STOP because 1
;; STOP because 1
;; 16 =                    10000 -- compressed Collatz 7 X ->                        1
;; 17 =                    10001 -- compressed Collatz 7 X ->                      100
;; 18 =                    10010 -- compressed Collatz 7 X ->                     1101
;; 19 =                    10011 -- compressed Collatz 7 X ->                     1101
;; STOP because 1
;; 20 =                    10100 -- compressed Collatz 7 X ->                        1
;; STOP because 1
;; 21 =                    10101 -- compressed Collatz 7 X ->                        1
;; 22 =                    10110 -- compressed Collatz 7 X ->                      101
;; 23 =                    10111 -- compressed Collatz 7 X ->                      101
;; 24 =                    11000 -- compressed Collatz 7 X ->                       10
;; 25 =                    11001 -- compressed Collatz 7 X ->                    10001
;; 26 =                    11010 -- compressed Collatz 7 X ->                       10
;; 27 =                    11011 -- compressed Collatz 7 X ->                 10100001
;; 28 =                    11100 -- compressed Collatz 7 X ->                    10100
;; 29 =                    11101 -- compressed Collatz 7 X ->                    10100
;; 30 =                    11110 -- compressed Collatz 7 X ->                    10100
;; 31 =                    11111 -- compressed Collatz 7 X ->                 10110110

;; > (compute-fc-X-times-stop-over-numbers-gap 1 1 16)
;; STOP because 1
;; 1 =                        1 -- compressed Collatz 1 X ->                        1
;; 2 =                       10 -- compressed Collatz 1 X ->                        1
;; 3 =                       11 -- compressed Collatz 1 X ->                      101
;; 4 =                      100 -- compressed Collatz 1 X ->                       10
;; 5 =                      101 -- compressed Collatz 1 X ->                     1000
;; 6 =                      110 -- compressed Collatz 1 X ->                       11
;; 7 =                      111 -- compressed Collatz 1 X ->                     1011
;; 8 =                     1000 -- compressed Collatz 1 X ->                      100
;; 9 =                     1001 -- compressed Collatz 1 X ->                     1110
;; 10 =                     1010 -- compressed Collatz 1 X ->                      101
;; 11 =                     1011 -- compressed Collatz 1 X ->                    10001
;; 12 =                     1100 -- compressed Collatz 1 X ->                      110
;; 13 =                     1101 -- compressed Collatz 1 X ->                    10100
;; 14 =                     1110 -- compressed Collatz 1 X ->                      111
;; 15 =                     1111 -- compressed Collatz 1 X ->                    10111
;; 16 =                    10000 -- compressed Collatz 1 X ->                     1000
;; > (compute-fc-X-times-stop-over-numbers-gap 2 2 16)
;; STOP because 1
;; 2 =                       10 -- compressed Collatz 2 X ->                        1
;; 3 =                       11 -- compressed Collatz 2 X ->                     1000
;; 4 =                      100 -- compressed Collatz 2 X ->                        1
;; 5 =                      101 -- compressed Collatz 2 X ->                      100
;; 6 =                      110 -- compressed Collatz 2 X ->                      101
;; 7 =                      111 -- compressed Collatz 2 X ->                    10001
;; 8 =                     1000 -- compressed Collatz 2 X ->                       10
;; 9 =                     1001 -- compressed Collatz 2 X ->                      111
;; 10 =                     1010 -- compressed Collatz 2 X ->                     1000
;; 11 =                     1011 -- compressed Collatz 2 X ->                    11010
;; 12 =                     1100 -- compressed Collatz 2 X ->                       11
;; 13 =                     1101 -- compressed Collatz 2 X ->                     1010
;; 14 =                     1110 -- compressed Collatz 2 X ->                     1011
;; 15 =                     1111 -- compressed Collatz 2 X ->                   100011
;; 16 =                    10000 -- compressed Collatz 2 X ->                      100
;; > (compute-fc-X-times-stop-over-numbers-gap 2 3 16)
;; 3 =                       11 -- compressed Collatz 2 X ->                     1000
;; 4 =                      100 -- compressed Collatz 2 X ->                        1
;; 5 =                      101 -- compressed Collatz 2 X ->                      100
;; 6 =                      110 -- compressed Collatz 2 X ->                      101
;; 7 =                      111 -- compressed Collatz 2 X ->                    10001
;; 8 =                     1000 -- compressed Collatz 2 X ->                       10
;; 9 =                     1001 -- compressed Collatz 2 X ->                      111
;; 10 =                     1010 -- compressed Collatz 2 X ->                     1000
;; 11 =                     1011 -- compressed Collatz 2 X ->                    11010
;; 12 =                     1100 -- compressed Collatz 2 X ->                       11
;; 13 =                     1101 -- compressed Collatz 2 X ->                     1010
;; 14 =                     1110 -- compressed Collatz 2 X ->                     1011
;; 15 =                     1111 -- compressed Collatz 2 X ->                   100011
;; 16 =                    10000 -- compressed Collatz 2 X ->                      100
;; > (compute-fc-X-times-stop-over-numbers-gap 2 3 35)
;; 3 =                       11 -- compressed Collatz 2 X ->                     1000
;; 4 =                      100 -- compressed Collatz 2 X ->                        1
;; 5 =                      101 -- compressed Collatz 2 X ->                      100
;; 6 =                      110 -- compressed Collatz 2 X ->                      101
;; 7 =                      111 -- compressed Collatz 2 X ->                    10001
;; 8 =                     1000 -- compressed Collatz 2 X ->                       10
;; 9 =                     1001 -- compressed Collatz 2 X ->                      111
;; 10 =                     1010 -- compressed Collatz 2 X ->                     1000
;; 11 =                     1011 -- compressed Collatz 2 X ->                    11010
;; 12 =                     1100 -- compressed Collatz 2 X ->                       11
;; 13 =                     1101 -- compressed Collatz 2 X ->                     1010
;; 14 =                     1110 -- compressed Collatz 2 X ->                     1011
;; 15 =                     1111 -- compressed Collatz 2 X ->                   100011
;; 16 =                    10000 -- compressed Collatz 2 X ->                      100
;; 17 =                    10001 -- compressed Collatz 2 X ->                     1101
;; 18 =                    10010 -- compressed Collatz 2 X ->                     1110
;; 19 =                    10011 -- compressed Collatz 2 X ->                   101100
;; 20 =                    10100 -- compressed Collatz 2 X ->                      101
;; 21 =                    10101 -- compressed Collatz 2 X ->                    10000
;; 22 =                    10110 -- compressed Collatz 2 X ->                    10001
;; 23 =                    10111 -- compressed Collatz 2 X ->                   110101
;; 24 =                    11000 -- compressed Collatz 2 X ->                      110
;; 25 =                    11001 -- compressed Collatz 2 X ->                    10011
;; 26 =                    11010 -- compressed Collatz 2 X ->                    10100
;; 27 =                    11011 -- compressed Collatz 2 X ->                   111110
;; 28 =                    11100 -- compressed Collatz 2 X ->                      111
;; 29 =                    11101 -- compressed Collatz 2 X ->                    10110
;; 30 =                    11110 -- compressed Collatz 2 X ->                    10111
;; 31 =                    11111 -- compressed Collatz 2 X ->                  1000111
;; 32 =                   100000 -- compressed Collatz 2 X ->                     1000
;; 33 =                   100001 -- compressed Collatz 2 X ->                    11001
;; 34 =                   100010 -- compressed Collatz 2 X ->                    11010
;; 35 =                   100011 -- compressed Collatz 2 X ->                  1010000
;; une fonction qui compte le LSB:
;;
;; > (compute-fc-X-times-stop-over-numbers-gap 5 128 255)
;; 128 =                 10000000 -- compressed Collatz 5 X ->                      100
;; 129 =                 10000001 -- compressed Collatz 5 X ->                  1101110
;; 130 =                 10000010 -- compressed Collatz 5 X ->                   100101
;; 131 =                 10000011 -- compressed Collatz 5 X ->                   100101
;; 132 =                 10000100 -- compressed Collatz 5 X ->                   100110
;; 133 =                 10000101 -- compressed Collatz 5 X ->                   100110
;;...
;; 252 =                 11111100 -- compressed Collatz 5 X ->                 11010111
;; 253 =                 11111101 -- compressed Collatz 5 X ->                 11010111
;; 254 =                 11111110 -- compressed Collatz 5 X ->               1010000111
;; 255 =                 11111111 -- compressed Collatz 5 X ->              11110010111
;; lsb1 = 64
;; lsb0 = 64


;; > (compute-fc-comp-X-times-stop-over-numbers-gap 5 32 63)
;; 32 =                   100000 -- compressed Collatz 5 X ->                        1
;; 33 =                   100001 -- compressed Collatz 5 X ->                    11101
;; 34 =                   100010 -- compressed Collatz 5 X ->                     1010
;; 35 =                   100011 -- compressed Collatz 5 X ->                     1010
;; 36 =                   100100 -- compressed Collatz 5 X ->                     1011
;; 37 =                   100101 -- compressed Collatz 5 X ->                     1011
;; 38 =                   100110 -- compressed Collatz 5 X ->                     1011
;; 39 =                   100111 -- compressed Collatz 5 X ->                  1100101
;; 40 =                   101000 -- compressed Collatz 5 X ->                      100
;; 41 =                   101001 -- compressed Collatz 5 X ->                  1101011
;; 42 =                   101010 -- compressed Collatz 5 X ->                      100
;; 43 =                   101011 -- compressed Collatz 5 X ->                   100101
;; 44 =                   101100 -- compressed Collatz 5 X ->                     1101
;; 45 =                   101101 -- compressed Collatz 5 X ->                     1101
;; 46 =                   101110 -- compressed Collatz 5 X ->                   101000
;; 47 =                   101111 -- compressed Collatz 5 X ->                  1111001
;; 48 =                   110000 -- compressed Collatz 5 X ->                      101
;; 49 =                   110001 -- compressed Collatz 5 X ->                     1110
;; 50 =                   110010 -- compressed Collatz 5 X ->                   101100
;; 51 =                   110011 -- compressed Collatz 5 X ->                   101100
;; 52 =                   110100 -- compressed Collatz 5 X ->                      101
;; 53 =                   110101 -- compressed Collatz 5 X ->                      101
;; 54 =                   110110 -- compressed Collatz 5 X ->                   101111
;; 55 =                   110111 -- compressed Collatz 5 X ->                   101111
;; 56 =                   111000 -- compressed Collatz 5 X ->                    10001
;; 57 =                   111001 -- compressed Collatz 5 X ->                   110001
;; 58 =                   111010 -- compressed Collatz 5 X ->                    10001
;; 59 =                   111011 -- compressed Collatz 5 X ->                 10011000
;; 60 =                   111100 -- compressed Collatz 5 X ->                   110101
;; 61 =                   111101 -- compressed Collatz 5 X ->                   110101
;; 62 =                   111110 -- compressed Collatz 5 X ->                 10100001
;; 63 =                   111111 -- compressed Collatz 5 X ->                111100101
;; lsb1 = 23
;; lsb0 = 9
;; > (compute-fc-comp-X-times-stop-over-numbers-gap 4 32 63)
;; 32 =                   100000 -- compressed Collatz 4 X ->                       10
;; 33 =                   100001 -- compressed Collatz 4 X ->                    10011
;; 34 =                   100010 -- compressed Collatz 4 X ->                    10100
;; 35 =                   100011 -- compressed Collatz 4 X ->                    10100
;; 36 =                   100100 -- compressed Collatz 4 X ->                      111
;; 37 =                   100101 -- compressed Collatz 4 X ->                      111
;; 38 =                   100110 -- compressed Collatz 4 X ->                    10110
;; 39 =                   100111 -- compressed Collatz 4 X ->                  1000011
;; 40 =                   101000 -- compressed Collatz 4 X ->                     1000
;; 41 =                   101001 -- compressed Collatz 4 X ->                  1000111
;; 42 =                   101010 -- compressed Collatz 4 X ->                     1000
;; 43 =                   101011 -- compressed Collatz 4 X ->                  1001010
;; 44 =                   101100 -- compressed Collatz 4 X ->                    11010
;; 45 =                   101101 -- compressed Collatz 4 X ->                    11010
;; 46 =                   101110 -- compressed Collatz 4 X ->                  1010000
;; 47 =                   101111 -- compressed Collatz 4 X ->                 11110010
;; 48 =                   110000 -- compressed Collatz 4 X ->                       11
;; 49 =                   110001 -- compressed Collatz 4 X ->                    11100
;; 50 =                   110010 -- compressed Collatz 4 X ->                    11101
;; 51 =                   110011 -- compressed Collatz 4 X ->                    11101
;; 52 =                   110100 -- compressed Collatz 4 X ->                     1010
;; 53 =                   110101 -- compressed Collatz 4 X ->                     1010
;; 54 =                   110110 -- compressed Collatz 4 X ->                    11111
;; 55 =                   110111 -- compressed Collatz 4 X ->                  1011110
;; 56 =                   111000 -- compressed Collatz 4 X ->                     1011
;; 57 =                   111001 -- compressed Collatz 4 X ->                  1100010
;; 58 =                   111010 -- compressed Collatz 4 X ->                     1011
;; 59 =                   111011 -- compressed Collatz 4 X ->                  1100101
;; 60 =                   111100 -- compressed Collatz 4 X ->                   100011
;; 61 =                   111101 -- compressed Collatz 4 X ->                   100011
;; 62 =                   111110 -- compressed Collatz 4 X ->                  1101011
;; 63 =                   111111 -- compressed Collatz 4 X ->                101000011
;; lsb1 = 16
;; lsb0 = 16
;; > 
(define (compute-fc-comp-X-times-stop-over-numbers-gap x n-start n-end)
  (define lsb1 0)
  (define lsb0 0)
  (for (n n-start n-end)
       (let ((res (fc-comp-x-stop n x)))
	 (if (bit-test? res 0)
	     (incf lsb1)
	     (incf lsb0))
	 (display n) (display " = ")
	 (display (padding-spc n))
	 (display " -- compressed Collatz ") (display x) (display " X -> ")
	 (display (padding-spc res))
	 (newline)))
  (display-symb-nl lsb1)
  (display-symb-nl lsb0))


;; > (compute-fc-half-size-times-over-numbers 16)
;;                       10
;;                        1
;;                      101
;;                       10
;;                     1000
;;                       11
;;                     1011
;;                       10
;;                      111
;;                     1000
;;                    11010
;;                       11
;;                     1010
;;                     1011
;;                   100011
;;                      100
(define (compute-fc-half-size-times-over-numbers n-end)
  (for (n 1 n-end)
       (let ((res (fc-comp-half-size-times n)))
	 (display (padding-spc res))
	 (newline))))

(define (compute-fc-x-times-over-numbers x n-end)
  (for (n 1 n-end)
       (let ((res (fc-comp-x n x)))
	 (display (padding-spc res))
	 (newline))))















;; space time tradeoff

;; compute fc compressed x times and count the odd steps
;; n0 : starting number in the collatz sequence
;; x0 : k , number of bits
;; return: pair ( computed number of collatz sequence, number of odd steps encountered during computation )
;; (fc-comp-x-count-odd 7 5) -> '(20 . 4)
(define (fc-comp-x-count-odd n0 x0)
  (let ((n-odd 0))
    (letrec ((fc-comp-count-odd ;; Collatz function compressed with odd counting
	      (lambda (n)
		(if (zero? (modulo n 2))
		    (quotient n 2)
		    (begin	
		      (set! n-odd (+ 1 n-odd))
		      (quotient (+ (* 3 n) 1) 2)))))

	     (fc-comp-x-count-odd-rec ;; Collatz function composed x times with odd counting
	      (lambda (n x)
		(if (equal? x 1)
		    (fc-comp-count-odd n)
		    (fc-comp-x-count-odd-rec (fc-comp-count-odd n) (- x 1))))))

      (cons (fc-comp-x-count-odd-rec n0 x0) n-odd))))




;; first compute the tables

(define k 10) ;; size in bits 24 seems a max with Racket and extending memory many times

;;(define d (make-vector (arithmetic-shift 1 k))) ;; size = 2^k

(define collatz-odd (make-vector (arithmetic-shift 1 k))) ;; size = 2^k

;;collatz-odd
;; DEPRECATED
;; (define (compute-d)
;;   (for (i (arithmetic-shift 1 k))
;;        (vector-set! d i (fc-comp-x i k))))
  
(define (compute-collatz-odd)
  (for (i (arithmetic-shift 1 k))
       ;;(dv i)
       (vector-set! collatz-odd i (fc-comp-x-count-odd i k))))

;;(compute-d)

(compute-collatz-odd)

;;collatz-odd

;; return the collatz value from the list of pair (collatz-value . odd-value)
(define (get-collatz-values)
  (map car 
       (vector->list collatz-odd)))

;; second ,use them ! (the pre-computed tables)
;; TODO : write a function that use them on a number displaying step by step !

;; compute statistics time space tradeoff numbers over HSB-1 and HSB-2 positions
;; HSB = Highest Significant Bit
;; DEPRECATED : replaced by stat-HSB-1-* and stat-HSB-2-*
(define (stat-significant-bits-time-space-tradeoff)

  ;; get all the values of collatz recursion over the tradeoff
  (define lst-val-collatz-recursion (map car
					 (vector->list collatz-odd)))
  (define stat (cons 0 0)) ; for further statistics over HSB-1 and HSB-2 positions

  
  (define (stat-bits n) ; compute statistics for a number over HSB-1 and HSB-2 positions
    (define hsb-pos (last-bit-position n)) ; position of HSB
    (define hsb-1-pos (sub1 hsb-pos)) ; position HSB-1
    (define hsb-2-pos (sub1 hsb-1-pos)) ;  position HSB-2
    (define hsb-1-val (bit-value n hsb-1-pos)) ; value of HSB-1
    (define hsb-2-val (bit-value n hsb-2-pos)) ; value of HSB-2
    (define hsb-1-hsb-2-val (cons hsb-1-val hsb-2-val))
    (set! stat (add-pair stat hsb-1-hsb-2-val))
    hsb-1-hsb-2-val)

  ;; stat all the number of the list of values of  collatz recursion over the tradeoff
  (define stat-lst-val-collatz-recursion (map stat-bits lst-val-collatz-recursion))
  stat
  )

;; mean = 0.41482724815599153 avec k=24
;; TODO : faire une meme routine sans space time tradeoff  juste en calculant une iteration de collatz
(define (stat-HSB-1-time-space-tradeoff)

  (define min-len 2) ; minimal length of 2 for getting valid HSB-1
  ;; get all the values of collatz recursion over the tradeoff
  (define lst-val-collatz-recursion (filter ; filtering on length in bits
				     (lambda (n) (>= (size-bit n)
						     min-len)) 
				     (map car
					  (vector->list collatz-odd))))
  ;;(dv lst-val-collatz-recursion)
  
  (define stat 0) ; for further statistics over HSB-1 positions
  (define cnt 0) ; counter of elements in list,used for mean later
  
  (define (stat-bits n) ; compute statistics for a number over HSB-1 positions
    (define hsb-pos (last-bit-position n)) ; position of HSB
    (define hsb-1-pos (sub1 hsb-pos)) ; position HSB-1
    (define hsb-1-val (bit-value n hsb-1-pos)) ; value of HSB-1
    (set! stat (+ stat hsb-1-val))
    (incf cnt)
    hsb-1-val)

  ;; stat all the number of the list of values of  collatz recursion over the tradeoff
  (define stat-lst-val-collatz-recursion (map stat-bits lst-val-collatz-recursion))
  (define mean (exact->inexact (/ stat cnt)))
  (dv mean)
  mean
  )

;; stat all the values of collatz calculus over the gap
;; statistics: return mean and count optionally
;; (stat-HSB-1-gap-collatz 256)
;; mean = 0.3359375
;; 0.3359375
;; > (stat-HSB-1-gap-collatz 8192)
;; mean = 0.33349609375
;; 0.33349609375
;; > (stat-HSB-1-gap-collatz 65536)
;; mean = 0.333343505859375
;; 0.333343505859375
;; > (stat-HSB-1-gap-collatz 16000000)
;; mean = 0.349525375
;; 0.349525375
;; > (stat-HSB-1-gap-collatz 96000000)
;; mean = 0.46603379166666664
;; 0.46603379166666664
;; > (stat-HSB-1-gap-collatz 96)
;; mean = 0.4583333333333333
;; 0.4583333333333333
;; > (stat-HSB-1-gap-collatz 16)
;; mean = 0.375
;; 0.375

;; convergence vers 1/3:
;; (stat-HSB-1-gap-collatz (expt 2 16))
;; mean = 0.333343505859375
;; 0.333343505859375
;; > (stat-HSB-1-gap-collatz (expt 2 20))
;; mean = 0.33333396911621094
;; 0.33333396911621094
;; > (stat-HSB-1-gap-collatz (expt 2 24))
;; mean = 0.3333333730697632
;; 0.3333333730697632
;; > (stat-HSB-1-gap-collatz (expt 2 27))
;; mean = 0.3333333432674408
;; 0.3333333432674408
(define (stat-HSB-1-gap-collatz m)

  (define min-len 2) ; minimal length of 2 for getting valid HSB-1
  
  (define stat 0) ; for further statistics over HSB-1 positions
  (define cnt 0) ; counter of elements in list,used for mean later
  
  (define (stat-bits n) ; compute statistics for a number over HSB-1 positions
    (define hsb-pos (last-bit-position n)) ; position of HSB
    (define hsb-1-pos (sub1 hsb-pos)) ; position HSB-1
    (define hsb-1-val (bit-value n hsb-1-pos)) ; value of HSB-1
    (set! stat (+ stat hsb-1-val))
    (incf cnt)
    hsb-1-val)

  ;; stat all the number of the gap
 
  (for-next i = 1 to m step 2
	    (stat-bits (mac-mult3 i)))
  
  (define mean (exact->inexact (/ stat cnt)))
  (dv mean)
  mean
  )

;; > (study-time-space-tradeoff)
;; number                    % of 1 initial     % of 1 final   result
;;                        0    0 > #   0                         0
;;                        1  100 > # 100                         1
;;                       10   50 > #  50                        10
;;                       11  100 > #  50                        10
;;                      100   33 >   100                         1
;;                      101   67 >   100                         1
;;                      110   67 >   100                         1
;;                      111  100   #  25                      1000
;;                     1000   25 >    50                        10
;;                     1001   50 > #  50                      1010
;;                     1010   50 > #  50                        10
(define (study-time-space-tradeoff)
  ;; environement (bindings)
  (define i 0)
  (define (display-per100 p)
    (display (~r p #:base 10 #:precision 0 #:min-width 3 #:pad-string " ")))
  

  ;; procedure that display statistic informations
  (define (proc elem)
    (define tsto (vector-ref collatz-odd i)) ;; time space tradeoff
    (define res (car tsto)) ;; result value of collatz recursion
    (define p100-i (per-cent (count-ones i)
			     (size-bit i)))
    (define p100-res (per-cent (count-ones res)
			       (size-bit res)))
    (display-binary-pad i)
    (display "  ")
    (display-per100 p100-i)
    (display " ")
    (if (>= (size-bit i) (size-bit res))
	(display ">")
	(display " "))
    (display " ")
    (if (>= p100-i p100-res)
	(display "#")
	(display " "))
    (display " ")
    (display-per100 p100-res)
    (display "  ")
    (display-binary-pad res)
    (newline)
    (incf i))

  ;; application of procedure to array
  (display-nl "number                    % of 1 initial     % of 1 final   result")
  (vector-map proc collatz-odd))



;; count the 0 and 1 of a number 
;; (scan-and-stat-bits #b10101 0 4) -> '(2 . 3)
(define (scan-and-stat-bits n start stop)
  (define v-one-sum 0)
  (define v-zero-sum 0)
  (for (k start stop)
       (when (or (< k 0)
		 (>= k (size-bit n)))
	     (dv k)
	     (error "scan-and-stat-bits : testing bits out of range, check k"))
       (if (bit-test? n k)
	   (incf v-one-sum)
	   (incf v-zero-sum)))
  (cons v-zero-sum v-one-sum))


;; no high significant bit in stats
;; (scan-and-stat-bits-no-HSB 27) -> '(1 . 3)
(define (scan-and-stat-bits-no-HSB n)
  (define stop (- (size-bit n) 2))
  (define v-one-sum 0)
  (define v-zero-sum 0)
  (for (k 0 stop)
       (if (bit-test? n k)
	   (incf v-one-sum)
	   (incf v-zero-sum)))
  (cons v-zero-sum v-one-sum))



;; count all the 0 and 1 of the timespace trade off values

;; with k,size in bits of 20:
;; (stat-collatz-time-space-tradeoff-bits)
;; stat-on-all-values = (7481216 . 7105222)

;; with k,size in bits of 5:
;; >  (stat-collatz-time-space-tradeoff-bits)
;; stat-on-each-value = ((0 . 0) (1 . 0) (0 . 0) (0 . 0) (1 . 0) (1 . 0) (1 . 0) (3 . 1) (0 . 0) (2 . 2) (0 . 0) (2 . 1) (2 . 0) (2 . 0) (1 . 2) (4 . 1) (1 . 0) (1 . 1) (3 . 1) (3 . 1) (1 . 0) (1 . 0) (3 . 1) (3 . 1) (3 . 0) (2 . 2) (3 . 0) (3 . 3) (2 . 2) (2 . 2) (5 . 1) (3 . 4))
;; zerosVSones = (#t #f #t #t #f #f #f #f #t #t #t #f #f #f #t #f #f #t #f #f #f #f #f #f #f #t #f #t #t #t #f #t)
;; zeroWin = 13
;; oneWin = 19
;; ( zeros . ones )
;; stat-on-all-values = (59 . 26)
;; > (get-collatz-values)
;; '(0 2 1 1 2 2 2 20 1 26 1 10 4 4 13 40 2 5 17 17 2 2 20 20 8 22 8 71 26 26 80 242)
;; k=12 second verison (skip first bit)
;;( zeros . ones )
;;stat-on-all-values = (14366 . 12836)
;;  k=7
;;  (stat-collatz-time-space-tradeoff-bits)
;; collatz-values = (0 2 1 1 2 2 2 5 1 20 1 8 1 1 10 10 2 4 13 13 2 2 5 5 2 17 2 161 20 20 20 182 1 22 8 8 26 26 26 76 1 242 1 28 10 10 10 91 4 11 11 11 4 4 107 107 13 37 13 38 40 40 121 364 2 14 44 44 5 5 5 137 17 47 17 16 17 17 152 152 2 53 161 161 2 2 56 56 20 19 20 175 20 20 182 182 8 188 7 7 22 22 22 593 8 202 8 206 71 71 71 638 26 8 74 74 26 26 76 76 80 233 80 236 242 242 728 2186)
;; collatz-values-significant = (5 20 8 10 10 4 13 13 5 5 17 161 20 20 20 182 22 8 8 26 26 26 76 242 28 10 10 10 91 4 11 11 11 4 4 107 107 13 37 13 38 40 40 121 364 14 44 44 5 5 5 137 17 47 17 16 17 17 152 152 53 161 161 56 56 20 19 20 175 20 20 182 182 8 188 7 7 22 22 22 593 8 202 8 206 71 71 71 638 26 8 74 74 26 26 76 76 80 233 80 236 242 242 728 2186)
;; stat-on-each-value = ((1 . 0) (2 . 1) (2 . 0) (1 . 1) (1 . 1) (1 . 0) (1 . 1) (1 . 1) (1 . 0) (1 . 0) (3 . 0) (5 . 1) (2 . 1) (2 . 1) (2 . 1) (2 . 4) (1 . 2) (2 . 0) (2 . 0) (1 . 2) (1 . 2) (1 . 2) (3 . 2) (2 . 4) (1 . 2) (1 . 1) (1 . 1) (1 . 1) (2 . 3) (1 . 0) (1 . 1) (1 . 1) (1 . 1) (1 . 0) (1 . 0) (2 . 3) (2 . 3) (1 . 1) (3 . 1) (1 . 1) (2 . 2) (3 . 1) (3 . 1) (2 . 3) (3 . 4) (0 . 2) (2 . 2) (2 . 2) (1 . 0) (1 . 0) (1 . 0) (5 . 1) (3 . 0) (1 . 3) (3 . 0) (3 . 0) (3 . 0) (3 . 0) (4 . 2) (4 . 2) (2 . 2) (5 . 1) (5 . 1) (2 . 2) (2 . 2) (2 . 1) (2 . 1) (2 . 1) (2 . 4) (2 . 1) (2 . 1) (2 . 4) (2 . 4) (2 . 0) (2 . 4) (0 . 1) (0 . 1) (1 . 2) (1 . 2) (1 . 2) (6 . 2) (2 . 0) (3 . 3) (2 . 0) (2 . 4) (3 . 2) (3 . 2) (3 . 2) (2 . 6) (1 . 2) (2 . 0) (3 . 2) (3 . 2) (1 . 2) (1 . 2) (3 . 2) (3 . 2) (4 . 1) (3 . 3) (4 . 1) (2 . 4) (2 . 4) (2 . 4) (4 . 4) (7 . 3))
;; zerosVSones = (#f #f #f #t #t #f #t #t #f #f #f #f #f #f #f #t #t #f #f #t #t #t #f #t #t #t #t #t #t #f #t #t #t #f #f #t #t #t #f #t #t #f #f #t #t #t #t #t #f #f #f #f #f #t #f #f #f #f #f #f #t #f #f #t #t #f #f #f #t #f #f #t #t #f #t #t #t #t #t #t #f #f #t #f #t #f #f #f #t #t #f #f #f #t #t #f #f #f #t #f #t #t #t #t #f)
;; zeroWin = 52
;; oneWin = 53
;; ( zeros . ones )
;; stat-on-all-values = (221 . 168)

;;  (stat-collatz-time-space-tradeoff-bits)
;; zeroWin = 42355
;; oneWin = 22817
;; ( zeros . ones )
;; stat-on-all-values = (331847 . 372726)
;; delta01 = -40879 si on compte les MSB les 1 l'emportent sur les 0
(define (stat-collatz-time-space-tradeoff-bits)
  (define collatz-values (get-collatz-values))
  ;;(dv collatz-values)
  (define collatz-values-significant (filter (lambda (n) (> (last-bit-position n) 1))
					     collatz-values))
  ;; compute stats on all values from second bit (skip the first bit that is more often 0 than 1 (probability double for first bit) to before the last bit (skip the leftmost 1)
 
  (define stat-on-each-value (map (lambda (n)
				    ;;(scan-and-stat-bits n 0 (- (last-bit-position n) 1)))
				    ;; skip first and last bit
				    (scan-and-stat-bits n 1 (- (last-bit-position n) 1)))
				    
				    ;;(scan-and-stat-bits n 0 (last-bit-position n)))
				  ;; skip first bit
				    ;;(scan-and-stat-bits n 1 (last-bit-position n)))
				  collatz-values-significant))
  ;;(dv collatz-values)
  ;;(dv collatz-values-significant)
  ;;(dv stat-on-each-value)

  (define zerosVSones (map (lambda (p)
			     (<= (car p) (cdr p)))
			   stat-on-each-value))
  ;;(dv zerosVSones)

  (define zeroWin 0)
  (define oneWin 0)
  (map (lambda (b)
	 (if b
	     (incf zeroWin)
	     (incf oneWin)))
       zerosVSones)
  (dv zeroWin)
  (dv oneWin)
  
  (define stat-on-all-values (apply proc-add-pair stat-on-each-value))
  (display-nl "( zeros . ones )")
  (dv stat-on-all-values)
  (define delta01 (- (car stat-on-all-values) (cdr stat-on-all-values)))
  (dv delta01)
  )


  







;; compute the statistic of congruences for the cycles on a single number


;; compute Collatz until it reach 1
;; > (collatz-comp-stat 2357)
;; even-sum = 36
;; odd-sum = 42
;; 1
;; > (collatz-comp-stat 4721)
;; even-sum = 37
;; odd-sum = 42
;; 1
;; > (collatz-comp-stat 7231)
;; even-sum = 26
;; odd-sum = 22
;; 1
;; > (collatz-comp-stat 157)
;; even-sum = 14
;; odd-sum = 11
;; 1
(define (collatz-comp-stat n)
  
  (define even-sum 0)
  (define odd-sum 0)
  
  (define (collatz-rec n)
    (begin ;;(display (padding-spc n))
	   ;;(newline)
	   ;;(printf "~B\n" n)
	   (cond ((eq? n 1)
		  (dv even-sum)
		  (dv odd-sum)
		  1)
		 ((zero? (modulo n 2))
		  (incf even-sum) 
		  (collatz-rec (quotient n 2)))
		 (else
		  (incf odd-sum)
		  (collatz-rec (quotient (+ (* 3 n) 1) 2))))))
  
  (collatz-rec n))



;; (collatz-comp-stat-mod-2 2127) -> '#(24 25)
;; (collatz-comp-stat-mod-2 2100) -> '#(14 8)
(define (collatz-comp-stat-mod-2 n)
  ;; congruence class list
  (define C (make-vector 2 0))
  (define r 0)
  (define (collatz-rec n)
    (begin
      ;;(display (padding-spc n))
      ;;(newline)
      ;;(printf "~B\n" n)
      (set! r (modulo n 2))
      (cond ((or 
	      (= n 1)
	      ;;(= n 2)
	      #;(= n 4))
	     ;;(dv C)
	     ;;(dv n)
	     C)
	    (else
	     (vector-set! C 
			  r 
			  (+ 1 (vector-ref C r))) 
	     (collatz-rec (fc-comp n))))))
  (collatz-rec n))

(define (collatz-comp-stat-mod-2-not-compressed n)
  ;; congruence class list
  (define C (make-vector 2 0))
  (define r 0)
  (define (collatz-rec n)
    (begin
      ;;(display (padding-spc n))
      ;;(newline)
      ;;(printf "~B\n" n)
      (set! r (modulo n 2))
      (cond ((or 
	      (= n 1)
	      (= n 2)
	      (= n 4))
	     ;;(dv C)
	     ;;(dv n)
	     C)
	    (else
	     (vector-set! C 
			  r 
			  (+ 1 (vector-ref C r))) 
	     (collatz-rec (fc n))))))
  (collatz-rec n))


;; copute the statistic of congruences for the cycles on a single number
;;  (collatz-comp-stat-mod-8 10) -> '#(1 0 1 0 0 0 0 0)  
(define (collatz-comp-stat-mod-8 n)
  ;; congruence class list
  (define C (make-vector 8 0))
  (define r 0)
  (define (collatz-rec n)
    (begin
      ;;(display (padding-spc n))
      ;;(newline)
      ;;(printf "~B\n" n)
      (set! r (modulo n 8))
      (cond ((or 
	      (= n 1)
	      (= n 2)
	      (= n 4))
	     ;;(dv C)
	     ;;(dv n)
	     C)
	    (else ; increment the congruence counter
	     (vector-set! C 
			  r 
			  (+ 1 (vector-ref C r))) 
	     (collatz-rec (fc (fc (fc n))))))))
  (collatz-rec n))

(define (collatz-comp-stat-mod-4 n)
  ;; congruence class list
  (define C (make-vector 4 0))
  (define r 0)
  (define (collatz-rec n)
    (begin
      ;;(display (padding-spc n))
      ;;(newline)
      ;;(printf "~B\n" n)
      (set! r (modulo n 4))
      (cond ((or 
	      (= n 1)
	      ;;(= n 2)
	      ;;(= n 4)
	      )
	     ;;(dv C)
	     ;;(dv n)
	     C)
	    (else
	     (vector-set! C 
			  r 
			  (+ 1 (vector-ref C r))) 
	     (collatz-rec (fc (fc n)))))))
  (collatz-rec n))



;; compute stat of collatz mod2 for a gap of numbers
;;
;; ...
;; C-stat = #(18157 19569)
;; C-stat = #(18174 19584)
;; C-stat = #(18191 19599)
;; C-stat = #(18208 19614)
;; C-stat = #(18239 19653)
;; freq= 0,4813 0,5187

;;(stat-collatz-mod-2 5 (expt 2 8))
;; ...
;; C-stat = #(3439 3781)

;; (stat-collatz-mod-2 5 (expt 2 8))
;; C-stat = #(3943 3781)
;; C-freq = #(0.510486794407043 0.489513205592957)

;; (stat-collatz-mod-2 5 (expt 2 16))
;; C-stat = #(2279892 2241897)
;; C-freq = #(0.5042013238565533 0.49579867614344675)

;; (stat-collatz-mod-2 5 (expt 2 20))
;; C-stat = #(46530867 45884477)
;; C-freq = #(0.503497200638024 0.49650279936197605)

;; >  (stat-collatz-mod-2 5 (expt 2 24))
;; C-stat = #(905534300 894725369)
;; C-freq = #(0.5030020477562562 0.49699795224374377)
(define (stat-collatz-mod-2 n-start n-end)
  (define C-stat (make-vector 2 0))
  (for (n n-start n-end)
       (set! C-stat
	     (vector-map +
			 C-stat
			 (collatz-comp-stat-mod-2 n))))
  (dv C-stat)
  (newline)
  (compute-frequencies-2 C-stat))

;; compute stat of collatz mod2 for a gap of numbers
;; (stat-collatz-mod-2-not-compressed 1 10000)
;; ...
;; C-stat = #(547567 281986)
;; C-stat = #(547626 282016)
;; C-stat = #(547647 282022)
;; freq= 0,6601 0,3399
;;
;;(stat-collatz-mod-2-not-compressed 1 100000)
;; ...
;;C-stat = #(6988725 3564768)
;;C-stat = #(6988868 3564849)
;;C-stat = #(6988951 3564892)
;; freq= 0,6622 0,3378

;; >  (stat-collatz-mod-2-not-compressed 1 (expt 2 16))
;; C-stat = #(4390728 2241899)

;; C-freq = #(0.6619892841855874 0.3380107158144126)
(define (stat-collatz-mod-2-not-compressed n-start n-end)
  (define C-stat (make-vector 2 0))
  (for (n n-start n-end)
       (set! C-stat
	     (vector-map +
			 C-stat
			 (collatz-comp-stat-mod-2-not-compressed n)))
       )
  (dv C-stat)
  (newline)
  (compute-frequencies-2 C-stat))


;; compute stat of collatz mod4 for a gap of numbers
;; (stat-collatz-mod-4 1 10000)
;; ...
;; C-stat = #(135286 71373 138452 72161)
;; C-stat = #(135301 71382 138456 72178)
;; C-stat = #(135309 71385 138459 72178)

;; > (stat-collatz-mod-4 1 (expt 2 16))
;; C-stat = #(1156337 568240 1137668 568767)
(define (stat-collatz-mod-4 n-start n-end)
  (define C-stat (make-vector 4 0))
  (for (n n-start n-end)
       (set! C-stat
	     (vector-map +
			 C-stat
			 (collatz-comp-stat-mod-4 n))))
  (dv C-stat))



;; compute stat of collatz mod8 for a gap of numbers
;; (stat-collatz-mod-8 1 10000)
;; ...
;; C-stat = #(46195 22440 46314 22500 44418 25465 46733 25808)
;; C-stat = #(46201 22441 46319 22502 44421 25468 46739 25812)
;; C-stat = #(46205 22441 46321 22503 44422 25469 46739 25812)
(define (stat-collatz-mod-8 n-start n-end)
  (define C-stat (make-vector 8 0))
  (for (n n-start n-end)
       (set! C-stat
	     (vector-map +
			 C-stat
			 (collatz-comp-stat-mod-8 n)))
       (dv C-stat)))

;; restart statistic with a given stat sum
;; (re-stat-collatz-mod-8 #(46205 22441 46321 22503 44422 25469 46739 25812) 10001 19999)
;; ...
;; C-stat = #(100355 48272 100003 49104 96899 54617 100542 55039)
;; C-stat = #(100362 48273 100004 49108 96904 54618 100544 55040)
;; (re-stat-collatz-mod-8 #(276283 133545 274222 134639 266215 149305 275142 148970) 50000 99999)
;;
;; > (re-stat-collatz-mod-8 #(276283 133545 274222 134639 266215 149305 275142 148970) 50000 99999)
;; C-stat = #(592061 286463 588485 288847 570974 318528 589081 316812)
;;
;; > (re-stat-collatz-mod-8 #(592061 286463 588485 288847 570974 318528 589081 316812)  100000 199999)
;; n = 199900
;; C-stat = #(1264210 611303 1255773 617832 1223822 676359 1257351 672827)
;;
;;
;; > (re-stat-collatz-mod-8 #(1264210 611303 1255773 617832 1223822 676359 1257351 672827)  200000 999999)
;; n = 999800
;; n = 999900
;; C-stat = #(7254124 3526845 7209272 3554006 7038403 3847956 7216917 3830245)
;;

;; > (re-stat-collatz-mod-8 #(7254124 3526845 7209272 3554006 7038403 3847956 7216917 3830245)  1000000 9999999)
;; ...
;; n = 9999700
;; n = 9999800
;; n = 9999900
;; C-stat = #(85823360 41885509 85350252 42146170 83653742 45121692 85372334 44885200)
(define (re-stat-collatz-mod-8 C-stat n-start n-end)
  (for (n n-start n-end)
       (set! C-stat
	     (vector-map +
			 C-stat
			 (collatz-comp-stat-mod-8 n)))
       ;;(dv C-stat)
       (when (= (modulo n 100) 0)
	     (dv n)))
  (dv C-stat))


;; compute freq

;; > (compute-frequencies  #(85823360 41885509 85350252 42146170 83653742 45121692 85372334 44885200))
;; C-freq = #(0.16689415557468273 0.08145156115270684 0.16597413845864784 0.08195844875867161 0.16267506459491182 0.0877447199042419 0.16601707964323206 0.08728483191290518)
;; delta = #(0.0002274889080160769 -0.0018817721806264898 -0.0006925282080188222 -0.0013748845746617139 -0.003991602071754835 0.004411386570908574 -0.0006495870234345946 0.003951498579571847)
(define (compute-frequencies C)
  (define s (apply + (vector->list C)))
  (define C-freq (vector-map (lambda (x) (exact->inexact (/ x s)))
			     C))
  (define C-freq-theoric-symbolic  ;;#(1/8 1/16 1/8 1/16 1/8 1/16 1/8 1/16)
				     #(1/6 1/12 1/6 1/12 1/6 1/12 1/6 1/12))
  (define C-freq-theoric (vector-map exact->inexact C-freq-theoric-symbolic))
  (define delta (vector-map - C-freq C-freq-theoric))
  (dv C-freq-theoric-symbolic)
  (dv C-freq)
  (dv delta))

;;  (compute-frequencies-4 #(135309 71385 138459 72178))
;; C-freq = #(0.3242246562081417 0.17105127584579147 0.33177262173191063 0.17295144621415615)
;; delta = #(-0.009108677125191589 0.00438460917912481 -0.0015607116014226818 0.0062847795474894885)

;; > (compute-frequencies-4   #(1156337 568240 1137668 568767))
;; Theoric frequencies for 4: 1/3 1/6 1/3 1/6
;; C-freq = #(0.33702505266667676 0.16561877370291914 0.33158380093103723 0.16577237269936684)
;; delta = #(0.003691719333343446 -0.001047892963747521 -0.0017495324022960834 -0.000894293967299814)
(define (compute-frequencies-4 C)
  (define s (apply + (vector->list C)))
  (define C-freq (vector-map (lambda (x) (exact->inexact (/ x s)))
			     C))
  (define C-freq-theoric (vector-map exact->inexact #(1/3 1/6 1/3 1/6)))
  (define delta (vector-map - C-freq C-freq-theoric))
  (display-nl "Theoric frequencies for 4: 1/3 1/6 1/3 1/6")
  (dv C-freq)
  (dv delta))


(define (compute-frequencies-2 C)
  (define s (apply + (vector->list C)))
  (define C-freq (vector-map (lambda (x) (exact->inexact (/ x s)))
			     C))
;;  (define C-freq-theoric (vector-map exact->inexact #(1/3 1/6 1/3 1/6)))
;;  (define delta (vector-map - C-freq C-freq-theoric))
;;  (display-nl "Theoric frequencies for 4: 1/3 1/6 1/3 1/6")
  (dv C-freq)
  #;(dv delta))

;; count the zero and one in a number
;;(define v-zero-sum (make-vector 7 0))
;;(define v-one-sum (make-vector 7 0))

;; > (scan-and-stat #b10101 5 v-zero-sum v-one-sum)
;; > v-zero-sum
;; '#(0 1 0 1 0)
;; > v-one-sum
;; '#(1 0 1 0 1)
(define (scan-and-stat n sb v-zero-sum v-one-sum)
  (for (k 0 (- sb 1))
       (if (bit-test? n k)
	   (vector-set! v-one-sum 
			k 
			(+ (vector-ref v-one-sum k) 1))
	   (vector-set! v-zero-sum 
			k 
			(+ (vector-ref v-zero-sum k) 1)))))




;; (stat '(1 29 10 10 11 11 11 101 4 107 4 37 13 13 40 121 5 14 44 44 5 5 47 47 17 49 17 152 53 53 161 485))
;; mx = 9
;; v-zero-sum = #(9 22 13 9 10 1 2 0 0)
;; v-one-sum = #(22 9 13 9 5 5 1 1 0)
;; P-one = #(0.7096774193548387 0.2903225806451613 0.5 0.5 0.3333333333333333 0.8333333333333334 0.3333333333333333 1.0 0.5)
;; '(#(9 22 13 9 10 1 2 0 0) . #(22 9 13 9 5 5 1 1 0))

;; (stat-01 4 8191)
;; ...

;;  size:13.0 8183 :            1111111110111 -- compressed Collatz size of n-2 times ->           10001000100101
;;  size:13.0 8184 :            1111111111000 -- compressed Collatz size of n-2 times ->          110011010000011
;;  size:13.0 8185 :            1111111111001 -- compressed Collatz size of n-2 times ->           10001000100111
;;  size:13.0 8186 :            1111111111010 -- compressed Collatz size of n-2 times ->          110011010000011
;;  size:13.0 8187 :            1111111111011 -- compressed Collatz size of n-2 times ->          110011001111010
;;  size:13.0 8188 :            1111111111100 -- compressed Collatz size of n-2 times ->        10011001110001011
;;  size:13.0 8189 :            1111111111101 -- compressed Collatz size of n-2 times ->        10011001110001011
;;  size:13.0 8190 :            1111111111110 -- compressed Collatz size of n-2 times ->       111001101010100011
;;  size:13.0 8191 :            1111111111111 -- compressed Collatz size of n-2 times ->     10101100111111101011
;; mx = 20
;; v-zero-sum = #(4083 3975 4064 3941 3904 3153 3177 2639 1572 1373 879 461 254 80 62 12 2 1 0 0)
;; v-one-sum = #(4083 4161 3970 3748 3535 3451 2511 2280 1600 1186 709 293 225 78 11 13 0 1 0 0)
;; P-one = #(0.5 0.5114306784660767 0.4941498630819019 0.48744960332943166 0.4751982793386208 0.5225620835857057 0.44145569620253167 0.46350884326082537 0.5044136191677175 0.46346228995701444 0.4464735516372796 0.3885941644562334 0.4697286012526096 0.4936708860759494 0.1506849315068493 0.52 0.0 0.5 0.5 0.5)
;; n-zero = 33632
;; n-one = 31855
;; '(33632 . 31855)
(define (stat L) ;; L : number list

  (define L-size (map size-bit L)) ;; construct the list of the size of numbers
  (define mx (apply max L-size)) ;; get the maximum size
  (dv mx)
  (define v-zero-sum (make-vector mx 0))
  (define v-one-sum (make-vector mx 0))
  (define P-one (make-vector mx 0))

  ;; scan and stat each number of the list
  (for-each (lambda (n)
	      (when (> (size-bit n) 1)
	      ;;(when (= (size-bit n) 5)
		    (scan-and-stat n 
				   ;;(size-bit n)
				   (- (size-bit n) 2);1)
				   ;;mx
				   v-zero-sum 
				   v-one-sum)))
	    L)

  ;; compute probabilities
  (for (t 0 (- (vector-length P-one) 1))
       (if (not (zero? (+ (vector-ref v-one-sum t) (vector-ref v-zero-sum t))))
	   (vector-set! P-one
			t
			(exact->inexact (/ (vector-ref v-one-sum t)
					   (+ (vector-ref v-one-sum t) (vector-ref v-zero-sum t)))))
	   (vector-set! P-one
			t
			0.5)))
  
  (dv v-zero-sum)
  (dv v-one-sum)
  (dv P-one)
  (cons v-zero-sum v-one-sum))



;; > (stat-collatz collatz-odd)
;; mx = 39
;; v-zero-sum = #(8392634 8391547 8387484 8384705 8382036 8371512 8345638 8306779 8251531 8130667 7971121 7802845 7425923 7031209 6439694 5593393 5101831 3981727 3049653 2929017 1869661 1390338 948345 536103 388454 174915 72623 65565 19222 8484 4174 871 470 84 10 7 1 1 0)
;; v-one-sum = #(8383048 8382097 8382902 8378121 8365059 8349564 8324059 8267907 8175347 8055905 7814519 7483373 7090352 6493680 5973483 5250043 4347856 3878584 2929764 1919551 1577612 964169 697627 397705 207330 155379 65614 20840 14896 4251 1772 582 97 60 9 0 0 0 0)
;;
;;  (stat-collatz collatz-odd)
;; mx = 29
;; v-zero-sum = #(131356 130984 130835 130125 129730 128079 124929 119871 115777 105187 94411 86109 68911 56857 41575 26177 21433 10406 5085 4620 1473 811 330 68 42 6 0 1 0)
;; v-one-sum = #(130511 130517 130104 129575 127530 125600 122501 116905 108056 100530 86095 71618 58575 42400 33818 22380 12632 10040 4584 1615 1117 309 169 46 10 3 1 0 0)
;; P-one = #(0.4983865855567903 0.4991070779844054 0.4985992894891143 0.4989410858683096 0.49572417010028763 0.49511390379179987 0.49509356181546293 0.49373669628678585 0.48275276657150645 0.4886810521250067 0.47696475463419497 0.45406303296201667 0.4594622154589524 0.42717390209254763 0.4485562320109294 0.46090162077558333 0.3708204902392485 0.49104959405262644 0.4740924604405833 0.2590216519647153 0.4312741312741313 0.27589285714285716 0.33867735470941884 0.40350877192982454 0.19230769230769232 0.3333333333333333 1.0 0.0 0.5)
(define (stat-collatz V)
  ;; the list of collatz numbers
  (define L (map car 
		 (vector->list V)))
  (stat L))




(define (stat-collatz-i V i) ;; ommit the first i elements
  ;; the list of collatz numbers
  (define L (map car 
		 (vector->list V)))
  (stat (list-tail L i)))
  

(define (mult3add1 x)
  (+ (shift-left x) x 1)) ;; return 2x+x+1

(define (mult3add1div2 x)
  (shift-right (+ (shift-left x) x 1))) ;; return 2x+x+1 / 2







;; compute probability of C knowing S
;;
;; 
;; example:
;;   S =  #b00010000
;;   C = #b100000000
;;
;;  (proba-C-knowing-S #b100000000  #b00010000)
;;
;;   ...
;;
;; 0000000011111111
;; 0000000011111110
;; ----------------
;; 0000000111111101

;; 0000000011111111
;; 0000000011111111
;; ----------------
;; 0000000111111110

;; omega-universe = 65536
;; S-true = 32768
;; Probability of S = S-true / omega-universe = 32768 / 65536 = 1/2
;; Probability of C knowing S = C-true / S-true = 15296 / 32768 = 239/512
;; Probability of C = C-true / omega-universe = 32640 / 65536 = 255/512

;;
;; > (proba-C-knowing-S 256 #b100000000  #b00010000)
;; alea = 256
;; omega-universe = 65536
;; S-true = 32768
;; Probability of S = S-true / omega-universe = 32768 / 65536 = 1/2
;; Probability of C knowing S = C-true / S-true = 15296 / 32768 = 239/512 =0,4668
;; Probability of C = C-true / omega-universe = 32640 / 65536 = 255/512
(define (proba-C-knowing-S alea C S)
  (let (
	(omega-universe 0)
	(S-true 0)
	(pS 0)
	(C-true 0)
	(pCkS 0)
	(pC 0)
	(C-true-knowing-S 0)
	(display-enabled #f))
    (begin ; 0
      (display "alea = ")
      (display alea)
      (newline)
      (for (a alea)
	   ;;(display a)
	   ;;(newline)
	   (for (b alea)
		(incf omega-universe)
		(let ((S1 (+ a b)))
		  (if (flag-set? C S1)
		      (incf C-true)
		      '())
		  (if (flag-set? S S1)
		      (begin ; 1
			(incf S-true)
			(if (flag-set? C S1)
			    (begin ; 2
			      (incf C-true-knowing-S)
			      (if display-enabled
				  (begin
				    (display (padding a))
				    (newline)
				    (display (padding b))
				    (newline)
				    (display "-----------------------") 
				    (newline)
				    (display (padding S1))
				    (newline)
				    (newline))
				  '())
			      ) ; end begin 2
			    '())) ; end begin 1
		      '())))) ; end for a
      ) ; end begin 0
      (set! pS (/ S-true omega-universe))
      (set! pCkS (/ C-true-knowing-S S-true))
      (set! pC (/ C-true omega-universe))
      (display "omega-universe = ")
      (display omega-universe)
      (newline)
      (display "S-true = ")
      (display S-true)
      (newline)
      (display "Probability of S = S-true / omega-universe = ")
      (display S-true) (display " / ") (display omega-universe) (display " = ")
      (display pS)
      (newline)
      (display "Probability of C knowing S = C-true / S-true = ")
      (display C-true-knowing-S) (display " / ") (display S-true) (display " = ")
      (display pCkS)
      (newline)
      (display "Probability of C = C-true / omega-universe = ")
      (display C-true) (display " / ") (display omega-universe) (display " = ")
      (display pC)
      (newline)))




;; compute probability of C knowing S for Collatz function
;;
;;
;;  C
;;   1..............................11
;;    1..............................1
;;  ----------------------------------
;;    S2...............S.............0
;;
;;
;;  (collatz-proba-C-knowing-S #b1000000000000000  #b1000)
;;
;;
;; ...
;; 
;; 0011111111111101
;; -----------------------
;; 1011111111111000

;; 0011111111111111
;; -----------------------
;; 1011111111111110

;; omega-universe = 8192
;; S-true = 4096
;; Probability of S = S-true / omega-universe = 4096 / 8192 = 1/2 = 0.5
;; Probability of C knowing S = C-true / S-true = 1366 / 4096 = 683/2048 = 0.33349609375
;; Probability of C = C-true / omega-universe = 2731 / 8192 = 2731/8192 = 0.3333740234375
;;
;;
;;  (collatz-proba-C-knowing-S #b1000000000000 #b100000)
;;
;; omega-universe = 1024
;; S-true = 512
;; Probability of S = S-true / omega-universe = 512 / 1024 = 1/2 = 0.5
;; Probability of C knowing S = C-true / S-true = 171 / 512 = 171/512 = 0.333984375
;; Probability of C = C-true / omega-universe = 342 / 1024 = 171/512 = 0.333984375
;; nota: n'est pas 1/3, car omega-universe = 2^n et non divisible par 3 (decomposition en facteurs premiers)

;; TODO: revoir ce calcul ,pourrait etre faux comme l'autre deja corrige ,probablement faux on devrait peut etre trouver proche de 2/3 cf collatz-proba-SnP1-knowing-Si qui trouve 1/3 pour Sn+1 donc retenue à 2/3
;;
(define (collatz-proba-C-knowing-S C S)

  (let* ((alea (shift-right C)) ; right shift ,alea from 0 to 111....1 
	 (omega-universe 0)
	 (S-true 0)
	 (pS 0)
	 (C-true 0)
	 (pCkS 0)
	 (pC 0)
	 (C-true-knowing-S 0)
	 (display-enabled #f))
    
    (display "alea = ")
    (display alea)
    (newline)
    
    (for (b alea)
	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       (incf omega-universe)
	       (let ((S1 (+ (bitwise-ior 1 (shift-left b)) b))) ; shift to left and set lowest significant bit and add b, i.e compute 2b+incfb = 3b+1 
		 (if-t (flag-set? C S1)
		       (incf C-true))
		 (if-t (flag-set? S S1)
		       (incf S-true)
		       (if-t (flag-set? C S1)
			     (incf C-true-knowing-S)
			     (if-t display-enabled
				   (display (padding b))
				   (newline)
				   (display "-----------------------") 
				   (newline)
				   (display (padding S1))
				   (newline)
				   (newline)))))))

    ;; display results
    (set! pS (/ S-true omega-universe))
    (set! pCkS (/ C-true-knowing-S S-true))
    (set! pC (/ C-true omega-universe))
    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    (display "S-true = ")
    (display S-true)
    (newline)
    (display "Probability of S = S-true / omega-universe = ")
    (display S-true) (display " / ") (display omega-universe) (display " = ")
    (display pS) (display " = ") (display (exact->inexact pS))
    (newline)
    (display "Probability of C knowing S = C-true / S-true = ")
    (display C-true-knowing-S) (display " / ") (display S-true) (display " = ")
    (display pCkS)  (display " = ") (display (exact->inexact pCkS))
    (newline)
    (display "Probability of C = C-true / omega-universe = ")
    (display C-true) (display " / ") (display omega-universe) (display " = ")
    (display pC)  (display " = ") (display (exact->inexact pC))
    (newline)))




;; compute probability of Sk knowing Si for Collatz function
;;
;; definition of Sk: a bit computed with two operands and a carry, see calculus below:
;;
;;  C
;;   1..............................11    2b+1
;;    1..............................1    b
;;  ----------------------------------
;;  ..Sn.....Sk........Si............0    S
;;
;;
;; (collatz-proba-Sk-knowing-Si #b100000  #b100)
;;
;; omega-universe = 32
;; Si-true = 16
;; Probability of Si = Si-true / omega-universe = 16 / 32 = 1/2 = 0.5
;; Probability of Sk knowing Si = Sk-true-knowing-Si / Si-true = 8 / 16 = 1/2 = 0.5
;; Probability of Sk = Sk-true / omega-universe = 16 / 32 = 1/2 = 0.5
;;


(define (collatz-proba-Sk-knowing-Si Sk Si)

  (let* ((alea (shift-left Sk))
	 ;; note: taking an alea greater than Sk do not change the probability result because, both omega-universe and Si-true will be greater in the same proportions
	 ;; each "shift left" will double omega-universe and Si-true 
	 (omega-universe 0)
	 (Si-true 0)
	 (pSi 0) ; probability of Si
	 (Sk-true 0)
	 (pSkkSi 0) ; probability Sk knowing Si
	 (pSk 0)
	 (Sk-true-knowing-Si 0)
	 (Bk Sk)
	 (Bk-true 0)
	 (pBk 0) ; probability of Bk
	 (display-enabled #f))
    
    (display "alea = ")
    (display alea)
    (newline)

    (for (b alea)

	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       
	       (incf omega-universe)

	       (if-t (flag-set? Bk b)
		     (incf Bk-true))
	       	       	       
	       (let ((S (+ (bitwise-ior 1 (shift-left b)) b))) ; shift to left and set lowest significant bit and add b, i.e compute 2b+incfb = 3b+1 

		 (if-t display-enabled
		       (display (padding b))
		       (display "-->") 
		       (display (padding S))
		       (newline)
		       (newline))
		 
		 (if-t (flag-set? Sk S)
		       (incf Sk-true))

		 (if-t (flag-set? Si S)
		       (incf Si-true)
		       (if-t (flag-set? Sk S)
			     (incf Sk-true-knowing-Si)
			     
			     ))))) ; end for
    
    ;; display results
    (set! pSi (/ Si-true omega-universe))
    (set! pSkkSi (/ Sk-true-knowing-Si Si-true))
    (set! pSk (/ Sk-true omega-universe))

    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    
    (display "Si-true = ")
    (display Si-true)
    (newline)
    
    (display "Probability of Si = Si-true / omega-universe = ")
    (display Si-true) (display " / ") (display omega-universe) (display " = ")
    (display pSi) (display " = ") (display (exact->inexact pSi))
    (newline)
    
    (display "Probability of Sk knowing Si = Sk-true-knowing-Si / Si-true = ")
    (display Sk-true-knowing-Si) (display " / ") (display Si-true) (display " = ")
    (display pSkkSi)  (display " = ") (display (exact->inexact pSkkSi))
    (newline)
    
    (display "Probability of Sk = Sk-true / omega-universe = ")
    (display Sk-true) (display " / ") (display omega-universe) (display " = ")
    (display pSk)  (display " = ") (display (exact->inexact pSk))
    (newline)

    
    ;; compute and display probability of Bk
    (set! pBk (/ Bk-true omega-universe))
    (display "Probability of Bk = Bk-true / omega-universe = ")
    (display Bk-true) (display " / ") (display omega-universe) (display " = ")
    (display pBk)  (display " = ") (display (exact->inexact pBk))
    (newline)

    ))



;; compute probability of Sn knowing Si for Collatz function
;;
;; definition of Sn: a bit computed with two operands, a constant 1 and a random bit, and a carry, see calculus below:
;;
;;  C                              11
;;   1.............................x11    2b+1
;;    1.............................x1    b
;;  ----------------------------------
;;  ..S ...............S ...........x0    S
;;     n                i
;;
;;
;; (collatz-proba-Sn-knowing-Si #b1000000000000000000  #b100)
;;
;; omega-universe = 131072
;; Si-true = 65536
;; Probability of Si = Si-true / omega-universe = 65536 / 131072 = 1/2 = 0.5
;; Probability of Sn knowing Si = Sn-true-knowing-Si / Si-true = 43690 / 65536 = 21845/32768 = 0.666656494140625
;; Probability of Sn = Sn-true / omega-universe = 87381 / 131072 = 87381/131072 = 0.6666641235351562
;;
;; > (collatz-proba-Sn-knowing-Si #b1000000000000000000  #b100000000000000000)
;;
;; alea = #<stream>
;; omega-universe = 131072
;; Si-true = 65536
;; Probability of Si = Si-true / omega-universe = 65536 / 131072 = 1/2 = 0.5
;; Probability of Sn knowing Si = Sn-true-knowing-Si / Si-true = 43691 / 65536 = 43691/65536 = 0.6666717529296875
;; Probability of Sn = Sn-true / omega-universe = 87381 / 131072 = 87381/131072 = 0.6666641235351562

(define (collatz-proba-Sn-knowing-Si Sn Si)

  (let* (;;(alea (in-range Sn (shift-left Sn))) ; le MSB de b est connu et egal a 1 donc b de type 1XXX...XXXX1
	 (alea (in-range 1 (shift-left Sn))) ;; this solution gives a proba of 1/2 for Sn
	 (omega-universe 0)
	 (Si-true 0)
	 (pSi 0) ; probability of Si
	 (Sn-true 0)
	 (pSnkSi 0) ; probability Sn knowing Si
	 (pSn 0)
	 (Sn-true-knowing-Si 0)
	 (display-enabled #f))
    
    (display "alea = ")
    (display alea)
    (newline)

    (for-rack ((b alea)) ; alea is in range of 100...00 to 111...11

	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       (incf omega-universe)

	       (let ((S (+ (bitwise-ior 1 (shift-left b)) b))) ; shift to left and set lowest significant bit and add b, i.e compute 2b+incfb = 3b+1 

		 (if-t display-enabled
		       (display (padding b))
		       (display "-->") 
		       (display (padding S))
		       (newline)
		       (newline))
		 
		 (if-t (flag-set? Sn S)
		       (incf Sn-true))

		 (if-t (flag-set? Si S)
		       (incf Si-true)
		       (if-t (flag-set? Sn S)
			     (incf Sn-true-knowing-Si)
			     
			     ))))) ; end for
    
    ;; display results
    (set! pSi (/ Si-true omega-universe))
    (set! pSnkSi (/ Sn-true-knowing-Si Si-true))
    (set! pSn (/ Sn-true omega-universe))
    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    (display "Si-true = ")
    (display Si-true)
    (newline)
    (display "Probability of Si = Si-true / omega-universe = ")
    (display Si-true) (display " / ") (display omega-universe) (display " = ")
    (display pSi) (display " = ") (display (exact->inexact pSi))
    (newline)
    (display "Probability of Sn knowing Si = Sn-true-knowing-Si / Si-true = ")
    (display Sn-true-knowing-Si) (display " / ") (display Si-true) (display " = ")
    (display pSnkSi)  (display " = ") (display (exact->inexact pSnkSi))
    (newline)
    (display "Probability of Sn = Sn-true / omega-universe = ")
    (display Sn-true) (display " / ") (display omega-universe) (display " = ")
    (display pSn)  (display " = ") (display (exact->inexact pSn))
    (newline)))


;; compute probability of Sn+1 knowing Si for Collatz function
;;
;; definition of Sn+1: a bit computed with two operands, a constant 1 and a carry, see calculus below:
;;
;;  C                              11
;;   1.............................x11    2b+1
;;    1.............................x1    b
;;  ----------------------------------
;;  .S ................S ...........x0    S
;;    n+1               i
;;
;;
;; > (collatz-proba-SnP1-knowing-Si #b1000000000000000000  #b100000)
;;
;; 000000101010101010100101-->000001111111111111110000

;; 000000101010101010100111-->000001111111111111110110

;; 000000101010101010101001-->000001111111111111111100

;; omega-universe = 65536
;; Si-true = 32768
;; Probability of Si = Si-true / omega-universe = 32768 / 65536 = 1/2 = 0.5
;; Probability of Sn+1 knowing Si = SnP1-true-knowing-Si / Si-true = 10922 / 32768 = 5461/16384 = 0.33331298828125
;; Probability of Sn+1 = SnP1-true / omega-universe = 21845 / 65536 = 21845/65536 = 0.3333282470703125
;;
;; (collatz-proba-SnP1-knowing-Si #b1000000000000000000 #b100000000000000000  )
;;
;;
;;000000101010101010100111-->000001111111111111110110
;;
;;000000101010101010101001-->000001111111111111111100
;;
;;omega-universe = 65536
;;Si-true = 43691
;;Probability of Si = Si-true / omega-universe = 43691 / 65536 = 43691/65536 = 0.6666717529296875
;;Probability of Sn+1 knowing Si = SnP1-true-knowing-Si / Si-true = 21845 / 43691 = 21845/43691 = 0.49998855599551395
;;Probability of Sn+1 = SnP1-true / omega-universe = 21845 / 65536 = 21845/65536 = 0.3333282470703125
;;
;; conclusion : probability of Sn+1 converging to 1/3 with very little dependancy to Si except if Si = Sn
;; todo: revoir ces calculs : done
(define (collatz-proba-SnP1-knowing-Si SnP1 Si)

  (let* (;;(alea (in-range (shift-right SnP1) SnP1)) ; le MSB de b est connu et egal a 1 donc b de type 1XXX...XXXX1
	 ;;(alea (in-range 1 (shift-right SnP1))) ;; same proba but 0.5 for Si
	 (alea (in-range 1 SnP1))  ;; same results
	 (omega-universe 0)
	 (Si-true 0)
	 (pSi 0) ; probability of Si
	 (SnP1-true 0)
	 (pSnP1kSi 0) ; probability Sn+1 knowing Si
	 (pSnP1 0)
	 (SnP1-true-knowing-Si 0)
	 (display-enabled #f))
    
    (display "alea    = ")
    (display alea)
    (newline)

    (for-rack ((b alea)) ; alea is in range of 100...00 to 111...11

	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       (incf omega-universe)

	       (let ((S (+ (bitwise-ior 1 (shift-left b)) b))) ; shift to left and set lowest significant bit and add b, i.e compute 2b+incfb = 3b+1 

		 (if-t display-enabled
		       (display "display-enabled = ") (display display-enabled)(newline)
		       (display (padding b))
		       (display "-->") 
		       (display (padding S))
		       ;;(newline)
		       (newline))
		 
		 (if-t (flag-set? SnP1 S)
		       (incf SnP1-true))

		 (if-t (flag-set? Si S)
		       (incf Si-true)
		       (if-t (flag-set? SnP1 S)
			     (incf SnP1-true-knowing-Si)
			     (if-t display-enabled
				   (display (padding b))
				   (display "-->") 
				   (display (padding S))
				   (newline)
				   (newline))))))) ; end for
    
    ;; display results
    (set! pSi (/ Si-true omega-universe))
    (set! pSnP1kSi (/ SnP1-true-knowing-Si Si-true))
    (set! pSnP1 (/ SnP1-true omega-universe))
    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    (display "Si-true = ")
    (display Si-true)
    (newline)
    (display "Probability of Si = Si-true / omega-universe = ")
    (display Si-true) (display " / ") (display omega-universe) (display " = ")
    (display pSi) (display " = ") (display (exact->inexact pSi))
    (newline)
    (display "Probability of Sn+1 knowing Si = SnP1-true-knowing-Si / Si-true = ")
    (display SnP1-true-knowing-Si) (display " / ") (display Si-true) (display " = ")
    (display pSnP1kSi)  (display " = ") (display (exact->inexact pSnP1kSi))
    (newline)
    (display "Probability of Sn+1 = SnP1-true / omega-universe = ")
    (display SnP1-true) (display " / ") (display omega-universe) (display " = ")
    (display pSnP1)  (display " = ") (display (exact->inexact pSnP1))
    (newline)))


;; compute probability of Sn+2 knowing Si for Collatz function
;;
;; definition of Sn+2: a bit equal to the most significant carry, see calculus below:
;;
;;  C
;;   n+2
;;   1..............................11    2b+1
;;    1..............................1    b
;;  ----------------------------------
;;  S .................S ............0    S
;;   n+2                i
;;
;;
;; Bienvenue dans DrRacket, version 6.1.1 [3m].
;; Langage: racket [personnalisé]; memory limit: 256 MB.
;; > (collatz-proba-SnP2-knowing-Si #b1000000000000000000  #b100000)
;;
;; ...
;;
;; 000000011111111111111011-->000001011111111111110010

;; 000000011111111111111101-->000001011111111111111000

;; 000000011111111111111111-->000001011111111111111110

;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Sn+2 knowing Si = SnP2-true-knowing-Si / Si-true = 10923 / 16384 = 10923/16384 = 0.66668701171875
;; Probability of Sn+2 = SnP2-true / omega-universe = 21846 / 32768 = 10923/16384 = 0.66668701171875
;;
;;
;; > (collatz-proba-SnP2-knowing-Si #b1000000000000000  #b100000)
;; ...
;; 000000000011111111111001-->000000001011111111101100

;; 000000000011111111111011-->000000001011111111110010

;; 000000000011111111111101-->000000001011111111111000

;; 000000000011111111111111-->000000001011111111111110

;; omega-universe = 4096
;; Si-true = 2048
;; Probability of Si = Si-true / omega-universe = 2048 / 4096 = 1/2 = 0.5
;; Probability of Sn+2 knowing Si = SnP2-true-knowing-Si / Si-true = 1366 / 2048 = 683/1024 = 0.6669921875
;; Probability of Sn+2 = SnP2-true / omega-universe = 2731 / 4096 = 2731/4096 = 0.666748046875
;;
;;
;;
;; conclusion : convergence to 2/3 of Sn+2 with very little dependancy to Si
;; todo: revoir ce resultat
;;
;; probability of Sn+2 knowing Si = Sn+1 true:
;;
;; (collatz-proba-SnP2-knowing-Si #b1000000000000000000 #b100000000000000000  )
;;
;; 000000011111111111111011-->000001011111111111110010

;; 000000011111111111111101-->000001011111111111111000

;; 000000011111111111111111-->000001011111111111111110

;; omega-universe = 32768
;; Si-true = 10922
;; Probability of Si = Si-true / omega-universe = 10922 / 32768 = 5461/16384 = 0.33331298828125
;; Probability of Sn+2 knowing Si = SnP2-true-knowing-Si / Si-true = 0 / 10922 = 0 = 0.0
;; Probability of Sn+2 = SnP2-true / omega-universe = 21846 / 32768 = 10923/16384 = 0.66668701171875
;;
;; with : (alea (in-range 1 (shift-right SnP2)))
;; >  (collatz-proba-SnP2-knowing-Si #b1000000000  #b100)
;; alea = #<stream>
;; Sn+2 = 512

;; omega-universe = 128
;; Si-true = 64
;; Probability of Si = Si-true / omega-universe = 64 / 128 = 1/2 = 0.5
;; Probability of Sn+2 knowing Si = SnP2-true-knowing-Si / Si-true = 21 / 64 = 21/64 = 0.328125
;; Probability of Sn+2 = SnP2-true / omega-universe = 43 / 128 = 43/128 = 0.3359375
(define (collatz-proba-SnP2-knowing-Si SnP2 Si)

  (let* (;;(alea (in-range (shift-right SnP2 2) (shift-right SnP2))) ; le MSB de b est connu et egal a 1 donc b de type 1XXX...XXXX1 ,  b is in range of 100...00 to 111...11,
	 (alea (in-range 1 (shift-right SnP2)))
	 (omega-universe 0)
	 (Si-true 0)
	 (pSi 0) ; probability of Si
	 (SnP2-true 0)
	 (pSnP2kSi 0) ; probability Sn+2 knowing Si
	 (pSnP2 0)
	 (SnP2-true-knowing-Si 0)
	 (display-enabled #f))
    
    (display "alea = ")
    (display alea) (newline)
    (display "Sn+2 = ") (display SnP2) (newline) ; checking Sn+2
    (newline)

    (for-rack ((b alea))

	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       (incf omega-universe)

	       (let ((S (+ (bitwise-ior 1 (shift-left b)) b))) ; shift to left and set lowest significant bit and add b, i.e compute 2b+1+b = 3b+1 

		 (if-t display-enabled
		       (display (padding b))
		       (display "-->") 
		       (display (padding S))
		       (newline)
		       (newline))
		 
		 (if-t (flag-set? SnP2 S)
		       (incf SnP2-true))

		 (if-t (flag-set? Si S)
		       (incf Si-true)
		       (if-t (flag-set? SnP2 S)
			     (incf SnP2-true-knowing-Si)
			     
			     ))))) ; end for
    
    ;; display results
    (set! pSi (/ Si-true omega-universe))
    (set! pSnP2kSi (/ SnP2-true-knowing-Si Si-true))
    (set! pSnP2 (/ SnP2-true omega-universe))
    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    (display "Si-true = ")
    (display Si-true)
    (newline)
    (display "Probability of Si = Si-true / omega-universe = ")
    (display Si-true) (display " / ") (display omega-universe) (display " = ")
    (display pSi) (display " = ") (display (exact->inexact pSi))
    (newline)
    (display "Probability of Sn+2 knowing Si = SnP2-true-knowing-Si / Si-true = ")
    (display SnP2-true-knowing-Si) (display " / ") (display Si-true) (display " = ")
    (display pSnP2kSi)  (display " = ") (display (exact->inexact pSnP2kSi))
    (newline)
    (display "Probability of Sn+2 = SnP2-true / omega-universe = ")
    (display SnP2-true) (display " / ") (display omega-universe) (display " = ")
    (display pSnP2)  (display " = ") (display (exact->inexact pSnP2))
    (newline)))






;; compute probability of Ck knowing Si for Collatz function
;;

;; > (collatz-proba-Ck-knowing-Si #b10000  #b10)
;; alea = 16
;; 000000000000000000000011
;; 000000000000000000000001
;; ----------------------------------------------
;; 000000000000000000000100

;; 000000000000000000000111
;; 000000000000000000000011
;; ----------------------------------------------
;; 000000000000000000001010

;; 000000000000000000001011
;; 000000000000000000000101
;; ----------------------------------------------
;; 000000000000000000010000 C

;; 000000000000000000001111
;; 000000000000000000000111
;; ----------------------------------------------
;; 000000000000000000010110 C

;; 000000000000000000010011
;; 000000000000000000001001
;; ----------------------------------------------
;; 000000000000000000001100

;; 000000000000000000010111
;; 000000000000000000001011
;; ----------------------------------------------
;; 000000000000000000010010 C

;; 000000000000000000011011
;; 000000000000000000001101
;; ----------------------------------------------
;; 000000000000000000011000 C

;; 000000000000000000011111
;; 000000000000000000001111
;; ----------------------------------------------
;; 000000000000000000011110 C

;; omega-universe = 8
;; Si-true = 4
;; Probability of Si = Si-true / omega-universe = 4 / 8 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 3 / 4 = 3/4 = 0.75
;; Probability of Ck = Ck-true / omega-universe = 5 / 8 = 5/8 = 0.625

;; 
;;
;; definition of Sk: a bit computed with two operands and a carry, see calculus below:
;;
;;  C        C
;;   n+2      k
;;   1.......b......................11    2b+1
;;            k-1
;;    1......b.......................1    b
;;            k
;;  ----------------------------------
;;  ..S......S.........S.............0    S
;;     n      k         i            0    index
;;
;;

;; > (collatz-proba-Ck-knowing-Si #b100000000000000  #b10)
;; alea = 16384
;; omega-universe = 8192
;; Si-true = 4096
;; Probability of Si = Si-true / omega-universe = 4096 / 8192 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 2049 / 4096 = 2049/4096 = 0.500244140625
;; Probability of Ck = Ck-true / omega-universe = 4097 / 8192 = 4097/8192 = 0.5001220703125

;; > (collatz-proba-Ck-knowing-Si #b100000000000000  #b100)
;; alea = 16384
;; omega-universe = 8192
;; Si-true = 4096
;; Probability of Si = Si-true / omega-universe = 4096 / 8192 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 2048 / 4096 = 1/2 = 0.5
;; Probability of Ck = Ck-true / omega-universe = 4097 / 8192 = 4097/8192 = 0.5001220703125

;; > (collatz-proba-Ck-knowing-Si #b100000000000000  #b1000)
;; alea = 16384
;; omega-universe = 8192
;; Si-true = 4096
;; Probability of Si = Si-true / omega-universe = 4096 / 8192 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 2049 / 4096 = 2049/4096 = 0.500244140625
;; Probability of Ck = Ck-true / omega-universe = 4097 / 8192 = 4097/8192 = 0.5001220703125

;;
;;  (collatz-proba-Ck-knowing-Si #b100000000  #b1000000)
;; alea = 256
;; omega-universe = 128
;; Si-true = 64
;; Probability of Si = Si-true / omega-universe = 64 / 128 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 32 / 64 = 1/2 = 0.5
;; Probability of Ck = Ck-true / omega-universe = 65 / 128 = 65/128 = 0.5078125

;;  (collatz-proba-Ck-knowing-Si #b100000000000000  #b1000000000000)
;; > (collatz-proba-Ck-knowing-Si #b100000000000000
;;                                  #b1000000000000)
;; alea = 16384
;; omega-universe = 8192
;; Si-true = 4096
;; Probability of Si = Si-true / omega-universe = 4096 / 8192 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 2048 / 4096 = 1/2 = 0.5
;; Probability of Ck = Ck-true / omega-universe = 4097 / 8192 = 4097/8192 = 0.5001220703125

;; (collatz-proba-Ck-knowing-Si #b100000000000000
;;                               #b10000000000000)
;; > (collatz-proba-Ck-knowing-Si #b100000000000000  #b10000000000000)
;; alea = 16384
;; omega-universe = 8192
;; Si-true = 4096
;; Probability of Si = Si-true / omega-universe = 4096 / 8192 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 1366 / 4096 = 683/2048 = 0.33349609375
;; Probability of Ck = Ck-true / omega-universe = 4097 / 8192 = 4097/8192 = 0.5001220703125


;;
(define (collatz-proba-Ck-knowing-Si Ck Si)

  (let* ((alea Ck)
	 (omega-universe 0)
	 (Si-true 0)
	 (pSi 0) ; probability of Si
	 (Ck-true 0)
	 (pCkkSi 0) ; probability Ck knowing Si
	 (pCk 0)
	 (Ck-true-knowing-Si 0)
	 (display-enabled #f)
	 (mask (- Ck 1)))
    
    (display "alea = ")
    (display alea)
    (newline)
    
    (for (b alea)
	 
	 (if-t (flag-set? #b1 b) ; only for odd numbers

	       (incf omega-universe)

	       (let ((S (+ (bitwise-and (bitwise-ior 1 (shift-left b)) mask) b))) ; shift to left and set lowest significant bit and mask the upper partial result and finally add b, i.e compute 2b+incfb = 3b+1 but not some high bits

		 (if-t display-enabled
		       (display (padding (bitwise-ior 1 (shift-left b)))) (newline)
		       (display (padding b)) (newline)
		       (display "----------------------------------------------") (newline) 
		       (display (padding S))
		       (if-t (flag-set? Ck S) ; test if carry set
			     (display " C"))
		       (newline)
		       (newline))
		 
		 (if-t (flag-set? Ck S) ; test if carry set
		       (incf Ck-true))

		 (if-t (flag-set? Si S)
		       (incf Si-true)
		       (if-t (flag-set? Ck S)
			     (incf Ck-true-knowing-Si)))))) ; end for
    
    ;; display results
    (set! pSi (/ Si-true omega-universe))
    (set! pCkkSi (/ Ck-true-knowing-Si Si-true))
    (set! pCk (/ Ck-true omega-universe))

    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    
    (display "Si-true = ")
    (display Si-true)
    (newline)
    
    (display "Probability of Si = Si-true / omega-universe = ")
    (display Si-true) (display " / ") (display omega-universe) (display " = ")
    (display pSi) (display " = ") (display (exact->inexact pSi))
    (newline)
    
    (display "Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = ")
    (display Ck-true-knowing-Si) (display " / ") (display Si-true) (display " = ")
    (display pCkkSi)  (display " = ") (display (exact->inexact pCkkSi))
    (newline)
    
    (display "Probability of Ck = Ck-true / omega-universe = ")
    (display Ck-true) (display " / ") (display omega-universe) (display " = ")
    (display pCk)  (display " = ") (display (exact->inexact pCk))
    (newline)

    ))



;; below other ways to compute the same thing (seems more true! but do not change the result)



;; (collatz-proba-Ck-knowing-Si-stat #b100000000000000  #b1000)
;; alea = 4194304
;; omega-universe = 2097152
;; Si-true = 1048576
;; Probability of Si = Si-true / omega-universe = 1048576 / 2097152 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 524544 / 1048576 = 2049/4096 = 0.500244140625
;; Probability of Ck = Ck-true / omega-universe = 1048832 / 2097152 = 4097/8192 = 0.5001220703125
;; > 
;;
;; definition of Sk: a bit computed with two operands and a carry, see calculus below:
;;
;;  C        
;;   n+2     Ck
;;   1.......b......................11    2b+1
;;            k-1
;;    1......b.......................1    b
;;            k
;;  ----------------------------------
;;  ..S......S.........S.............0    S
;;     n      k         i            0    index
;;
;;

;; >  (collatz-proba-Ck-knowing-Si-stat #b100000000  #b10000000000)
;; alea = 65536
;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 8320 / 16384 = 65/128 = 0.5078125
;; Probability of Ck = Ck-true / omega-universe = 16640 / 32768 = 65/128 = 0.5078125
;; >  (collatz-proba-Ck-knowing-Si-stat #b100000000  #b100000000000)
;; alea = 65536
;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 8320 / 16384 = 65/128 = 0.5078125
;; Probability of Ck = Ck-true / omega-universe = 16640 / 32768 = 65/128 = 0.5078125
;; >  (collatz-proba-Ck-knowing-Si-stat #b100000000  #b100000)
;; alea = 65536
;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 8448 / 16384 = 33/64 = 0.515625
;; Probability of Ck = Ck-true / omega-universe = 16640 / 32768 = 65/128 = 0.5078125
;; >  (collatz-proba-Ck-knowing-Si-stat #b100000000  #b1000000)
;; alea = 65536
;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 8192 / 16384 = 1/2 = 0.5
;; Probability of Ck = Ck-true / omega-universe = 16640 / 32768 = 65/128 = 0.5078125
;; >  (collatz-proba-Ck-knowing-Si-stat #b100000000 #b100000000  )
;; alea = 65536
;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 8320 / 16384 = 65/128 = 0.5078125
;; Probability of Ck = Ck-true / omega-universe = 16640 / 32768 = 65/128 = 0.5078125
;; >  (collatz-proba-Ck-knowing-Si-stat #b100000000 #b10000000  )
;; alea = 65536
;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 5632 / 16384 = 11/32 = 0.34375
;; Probability of Ck = Ck-true / omega-universe = 16640 / 32768 = 65/128 = 0.5078125
;; >  (collatz-proba-Ck-knowing-Si-stat #b100000000 #b1000000  )
;; alea = 65536
;; omega-universe = 32768
;; Si-true = 16384
;; Probability of Si = Si-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = 8192 / 16384 = 1/2 = 0.5
;; Probability of Ck = Ck-true / omega-universe = 16640 / 32768 = 65/128 = 0.5078125
;; > 
(define (collatz-proba-Ck-knowing-Si-stat Ck Si)

  (let* ((alea (shift-left Ck 8)) 
	 (omega-universe 0)
	 (Si-true 0)
	 (pSi 0) ; probability of Si
	 (Ck-true 0)
	 (pCkkSi 0) ; probability Ck knowing Si
	 (pCk 0)
	 (Ck-true-knowing-Si 0)
	 (display-enabled #f)
	 (display-enabled2 #f)
	 (mask (- Ck 1)))
    
    (display "alea = ")
    (display alea)
    (newline)
    
    (for (b alea)
	 
	 (if-t (flag-set? #b1 b) ; only for odd numbers

	       (incf omega-universe)

	       (let* ((S-masked (+ (bitwise-and (bitwise-ior 1 (shift-left b)) mask)
				   (bitwise-and b mask))) ; shift to left and set lowest significant bit and mask the upper partial result and finally add b masked too, i.e compute 2b+incfb = 3b+1 but not some high bits that will be the carry
		      (Ck-set (flag-set? Ck S-masked)) ;; Ck flag
		      (S (+ (bitwise-ior 1 (shift-left b)) b)) ; shift to left and set lowest significant bit and finally add b, i.e compute 2b+incfb = 3b+1
		      (Si-set (flag-set? Si S))) ;; Si flag
		      
		 (if-t display-enabled
		       (display (padding (bitwise-ior 1 (shift-left b)))) (newline)
		       (display (padding b)) (newline)
		       (display "----------------------------------------------") (newline) 
		       (display (padding S))
		       (if-t Ck-set
			     (display " C"))
		       (newline)
		       (newline))
		 
		 (if-t Ck-set
		       (incf Ck-true))

		 (if-t Si-set
		       (incf Si-true)
		       (if-t Ck-set
			     (if-t display-enabled2
				   (display (padding (bitwise-ior 1 (shift-left b)))) (newline)
				   (display (padding b)) (newline)
				   (display "----------------------------------------------") (newline) 
				   (display (padding S))
				   (if-t Ck-set
					 (display " C"))
				   (newline)
				   (newline))
			     (incf Ck-true-knowing-Si)))))) ; end for
    
    ;; display results
    (set! pSi (/ Si-true omega-universe))
    (set! pCkkSi (/ Ck-true-knowing-Si Si-true))
    (set! pCk (/ Ck-true omega-universe))

    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    
    (display "Si-true = ")
    (display Si-true)
    (newline)
    
    (display "Probability of Si = Si-true / omega-universe = ")
    (display Si-true) (display " / ") (display omega-universe) (display " = ")
    (display pSi) (display " = ") (display (exact->inexact pSi))
    (newline)
    
    (display "Probability of Ck knowing Si = Ck-true-knowing-Si / Si-true = ")
    (display Ck-true-knowing-Si) (display " / ") (display Si-true) (display " = ")
    (display pCkkSi)  (display " = ") (display (exact->inexact pCkkSi))
    (newline)
    
    (display "Probability of Ck = Ck-true / omega-universe = ")
    (display Ck-true) (display " / ") (display omega-universe) (display " = ")
    (display pCk)  (display " = ") (display (exact->inexact pCk))
    (newline)

    ))




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
;; >  (collatz-proba-Ck-knowing-Bk-1 #b1000000000 1)
;; alea = 512
;; omega-universe = 256
;; Bk-1-true = 128
;; Probability of Bk-1 = Bk-1-true / omega-universe = 128 / 256 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 86 / 128 = 43/64 = 0.671875
;; Probability of Ck = Ck-true / omega-universe = 129 / 256 = 129/256 = 0.50390625

;; if computed with 3b we have a perfect 1/2 probability for Ck:

;; (collatz-proba-Ck-knowing-Bk-1 #b1000000000 0)
;; alea = 512
;; omega-universe = 256
;; Bk-1-true = 128
;; Probability of Bk-1 = Bk-1-true / omega-universe = 128 / 256 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 85 / 128 = 85/128 = 0.6640625
;; Probability of Ck = Ck-true / omega-universe = 128 / 256 = 1/2 = 0.5

;; > (collatz-proba-Ck-knowing-Bk-1 #b10000000000000 1)
;; alea = 8192
;; omega-universe = 4096
;; Bk-1-true = 2048
;; Probability of Bk-1 = Bk-1-true / omega-universe = 2048 / 4096 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 1366 / 2048 = 683/1024 = 0.6669921875

					; seems to converge to 2/3

;; Probability of Ck = Ck-true / omega-universe = 2049 / 4096 = 2049/4096 = 0.500244140625

;;
 
(define (collatz-proba-Ck-knowing-Bk-1 Ck C0)

  (let* ((Bk-1 (shift-right Ck))
	 (alea Ck)
	 (omega-universe 0)
	 (Bk-1-true 0)
	 (pBk-1 0) ; probability of Bk-1
	 (Ck-true 0)
	 (pCkkBk-1 0) ; probability Ck knowing Bk-1
	 (pCk 0)
	 (Ck-true-knowing-Bk-1 0)
	 (display-enabled #t)
	 (mask (- Ck 1)))
   
    (display "alea = ")
    (display alea)
    (newline)

    (for (b alea)

	 ;;(incf omega-universe)
	 
	 (if-t (flag-set? #b1 b) ; only for odd numbers

	       (incf omega-universe)

	       (let* ((2b (shift-left b))    ; 2*b
		      (a (bitwise-ior C0 2b)) ; a = 2b + C0
		      (S (+ (bitwise-and a mask) b))) ; compute 3b or compute 2b+incfb = 3b+1 and mask the upper partial result
		 
		 (if-t display-enabled
		       (display (padding (bitwise-ior 1 (shift-left b)))) (newline)
		       (display (padding b))
		       (if-t (flag-set? Bk-1 b)
			     (display " Bk-1 set"))
		       (newline)
		       (display "------------------------") (newline) 
		       (display (padding S))
		       (if-t (flag-set? Ck S) ; test if carry set
			     (display " C set"))
		       (newline)
		       (newline))
		 
		 (if-t (flag-set? Ck S) ; test if carry set
		       (incf Ck-true))

		 (if-t (flag-set? Bk-1 b)
		       (incf Bk-1-true)
		       (if-t (flag-set? Ck S)
			     (incf Ck-true-knowing-Bk-1)))))) ; end for
    
    ;; display results
    (set! pBk-1 (/ Bk-1-true omega-universe))
    (set! pCkkBk-1 (/ Ck-true-knowing-Bk-1 Bk-1-true))
    (set! pCk (/ Ck-true omega-universe))

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

    ))


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
;; >  (collatz-proba-Ck-knowing-Bk-1-stat #b1000000000 1)
;; alea = 131072
;; omega-universe = 65536
;; Bk-1-true = 32768
;; Probability of Bk-1 = Bk-1-true / omega-universe = 32768 / 65536 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 22016 / 32768 = 43/64 = 0.671875
;; Probability of Ck = Ck-true / omega-universe = 33024 / 65536 = 129/256 = 0.50390625

;; > (collatz-proba-Ck-knowing-Bk-1-stat #b1000000000 0)
;; alea = 131072
;; omega-universe = 65536
;; Bk-1-true = 32768
;; Probability of Bk-1 = Bk-1-true / omega-universe = 32768 / 65536 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 21760 / 32768 = 85/128 = 0.6640625
;; Probability of Ck = Ck-true / omega-universe = 32768 / 65536 = 1/2 = 0.5
;; > ; seems to converge to 2/3
(define (collatz-proba-Ck-knowing-Bk-1-stat Ck C0)

  (let* ((Bk-1 (shift-right Ck))
	 (alea (shift-left Ck 8))
	 (omega-universe 0)
	 (Bk-1-true 0)
	 (pBk-1 0) ; probability of Bk-1
	 (Ck-true 0)
	 (pCkkBk-1 0) ; probability Ck knowing Bk-1
	 (pCk 0)
	 (Ck-true-knowing-Bk-1 0)
	 (display-enabled #f)
	 (mask (- Ck 1)))
   
    (display "alea = ")
    (display alea)
    (newline)

    (for (b alea)

	 ;;(incf omega-universe)
	 
	 (if-t (flag-set? #b1 b) ; only for odd numbers

	       (incf omega-universe)

	       (let* ((S-masked (+ (bitwise-and (bitwise-ior C0 (shift-left b)) mask)
				   (bitwise-and b mask))) ; shift to left and set lowest significant bit and mask the upper partial result and finally add b masked too, i.e compute 2b+incfb = 3b+1 but not some high bits that will be the carry
		      (Ck-set (flag-set? Ck S-masked)) ;; Ck flag
		      (S (+ (bitwise-ior C0 (shift-left b)) b)) ; shift to left and set lowest significant bit and finally add b, i.e compute 2b+incfb = 3b+1
		      (Bk-1-set (flag-set? Bk-1 b))) ;; Bk-1 flag
		      
		 
		 (if-t display-enabled
		       (display (padding (bitwise-ior C0 (shift-left b)))) (newline)
		       (display (padding b))
		       (if-t Bk-1-set
			     (display " Bk-1 set"))
		       (newline)
		       (display "------------------------") (newline) 
		       (display (padding S))
		       (if-t Ck-set ; test if carry set
			     (display " C set"))
		       (newline)
		       (newline))
		 
		 (if-t Ck-set ; test if carry set
		       (incf Ck-true))

		 (if-t Bk-1-set
		       (incf Bk-1-true)
		       (if-t Ck-set
			     (incf Ck-true-knowing-Bk-1)))))) ; end for
    
    ;; display results
    (set! pBk-1 (/ Bk-1-true omega-universe))
    (set! pCkkBk-1 (/ Ck-true-knowing-Bk-1 Bk-1-true))
    (set! pCk (/ Ck-true omega-universe))

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

    ))




;; compute probability of Ck knowing Bk-1 for Collatz (3x+1) function and 3x function
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
;;     n      k         i            0     index
;;
;;


;; > (collatz-3x-3x+1-proba-Ck-knowing-Bk-1 #b100000)
;; k = 5
;; alea = 32
;; mask =                    11111
;;
;; ...
;;
;;                                                             
;;                     1111                                 11       Carries
;;                   100110                               100110     2b
;;                    10011                                10011      b
;; ------------------------             ------------------------
;;                   111010                               111001     Sum
;;
;;                   C                                               Carry after the mask
;;                  1111111                                          Carries
;;                   101010                               101010     2b
;;                    10101                                10101      b
;; ------------------------ CARRY       ------------------------
;;                  1000000                               111111     Sum
;;
;;                   C                                    C          Carry after the mask
;;                  1111111                              11111       Carries
;;                   101110                               101110     2b
;;                    10111                                10111      b
;; ------------------------ CARRY CARRY ------------------------
;;                  1000110                              1000101     Sum
;;
;; ...
;;
;; omega-universe = 16
;; Bk-1-true = 8
;; Probability of Bk-1 = Bk-1-true / omega-universe = 8 / 16 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 6 / 8 = 3/4 = 0.75
;; Probability of Ck for 3x+1 = Ck-true / omega-universe = 9 / 16 = 9/16 = 0.5625
;; Probability of Ck for 3x = Ck-true-3x / omega-universe = 8 / 16 = 1/2 = 0.5
;;
;; k = 5
;; P(Ck) = 1/2 + (1/2^(k-1)) = 0.5625
;;
;;
;;
;; P(Ck) = 1/2 + (1/2^(k-1)) cf page 12-13  et 3
;;
;;
;;

;; > (collatz-3x-3x+1-proba-Ck-knowing-Bk-1 #b10000000000)
;; k = 10
;; alea = 1024
;; mask =               1111111111
;;
;; ...
;;
;;                     1111                                 11       Carries
;;               1010100110                           1010100110     2b
;;                101010011                            101010011      b
;; ------------------------             ------------------------
;;               1111111010                           1111111001     Sum
;;
;;               ##########                           ##########    Mask
;;              C                                                    Carry after the mask
;;              11111111111                                          Carries
;;               1010101010                           1010101010     2b
;;                101010101                            101010101      b
;; ------------------------ CARRY       ------------------------
;;              10000000000                           1111111111     Sum
;;
;;               ##########                           ##########    Mask
;;              C                                    C               Carry after the mask
;;              11111111111                          111111111       Carries
;;               1010101110                           1010101110     2b
;;                101010111                            101010111      b
;; ------------------------ CARRY CARRY ------------------------
;;              10000000110                          10000000101     Sum
;; ...
;;
;;
;; omega-universe = 512
;; Bk-1-true = 256
;; Probability of Bk-1 = Bk-1-true / omega-universe = 256 / 512 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 171 / 256 = 171/256 = 0.66796875
;; Probability of Ck for 3x+1 = Ck-true / omega-universe = 257 / 512 = 257/512 = 0.501953125
;; Probability of Ck for 3x = Ck-true-3x / omega-universe = 256 / 512 = 1/2 = 0.5
;; k = 10
;; P(Ck) = 1/2 + (1/2^(k-1)) = 0.501953125
;;
;;

;; > (collatz-3x-3x+1-proba-Ck-knowing-Bk-1 #b10000000000000000)
;; k = 16
;; alea = 65536
;; omega-universe = 32768
;; Bk-1-true = 16384
;; Probability of Bk-1 = Bk-1-true / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 10923 / 16384 = 10923/16384 = 0.66668701171875

					; seems to converge to 2/3

;; Probability of Ck for 3x+1 = Ck-true / omega-universe = 16385 / 32768 = 16385/32768 = 0.500030517578125
;; Probability of Ck for 3x = Ck-true-3x / omega-universe = 16384 / 32768 = 1/2 = 0.5
;; k = 16
;; P(Ck) = 1/2 + (1/2^(k-1)) = 0.500030517578125
;;

(define (collatz-3x-3x+1-proba-Ck-knowing-Bk-1 Ck)

  ;; initialisation of variables
  (let* (
	 (k (- (size-bit Ck) 1))
	 (Bk-1 (shift-right Ck)) ;; ex: Ck = 100000, Bk-1 = 10000
	 (alea Ck) ;; 1000...00 = 111...11 + 1 , example with Ck, alea = 100000 
	 (omega-universe 0)
	 (Bk-1-true 0)
	 (pBk-1 0) ; probability of Bk-1
	 (Ck-true 0)
	 (Ck-true-3x 0)
	 (pCkkBk-1 0) ; probability Ck knowing Bk-1
	 (pCk 0)
	 (pCk-3x 0)
	 (Ck-true-knowing-Bk-1 0)
	 (Ck-true-knowing-NOT-Bk-1 0)
	 (Bk-1-false 0)
	 (pCkkNotBk-1 0) ; probability Ck knowing Bk-1 is false
	 (display-enabled #t)
	 (mask (- Ck 1))) ;; example with Ck, mask = 11111

    (display "k = ") (display k) (newline)
    (display "alea = ") (display alea) (newline)
    (display "mask = ") (display (padding-spc mask)) (newline)

    ;; loop over numbers
    (for (b alea) ; b is in range of 0 to 111...11 , alea being 1000...00 = 111...11 + 1 , example with Ck = 100000, b is in range of 0 to 11111
	 
	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       
	       (incf omega-universe)

	       ;; computation

	       (let* ((2b (shift-left b))    ; 2*b
		      (a (bitwise-ior 1 2b)) ; a = 2b + 1
		      ;;(S (+ (bitwise-and a mask) b)) ; compute a + b = 2b + 1 + b = 3b + 1 and mask the upper partial result to show carry
		      (S_up_to_k-1_Carry (+ (bitwise-and a mask) b)) ; compute k-1 LSBits of a + b = 2b + 1 + b = 3b + 1 and mask the upper partial result to show carry
		      (S (+ a b)) ; compute a + b = 2b + 1 + b = 3b + 1
		      ;;(S_3b (+ (bitwise-and 2b mask) b))  ; compute 3b
		      (S_3b_up_to_k-1_Carry (+ (bitwise-and 2b mask) b))  ; compute k-1 LSBits of 3b and mask the upper partial result to show carry
		      (S_3b (+  2b b))  ; compute 3b
		      (C (compute-carries-bin-numeric b)) ; compute Carries
		      (C_3b (compute-carries-bin-numeric-3b b)))

		 
		 ;; displaying
		 
		 (if-t display-enabled
		       (display "              3b+1                   ") (display "             ") (display "     3b") (newline)
		  
		       ;;(display (binary-string->sharp (padding-spc mask))) (display "             ") (display (binary-string->sharp (padding-spc mask))) (displayln "    Mask")
		       (display (padding-spc mask)) (display "             ") (display (padding-spc mask)) (displayln "    Mask")
		       (if (flag-set? Ck S_up_to_k-1_Carry) ; test if carry set on Ck of S_up_to_k-1_Carry
			   (display (binary-string->carries-c (padding-spc Ck)))
			   (display (binary-string->carries-c (padding-spc 0))))
		       (display "             ")
		       (if (flag-set? Ck S_3b_up_to_k-1_Carry) ; test if carry set on Ck of Sum of 3b
			   (display (binary-string->carries-c (padding-spc Ck)))
			   (display (binary-string->carries-c(padding-spc 0))))
		       (if (or
			    (flag-set? Ck S_up_to_k-1_Carry)
			    (flag-set? Ck S_3b_up_to_k-1_Carry))
			   (displayln "     Carry after the mask")
			   (newline))
		       (display (binary-string->carries (padding-spc C))) (display "             ") (display (binary-string->carries (padding-spc C_3b))) (display-nl "     Carries") ;; carries
		       (display (padding-spc 2b)) (display "             ") (display (padding-spc 2b))  (display-nl "     2b") ;; 2b and 2b
		       (display (padding-spc b)) (display "             ") (display (padding-spc b))  (display-nl "      b") ;; b
		       (display "------------------------")
		       (if (flag-set? Ck S_up_to_k-1_Carry) ; test if carry set on Ck of S_up_to_k-1_Carry
			   (display " CARRY ")
			   (display "       "))
		       (if (flag-set? Ck S_3b_up_to_k-1_Carry) ; test if carry set on Ck of Sum of 3b
			   (display "CARRY ")
			   (display "      "))
		       (display "------------------------") (newline) 
		       (display (padding-spc S)) (display "             ") (display (padding-spc S_3b))  (display-nl "     Sum") ;; 3b+1 and 3b		       
		       (newline))

		 ;; cross-checking the carries
		 (if-t (or
			(and (flag-set? Ck S_up_to_k-1_Carry)
			     (not (flag-set? Ck C)))
			(and (flag-set? Ck S_3b_up_to_k-1_Carry)
			     (not (flag-set? Ck C_3b))))
		       (newline)
		       (displayln "ERROR : carries cross-checking fails !!!")
		       (newline))
		 
		 
		 ;; statistics
		 
		 (if-t (flag-set? Ck S_up_to_k-1_Carry) ; test if carry set for S_up_to_k-1_Carry
		       (incf Ck-true))

		 (if-t (flag-set? Ck S_3b_up_to_k-1_Carry) ; test if carry set for S'_up_to_k-1_Carry
		       (incf Ck-true-3x))
		 
		 (if (flag-set? Bk-1 b) ; test if Bk-1 was up
		     (begin 
		       (incf Bk-1-true)
		       (if-t (flag-set? Ck S_up_to_k-1_Carry) ; test if Ck was set in S_up_to_k-1_Carry
			     (incf Ck-true-knowing-Bk-1)))
		     (begin
		       (incf Bk-1-false)
		       (if-t (flag-set? Ck S_up_to_k-1_Carry) ; test if Ck was set in S_up_to_k-1_Carry
			     (incf Ck-true-knowing-NOT-Bk-1))))

		 ))) ; end let/if/for

    
    ;; probabilities computed from statistics
   
    (set! pBk-1 (/ Bk-1-true omega-universe))
    (set! pCkkBk-1 (/ Ck-true-knowing-Bk-1 Bk-1-true))
    (set! pCk (/ Ck-true omega-universe))
    (set! pCkkNotBk-1 (/ Ck-true-knowing-NOT-Bk-1 Bk-1-false))
    
     ;; display results
    
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

    (display "Probability of Ck knowing Bk-1 is false = Ck-true-knowing-NOT-Bk-1 / Bk-1-false = ")
    (display Ck-true-knowing-NOT-Bk-1) (display " / ") (display Bk-1-false) (display " = ")
    (display pCkkNotBk-1)  (display " = ") (display (exact->inexact pCkkNotBk-1))
    (newline)
    
    (display "Probability of Ck for 3x+1 = Ck-true / omega-universe = ")
    (display Ck-true) (display " / ") (display omega-universe) (display " = ")
    (display pCk)  (display " = ") (display (exact->inexact pCk))
    (newline)

    (set! pCk-3x (/ Ck-true-3x omega-universe))
    (display "Probability of Ck for 3x = Ck-true-3x / omega-universe = ")
    (display Ck-true-3x) (display " / ") (display omega-universe) (display " = ")
    (display pCk-3x)  (display " = ") (display (exact->inexact pCk-3x))
    (newline)
    (newline)

    (display "k = ") (display k) (newline)
    ;;(display "P(Ck) = 1/2 + (1/2^(k-1)) = ") (display (0.5 . + . (1 . / . (expt 2 (k . - . 1)))))
    (display "P(Ck) = 1/2 + (1/2^(k-1)) = ") (display (+ 0.5  (/ 1  (expt 2 (- k  1)))))
    (newline)
    
    ))



;; la suite de collatz serait convergente si la parité du résultat à chaque étape de la suite de collatz compressée
;; était equiprobable


;; >  (syracuse-probabilistic-heuristic-n-bits 3)
;; mb = 5
;; m = 7
;;                        0 ->                        1
;;                        1 ->                      100
;;                       10 ->                      111
;;                       11 ->                     1010
;;                      100 ->                     1101
;;                      101 ->                    10000
;;                      110 ->                    10011
;;                      111 ->                    10110
;; v-zero-sum = #(4 4 4 3 2)
;; v-one-sum = #(4 4 4 2 3)
(define (syracuse-probabilistic-heuristic-n-bits n)
  
  (let* ((mb (+ n 2)) ;; maximum number of bits used
	 (m (- (expt 2 n) 1)) ;; 2^n - 1 is the maximum number to test
	 (v-zero-sum  (make-vector mb 0)) ;; to count the zeros
	 (v-one-sum (make-vector mb 0)) ;; to count the ones
	 (fx 0) ;; result of f(x) with f : x -> 3x+1 
	 )

    (dv mb)
    (dv m)

    (for (x 0 m) ;; x : input number
	 
	 (set! fx (mult3add1 x))
	 (display (padding-spc x))
	 (display " -> ")
	 (display (padding-spc fx)) (newline)
	 
	 (for (k 0 (- mb 1)) ;; k : bit number

	      (if (bit-test? fx k)
		  (vector-set! v-one-sum 
			       k 
			       (+ (vector-ref v-one-sum k) 1))
		  (when (or (< k n) (> fx m))
			    ;;(and (> fx m)
				 ;;(< k (- mb 1))))
			(vector-set! v-zero-sum 
				     k 
				     (+ (vector-ref v-zero-sum k) 1)))))

	 ;;(dv v-zero-sum)
	 ;;(dv v-one-sum)
	 )
    (dv v-zero-sum)
    (dv v-one-sum)
    ))


(define (syracuse-odd-probabilistic-heuristic-n-bits n)
  
  (let* ((mb (+ n 2)) ;; maximum number of bits used
	 (m (- (expt 2 n) 1)) ;; 2^n - 1 is the maximum number to test
	 (v-zero-sum  (make-vector mb 0)) ;; to count the zeros
	 (v-one-sum (make-vector mb 0)) ;; to count the ones
	 (fx 0) ;; result of f(x) with f : x -> 3x+1 
	 )

    (dv mb)
    (dv m)

    (for (x 1 m 2) ;; x : input number
	 
	 (set! fx (mult3add1 x))
	 (display (padding-spc x))
	 (display " -> ")
	 (display (padding-spc fx)) (newline)
	 
	 (for (k 0 (- mb 1)) ;; k : bit number

	      (if (bit-test? fx k)
		  (vector-set! v-one-sum 
			       k 
			       (+ (vector-ref v-one-sum k) 1))
		  (when (or (< k n) (> fx m))
			    ;;(and (> fx m)
				 ;;(< k (- mb 1))))
			(vector-set! v-zero-sum 
				     k 
				     (+ (vector-ref v-zero-sum k) 1)))))

	 ;;(dv v-zero-sum)
	 ;;(dv v-one-sum)
	 )
    (dv v-zero-sum)
    (dv v-one-sum)
    ))



;; 2^3= 8 = 1000


;; > (syracuse-compressed-probabilistic-heuristic-n-bits 3)
;; mb = 5
;; m = 7
;;                        1 ->                       10
;;                       11 ->                      101
;;                      101 ->                     1000
;;                      111 ->                     1011
;; v-zero-sum = #(2 2 3 0 2)
;; v-one-sum = #(2 2 1 2 0)
;; > (syracuse-compressed-probabilistic-heuristic-n-bits 5)
;; mb = 7
;; m = 31
;;                        1 ->                       10
;;                       11 ->                      101
;;                      101 ->                     1000
;;                      111 ->                     1011
;;                     1001 ->                     1110
;;                     1011 ->                    10001
;;                     1101 ->                    10100
;;                     1111 ->                    10111
;;                    10001 ->                    11010
;;                    10011 ->                    11101
;;                    10101 ->                   100000
;;                    10111 ->                   100011
;;                    11001 ->                   100110
;;                    11011 ->                   101001
;;                    11101 ->                   101100
;;                    11111 ->                   101111
;; v-zero-sum = #(8 8 8 8 11 0 6)
;; v-one-sum = #(8 8 8 8 5 6 0)
;; > 
(define (syracuse-compressed-probabilistic-heuristic-n-bits n)
  
  (let* ((mb (+ n 2)) ;; maximum number of bits used
	 (m (- (expt 2 n) 1)) ;; 2^n - 1 is the maximum number to test
	 (v-zero-sum  (make-vector mb 0)) ;; to count the zeros
	 (v-one-sum (make-vector mb 0)) ;; to count the ones
	 (fx 0) ;; result of f(x) with f : x -> 3x+1 / 2
	 )

    (dv mb)
    (dv m)

    (for (x 1 m 2) ;; x : input number
	 
	 (set! fx (mult3add1div2 x))
	 (display (padding-spc x))
	 (display " -> ")
	 (display (padding-spc fx)) (newline)
	 
	 (for (k 0 (- mb 1)) ;; k : bit number

	      (if (bit-test? fx k)
		  (vector-set! v-one-sum 
			       k 
			       (+ (vector-ref v-one-sum k) 1))
		  (when (or (< k n) (> fx m))
			    ;;(and (> fx m)
				 ;;(< k (- mb 1))))
			(vector-set! v-zero-sum 
				     k 
				     (+ (vector-ref v-zero-sum k) 1)))))

	 ;;(dv v-zero-sum)
	 ;;(dv v-one-sum)
	 )
    (dv v-zero-sum)
    (dv v-one-sum)
    ))


;; > (double-heuristic-n-bits 3)
;; mb = 5
;; m = 7
;;                        0 ->                        0
;;                        1 ->                       10
;;                       10 ->                      100
;;                       11 ->                      110
;;                      100 ->                     1000
;;                      101 ->                     1010
;;                      110 ->                     1100
;;                      111 ->                     1110
;; v-zero-sum = #(8 4 4 0 4)
;; v-one-sum = #(0 4 4 4 0)
(define (double-heuristic-n-bits n)
  
  (let* ((mb (+ n 2)) ;; maximum number of bits used
	 (m (- (expt 2 n) 1)) ;; 2^n - 1 is the maximum number to test
	 (v-zero-sum  (make-vector mb 0)) ;; to count the zeros
	 (v-one-sum (make-vector mb 0)) ;; to count the ones
	 (f (lambda (x) (shift-left x)))
	 (fx 0) ;; result of f(x)
	 )

    (dv mb)
    (dv m)

    (for (x 0 m) ;; x : input number
	 
	 (set! fx (f x))
	 (display (padding-spc x))
	 (display " -> ")
	 (display (padding-spc fx)) (newline)
	 
	 (for (k 0 (- mb 1)) ;; k : bit number

	      (if (bit-test? fx k)
		  (vector-set! v-one-sum 
			       k 
			       (+ (vector-ref v-one-sum k) 1))
		  (when (or (< k n) (> fx m))
			    ;;(and (> fx m)
				 ;;(< k (- mb 1))))
			(vector-set! v-zero-sum 
				     k 
				     (+ (vector-ref v-zero-sum k) 1)))))

	 ;;(dv v-zero-sum)
	 ;;(dv v-one-sum)
	 )
    (dv v-zero-sum)
    (dv v-one-sum)
    ))

;; > (double-odd-heuristic-n-bits 3)
;; mb = 5
;; m = 7
;;                        1 ->                       10
;;                       11 ->                      110
;;                      101 ->                     1010
;;                      111 ->                     1110
;; v-zero-sum = #(4 0 2 0 2)
;; v-one-sum = #(0 4 2 2 0)
;; > (double-odd-heuristic-n-bits 4)
;; mb = 6
;; m = 15
;;                        1 ->                       10
;;                       11 ->                      110
;;                      101 ->                     1010
;;                      111 ->                     1110
;;                     1001 ->                    10010
;;                     1011 ->                    10110
;;                     1101 ->                    11010
;;                     1111 ->                    11110
;; v-zero-sum = #(8 0 4 4 0 4)
;; v-one-sum = #(0 8 4 4 4 0)
(define (double-odd-heuristic-n-bits n)
  
  (let* ((mb (+ n 2)) ;; maximum number of bits used
	 (m (- (expt 2 n) 1)) ;; 2^n - 1 is the maximum number to test
	 (v-zero-sum  (make-vector mb 0)) ;; to count the zeros
	 (v-one-sum (make-vector mb 0)) ;; to count the ones
	 (f (lambda (x) (shift-left x)))
	 (fx 0) ;; result of f(x)
	 )

    (dv mb)
    (dv m)

    (for (x 1 m 2) ;; x : input number
	 
	 (set! fx (f x))
	 (display (padding-spc x))
	 (display " -> ")
	 (display (padding-spc fx)) (newline)
	 
	 (for (k 0 (- mb 1)) ;; k : bit number

	      (if (bit-test? fx k)
		  (vector-set! v-one-sum 
			       k 
			       (+ (vector-ref v-one-sum k) 1))
		  (when (or (< k n) (> fx m))
			    ;;(and (> fx m)
				 ;;(< k (- mb 1))))
			(vector-set! v-zero-sum 
				     k 
				     (+ (vector-ref v-zero-sum k) 1)))))

	 ;;(dv v-zero-sum)
	 ;;(dv v-one-sum)
	 )
    (dv v-zero-sum)
    (dv v-one-sum)
    ))



;; given n bits check all the number between 0 and  2^n - 1
;; TODO: replace bit by digit

;; > (mult3-proba-n-bits 3)
;; mb = 5
;; m = 7
;; v-zero-sum = #(1 1 1 0 0)
;; v-one-sum = #(0 0 0 0 0)
;; v-zero-sum = #(1 1 2 0 0)
;; v-one-sum = #(1 1 0 0 0)
;; v-zero-sum = #(2 1 2 0 0)
;; v-one-sum = #(1 2 1 0 0)
;; v-zero-sum = #(2 2 3 0 1)
;; v-one-sum = #(2 2 1 1 0)
;; v-zero-sum = #(3 3 3 0 2)
;; v-one-sum = #(2 2 2 2 0)
;; v-zero-sum = #(3 3 3 0 3)
;; v-one-sum = #(3 3 3 3 0)
;; v-zero-sum = #(4 3 4 1 3)
;; v-one-sum = #(3 4 3 3 1)
;; v-zero-sum = #(4 4 4 2 3)
;; v-one-sum = #(4 4 4 3 2)
(define (mult3-proba-n-bits n)
  
  (let* ((mb (+ n 2)) ;; maximum number of bits used
	 (m (- (expt 2 n) 1)) ;; 2^n - 1 is the maximum number to test
	 (v-zero-sum  (make-vector mb 0)) ;; to count the zeros
	 (v-one-sum (make-vector mb 0)) ;; to count the ones
	 (fx 0) ;; result of f(x) with f : x -> 3x+1 
	 )

    (dv mb)
    (dv m)

    (for (x 0 m)
	 
	 (set! fx (mult3 x))
	 
	 (for (k 0 (- mb 1))
	      
	      
	      (if (bit-test? fx k)
		  (vector-set! v-one-sum 
			       k 
			       (+ (vector-ref v-one-sum k) 1))
		  (when (or (< k n) (> fx m))
			(vector-set! v-zero-sum 
				     k 
				     (+ (vector-ref v-zero-sum k) 1)))))

	 (dv v-zero-sum)
	 (dv v-one-sum))))


(if  (equal? (current-command-line-arguments) '#())
     (display-nl "If running the application in a standalone terminal then an argument must be provided otherwise ignore this message.")
     (begin
       (display-nl (string-red "Testing color in terminal: this string should display in RED."))
		   (collatz-verbose (string->number (vector-ref (current-command-line-arguments) 0)))))


;; compute probability of Cn+1 knowing Bk-1 for Collatz (3x+1) function and 3x function
;;
;; definition of Sk: a bit computed with two operands and a carry, see calculus below:
;;
;; index reference k are taken relative to number b (b,result S and carries C have same index )
;;
;;
;;  C   C       C         C             C    C0 = 1 for 3x+1 computation and C0 = 0 for 3x computation
;;   n+2 n+1     k         i             0
;;      1.......b........b.............10    2b + C0 = a with a = b
;;               k-1      i                                    k   k-1
;;       1.......b........b.............1    b
;;                k-1      i
;;     ----------------------------------
;;  S   .S......S.........S.............0    S
;;   n+2  n      k         i            0     index
;;
;;
(define (collatz-3x-3x+1-proba-Cn+1-knowing-Bn-1 Cn+1)

  ;; initialisation of variables
  (let* (
	 (n+1 (- (size-bit Cn+1) 1))
	 (Bn-1 (shift-right
		(shift-right Cn+1))) ;; ex: Cn+1 = 100000, Bn-1 = 1000
	 (omega-universe 0)
	 (Bn-1-true 0)
	 (pBn-1 0) ; probability of Bn-1
	 (Cn+1-true 0)
	 (Cn+1-true-3x 0)
	 (pCn+1kBn-1 0) ; probability Cn+1 knowing Bn-1
	 (pCn+1 0)
	 (pCn+1-3x 0)
	 (Cn+1-true-knowing-Bn-1 0)
	 (Cn+1-true-knowing-NOT-Bn-1 0)
	 (Bn-1-false 0)
	 (pCn+1kNotBn-1 0) ; probability Cn+1 knowing Bn-1 is false
	 (to (- Cn+1 1))
	 (from Bn-1)
	 (display-enabled #t)
	 (mask (- Cn+1 1))) ;; example with Cn+1, mask = 11111

    (display "n+1 = ") (display n+1) (newline)
    (display "mask = ") (display (padding-spc mask)) (newline)
    (dv from)
    (dv to)

    ;; loop over numbers
    (for (b from to) ; b is in range of 11...11 to 111...11 , example with Cn+1 = 100000, b is in range of 1111 to 11111
	 
	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       
	       (incf omega-universe)

	       ;; computation

	       (let* ((2b (shift-left b))    ; 2*b
		      (a (bitwise-ior 1 2b)) ; a = 2b + 1
		      ;;(S (+ (bitwise-and a mask) b)) ; compute a + b = 2b + 1 + b = 3b + 1 and mask the upper partial result to show carry
		      (S_up_to_n_Carry (+ (bitwise-and a mask) b)) ; compute n LSBits of a + b = 2b + 1 + b = 3b + 1 and mask the upper partial result to show carry
		      (S (+ a b)) ; compute a + b = 2b + 1 + b = 3b + 1
		      ;;(S_3b (+ (bitwise-and 2b mask) b))  ; compute 3b
		      (S_3b_up_to_n_Carry (+ (bitwise-and 2b mask) b))  ; compute n LSBits of 3b and mask the upper partial result to show carry
		      (S_3b (+  2b b))  ; compute 3b
		      (C (compute-carries-bin-numeric b)) ; compute Carries
		      (C_3b (compute-carries-bin-numeric-3b b)))

		 
		 ;; displaying
		 
		 (if-t display-enabled
		       (display "              3b+1                   ") (display "             ") (display "     3b") (newline)
		  
		       ;;(display (binary-string->sharp (padding-spc mask))) (display "             ") (display (binary-string->sharp (padding-spc mask))) (displayln "    Mask")
		       (display (padding-spc mask)) (display "             ") (display (padding-spc mask)) (displayln "    Mask")
		       (if (flag-set? Cn+1 S_up_to_n_Carry) ; test if carry set on Cn+1 of S_up_to_n_Carry
			   (display (binary-string->carries-c (padding-spc Cn+1)))
			   (display (binary-string->carries-c (padding-spc 0))))
		       (display "             ")
		       (if (flag-set? Cn+1 S_3b_up_to_n_Carry) ; test if carry set on Cn+1 of Sum of 3b
			   (display (binary-string->carries-c (padding-spc Cn+1)))
			   (display (binary-string->carries-c(padding-spc 0))))
		       (if (or
			    (flag-set? Cn+1 S_up_to_n_Carry)
			    (flag-set? Cn+1 S_3b_up_to_n_Carry))
			   (displayln "     Carry after the mask")
			   (newline))
		       (display (binary-string->carries (padding-spc C))) (display "             ") (display (binary-string->carries (padding-spc C_3b))) (display-nl "     Carries") ;; carries
		       (display (padding-spc 2b)) (display "             ") (display (padding-spc 2b))  (display-nl "     2b") ;; 2b and 2b
		       (display (padding-spc b)) (display "             ") (display (padding-spc b))  (display-nl "      b") ;; b
		       (display "------------------------")
		       (if (flag-set? Cn+1 S_up_to_n_Carry) ; test if carry set on Cn+1 of S_up_to_n_Carry
			   (display " CARRY ")
			   (display "       "))
		       (if (flag-set? Cn+1 S_3b_up_to_n_Carry) ; test if carry set on Cn+1 of Sum of 3b
			   (display "CARRY ")
			   (display "      "))
		       (display "------------------------") (newline) 
		       (display (padding-spc S)) (display "             ") (display (padding-spc S_3b))  (display-nl "     Sum") ;; 3b+1 and 3b		       
		       (newline))

		 ;; cross-checking the carries
		 (if-t (or
			(and (flag-set? Cn+1 S_up_to_n_Carry)
			     (not (flag-set? Cn+1 C)))
			(and (flag-set? Cn+1 S_3b_up_to_n_Carry)
			     (not (flag-set? Cn+1 C_3b))))
		       (newline)
		       (displayln "ERROR : carries cross-checking fails !!!")
		       (newline))
		 
		 
		 ;; statistics
		 
		 (if-t (flag-set? Cn+1 S_up_to_n_Carry) ; test if carry set for S_up_to_n_Carry
		       (incf Cn+1-true))

		 (if-t (flag-set? Cn+1 S_3b_up_to_n_Carry) ; test if carry set for S'_up_to_n_Carry
		       (incf Cn+1-true-3x))
		 
		 (if (flag-set? Bn-1 b) ; test if Bn-1 was up
		     (begin 
		       (incf Bn-1-true)
		       (if-t (flag-set? Cn+1 S_up_to_n_Carry) ; test if Cn+1 was set in S_up_to_n_Carry
			     (incf Cn+1-true-knowing-Bn-1)))
		     (begin
		       (incf Bn-1-false)
		       (if-t (flag-set? Cn+1 S_up_to_n_Carry) ; test if Cn+1 was set in S_up_to_n_Carry
			     (incf Cn+1-true-knowing-NOT-Bn-1))))

		 ))) ; end let/if/for

    
    ;; probabilities computed from statistics
   
    (set! pBn-1 (/ Bn-1-true omega-universe))
    (set! pCn+1kBn-1 (/ Cn+1-true-knowing-Bn-1 Bn-1-true))
    (set! pCn+1 (/ Cn+1-true omega-universe))
    (set! pCn+1kNotBn-1 (/ Cn+1-true-knowing-NOT-Bn-1 Bn-1-false))
    
     ;; display results
    
    (display "omega-universe = ")
    (display omega-universe)
    (newline)
    
    (display "Bn-1-true = ")
    (display Bn-1-true)
    (newline)
    
    (display "Probability of Bn-1 = Bn-1-true / omega-universe = ")
    (display Bn-1-true) (display " / ") (display omega-universe) (display " = ")
    (display pBn-1) (display " = ") (display (exact->inexact pBn-1))
    (newline)
    
    (display "Probability of Cn+1 knowing Bn-1 = Cn+1-true-knowing-Bn-1 / Bn-1-true = ")
    (display Cn+1-true-knowing-Bn-1) (display " / ") (display Bn-1-true) (display " = ")
    (display pCn+1kBn-1)  (display " = ") (display (exact->inexact pCn+1kBn-1))
    (newline)

    (display "Probability of Cn+1 knowing Bn-1 is false = Cn+1-true-knowing-NOT-Bn-1 / Bn-1-false = ")
    (display Cn+1-true-knowing-NOT-Bn-1) (display " / ") (display Bn-1-false) (display " = ")
    (display pCn+1kNotBn-1)  (display " = ") (display (exact->inexact pCn+1kNotBn-1))
    (newline)
    
    (display "Probability of Cn+1 for 3x+1 = Cn+1-true / omega-universe = ")
    (display Cn+1-true) (display " / ") (display omega-universe) (display " = ")
    (display pCn+1)  (display " = ") (display (exact->inexact pCn+1))
    (newline)

    (set! pCn+1-3x (/ Cn+1-true-3x omega-universe))
    (display "Probability of Cn+1 for 3x = Cn+1-true-3x / omega-universe = ")
    (display Cn+1-true-3x) (display " / ") (display omega-universe) (display " = ")
    (display pCn+1-3x)  (display " = ") (display (exact->inexact pCn+1-3x))
    (newline)
    (newline)

    (display "n+1 = ") (display n+1) (newline)
    ;;(display "P(Ck) = 1/2 + (1/2^(k-1)) = ") (display (0.5 . + . (1 . / . (expt 2 (k . - . 1)))))
    ;;(display "P(Cn+1) = 1/2 + (1/2^n) = ") (display (+ 0.5  (/ 1  (expt 2 (- n+1 1)))))
    (newline)
    
    ))






;; compute probability of Ck knowing Bk-1 for Collatz (3x+1) function and 3x function
;;
;; definition of Sk: a bit computed with two operands and a carry, see calculus below:
;;
;; index reference k are taken relative to number b (b,result S and carries C have same index )
;;
;;
;;  C   C       C         C             C    C0 = 1 for 3x+1 computation and C0 = 0 for 3x computation
;;   n+2 n+1     k         i             0
;;      1.......b........b.............10    2b + C0 = a with a = b
;;               k-1      i                                    k   k-1
;;       1.......b........b.............1    b
;;                k-1      i
;;     ----------------------------------
;;  S   .S......S.........S.............0    S
;;   n+2  n      k         i            0     index
;;
;;
;; (collatz-3x-3x+1-proba-Ck-knowing-Bk-1-over-Cn #b10000000000
;;                                                #b10000000000)
;;
;; omega-universe = 512
;; Bk-1-true = 256
;; Probability of Bk-1 = Bk-1-true / omega-universe = 256 / 512 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 171 / 256 = 171/256 = 0.66796875
;; Probability of Ck knowing Bk-1 is false = Ck-true-knowing-NOT-Bk-1 / Bk-1-false = 86 / 256 = 43/128 = 0.3359375
;; Probability of Ck for 3x+1 = Ck-true / omega-universe = 257 / 512 = 257/512 = 0.501953125
;; Probability of Ck for 3x = Ck-true-3x / omega-universe = 256 / 512 = 1/2 = 0.5
;; k = 10
;; P(Ck) = 1/2 + (1/2^(k-1)) = 0.501953125
;;
;;
;; (collatz-3x-3x+1-proba-Ck-knowing-Bk-1-over-Cn #b10000000000
;;                                                #b1000000000000)
;; ERROR : carries cross-checking fails !!!

;; omega-universe = 2048
;; Bk-1-true = 1024
;; Probability of Bk-1 = Bk-1-true / omega-universe = 1024 / 2048 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 = Ck-true-knowing-Bk-1 / Bk-1-true = 512 / 1024 = 1/2 = 0.5
;; Probability of Ck knowing Bk-1 is false = Ck-true-knowing-NOT-Bk-1 / Bk-1-false = 512 / 1024 = 1/2 = 0.5
;; Probability of Ck for 3x+1 = Ck-true / omega-universe = 1024 / 2048 = 1/2 = 0.5
;; Probability of Ck for 3x = Ck-true-3x / omega-universe = 1024 / 2048 = 1/2 = 0.5

;; k = 10
;; P(Ck) = 1/2 + (1/2^(k-1)) = 0.501953125
(define (collatz-3x-3x+1-proba-Ck-knowing-Bk-1-over-Cn Ck Cn)

  ;; initialisation of variables
  (let* (
	 (k (- (size-bit Ck) 1))
	 (Bk-1 (shift-right Ck)) ;; ex: Ck = 100000, Bk-1 = 10000
	 (alea Cn) ;; 1000...00 = 111...11 + 1 , example with Cn, alea = 100000 
	 (omega-universe 0)
	 (Bk-1-true 0)
	 (pBk-1 0) ; probability of Bk-1
	 (Ck-true 0)
	 (Ck-true-3x 0)
	 (pCkkBk-1 0) ; probability Ck knowing Bk-1
	 (pCk 0)
	 (pCk-3x 0)
	 (Ck-true-knowing-Bk-1 0)
	 (Ck-true-knowing-NOT-Bk-1 0)
	 (Bk-1-false 0)
	 (pCkkNotBk-1 0) ; probability Ck knowing Bk-1 is false
	 (display-enabled #t)
	 (mask (- Ck 1))) ;; example with Ck, mask = 11111

    ;;(display "k = ") (display k) (newline)
    (display "alea = ") (display alea) (newline)
    (display "mask = ") (display (padding-spc mask)) (newline)

    ;; loop over numbers
    (for (b alea) ; b is in range of 0 to 111...11 , alea being 1000...00 = 111...11 + 1 , example with Ck = 100000, b is in range of 0 to 11111
	 
	 (if-t (flag-set? #b1 b) ; only for odd numbers
	       
	       (incf omega-universe)

	       ;; computation

	       (let* ((2b (shift-left b))    ; 2*b
		      (a (bitwise-ior 1 2b)) ; a = 2b + 1
		      ;;(S (+ (bitwise-and a mask) b)) ; compute a + b = 2b + 1 + b = 3b + 1 and mask the upper partial result to show carry
		      (S_up_to_k-1_Carry (+ (bitwise-and a mask) b)) ; compute k-1 LSBits of a + b = 2b + 1 + b = 3b + 1 and mask the upper partial result to show carry
		      (S (+ a b)) ; compute a + b = 2b + 1 + b = 3b + 1
		      ;;(S_3b (+ (bitwise-and 2b mask) b))  ; compute 3b
		      (S_3b_up_to_k-1_Carry (+ (bitwise-and 2b mask) b))  ; compute k-1 LSBits of 3b and mask the upper partial result to show carry
		      (S_3b (+  2b b))  ; compute 3b
		      (C (compute-carries-bin-numeric b)) ; compute Carries
		      (C_3b (compute-carries-bin-numeric-3b b)))

		 
		 ;; displaying
		 
		 (if-t display-enabled
		       (display "              3b+1                   ") (display "             ") (display "     3b") (newline)
		  
		       ;;(display (binary-string->sharp (padding-spc mask))) (display "             ") (display (binary-string->sharp (padding-spc mask))) (displayln "    Mask")
		       (display (padding-spc mask)) (display "             ") (display (padding-spc mask)) (displayln "    Mask")
		       (if (flag-set? Ck S_up_to_k-1_Carry) ; test if carry set on Ck of S_up_to_k-1_Carry
			   (display (binary-string->carries-c (padding-spc Ck)))
			   (display (binary-string->carries-c (padding-spc 0))))
		       (display "             ")
		       (if (flag-set? Ck S_3b_up_to_k-1_Carry) ; test if carry set on Ck of Sum of 3b
			   (display (binary-string->carries-c (padding-spc Ck)))
			   (display (binary-string->carries-c(padding-spc 0))))
		       (if (or
			    (flag-set? Ck S_up_to_k-1_Carry)
			    (flag-set? Ck S_3b_up_to_k-1_Carry))
			   (displayln "     Carry after the mask")
			   (newline))
		       (display (binary-string->carries (padding-spc C))) (display "             ") (display (binary-string->carries (padding-spc C_3b))) (display-nl "     Carries") ;; carries
		       (display (padding-spc 2b)) (display "             ") (display (padding-spc 2b))  (display-nl "     2b") ;; 2b and 2b
		       (display (padding-spc b)) (display "             ") (display (padding-spc b))  (display-nl "      b") ;; b
		       (display "------------------------")
		       (if (flag-set? Ck S_up_to_k-1_Carry) ; test if carry set on Ck of S_up_to_k-1_Carry
			   (display " CARRY ")
			   (display "       "))
		       (if (flag-set? Ck S_3b_up_to_k-1_Carry) ; test if carry set on Ck of Sum of 3b
			   (display "CARRY ")
			   (display "      "))
		       (display "------------------------") (newline) 
		       (display (padding-spc S)) (display "             ") (display (padding-spc S_3b))  (display-nl "     Sum") ;; 3b+1 and 3b		       
		       (newline))

		 ;; cross-checking the carries
		 (if-t (or
			(and (flag-set? Ck S_up_to_k-1_Carry)
			     (not (flag-set? Ck C)))
			(and (flag-set? Ck S_3b_up_to_k-1_Carry)
			     (not (flag-set? Ck C_3b))))
		       (newline)
		       (displayln "ERROR : carries cross-checking fails !!!")
		       (newline))
		 
		 
		 ;; statistics
		 
		 (if-t (flag-set? Ck S_up_to_k-1_Carry) ; test if carry set for S_up_to_k-1_Carry
		       (incf Ck-true))

		 (if-t (flag-set? Ck S_3b_up_to_k-1_Carry) ; test if carry set for S'_up_to_k-1_Carry
		       (incf Ck-true-3x))
		 
		 (if (flag-set? Bk-1 b) ; test if Bk-1 was up
		     (begin 
		       (incf Bk-1-true)
		       (if-t (flag-set? Ck S_up_to_k-1_Carry) ; test if Ck was set in S_up_to_k-1_Carry
			     (incf Ck-true-knowing-Bk-1)))
		     (begin
		       (incf Bk-1-false)
		       (if-t (flag-set? Ck S_up_to_k-1_Carry) ; test if Ck was set in S_up_to_k-1_Carry
			     (incf Ck-true-knowing-NOT-Bk-1))))

		 ))) ; end let/if/for

    
    ;; probabilities computed from statistics
   
    (set! pBk-1 (/ Bk-1-true omega-universe))
    (set! pCkkBk-1 (/ Ck-true-knowing-Bk-1 Bk-1-true))
    (set! pCk (/ Ck-true omega-universe))
    (set! pCkkNotBk-1 (/ Ck-true-knowing-NOT-Bk-1 Bk-1-false))
    
     ;; display results
    
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

    (display "Probability of Ck knowing Bk-1 is false = Ck-true-knowing-NOT-Bk-1 / Bk-1-false = ")
    (display Ck-true-knowing-NOT-Bk-1) (display " / ") (display Bk-1-false) (display " = ")
    (display pCkkNotBk-1)  (display " = ") (display (exact->inexact pCkkNotBk-1))
    (newline)
    
    (display "Probability of Ck for 3x+1 = Ck-true / omega-universe = ")
    (display Ck-true) (display " / ") (display omega-universe) (display " = ")
    (display pCk)  (display " = ") (display (exact->inexact pCk))
    (newline)

    (set! pCk-3x (/ Ck-true-3x omega-universe))
    (display "Probability of Ck for 3x = Ck-true-3x / omega-universe = ")
    (display Ck-true-3x) (display " / ") (display omega-universe) (display " = ")
    (display pCk-3x)  (display " = ") (display (exact->inexact pCk-3x))
    (newline)
    (newline)
    
    (display "k = ") (display k) (newline)
    ;;(display "P(Ck) = 1/2 + (1/2^(k-1)) = ") (display (0.5 . + . (1 . / . (expt 2 (k . - . 1)))))
    (display "P(Ck) = 1/2 + (1/2^(k-1)) = ") (display (+ 0.5  (/ 1  (expt 2 (- k  1)))))
    (newline)
    
    ))

;; (for (n 0 24) (display n) (display " -f8-> ") (display (modulo (fc1-8 n) 8)) (newline))
;; 0 -f8-> 1
;; 1 -f8-> 7
;; 2 -f8-> 5
;; 3 -f8-> 3
;; 4 -f8-> 1
;; 5 -f8-> 7
;; 6 -f8-> 5
;; 7 -f8-> 3
;; 8 -f8-> 1
;; 9 -f8-> 7
;; 10 -f8-> 5
;; 11 -f8-> 3
;; 12 -f8-> 1
;; 13 -f8-> 7
;; 14 -f8-> 5
;; 15 -f8-> 3
;; 16 -f8-> 1
;; 17 -f8-> 7
;; 18 -f8-> 5
;; 19 -f8-> 3
;; 20 -f8-> 1
;; 21 -f8-> 7
;; 22 -f8-> 5
;; 23 -f8-> 3
;; 24 -f8-> 1
(define (fc1-8 n)
  (+ 1 (* 6 n)))


;; compute Collatz until it reach 1
;;  (collatz-5x+1 5)
;;                      101       5
;;                    11010       26
;;                     1101       13
;;                  1000010       66
;;                   100001       33
;;                 10100110       166
;;                  1010011       83
;;                110100000       416
;;                 11010000       208
;;                  1101000       104
;;                   110100       52
;; S = (52 104 208 416 83 166 33 66 13 26 5)
;; "Cycle found !!!"
;; > (collatz-5x+1 3)
;;                       11       3
;;                    10000       16
;;                     1000       8
;;                      100       4
;;                       10       2
;;                        1       1
;; 
;; diverge pour 7
(define (collatz-5x+1 n)
  (define S (list n))
  (define Sk n)
  (letrec ((collatz-rec 
	    (lambda (n)
	      (display (padding-spc n))
	      (display "       ")
	      (display n)
	      (newline)
	      ;;(printf "~B\n" n)
	      (cond ((eq? n 1) 1)
		    
		    ((zero? (modulo n 2))
		     (set! Sk (quotient n 2))
		     (if (member Sk S)
			 (then-block
			  (dv S)
			  "Cycle found !!!")
			 (else-block
			   (set! S (cons Sk S))
			   ;;(dv S)
			   (collatz-rec Sk))))
		    
		    (else   
		     (set! Sk (+ (* 5 n) 1))
		     (if (member Sk S)
			 (then-block
			  (dv S)
			  "Cycle found !!!")
			 (else-block
			  (set! S (cons Sk S))
			  ;;(dv S)
			  (collatz-rec Sk))))))))
    (collatz-rec n)))

;; (scan-3x-1 100000)

;; ...
;;
;; ...
;; 99988
;; 99989
;; Converge
;; 99990
;; 99991
;; 99992
;; 99993
;; 99994
;; 99995
;; 99996
;; 99997
;; 99998
;; 99999
;; 100000
;; L-cycle = ((17 50 25 74 37 110 55 164 82 41 122 61 182 91 272 136 68 34) (5 14 7 20 10))

;; > (scan-3x-1 15)
;; 1
;; Converge

;; 2
;; Converge

;; 3
;; Converge

;; 4
;; Converge

;; 5
;; (5 14 7 20 10)
;; Cycle found !!!

;; 6
;; Converge

;; 7
;; (7 20 10 5 14)
;; Cycle found !!!

;; 8
;; Converge

;; 9
;; (9 26 13 38 19 56 28 14 7 20 10 5)
;; Cycle found !!!

;; 10
;; (10 5 14 7 20)
;; Cycle found !!!

;; 11
;; Converge

;; 12
;; Converge

;; 13
;; (13 38 19 56 28 14 7 20 10 5)
;; Cycle found !!!

;; 14
;; (14 7 20 10 5)
;; Cycle found !!!

;; 15
;; Converge

(define (scan-3x-1 n-end)
  (define L-cycle '())
  (define s 0)
  (for (n 1 n-end)
       (set! s (syracuse-3x-1 n))
       (when (not (number? s))
	     (when (not (member s L-cycle))
		   (set! L-cycle (cons s L-cycle))))
       ;;(newline)
       )
  (dv L-cycle))

;; (syracuse-3x-1 5)
;; 5
;; 14
;; 7
;; 20
;; 10
;; S = (10 20 7 14 5)
;; Cycle found !!!
(define (syracuse-3x-1 n)
  (define S (list n))
  (define Sk n)
  (define revS '())
  (define cycle '())
  (display n)
  (newline)
  (letrec ((syracuse-rec 
	    (lambda (n)
	      ;;(display (padding-spc n))
	      ;;(display "       ")
	      ;;(display n)
	      ;;(newline)
	      ;;(printf "~B\n" n)
	      (cond ((eq? n 1) (begin
				 (display-nl "Converge")
				 1))
		    
		    ((zero? (modulo n 2))
		     (set! Sk (quotient n 2))
		     (if (member Sk S)
			 (then-block
			  (set! revS (reverse S))
			  ;;(display-nl revS)
			  (set! cycle (get-cycle Sk revS))
			  ;;(display-nl cycle) 
			  (display-nl  "Cycle found !!!")
			  cycle)
			 (else-block
			   (set! S (cons Sk S))
			   ;;(dv S)
			   (syracuse-rec Sk))))
		    
		    (else   
		     (set! Sk (- (* 3 n) 1))
		     (if (member Sk S)
			 (then-block
			  (set! revS (reverse S))
			  ;;(display-nl revS)
			  (set! cycle (get-cycle Sk revS))
			  ;;(display-nl cycle) 
			  (display-nl  "Cycle found !!!")
			  cycle)
			 (else-block
			  (set! S (cons Sk S))
			  ;;(dv S)
			  (syracuse-rec Sk))))))))
    (syracuse-rec n)))



;; compute Collatz until it reach 1
(define (collatz-Kx n k)
  (display (padding-spc n))
  (display "       ")
  (display n)
  (newline)
  ;;(printf "~B\n" n)
  (cond ((eq? n 1) 1)
	((zero? (modulo n 2)) (collatz-Kx (quotient n 2)
					  k))
	(else (collatz-Kx (+ (* k n) 1)
			  k))))


;; compute Collatz until it reach 1
(define (collatz-5x-1 n)
  (define S (list n))
  (define Sk n)
  (letrec ((collatz-rec 
	    (lambda (n)
	      (display (padding-spc n))
	      (display "       ")
	      (display n)
	      (newline)
	      ;;(printf "~B\n" n)
	      (cond ((eq? n 1) 1)
		    
		    ((zero? (modulo n 2))
		     (set! Sk (quotient n 2))
		     (if (member Sk S)
			 (then-block
			  (dv S)
			  "Cycle found !!!")
			 (else-block
			   (set! S (cons Sk S))
			   ;;(dv S)
			   (collatz-rec Sk))))
		    
		    (else   
		     (set! Sk (- (* 5 n) 1))
		     (if (member Sk S)
			 (then-block
			  (dv S)
			  "Cycle found !!!")
			 (else-block
			  (set! S (cons Sk S))
			  ;;(dv S)
			  (collatz-rec Sk))))))))
    (collatz-rec n)))


;; compute Collatz until it reach 1
;; > (collatz-3x+5 5)
;;                      101       5
;;                    10100       20
;;                     1010       10
;; Sk = 5
;; Collatz sequence =rev-S = (5 20 10)
;; "Cycle found !!!"
;; > (collatz-3x+5 7)
;;                      111       7
;;                    11010       26
;;                     1101       13
;;                   101100       44
;;                    10110       22
;;                     1011       11
;;                   100110       38
;;                    10011       19
;;                   111110       62
;;                    11111       31
;;                  1100010       98
;;                   110001       49
;;                 10011000       152
;;                  1001100       76
;; Sk = 38
;; Collatz sequence =rev-S = (7 26 13 44 22 11 38 19 62 31 98 49 152 76)
;; "Cycle found !!!"
;; > (collatz-3x+5 9)
;;                     1001       9
;;                   100000       32
;;                    10000       16
;;                     1000       8
;;                      100       4
;;                       10       2
;;                        1    


(define (collatz-3x+5 n)
  (define S (list n))
  (define Sk n)
  (define rev-S '())
  (letrec ((collatz-rec 
	    (lambda (n)
	      (display (padding-spc n))
	      (display "       ")
	      (display n)
	      (newline)
	      ;;(printf "~B\n" n)
	      (cond ((eq? n 1) 1)
		    
		    ((zero? (modulo n 2))
		     (set! Sk (quotient n 2))
		     (if (member Sk S)
			 (then-block
			  (dv Sk)
			  (set! rev-S (reverse S))
			  (display "Collatz sequence =")
			  (dv rev-S)
			  "Cycle found !!!")
			 (else-block
			   (set! S (cons Sk S))
			   ;;(dv S)
			   (collatz-rec Sk))))
		    
		    (else   
		     (set! Sk (+ (* 3 n) 5))
		     (if (member Sk S)
			 (then-block
			  (dv Sk)
			  (set! rev-S (reverse S))
			  (display "Collatz sequence =")
			  (dv rev-S)
			  "Cycle found !!!")
			 (else-block
			  (set! S (cons Sk S))
			  ;;(dv S)
			  (collatz-rec Sk))))))))
    (collatz-rec n)))

;; compute Collatz until it reach 1

;; fallback always on odds !!!!!
(define (collatz-3x+2 n)
  (define S (list n))
  (define Sk n)
  (define rev-S '())
  (letrec ((collatz-rec 
	    (lambda (n)
	      (display (padding-spc n))
	      (display "       ")
	      (display n)
	      (newline)
	      ;;(printf "~B\n" n)
	      (cond ((eq? n 1) 1)
		    
		    ((zero? (modulo n 2))
		     (set! Sk (quotient n 2))
		     (if (member Sk S)
			 (then-block
			  (dv Sk)
			  (set! rev-S (reverse S))
			  (display "Collatz sequence =")
			  (dv rev-S)
			  "Cycle found !!!")
			 (else-block
			   (set! S (cons Sk S))
			   ;;(dv S)
			   (collatz-rec Sk))))
		    
		    (else   
		     (set! Sk (+ (* 3 n) 2))
		     (if (member Sk S)
			 (then-block
			  (dv Sk)
			  (set! rev-S (reverse S))
			  (display "Collatz sequence =")
			  (dv rev-S)
			  "Cycle found !!!")
			 (else-block
			  (set! S (cons Sk S))
			  ;;(dv S)
			  (collatz-rec Sk))))))))
    (collatz-rec n)))


