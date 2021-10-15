;; Sub Set Sum problem
;; Recursive solution
;; Guile compatible

;; see .guile file for options

;; VERY IMPORTANT: export the variable below into shell before launching Guile
;;( export GUILE_AUTO_COMPILE=0 )
;; export GUILE_AUTO_COMPILE=fresh

;; (load "SssRec.scm")



;;(use-modules (syntax define))
;;(use-modules (guile/define))
(use-modules (guile define-guile-3))


;; (include "../library-FunctProg/first-and-rest.scm")
;; (include "../library-FunctProg/list.scm")
;; (include "../library-FunctProg/postfix.scm")
;; (include "../library-FunctProg/let.scm")
;; (include "../library-FunctProg/definition.scm")
;; (include "../library-FunctProg/guile/array.scm")
;; (include "../library-FunctProg/block.scm")

(include "first-and-rest.scm")
(include "list.scm")
(include "postfix.scm")
(include "let.scm")
(include "definition.scm")
(include "guile/array.scm")
(include "block.scm")

;; if data are disordered the algo works also
;;(define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
(define L-init '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093 1099 1111 1284 1344 1520 2027 2500 2734 3000 3267 3610 4285 5027))

;;(define t-init 19836)

(define t-init 35267)
  
(define cpt 0)

;; scheme@(guile-user)> (define t-init (apply + L-init))
;; scheme@(guile-user)> (ssigma L-init t-init)
;; $4 = #t

;; scheme@(guile-user)> cpt
;; $5 = 0
;; scheme@(guile-user)> (ssigma L-init t-init)
;; $6 = #t
;; scheme@(guile-user)> cpt
;; $7 = 133867
(define (ssigma L t)

  (set! cpt {cpt + 1})
  ;;(display L) (display " ") (display t) (newline)
  (if (null? L)
      (begin
	;; (display "null L")
	;; (newline)
	;; (newline)
	#f)
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} #t ] ;; c is the solution
	      [ {c > t} (ssigma R t) ] ;; c is to big to be a solution but can be an approximation
	      ;; c < t at this point
	      ;; c is part of the solution or his approximation
	      ;; or c is not part of solution or his approximation
	      [ else {(ssigma R {t - c}) or (ssigma R t)} ] ))))

	      ;;[ else (or (ssigma R {t - c}) (ssigma R t)) ] ))))

;; (ssigma-sol L-init 603 '())
;; (275 256 64 4 3 1)


;;(ssigma-sol L-init 601 '())
;; #f

;; (ssigma-sol L-init t-init '())
;; (5027 4285 3000 1520 1344 1284 1041 1040 723 275 256 17 16 4 3 1)

;; scheme@(guile-user)> (ssigma-sol L-init t-init '())
;; $5 = (5027 4285 3000 2734 2027 1520 1344 1284 1111 1093 1041 1040 889 723 275 256 64 17 16 4 3 1)
;; scheme@(guile-user)> (define sol (ssigma-sol L-init t-init '()))
;; scheme@(guile-user)> (eq? sol L-init)
;; $6 = #f
;; scheme@(guile-user)> (equal? sol L-init)
;; $7 = #f
;; scheme@(guile-user)> (equal? sol (reverse L-init))
;; $8 = #t

;; scheme@(guile-user)> (apply + L-init)
;; $2 = 40274
;; scheme@(guile-user)> (ssigma-sol L-init (apply + L-init) '())
;; $3 = (5027 4285 3610 3267 3000 2734 2500 2027 1520 1344 1284 1111 1099 1093 1041 1040 915 889 723 540 323 275 256 197 64 45 24 17 16 4 3 1)
;; scheme@(guile-user)> $3
;; $4 = (5027 4285 3610 3267 3000 2734 2500 2027 1520 1344 1284 1111 1099 1093 1041 1040 915 889 723 540 323 275 256 197 64 45 24 17 16 4 3 1)
;; scheme@(guile-user)> (apply + $3)
;; $5 = 40274

;; scheme@(guile-user)> (ssigma-sol L-init 19836 '())
;; $5 = (3267 3000 2734 2027 1284 1099 1093 1040 915 889 723 540 323 275 256 197 64 45 24 17 16 4 3 1)

(define (ssigma-sol L t S)

  (set! cpt {cpt + 1})
  (if (null? L)
      (begin
	;; (display "null L")
	;; (newline)
	;; (display S)
	;; (newline)
	#f)
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} (cons c S) ] ;; c is the solution
	      [ {c > t} (ssigma-sol R t S) ] ;; c is to big to be a solution but can be an approximation
	      ;; c < t at this point
	      ;; c is part of the solution or his approximation
	      ;; or c is not part of solution or his approximation
	      [ else {(ssigma-sol R {t - c} (cons c S)) or (ssigma-sol R t S)} ] ))))







;; (best-sol 100 '(101) '(90 4 3))
;; (101)

(define (best-sol t L1 L2)
  ;; (display "L1=")
  ;; (display L1)
  ;; (newline)
  ;; (display "L2=")
  ;; (display L2)
  ;; (newline)
  (let [(s1 (apply + L1))
	(s2 (apply + L2))]
    (if {(abs {t - s1}) <= (abs {t - s2})}
	L1
	L2)))

(define (best-sol3 t L1 L2 L3)
  ;; (display "best-sol3") (newline)
  ;; (display "t=") (display t) (newline)
  ;; (display "L1=")
  ;; (display L1)
  ;; (newline)
  ;; (display "L2=")
  ;; (display L2)
  ;; (newline)
  ;; (display "L3=")
  ;; (display L3)
  ;; (newline)
  (let [(L22 (best-sol t L2 L3))]
    (best-sol t L1 L22)))



;; (start-ssigma-sol-approx '(1 3 10) 5)
;; (1 3)
;; (start-ssigma-sol-approx '(1 3 10) 12)
;; (1 10)

;;(start-ssigma-sol-approx L-init 19836) 
;;(1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)

;; (start-ssigma-sol-approx L-init 603) 
;; (1 3 4 64 256 275)

;; scheme@(guile-user)>  (start-ssigma-sol-approx L-init 601) 
;; $7 = (1 4 64 256 275)
;; scheme@(guile-user)> (+ 1 4 64 256 275)
;; $8 = 600

;; (start-ssigma-sol-approx '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093) (apply + '(1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093)))
;; (1 3 4 16 17 24 45 64 197 256 275 323 540 723 889 915 1040 1041 1093)

;; scheme@(guile-user)> (define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
;; scheme@(guile-user)> (start-ssigma-sol-approx L-init 19836)
;; $1 = (1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)

;; DEPRECATED
(define (start-ssigma-sol-approx L t)
  ;; (display "start-ssigma-sol-approx")
  ;; (newline)
  ;; (display "L=") (display L)
  ;; (newline)
  ;; (display "t=") (display t)
  ;; (newline)
  ;; (newline)
  ;;(if (null? L)
   ;;   L
   ;;   (ssigma-sol-approx L t '() t (list (first L)))))
    (ssigma-sol-approx L t '() t '()))

;; DEPRECATED
(define (ssigma-sol-approx L t S t-init AS) ;; AS:approximative solution

  ;; (display "L=") (display L)
  ;; (newline)
  ;; (display "S=") (display S)
  ;; (newline)
  ;; (display "AS=") (display AS)
  ;; (newline)
  ;; (newline)
  
  (if (null? L)
      
      (begin
	;; (display "null L")
	;; (newline)
	;; (display "S=") (display S)
	;; (newline) 
	;; (display "AS=") (display AS)
	;; (newline)
	;; (display "return best-sol")
	;; (newline)
	(best-sol t-init AS S)) ;; must return S or AS
      
      (let [ (c (first L))
	     (R (rest L)) ]
	(cond [ {c = t} (best-sol t-init AS (cons c S)) ] ;; c is the solution, TODO : test AS is not always null in this case
	      [ {c > t} (ssigma-sol-approx R t S t-init (best-sol t-init
								  AS
								  (list c))) ] ;; c is to big to be a solution but can be an approximation
	      ;; c < t at this point, 2 possibilities :
	      ;; c is part of the solution or his approximation
	      ;; or c is not part of solution or his approximation
	      [ else (best-sol3 t-init AS
				
				       (begin
					 ;; (display "append c=") (display c) (newline)

					 (append (cons c S) ;; c part of solution or is approximation
						 (start-ssigma-sol-approx R {t - c}))) ;; we have to find a solution or an approximation for t-c now
				       
				       ;; c is not part of solution or his approximation
				       (ssigma-sol-approx R t S t-init AS))]))))


;; package function will encapsulate some inner functions and reduce the number
;; of constant parameters passed to inner functions, inner functions will
;; be sort of clozures

;; code completely changed, there is less variable

;; scheme@(guile-user)> (define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
;; scheme@(guile-user)> (start-ssigma-sol-approx-pack L-init 19836)
;; $1 = (1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)

(define (start-ssigma-sol-approx-pack L t) ;; Sub Set Sum problem (find solution or approximation)
 

  (letrec-arrow* [ best-sol ← (lambda (L1 L2)
			   
				(let-arrow*  [ s1 ← (apply + L1)
					       s2 ← (apply + L2) ]
				    
					     (if {(abs {t - s1}) <= (abs {t - s2})}
						 L1
						 L2)))

		   best-sol3 ← (lambda (L1 L2 L3)
			     
				 (let [(L22 (best-sol L2 L3))]
				   (best-sol L1 L22)))
	       
	           ssigma-sol-approx ← (lambda (L)
				     				      
				      (if (null? L)
					  
					  L
					  
					  (let-arrow* [ c ← (first L)
						        R ← (rest L)  ]
					    
					    (cond [ {c = t} (list c) ] ;; c is the solution
						  [ {c > t} (best-sol (list c) (ssigma-sol-approx R)) ] ;; c is to big to be a solution but could be an approximation
						  ;; c < t at this point, 3 possibilities :
						  ;; c is the best solution
						  ;; c is part of the solution or his approximation
						  ;; or c is not part of solution or his approximation
						  [ else (best-sol3 (list c) ;; c is the best solution
								    
								    ;;(begin
								      ;; (display "append c=") (display c) (newline)
								    ;; c part of solution or is approximation
								    (cons c (start-ssigma-sol-approx-pack R {t - c}));;) ;; we have to find a solution or an approximation for t-c now
								    
								    ;; c is not part of solution or his approximation
								    (ssigma-sol-approx R))]))))

	       
	       ]

	     ;; start the function
	     (ssigma-sol-approx L)))



;; scheme@(guile-user)> (define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
;; scheme@(guile-user)> (start-ssigma-sol-approx-pack-define-anywhere L-init 19836)
;; $2 = (1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)

(define (start-ssigma-sol-approx-pack-define-anywhere L t) ;; Sub Set Sum problem (find solution or approximation)
  ;; { } are for infix notation as defined in SRFI 105
  ;; <+ and := are equivalent to (define var value) 
  { best-sol <+ (lambda (L1 L2)
		  
		  {s1 <+ (apply + L1)}
		  {s2 <+ (apply + L2)}
		  
		  (if {(abs {t - s1}) <= (abs {t - s2})}
		      L1
		      L2)) }

  ;; := is the same macro as <+
  { best-sol3 := (lambda (L1 L2 L3)
		   
		   {L22 <+ (best-sol L2 L3)}
		   (best-sol L1 L22)) }

  
  { ssigma-sol-approx <+ (lambda (L)
			   ;; def is a macro for declared but unasigned variable, it is same as (define var '())
			   (def c) 
			   (def R)
			   
			   (if (null? L)
			       L
			       (begin {c <- (first L)}
				      {R <- (rest L)}
				      
				      (cond [ {c = t} (list c) ] ;; c is the solution
					    [ {c > t} (best-sol (list c) (ssigma-sol-approx R)) ] ;; c is to big to be a solution but could be an approximation
					    ;; c < t at this point, 3 possibilities :
					    ;; c is the best solution
					    ;; c is part of the solution or his approximation
					    ;; or c is not part of solution or his approximation
					    [ else (best-sol3 (list c) ;; c is the best solution
							      
							      ;; c part of solution or is approximation
							      (cons c (start-ssigma-sol-approx-pack-define-anywhere R {t - c})) ;; we have to find a solution or an approximation for t-c now
									    
							      ;; c is not part of solution or his approximation
							      (ssigma-sol-approx R))])))) }
		   
	       
  ;; start the function
  (ssigma-sol-approx L))


;; scheme@(guile-user)> (define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
;; scheme@(guile-user)>  (start-ssigma-sol-approx-basic L-init 19836)
;; $1 = (1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)
;; scheme@(guile-user)> (apply + $1)
;; $2 = 19836
(define (start-ssigma-sol-approx-basic L t) ;; Sub Set Sum problem (find solution or approximation)
  ;; { } are for infix notation as defined in SRFI 105
  ;; <+ and := are equivalent to (define var value) 
  { best-sol <+ (lambda (L1 L2)
		  
		  {s1 <+ (apply + L1)}
		  {s2 <+ (apply + L2)}
		  
		  (if {(abs {t - s1}) <= (abs {t - s2})}
		      L1
		      L2)) }

  ;; := is the same macro as <+
  { best-sol3 := (lambda (L1 L2 L3)
		   
		   {L22 <+ (best-sol L2 L3)}
		   (best-sol L1 L22)) }

  
  { ssigma-sol-approx <+ (lambda (L)
			   ;; def is a macro for declared but unasigned variable, it is same as (define var '())
			   (def c) 
			   (def R)
			   
			   (if (null? L)
			       L
			       ($ {c <- (first L)} ;; $ = begin
				  (if {c = t}
				      (list c)  ;; c is the solution
				      ($ {R <- (rest L)}
					 (if {c > t}
					     (best-sol (list c) (ssigma-sol-approx R))  ;; c is to big to be a solution but could be an approximation
					     ;; c < t at this point, 3 possibilities :
					     ;; c is the best solution
					     ;; c is part of the solution or his approximation
					     ;; or c is not part of solution or his approximation
					     (best-sol3 (list c) ;; c is the best solution
							;; c part of solution or is approximation
							(cons c (start-ssigma-sol-approx-basic R {t - c})) ;; we have to find a solution or an approximation for t-c now
							;; c is not part of solution or his approximation
							(ssigma-sol-approx R)))))))) }
		   
	       
  ;; start the function
  (ssigma-sol-approx L))



;; scheme@(guile-user)> (define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
;; scheme@(guile-user)>  (start-ssigma-sol-approx-linus L-init 19836)
;; $1 = (1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)
(define (start-ssigma-sol-approx-linus L t) ;; Sub Set Sum problem (find solution or approximation)
  ;; { } are for infix notation as defined in SRFI 105
  ;; <+ and := are equivalent to (define var value) 
  { best-sol <+ (lambda (L1 L2)
		  
		  {s1 <+ (apply + L1)}
		  {s2 <+ (apply + L2)}
		  
		  (if {(abs {t - s1}) <= (abs {t - s2})}
		      L1
		      L2)) }

  ;; := is the same macro as <+
  { best-sol3 := (lambda (L1 L2 L3)
		   
		   {L22 <+ (best-sol L2 L3)}
		   (best-sol L1 L22)) }

  
  { ssigma-sol-approx <+ (lambda (L)
			   ;; def is a macro for declared but unasigned variable, it is same as (define var '())
			   ;;(def c) 
			   ;;(def R)
			   
			   (if (null? L)
			       L
			       ($ (define c (first L)) ;; $ = begin
				  (display "define inside") (newline)
				  (if {c = t}
				      (list c)  ;; c is the solution
				      ($ (define R (rest L))
					 (if {c > t}
					     (best-sol (list c) (ssigma-sol-approx R))  ;; c is to big to be a solution but could be an approximation
					     ;; c < t at this point, 3 possibilities :
					     ;; c is the best solution
					     ;; c is part of the solution or his approximation
					     ;; or c is not part of solution or his approximation
					     (best-sol3 (list c) ;; c is the best solution
							;; c part of solution or is approximation
							(cons c (start-ssigma-sol-approx-linus R {t - c})) ;; we have to find a solution or an approximation for t-c now
							;; c is not part of solution or his approximation
							(ssigma-sol-approx R)))))))) }
		   
	       
  ;; start the function
  (ssigma-sol-approx L))




;; scheme@(guile-user)>  (define L-init '(1 3 4 16 17 64 256 275 723 889 1040 1041 1093 1111 1284 1344 1520 2027 2734 3000 4285 5027))
;; scheme@(guile-user)> (start-ssigma-sol-approx-cond L-init 19836)
;; $1 = (1 3 4 16 17 256 275 723 1040 1041 1284 1344 1520 3000 4285 5027)

(define (start-ssigma-sol-approx-cond L t) ;; Sub Set Sum problem (find solution or approximation)
  ;; { } are for infix notation as defined in SRFI 105
  ;; <+ and := are equivalent to (define var value) 
  (define (best-sol L1 L2)
		  
    {s1 <+ (apply + L1)}
    {s2 <+ (apply + L2)}
		  
    (if {(abs {t - s1}) <= (abs {t - s2})}
	L1
	L2))

  (define (best-sol3 L1 L2 L3)
		   
    {L22 <+ (best-sol L2 L3)}
    (best-sol L1 L22)) 

  
  (define (ssigma-sol-approx L)
    ;; def is a macro for declared but unasigned variable, it is same as (define var '())
    (def c) 
    (def R)
    
    (cond [(null? L) L]
	  [ { {c <- (first L)} = t} (list c) ] ;; c is the solution
	  [ {c > t} {R <- (rest L)}
	            (best-sol (list c) (ssigma-sol-approx R)) ] ;; c is to big to be a solution but could be an approximation
	  ;; c < t at this point, 3 possibilities :
	  ;; c is the best solution
	  ;; c is part of the solution or his approximation
	  ;; or c is not part of solution or his approximation
	  [ else {R <- (rest L)}
		 (best-sol3 (list c) ;; c is the best solution
			    
			    ;; c part of solution or is approximation
			    (cons c (start-ssigma-sol-approx-pack-define-anywhere R {t - c})) ;; we have to find a solution or an approximation for t-c now
			    
			    ;; c is not part of solution or his approximation
			    (ssigma-sol-approx R))]))
		   
    
    ;; start the function
    (ssigma-sol-approx L))

