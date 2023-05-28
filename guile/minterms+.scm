;;  minterms definitions

;; Copyright (C) 2014-2022  Damien MATTEI
;;
;;
;; e-mail: damien.mattei@gmail.com 
;;         (damien.mattei@unice.fr , damien.mattei@oca.eu)
;;   


;;  (var->binary '((not A) B)) -> '(0 1)
(define (var->binary lst)
  (let ((expr->binary (lambda (expr)
		       (if (symbol? expr) 1 0))))
    (map expr->binary lst)))

;;  (min-term->binary '(and (not A) B)) -> '(0 1)
(define (min-term->binary minterm)
  (var->binary (args minterm)))

;; (binary->min-term  '(1 1 0 1) '(a b c d)) -> '(and a b (not c) d)
;; (binary->min-term  '(1 x 0 1) '(a b c d)) -> '(and a (not c) d)
(define (binary->min-term blst varlist)
  (let ((terms (binary->term blst varlist)))
    (if (singleton-set? terms)
	(first terms)
	(cons 'and terms))))

;;  (binary->term  '(1 1 0 1) '(a b c d)) -> '(a b (not c) d)
(define (binary->term blst varlist)
  (map-nil bin->symb blst varlist))
;; le gain de temps de la definition ci dessous n est pas significatif voire negatif
;;(define (binary->term blst varlist)
;;  (filter (lambda (x) (not (null? x))) 
;;	  (map bin->symb blst varlist)))


;; > (bin->symb 1 'a)
;; 'a
;; > (bin->symb 0 'a)
;; '(not a)
;; (bin->symb 'x 'a) -> '()
(define (bin->symb b s)
  (cond ((not (number? b)) '())
	((= b 1) s)
	(else (list 'not s))))


;; (minterm-binary<? '(1 0) '(1 1)) -> #t
;; (minterm-binary<? '(1 1) '(1 1)) -> #f
(define (minterm-binary<? mtb1 mtb2)
  (< (binlist2number mtb1) (binlist2number mtb2)))


;;(insert-literal 'C '((A B))) -> '((C A B) ((not C) A B))
(define (insert-literal c lst)
  (let ((aff-list (map (lambda (var-list) (cons c var-list)) lst)) ;; affirmation
	(neg-list (map (lambda (var-list) (cons (list 'not c) var-list)) lst))) ;; negation
    (when debug-mode
      (display "insert-literal : ")
      (dv aff-list)
      (display "insert-literal : ")
      (dv neg-list))
    (append aff-list neg-list)
    ))

;; (expand-minterm '(A B c) '(and A B)) -> '((c A B) ((not c) A B))
;; (expand-minterm '(A B C D) '(and A B)) -> '((D C A B) (D (not C) A B) ((not D) C A B) ((not D) (not C) A B))
;;  (expand-minterm '(A B C) 'A)
;; '((C B A) (C (not B) A) ((not C) B A) ((not C) (not B) A))
;; > (expand-minterm '(A B) 'A)
;; '((B A) ((not B) A))
;; WARNING: we have list in input and we return a list of list as ouput !
(define (expand-minterm var-list min-term)

  (define min-term-args
    (if (symbol? min-term)
	(list min-term)
	(args min-term)))
  
  (define result (list min-term-args)) ;; the result, initialized with '((A B))
  
  (define (expand-minterm-rec cL) ;; at start, iteration list is variable list : cL
    (cond ((null? cL) '()) ;; end of subroutine
	  (else
	   (let ((c (first cL))
		 (L (rest cL)))
	     (when (and
		    (not (member c min-term-args)) ;; (c !C-? min-term-args) AND (!c !C-? min-term-args)
		    (not (member (list 'not c) min-term-args)))
	       (when debug-mode
		 (display "expand-minterm-rec : ")
		 (dv c)
		 (dv min-term-args))
	       (set! result (insert-literal c result))
	       (when debug-mode
		 (dv result)))
	     (expand-minterm-rec L))))) ;; expand-minterm-rec(L)
  
  (expand-minterm-rec var-list)
  
  result)


;; (bin-minterm-weight '(0 1 0 1 1)) -> 3
(define (bin-minterm-weight bin-minterm)
  (no-debug-region
  (when debug-mode
	(display-msg-symb-nl  "bin-minterm-weight ::" bin-minterm))
  
  (if (null? bin-minterm)
      0
      (+ (first bin-minterm) (bin-minterm-weight (rest bin-minterm))))))


;; change x to 0
(define (x->0 b)
  (if (equal? b 'x)
      0
      b))


;; (floor-bin-minterm-weight '(0 1 0 x 1)) -> 2
(define (floor-bin-minterm-weight bin-minterm)
  (nodebug
   (display-msg-symb-nl  "floor-bin-minterm-weight ::" bin-minterm))
  (bin-minterm-weight (map x->0 bin-minterm)))


;; (minterm-binary-weight<? '(0 1 1 0 1) '(0 1 1 0)) -> #f
;; (minterm-binary-weight<? '(0 1 1 0 1) '(0 1 1 0 1)) -> #f
;; (minterm-binary-weight<? '(0 1 1 0 1) '(0 1 1 1 1)) -> #t 
(define (minterm-binary-weight<? mtb1 mtb2)
  (< (bin-minterm-weight mtb1) (bin-minterm-weight mtb2)))


;; (minterm-binary-weight=? '(0 1 1) '(1 1 0)) -> #t
(define (minterm-binary-weight=? mtb1 mtb2)
  (and (not (< (bin-minterm-weight mtb1) (bin-minterm-weight mtb2)))
       (not (< (bin-minterm-weight mtb2) (bin-minterm-weight mtb1)))))


(define (minterm-binary-weight-fast=? mtb1 mtb2)
  (= (bin-minterm-weight mtb1) (bin-minterm-weight mtb2)))

;; (minterm-binary-weight-number<? '(0 1 1) '(1 1 0)) -> #t
;; (minterm-binary-weight-number<? '(0 1 1 0) '(1 1 0)) -> #f
;; (minterm-binary-weight-number<? '(0 1 1 0) '(1 1 0 0)) -> #t
;; (minterm-binary-weight-number<? '(0 1 1 1) '(1 1 0 0)) -> #f
(define (minterm-binary-weight-number<? mtb1 mtb2)
  (if (minterm-binary-weight=? mtb1 mtb2)
      (minterm-binary<? mtb1 mtb2)
      (minterm-binary-weight<? mtb1 mtb2)))

;; (order-by-weight-minterms '((0 1 1) (1 0 1) (1 1 0) (1 1 1))) -> '(((0 1 1) (1 0 1) (1 1 0)) ((1 1 1)))
(define (order-by-weight-minterms L)
  (reverse  (order-by-weight-rec L)))

;; (order-by-weight-rec '((0 1 1) (1 0 1) (1 1 0) (1 1 1))) -> '(((1 1 1)) ((0 1 1) (1 0 1) (1 1 0)))
(define (order-by-weight-rec L) ;; L : List of minterms
  (if (null? L)
      L
      (insert-minterm (first L)
		      (order-by-weight-rec (rest L)))))

(define (insert-minterm mt LL) ;; LL : List of Lists of minterms
  (if (null? LL)
      (list (list mt))
      (let* ((L (first LL))
	     (mt2 (first L)))
	(if (minterm-binary-weight-fast=? mt mt2)
	    (cons (cons mt L) ;; construct (mt mt2 mt3 ...)
		  (rest LL)) ;; construct ((mt mt2 mt3 ...) (...) (...) ... )
	    (cons L (insert-minterm mt (rest LL)))))))
  
;; (order-by-weight-basic '((0 1 1) (1 0 1) (1 1 0) (1 1 1))) -> '(((0 1 1) (1 0 1) (1 1 0)) ((1 1 1)))
;; (order-by-weight-basic '((0 1 0) (0 1 1) (1 0 1) (1 1 0) (1 1 1)))
;; first-minterm = (0 1 0)
;; cur-weight = 1
;; cur-minterm-list = ((0 1 0))
;; list-of-list-of-minterms = ()
;; resting-bin-minterm-list = ((0 1 1) (1 0 1) (1 1 0) (1 1 1))
;; '(((0 1 0)) ((0 1 1) (1 0 1) (1 1 0)) ((1 1 1)))
;;
;; (order-by-weight-basic '((0 0 1) (0 1 0) (0 1 1) (1 0 1) (1 1 0) (1 1 1))) -> '(((0 0 1) (0 1 0)) ((0 1 1) (1 0 1) (1 1 0)) ((1 1 1)))
(define (order-by-weight-basic bin-minterm-list)
  (let* (
	 (first-minterm (first bin-minterm-list))
	 (cur-weight (begin
		       (when debug-mode 
			     (display-msg-symb-nl  "order-by-weight-basic :: let* :: " first-minterm))
		       (bin-minterm-weight first-minterm))) ;; current weight
	 (cur-minterm-list (list first-minterm)) ;; minterm list of current weight
	 (list-of-list-of-minterms '()) ;; result list
	 (resting-bin-minterm-list (rest bin-minterm-list)) ;; to order bin minterm list
	 )
    (letrec (
	     (order-by-weight-rec ;; recursive version
	      (lambda (cur-lst) ;; current list of minterms
		(if (null? cur-lst)
		    ;; construct the result
		    (begin
		      (set! list-of-list-of-minterms (append list-of-list-of-minterms (list cur-minterm-list))) ;; update the result
		      list-of-list-of-minterms) ;; return result
		    ;; else
		    (let* (
			   (cur-minterm (first cur-lst)) ;; minterm
			   (minterm-weight (bin-minterm-weight cur-minterm)) ;; weight
			   )
		      (begin
			(if (= minterm-weight cur-weight)
			    (set! cur-minterm-list (append cur-minterm-list (list cur-minterm))) ;; update cur-minterm-list
			    ;; else start another list of minterm
			    (begin
			      (set! list-of-list-of-minterms (append list-of-list-of-minterms (list cur-minterm-list))) ;; update the result
			      (set! cur-minterm-list (list cur-minterm)) ;; update the current list
			      (set! cur-weight minterm-weight)) ;; update the weight
			    )
			(order-by-weight-rec (rest cur-lst))) ;; recursive call with rest of list
		    ) ;; end let
		  ) ;; end if
	      ) ;; end lambda
	    )) ;; end variable definition of letrec

      (when debug-mode
	    (dv first-minterm)
	    (dv cur-weight)
	    (dv cur-minterm-list)
	    (dv list-of-list-of-minterms)
	    (dv resting-bin-minterm-list))
    
      (order-by-weight-rec resting-bin-minterm-list))))




;; (apply unify-two-minterms '((1 0 1 0 0 1 0 1 0 1) (1 0 1 0 1 1 0 1 0 1))) -> '(1 0 1 0 x 1 0 1 0 1)
;; (unify-two-minterms '(1 0 1 0 0 1 0 1 0 1) '(1 0 1 0 1 1 0 1 0 1)) -> '(1 0 1 0 x 1 0 1 0 1)
;; (unify-two-minterms '(1 0 1 0 0 1 0 1 0 1) '(1 1 1 0 1 0 0 1 0 1)) -> #f
(define (unify-two-minterms mt1 mt2)

  (nodebug
   (display-nl "unify-two-minterms : ")
   (dv mt1)
   (dv mt2))
  
  (function-map-with-escaping-by-kontinuation2  (macro-function-compare-2-bits-with-continuation) mt1 mt2))



;; (apply unify-two-minterms-iter '((1 0 1 0 0 1 0 1 0 1) (1 0 1 0 1 1 0 1 0 1))) -> '(1 0 1 0 x 1 0 1 0 1)
;; (unify-two-minterms-iter '(1 0 1 0 0 1 0 1 0 1) '(1 0 1 0 1 1 0 1 0 1)) -> '(1 0 1 0 x 1 0 1 0 1)
;; (unify-two-minterms-iter '(1 0 1 0 0 1 0 1 0 1) '(1 1 1 0 1 0 0 1 0 1)) -> #f
(def (unify-two-minterms-iter mt1 mt2)

  (if (null? mt1) (return '()))
     
  {vmt1 <+ (list->vector mt1)}
  {vmt2 <+ (list->vector mt2)}
  {lvmt <+ (vector-length vmt1)}
  {vmt <+ (make-vector lvmt 'x)}

  {err <+ #f}
  
  (for ({k <+ 0} {k < lvmt} {k <- {k + 1}})
       (if {vmt1[k] equal? vmt2[k]}
	   {vmt[k] <- vmt1[k]}
	   (if err
	       (return #f)
	       {err <- #t})))

  (vector->list vmt))


  

;; scheme@(guile-user)> (unify-two-minterms-rec '(1 0 1 0 0 1 0 1 0 1) '(1 0 1 0 1 1 0 1 0 1))
;; $2 = (1 0 1 0 x 1 0 1 0 1)
(define (unify-two-minterms-rec mt1 mt2)

  {err <+ #f}

  (def (unify-two-lists-tolerant-one-mismatch mt1 mt2)

       (if {(null? mt1) and (null? mt2)}
	   (return '()))

       (if {{(null? mt1) and (not (null? mt2))} or {(not (null? mt1)) and (null? mt2)}}
	   (return-rec #f))


       {fst-mt1 <+ (first mt1)}
       {fst-mt2 <+ (first mt2)}

       (if (equal? fst-mt1 fst-mt2) (return (cons fst-mt1
						  (unify-two-lists-tolerant-one-mismatch (rest mt1) (rest mt2)))))
       (if err (return-rec #f))

       {err <- #t}
       (cons 'x
	     (unify-two-lists-tolerant-one-mismatch (rest mt1) (rest mt2))))

  (unify-two-lists-tolerant-one-mismatch mt1 mt2))



(define function-compare-2-bits-1-false-tolerant
 
  (let ((cnt 0)) ;; counter
    (lambda (continuation x y) (if (equal? x y)
				   x
				   (begin
				     (set! cnt (add1 cnt))
				     (if (> cnt 1) #f 
					 'x))))))



;; test if equal modulo 1 bit (strict version, arguments must NOT be equal)
;;
;; return the merged minterm or false
;;
;; (equal-modulo-1bit-strict? '(1 0 1) '(1 1 0))
;; result = (1 x x)
;; stop-flag = #t
;; #f
;;
;; (equal-modulo-1bit-strict? '(1 0 1) '(1 1 1))
;; result = (1 x 1)
;; stop-flag = #f
;; '(1 x 1)

(define (equal-modulo-1bit-strict? bin-minterm-1 bin-minterm-2)
  (let* (
         (stop-flag #f)
         (cmp (macro-compare-2-bits stop-flag)) ;; compare macro
         (result (my-map cmp bin-minterm-1 bin-minterm-2))
         )
    (dv result)
    (dv stop-flag)
    (if stop-flag #f result))) ;; return the result or false   


;; test if equal modulo 1 bit
;;
;; return the merged minterm or false
;;
;; (equal-modulo-1bit? '(1 0 1) '(1 0 1)) -> #f
;; (equal-modulo-1bit? '(1 0 1) '(1 1 0)) -> #f
;; (equal-modulo-1bit? '(1 0 1) '(1 1 1)) -> '(1 x 1)


(define (equal-modulo-1bit? bin-minterm-1 bin-minterm-2)
  (let* (
	 (stop-flag #f)
	 (cmp (macro-compare-2-bits stop-flag)) ;; compare macro
	 (result '())
	 )
    (if (equal? bin-minterm-1 bin-minterm-2)
	#f
	(begin
	  (set! result (my-map cmp bin-minterm-1 bin-minterm-2))
	  (if stop-flag #f result))))) ;; return the result or false   






;; (minterm->string '(0 x 1 0 1 x)) -> "0x101x"
(define (minterm->string mt)
  (if (null? mt)
      ""
      (string-append (minterm-digit->string (car mt))
		     (minterm->string (cdr mt)))))


;; a minterm digit is either a number or a symbol
(define (minterm-digit->string mt-dg)
  (if (symbol? mt-dg)
      (symbol->string mt-dg)
      (number->string mt-dg)))

;; (minterm->var '(0 x 1 0 1 x)) -> 'V0x101x
(define (minterm->var mt)
  (string->symbol (string-append "V" (minterm->string mt))))

;; (var-string->minterm-string "V0x101x") -> "0x101x"
(define (var-string->minterm-string s)
  (substring s 1))

;;  (char->minterm-digit #\x) -> 'x
;;  (char->minterm-digit #\1) -> 1
(define (char->minterm-digit c)
  (if (char=? #\x c)
      (quote x)
      (- (char->integer c) (char->integer #\0))))

;; (minterm-string->minterm "0x101x") -> '(0 x 1 0 1 x)
(define (minterm-string->minterm v)
  (if (string=? v "")
      '()
      (cons (char->minterm-digit (string-ref v 0))
	    (minterm-string->minterm (substring v 1)))))


;; (var->minterm 'V0x101x) -> '(0 x 1 0 1 x)
(define (var->minterm v)
  (minterm-string->minterm  (var-string->minterm-string (symbol->string v))))




;; put below because of . period bug in SRFI 105 reader

;; proc to be called with //
(define (proc-unify-minterms-seg-inner-definitions seg)

  (define (function-map-with-escaping-by-kontinuation2 clozure list1 . more-lists) ;; ERROR: . period not supported in curly infix reader
    (call/cc (lambda (kontinuation)
	       (let ((lists (cons list1 more-lists))
		     (funct-continu ;; this function have the kontinuation in his environment 
		      (lambda (arg1 . more-args)
			(let ((args (cons arg1 more-args)))
			  (apply clozure kontinuation args))))) ;; a tester: (apply clozure (cons conti args))
		 
		 (apply map funct-continu lists)))))

  ;; compare two list of bits until we got more than one difference
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

  
  (define (unify-two-minterms mt1 mt2)

    (nodebug
     (display-nl "unify-two-minterms : ")
     (dv mt1)
     (dv mt2))
    
    (function-map-with-escaping-by-kontinuation2  (macro-function-compare-2-bits-with-continuation) mt1 mt2))

  
  (nodebug
   (display "proc-unify-minterms-seg : ")
   (dv seg))
  
  (define function-unify-minterms-list (λ (L) (apply unify-two-minterms L))) ;; (apply unify-two-minterms-rec L))} ;; 
   
  (define start (segment-start seg))
  (define end (segment-end seg))
  (for ((define i start) (<= i end) (set! i (+ i 1)))
       (define mtL (vector-ref minterms-vector i))
       (nodebug
	(dv mtL))
       (vector-set! unified-minterms-vector-1 i (function-unify-minterms-list mtL))))



