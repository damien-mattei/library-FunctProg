;; (order-by-weight '((0 1 1) (1 0 1) (1 1 0) (1 1 1))) -> '(((0 1 1) (1 0 1) (1 1 0)) ((1 1 1)))
;;  minterms definitions

;; Copyright (C) 2014-2018  Damien MATTEI
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
	  (dv aff-list)
	  (dv neg-list))
    (append aff-list neg-list)
    ))

;; (expand-minterm '(A B c) '(and A B)) -> '((c A B) ((not C) A B))
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
  (when debug-mode
	(display-msg-symb-nl  "bin-minterm-weight ::" bin-minterm))
  (if (null? bin-minterm)
      0
      (+ (first bin-minterm) (bin-minterm-weight (rest bin-minterm))))) 


;; change x to 0
(define (x->0 b)
  (if (equal? b 'x)
      0
      b))


;; (floor-bin-minterm-weight '(0 1 0 x 1)) -> 2
(define (floor-bin-minterm-weight bin-minterm)
  (when debug-mode
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
  #;(map-with-escaping macro-compare-2-bits mt1 mt2)
  #;(function-map-with-escaping-by-continuation (macro-function-compare-2-bits-with-continuation) mt1 mt2)
  #;(map-with-escaping-by-continuation macro-compare-2-bits-with-continuation mt1 mt2)
  (function-map-with-escaping-by-kontinuation2  (macro-function-compare-2-bits-with-continuation) mt1 mt2))
  ;;(map-with-escaping-by-kontinuation-clozure (macro-compare-2-bits-with-kontinuation) mt1 mt2) ;; do not works with DrRacket, too much hygiene in macros?

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




;; unify two sets of minterms separated by a weight distance of one unit (1 bit)
;;
;; (funct-unify-minterms-set '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0))) -> '((1 0 x 0) (1 x 0 0))
;;  minterms-ht
;; '#hash(((1 1 0 0) . #t) ((1 0 1 0) . #t) ((1 0 0 0) . #t))
;;
;; (funct-unify-minterms-set '((0 1 1) (1 0 1) (1 1 0)) '((1 1 1))) -> '((x 1 1) (1 x 1) (1 1 x))
;;
(define (funct-unify-minterms-set-1-unit set1 set2)

  ;;(debug-mode-on)
  (when debug-mode
	(display-nl "funct-unify-minterms-set-1-unit : ")
	(dvs set1)
	(dvs set2)
	(debug-mode-reload))
  
  (letrec ((function-unify-minterms-list (lambda (L) (apply function-unify-two-minterms-and-tag L))))
    (let* (
	   (minterms-set (associate-set-with-set set1 set2)) ;; create pair list of minterms
	   (unified-minterms-set-1 (map function-unify-minterms-list minterms-set))
	   (unified-minterms-set-2 (filter (lambda (x) x) unified-minterms-set-1)) ;; remove false results
	   (unified-minterms-set (remove-duplicates-sorted unified-minterms-set-2)) ;; uniq
	   )
      
      ;;(debug-mode-on)
      (when debug-mode
	(dvs unified-minterms-set)
	(debug-mode-reload))
      
      unified-minterms-set)))


;; unify two sets of minterms separated by a weight distance of one unit (1 bit)
;;
;; (funct-unify-minterms-set '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0))) -> '((1 0 x 0) (1 x 0 0))
;;  minterms-ht
;; '#hash(((1 1 0 0) . #t) ((1 0 1 0) . #t) ((1 0 0 0) . #t))
;;
;; (funct-unify-minterms-set '((0 1 1) (1 0 1) (1 1 0)) '((1 1 1))) -> '((x 1 1) (1 x 1) (1 1 x))
;;
;; (funct-unify-minterms-set '((1 x x 0)) '((x 1 x 1) (1 1 x x))) -> '()
(define (funct-unify-minterms-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (letrec ((function-unify-minterms-list (lambda (L) (apply function-unify-two-minterms-and-tag L))))
	(let* (
	       (minterms-set (associate-set-with-set set1 set2)) ;; create pair list of minterms
	       (unified-minterms-set-1 (map function-unify-minterms-list minterms-set))
	       (unified-minterms-set-2 (filter (lambda (x) x) unified-minterms-set-1)) ;; remove false results
	       (unified-minterms-set (remove-duplicates-sorted unified-minterms-set-2)) ;; uniq
	       )
	  unified-minterms-set))))


;; the hash table for minterms, better to be a top-level definition,it's nightmare otherwise...
(define minterms-ht (make-hash)) ;; DrRacket
;;(define minterms-ht (make-hashtable)) ;; Bigloo 




;; > (init-hash-table-with-set-and-value minterms-ht '((1 0 0 0) (0 1 0 1) (1 0 1 0) (1 1 0 0) (0 1 1 1) (1 1 0 1) (1 1 1 0) (1 1 1 1)) #f)
;; '(#<void> #<void> #<void> #<void> #<void> #<void> #<void> #<void>)
;; > minterms-ht
;; '#hash(((1 1 0 1) . #f)
;;        ((1 1 0 0) . #f)
;;        ((0 1 0 1) . #f)
;;        ((0 1 1 1) . #f)
;;        ((1 0 0 0) . #f)
;;        ((1 1 1 1) . #f)
;;        ((1 1 1 0) . #f)
;;        ((1 0 1 0) . #f))
;; > (funct-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; >
;; > minterms-ht
;; '#hash(((1 1 0 1) . #t)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t))
;;
;;  (funct-unify-minterms-set-of-sets '(((1 x x 0)) ((x 1 x 1) (1 1 x x)))) -> '(())
(define (funct-unify-minterms-set-of-sets sos) 
   (map-2-shift funct-unify-minterms-set sos))





;; argument: a set of sets of minterms
;; this function advance of a level in unify minterms set of sets
;; when there is no more things to do it returns a set of empty set i.e : '(()) or an empty set '() (for this reason this function will be wrapped)
;;
;; (funct-unify-minterms-set-of-sets-rec '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;;   -> '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;;
;; (funct-unify-minterms-set-of-sets-rec '(((1 x x 0)) ((x 1 x 1) (1 1 x x)))) -> '(())
;;
;; (funct-unify-minterms-set-of-sets-rec '(((x 1 x 1)))) -> '()
;;
;; TODO:
;;
;; (funct-unify-minterms-set-of-sets-rec '(() ((1 0 x x) (1 x 0 x) (1 x x 0)) ((x x 1 1) (x 1 x 1) (1 x x 1) (1 x 1 x) (1 1 x x))))
;; sos = (() ((1 0 x x) (1 x 0 x) (1 x x 0)) ((x x 1 1) (x 1 x 1) (1 x x 1) (1 x 1 x) (1 1 x x)))
;; . . car: contract violation
;;   expected: pair?
;;   given: '()
(define (funct-unify-minterms-set-of-sets-rec sos)
  
  ;;(set! debug-mode-save debug-mode)
  ;;(set! debug-mode #t)
  (when debug-mode
	(newline)
	(display "funct-unify-minterms-set-of-sets-rec :: ")
	(dvsos sos)
	(set! debug-mode debug-mode-save))
  
  (cond
   
   ((singleton-set? sos) ;;(null? (cdr sos)) ;; (equal? sos '(())) marcherais pas
    (debug
     (display-nl "funct-unify-minterms-set-of-sets-rec :: singleton-set? ")
     (dvsos sos))
    '())
    
   (else
    (let* ((mt-set1 (car sos)) ;; minterm set 1
	   (mt-set2 (cadr sos)) ;; minterm set 2
	   (mt-set2-to-mt-setn (cdr sos)) ;; minterm sets 2 to n
	   (weight-mt-set1 (floor-bin-minterm-weight (car mt-set1))) ;; in a set all minterms have same weight
	   (weight-mt-set2 (floor-bin-minterm-weight (car mt-set2)))
	   (delta-weight (- weight-mt-set2 weight-mt-set1)))

      (if (= delta-weight 1)
	  (let ((unified-mt-set1-and-mt-set2 (funct-unify-minterms-set-1-unit mt-set1 mt-set2)))

	    (if (null? unified-mt-set1-and-mt-set2)
		(funct-unify-minterms-set-of-sets-rec mt-set2-to-mt-setn)
		(insert unified-mt-set1-and-mt-set2 (funct-unify-minterms-set-of-sets-rec mt-set2-to-mt-setn)))) ;; end let
	  (funct-unify-minterms-set-of-sets-rec mt-set2-to-mt-setn))))))

  


;; it's a wrap of previous function to return empty set 
;; > (funct-unify-minterms-set-of-sets-rec-wrap '(((1 x x 0)) ((x 1 x 1) (1 1 x x))))
;; '()
;; > (funct-unify-minterms-set-of-sets-rec-wrap '(((x 1 x 1))))
;; '()
;; > (funct-unify-minterms-set-of-sets-rec-wrap '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; > 
(define (funct-unify-minterms-set-of-sets-rec-wrap sos)

  (debug-mode-on)
  (when debug-mode
	(newline)
	(newline)
	(display "funct-unify-minterms-set-of-sets-rec-wrap : ")
	(dvsos sos)
	(debug-mode-reload))

  (let ((rv (funct-unify-minterms-set-of-sets-rec sos)))
    #;(debug-mode-on)
    (when debug-mode
	(newline)
	(newline)
	(display "funct-unify-minterms-set-of-sets-rec-wrap : ")
	(dvsos rv)
	(debug-mode-reload))
    (if (set-of-empty-set? rv)
	'()
	rv)))
	  



;; (recursive-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '((0 1 x 1)
;;   (1 x 0 0)
;;   (1 0 1 0)
;;   (1 1 1 0)
;;   (x 1 0 1)
;;   (1 1 x 1)
;;   (0 1 1 1)
;;   (1 0 x 0)
;;   (1 1 1 x)
;;   (1 1 0 1)
;;   (1 1 0 x)
;;   (1 1 x x)
;;   (1 x 1 0)
;;   (1 1 1 1)
;;   (1 0 0 0)
;;   (x 1 x 1)
;;   (0 1 0 1)
;;   (1 x x 0)
;;   (1 1 0 0)
;;   (1 1 x 0)
;;   (x 1 1 1))
;;
;; > (recursive-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; sos = (((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1)))
;; sos = (((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; sos = (((1 x x 0)) ((x 1 x 1) (1 1 x x)))
;; sos = ()
;; '((1 1 0 0)
;;   (x 1 x 1)
;;   (1 1 x 1)
;;   (0 1 x 1)
;;   (1 1 x 0)
;;   (1 x x 0)
;;   (x 1 0 1)
;;   (1 x 1 0)
;;   (1 1 x x)
;;   (1 0 0 0)
;;   (1 0 1 0)
;;   (1 1 1 1)
;;   (0 1 1 1)
;;   (0 1 0 1)
;;   (1 x 0 0)
;;   (1 0 x 0)
;;   (1 1 0 1)
;;   (1 1 1 x)
;;   (x 1 1 1)
;;   (1 1 1 0)
;;   (1 1 0 x))
(define (recursive-unify-minterms-set-of-sets sos)

  ;;(debug-mode-on)
  (when debug-mode
	(newline)
	(display "recursive-unify-minterms-set-of-sets : ")
	(dvsos sos)
	(debug-mode-reload))
  
  (if (set-of-empty-sets? sos)
      ;;(equal? sos '(()))
      (hash-keys minterms-ht) ;; DrRacket
      ;;(hashtable-key-list minterms-ht)
      (begin
	(when debug-mode (display-msg-symb-nl "recursive-unify-minterms-set-of-sets ::" minterms-ht))
	(put-elements-of-set-of-sets-in-minterms-ht sos)
	(when debug-mode (display-msg-symb-nl "recursive-unify-minterms-set-of-sets :: after (put-elements-of-set-of-sets-in-minterms-ht sos)" minterms-ht))
	(recursive-unify-minterms-set-of-sets (funct-unify-minterms-set-of-sets-rec-wrap #;funct-unify-minterms-set-of-sets sos)))))




;; (put-elements-of-set-of-sets-in-minterms-ht '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x))))
;; '((#<void> #<void>) (#<void> #<void> #<void> #<void> #<void>) (#<void> #<void> #<void>))
;; > minterms-ht
;; '#hash(((1 x 1 0) . #f)
;;        ((1 1 0 x) . #f)
;;        ((1 1 x 1) . #f)
;;        ((x 1 1 1) . #f)
;;        ((1 0 x 0) . #f)
;;        ((1 1 1 x) . #f)
;;        ((1 1 x 0) . #f)
;;        ((x 1 0 1) . #f)
;;        ((1 x 0 0) . #f)
;;        ((0 1 x 1) . #f))
;;
;;
;;
;; Bienvenue dans DrRacket, version 6.1.1 [3m].
;; Langage: racket [personnalisé]; memory limit: 256 MB.
;; > (init-hash-table-with-set-and-value minterms-ht '((1 0 0 0) (0 1 0 1) (1 0 1 0) (1 1 0 0) (0 1 1 1) (1 1 0 1) (1 1 1 0) (1 1 1 1)) #f)
;; '(#<void> #<void> #<void> #<void> #<void> #<void> #<void> #<void>)
;; > minterms-ht
;; '#hash(((1 1 0 1) . #f)
;;        ((1 1 0 0) . #f)
;;        ((0 1 0 1) . #f)
;;        ((0 1 1 1) . #f)
;;        ((1 0 0 0) . #f)
;;        ((1 1 1 1) . #f)
;;        ((1 1 1 0) . #f)
;;        ((1 0 1 0) . #f))
;; > (funct-unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
;; '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x)))
;; > minterms-ht
;; '#hash(((1 1 0 1) . #t)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t))
;; > (put-elements-of-set-of-sets-in-minterms-ht '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x))))
;; '((#<void> #<void>) (#<void> #<void> #<void> #<void> #<void>) (#<void> #<void> #<void>))
;; > minterms-ht
;; '#hash(((x 1 1 1) . #f)
;;        ((1 1 x 0) . #f)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #f)
;;        ((1 1 0 x) . #f)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #f)
;;        ((1 0 x 0) . #f)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #f)
;;        ((x 1 0 1) . #f)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #f)
;;        ((0 1 x 1) . #f))
;; > (funct-unify-minterms-set-of-sets '(((1 0 x 0) (1 x 0 0)) ((0 1 x 1) (x 1 0 1) (1 x 1 0) (1 1 0 x) (1 1 x 0)) ((x 1 1 1) (1 1 x 1) (1 1 1 x))))
;; '(((1 x x 0)) ((x 1 x 1) (1 1 x x)))
;; > minterms-ht
;; '#hash(((x 1 1 1) . #t)
;;        ((1 1 x 0) . #t)
;;        ((1 1 0 0) . #t)
;;        ((0 1 0 1) . #t)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #t)
;;        ((1 1 0 x) . #t)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #t)
;;        ((1 0 x 0) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #t)
;;        ((x 1 0 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #t)
;;        ((0 1 x 1) . #t))
;; > (put-elements-of-set-of-sets-in-minterms-ht '(((1 x x 0)) ((x 1 x 1) (1 1 x x))))
;; '((#<void>) (#<void> #<void>))
;; > minterms-ht
;; '#hash(((x 1 1 1) . #t)
;;        ((1 1 x 0) . #t)
;;        ((1 1 0 0) . #t)
;;        ((1 x x 0) . #f)
;;        ((0 1 0 1) . #t)
;;        ((x 1 x 1) . #f)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #t)
;;        ((1 1 x x) . #f)
;;        ((1 1 0 x) . #t)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #t)
;;        ((1 0 x 0) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #t)
;;        ((x 1 0 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #t)
;;        ((0 1 x 1) . #t))
;; > (funct-unify-minterms-set-of-sets '(((1 x x 0)) ((x 1 x 1) (1 1 x x))))
;; '(())
;; > minterms-ht
;; '#hash(((x 1 1 1) . #t)
;;        ((1 1 x 0) . #t)
;;        ((1 1 0 0) . #t)
;;        ((1 x x 0) . #f)
;;        ((0 1 0 1) . #t)
;;        ((x 1 x 1) . #f)
;;        ((1 0 0 0) . #t)
;;        ((1 1 1 1) . #t)
;;        ((1 x 1 0) . #t)
;;        ((1 1 x x) . #f)
;;        ((1 1 0 x) . #t)
;;        ((1 1 0 1) . #t)
;;        ((1 1 1 x) . #t)
;;        ((1 0 x 0) . #t)
;;        ((0 1 1 1) . #t)
;;        ((1 1 x 1) . #t)
;;        ((x 1 0 1) . #t)
;;        ((1 1 1 0) . #t)
;;        ((1 0 1 0) . #t)
;;        ((1 x 0 0) . #t)
;;        ((0 1 x 1) . #t))
;; > 
;; e element
;; s set
;; sos set of sets
(define (put-elements-of-set-of-sets-in-minterms-ht sos)
  (map ;; deal with sets of the 'set of sets'
   (lambda (s) (map ;; deal with elements of a set
		(lambda (e) (hash-set! minterms-ht e #f)) ;; DrRacket
		;;(lambda (e) (hashtable-put! minterms-ht e #f))
		s))
	   sos))



;; unify function for two minterms
;;
;; (function-unify-two-minterms-and-tag '(1 0 0 0) '(1 0 1 0)) -> '(1 0 x 0)
;;
;; (unify-minterms-set '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0)) function-unify-two-minterms-and-tag)
;;
;; '((1 0 x 0) (1 x 0 0))
;; >  minterms-ht
;; '#hash(((1 1 0 0) . #t) ((1 0 1 0) . #t) ((1 0 0 0) . #t))
;;
;;
(define (function-unify-two-minterms-and-tag mt1 mt2)
  (let ((res (unify-two-minterms mt1 mt2)))
    (when res
	  (hash-set! minterms-ht mt1 #t) ;; DrRacket
	  (hash-set! minterms-ht mt2 #t)
	  ;;(hashtable-put! minterms-ht mt1 #t)
	  ;;(hashtable-put! minterms-ht mt2 #t)
	  )
    res))



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




