
;; DEPRECATED minterms definitions


;; Copyright (C) 2014-2018  Damien MATTEI
;;
;;
;; e-mail: damien.mattei@gmail.com 
;;         (damien.mattei@unice.fr , damien.mattei@oca.eu)
;; 


;; since minterms-ht is defined at top-level the definitions  below will be deprecated

;; > (define minterms-ht (make-hashtable)) 
;; > (macro-unify-minterms-set  '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0)))
;; '((1 0 x 0) (1 x 0 0))
;;
(define-syntax macro-unify-minterms-set ;; DEPRECATED
  (syntax-rules (minterms-ht)
   ((_ set1 set2) (unify-minterms-set set1 set2 (macro-unify-two-minterms-and-tag minterms-ht)))))

;; > (macro-lambda-unify-minterms-set) -> #<procedure>
(define-syntax macro-lambda-unify-minterms-set  ;; DEPRECATED
  (syntax-rules (minterms-ht)
   ((_) (lambda (s1 s2) (macro-unify-minterms-set s1 s2)))))

;; return a unify function for minterms given an hash table in parameter
;;  (define ht (make-hashtable)) 
;; > ht
;; '#hash()
;; > (unify-minterms-set '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0)) (macro-unify-two-minterms-and-tag ht))
;; '((1 0 x 0) (1 x 0 0))
;; > ht
;; '#hash(((1 1 0 0) . #t) ((1 0 1 0) . #t) ((1 0 0 0) . #t))
;;
;; DEPRECATED
;;
(define-syntax macro-unify-two-minterms-and-tag ;; i need a macro because of external variable to the function
  (syntax-rules ()
    ((_ hsh-tbl) (lambda (mt1 mt2)
		   (let ((res (unify-two-minterms mt1 mt2)))
		     (when res
			   (hash-set! hsh-tbl mt1 #t) ;; DrRacket
			   (hash-set! hsh-tbl mt2 #t)
			   ;;(hashtable-put! hsh-tbl mt1 #t) ;; Bigloo
			   ;;(hashtable-put! hsh-tbl mt2 #t)
			   )
		     res)))))





;; (order-by-weight '((0 1 0) (0 1 1) (1 0 1) (1 1 0) (1 1 1)))
;; first-minterm = (0 1 0)
;; cur-weight = 1
;; cur-minterm-list = ((0 1 0))
;; list-of-list-of-minterms = ()
;; resting-bin-minterm-list = ((0 1 1) (1 0 1) (1 1 0) (1 1 1))
;; '(((0 1 0)) ((0 1 1) (1 0 1) (1 1 0)) ((1 1 1)))
;;
;; (order-by-weight '((0 0 1) (0 1 0) (0 1 1) (1 0 1) (1 1 0) (1 1 1))) -> '(((0 0 1) (0 1 0)) ((0 1 1) (1 0 1) (1 1 0)) ((1 1 1)))
;;
;; >  (minimal-dnf '(or (and (not a) (not b) (not c) (not d)) (and (not a) b (not c) d) (and (not a) b c d) (and a (not b) c (not d)) (and a b (not c) d) (and a b c (not d)) (and a b c d)))
;; (order-by-weight uniq-sorted-binary-minterms) = (((0 0 0 0)) () ((0 1 0 1) (1 0 1 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1)))
;; . . escaping from Quine-Mc-Cluskey
;;
;; DEPRECATED order-by-weight-basic is now used
(define (order-by-weight bin-minterm-list)

  (let* (
	 (first-minterm (first bin-minterm-list))
	 (cur-weight (begin
		       (when debug-mode 
			     (display-msg-symb-nl  "order-by-weight :: let* :: " first-minterm))
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
		  
		      (cond ((= minterm-weight cur-weight)
			     (set! cur-minterm-list (append cur-minterm-list (list cur-minterm)))) ;; update cur-minterm-list
			    ((= minterm-weight (+ cur-weight 1))
			     ;; start another list of minterm
			     (set! list-of-list-of-minterms (append list-of-list-of-minterms (list cur-minterm-list))) ;; update the result
			     (set! cur-minterm-list (list cur-minterm)) ;; update the current list
			     (set! cur-weight minterm-weight)) ;; update the weight
			    (else ;;(error "order-by-weight-rec :: else case not defined"))
			     (let* ((gap-size-of-minterms-by-weight (- minterm-weight cur-weight))
				    (list-of-empty-sets 
				     (begin
				       (display-nl "call of create-list !")
				       (create-list '() (- gap-size-of-minterms-by-weight 1)))))
			       ;; start another list of minterm
			       (set! list-of-list-of-minterms (append list-of-list-of-minterms (list cur-minterm-list) list-of-empty-sets)) ;; update the result with set of cur-minterm-list and empty sets
			       (set! cur-minterm-list (list cur-minterm)) ;; update the current list
			       (set! cur-weight minterm-weight)) ;; update the weight
			     ) ;; closing else
			    ) ;; end cond
		      
		      (order-by-weight-rec (rest cur-lst)) ;; recursive call with rest of list
		      
		      ) ;; end let*
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



;; unify two sets of minterms separated by a weight distance of one unit (1 bit)
;;
;; (unify-minterms-set '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0)) unify-two-minterms) -> '((1 0 x 0) (1 x 0 0))
;; (unify-minterms-set '((0 1 1) (1 0 1) (1 1 0)) '((1 1 1)) unify-two-minterms) -> '((x 1 1) (1 x 1) (1 1 x))
;;
;; > (unify-minterms-set '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0)) (macro-unify-two-minterms-and-tag ht))
;; '((1 0 x 0) (1 x 0 0))
;;
;; DEPRECATED
;;
(define (unify-minterms-set set1 set2 unify-two-minterms-clozure)
  (let* (
	 (minterms-set (associate-set-with-set set1 set2)) ;; create pair list of minterms
	 (function-unify-minterms-list (lambda (L) (apply unify-two-minterms-clozure L)))
	 (unified-minterms-set-1 (map function-unify-minterms-list minterms-set))
	 (unified-minterms-set-2 (filter (lambda (x) x) unified-minterms-set-1)) ;; remove false results
	 (unified-minterms-set (remove-duplicates-sorted unified-minterms-set-2)) ;; uniq
	 )
    unified-minterms-set))



;; (init-hash-table-with-set-and-value minterms-ht '((1 0 0 0) (0 1 0 1) (1 0 1 0) (1 1 0 0) (0 1 1 1) (1 1 0 1) (1 1 1 0) (1 1 1 1)) #f)
;; '(#<void> #<void> #<void> #<void> #<void> #<void> #<void> #<void>)
;; > (unify-minterms-set-of-sets '(((1 0 0 0)) ((0 1 0 1) (1 0 1 0) (1 1 0 0)) ((0 1 1 1) (1 1 0 1) (1 1 1 0)) ((1 1 1 1))))
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
;; > 
(define (unify-minterms-set-of-sets sos)  ;; DEPRECATED
   (map-2-shift (macro-lambda-unify-minterms-set) sos))



;; (function-unify-minterms-set  '((1 0 0 0)) '((1 0 1 0) (0 1 0 1) (1 1 0 0))) -> '((1 0 x 0) (1 x 0 0))
;; > minterms-ht
;; '#hash(((1 1 0 0) . #t) ((1 0 1 0) . #t) ((1 0 0 0) . #t))
 (define (function-unify-minterms-set set1 set2) ;; DEPRECATED
   ((macro-lambda-unify-minterms-set) set1 set2))
