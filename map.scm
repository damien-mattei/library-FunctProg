;; (define-syntax get-out
;;      (identifier-syntax break)) 


;;  map-with-escaping
;;
;;  return the result or false if (the) condition is true before end of calculus
;;
;; (map-with-escaping macro-compare-2-bits '(1 0 1) '(1 1 1)) -> '(1 x 1)
;;
;; (map-with-escaping macro-compare-2-bits '(1 0 1) '(1 1 0)) -> #f
;;
;; (map-with-escaping macro-compare-2-bits '(1 0 1 0 0 1 0 1 0 1) '(1 1 1 0 1 0 0 1 0 1)) -> #f
;;
;;  (map-with-escaping macro-compare-2-bits '(0) '(1)) -> '(x)
(define-syntax map-with-escaping
  (syntax-rules ()
    ((_ clozure-maker list1 ...) (let* (
					(stop-flag #f)
					(clozure (clozure-maker stop-flag))
					(map-w-esc (eval (map-with-escaping-macro stop-flag)))
					)
				   (map-w-esc clozure list1 ...)))))


;; map-with-escaping-by-continuation
;;
;;  return the result or false if computation is abort before end of calculus
;;
;; (map-with-escaping-by-continuation macro-compare-2-bits-with-continuation '(1 0 1) '(1 1 1)) -> '(1 x 1)
;; (map-with-escaping-by-continuation macro-compare-2-bits-with-continuation '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0)) -> '(1 1 0 1 x 1 1 0)
;; (map-with-escaping-by-continuation macro-compare-2-bits-with-continuation '(1 0 1 0 0 1 0 1 0 1) '(1 1 1 0 1 0 0 1 0 1)) -> #f
;;
(define-syntax map-with-escaping-by-continuation
  (syntax-rules ()
    ((_ clozure-maker list1 ...) (call/cc (lambda (kontinuation)
					    (map #;map-with-lambdas (clozure-maker kontinuation) list1 ...))))))




;; map-with-escaping-by-kontinuation
;;
;;  return the result or false if (the) condition is true before end of calculus
;;
;; (map-with-escaping-by-kontinuation macro-compare-2-bits-with-kontinuation '(1 0 1) '(1 1 1)) -> '(1 x 1)
;; (map-with-escaping-by-kontinuation macro-compare-2-bits-with-kontinuation '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0)) -> '(1 1 0 1 x 1 1 0)
;; (map-with-escaping-by-kontinuation macro-compare-2-bits-with-kontinuation '(1 0 1 0 0 1 0 1 0 1) '(1 1 1 0 1 0 0 1 0 1)) -> #f
;;
;; > (map-with-escaping-by-kontinuation macro-compare-2-bits-with-kontinuation '(1 0 1 0 0 1 0 1 0 1) '(1 1 1 0 1 0 0 1 0 1))
;; . . kontinuation: undefined;
;;  cannot reference an identifier before its definition
(define-syntax map-with-escaping-by-kontinuation
  (syntax-rules ()
    ((_ clozure-maker list1 ...) (call/cc (lambda (kontinuation)
					    (map #;map-with-lambdas (clozure-maker) list1 ...))))))

;; > (define cloz (macro-compare-2-bits-with-kontinuation))
;; > cloz
;; #<procedure:cloz>
;; > (map-with-escaping-by-kontinuation-clozure cloz '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0))
;; '(1 1 0 1 x 1 1 0)
;;
;; (define closer-magazine (let ((cnt 0)) ;; counter
;;			(lambda (b1 b2) (if (equal? b1 b2)
;;					  b1
;;					  (begin
;;					    (set! cnt (add1 cnt))
;;					    (when (> cnt 1) (kontinuation #f)) ;; escaping with the continuation
;;					    'x)))))
;; (map-with-escaping-by-kontinuation-clozure closer-magazine '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0)) -> '(1 1 0 1 x 1 1 0)
;;
;; Warning:
;; (map-with-escaping-by-kontinuation-clozure (macro-compare-2-bits-with-kontinuation) '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 1))
;; . . kontinuation: undefined;
;;  cannot reference an identifier before its definition
;;
;; incompatible with DrRacket , too much hygiene in macros?
(define-syntax map-with-escaping-by-kontinuation-clozure
  (syntax-rules ()
    ((_ clozure list1 ...) (call/cc (lambda (kontinuation)
					    (map #;map-with-lambdas clozure list1 ...))))))




;; return a lambda
(define-syntax map-with-escaping-macro ;; i need a macro because of external variable to the function
  (syntax-rules ()
    ((_ condition) (letrec (
			    (some? (lambda (fct list)
				       ;; returns #f if (function x) returns #t for 
				       ;; some x in the list
				       (and (pair? list)
					    (or (fct (car list))
						(some? fct (cdr list))))))

			    ;; (map1 (lambda (fct list)
			    ;; 	      ;; non-variadic map.  Returns a list whose elements
			    ;; 	      ;; the result of calling function with corresponding
			    ;; 	      ;; elements of list
			    ;; 	      (if (null? list)
			    ;; 		  '()
			    ;; 		  (cons (fct (car list))
			    ;; 			(map1 fct (cdr list))))))

			    (map-with-escape  (lambda (function list1 . more-lists)
						(let ((lists (cons list1 more-lists)))
						  (if (or condition ;; escaping
                                                          (some? null? lists))
                                                      '()
                                                      (cons (apply function (map car lists))
                                                            (apply map-with-escape function (map cdr lists)))))))
			    )

		     (lambda (function list1 . more-lists)
		       (let* (
			      (lists (cons list1 more-lists))
			      (result-map (apply map-with-escape function lists))
			     )
			 (if condition
			     #f
			     result-map)))))))


;; a map that the same list twice but with a shift
(define (map-2-shift fct lst)
  (map fct (remove-last lst) (rest lst)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A BIG LOT OF DEFINITIONS OF MAP FUNCTIONS  !!!



;; function-map-with-escaping-by-continuation
;; the same as macro map-with-escaping-by-continuation but with a function !!!
;; (function-map-with-escaping-by-continuation function-compare-2-bits-with-continuation   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0)) -> '(1 1 0 1 x 1 1 0)

;; (function-map-with-escaping-by-continuation function-compare-2-bits-with-continuation   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0)) -> #f
;; Warning : la fonction renvoie deux valeurs differentes a cause du static... pas terrible! il faut utiliser une macro:  macro-function-compare-2-bits-with-continuation
 ;; (function-map-with-escaping-by-continuation (macro-function-compare-2-bits-with-continuation)   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0)) -> '(1 1 0 1 x 1 1 0)
 ;; (function-map-with-escaping-by-continuation (macro-function-compare-2-bits-with-continuation)   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0)) -> '(1 1 0 1 x 1 1 0)
 ;; (function-map-with-escaping-by-continuation (macro-function-compare-2-bits-with-continuation)   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 1)) -> #f
(define (function-map-with-escaping-by-continuation clozure list1 . more-lists)
  (call/cc (lambda (kontinuation)
	     (letrec (
		      (some? (lambda (fct list)
			       ;; returns #f if (function x) returns #t for 
			       ;; some x in the list
			       (and (pair? list)
				    (or (fct (car list))
					(some? fct (cdr list))))))
		      (inner-map
		       (lambda (clozure list1 . more-lists) ;; clozure is call like this : (clozure continuation arg1 arg2 ...) il peut y avoir autant d'arguments que l'on veux.
	     		
			 ;; variadic map implementation terminates
			 ;; when any of the argument lists is empty.
			 (let ((lists (cons list1 more-lists)))
			   
			   (if (some? null? lists)
			       '()
			       (let ((args (map car lists))
				     (rest-args (map cdr lists)))
				 ;; (dv list1)
				 ;; (dv more-lists)
				 ;; (dv lists)
				 ;; (dv args)
				 ;; (dv rest-args)
				 (cons (apply clozure kontinuation args)
				       (apply inner-map clozure rest-args)))))))
		      
		       (lists (cons list1 more-lists))
		       )
	          (newline)
	          (dv list1)
	          (dv more-lists)
	          (dv lists)
		  (dv clozure)
	          (newline)
	       (apply inner-map clozure lists)))))


;; (function-map-with-escaping-by-kontinuation (macro-return-function-compare-2-bits-with-kontinuation)   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 1))

;; list1 = (1 1 0 1 0 1 1 0)
;; more-lists = ((1 1 0 1 1 1 1 1))
;; lists = ((1 1 0 1 0 1 1 0) (1 1 0 1 1 1 1 1))
;; clozure = #<procedure:...gos-DrRacket.scm:206:24>

;; . . kontinuation: undefined;
;;  cannot reference an identifier before its definition
;; > 
(define (function-map-with-escaping-by-kontinuation clozure list1 . more-lists)
  (call/cc (lambda (kontinuation)
	     (let ((lists (cons list1 more-lists)))

	          (newline)
	          (dv list1)
	          (dv more-lists)
	          (dv lists)
		  (dv clozure)
	          (newline)

	       (apply map clozure lists)))))


;;  (function-map-with-escaping-by-kontinuation-compact (macro-function-compare-2-bits-with-continuation)   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 1))

;; list1 = (1 1 0 1 0 1 1 0)
;; more-lists = ((1 1 0 1 1 1 1 1))
;; lists = ((1 1 0 1 0 1 1 0) (1 1 0 1 1 1 1 1))
;; clozure = #<procedure:...gos-DrRacket.scm:195:11>

;; . . map: contract violation
;;   expected: list?
;;   given: #<continuation>
;;   argument position: 2nd
;;   other arguments.:
(define (function-map-with-escaping-by-kontinuation-compact clozure list1 . more-lists)
  (call/cc (lambda (kontinuation)
	     (let ((lists (cons list1 more-lists)))

	          (newline)
	          (dv list1)
	          (dv more-lists)
	          (dv lists)
		  (dv clozure)
	          (newline)

	       (apply map clozure kontinuation lists)))))


;; (function-map-with-escaping-by-kontinuation2 (macro-function-compare-2-bits-with-continuation)   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 1))

;; list1 = (1 1 0 1 0 1 1 0)
;; more-lists = ((1 1 0 1 1 1 1 1))
;; lists = ((1 1 0 1 0 1 1 0) (1 1 0 1 1 1 1 1))
;; clozure = #<procedure:...gos-DrRacket.scm:195:11>

;; #f
;;
;;  (function-map-with-escaping-by-kontinuation2 (macro-function-compare-2-bits-with-continuation)    '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 0))

;; list1 = (1 1 0 1 0 1 1 0)
;; more-lists = ((1 1 0 1 1 1 1 0))
;; lists = ((1 1 0 1 0 1 1 0) (1 1 0 1 1 1 1 0))
;; clozure = #<procedure:...gos-DrRacket.scm:195:11>

;; '(1 1 0 1 x 1 1 0)
(define (function-map-with-escaping-by-kontinuation2 clozure list1 . more-lists)
  (call/cc (lambda (kontinuation)
	     (let ((lists (cons list1 more-lists))
		   (funct-continu ;; this function have the kontinuation in his environment 
		    (lambda (arg1 . more-args)
		      (let ((args (cons arg1 more-args)))
			(apply clozure kontinuation args))))) ;; a tester: (apply clozure (cons conti args))
		     
	          ;; (newline)
	          ;; (dv list1)
	          ;; (dv more-lists)
	          ;; (dv lists)
		  ;; (dv clozure)
	          ;; (newline)

	       (apply map funct-continu lists)))))



;; > (function-map-with-escaping-by-continuation2 (macro-return-function-compare-2-bits-with-kontinuation)   '(1 1 0 1 0 1 1 0) '(1 1 0 1 1 1 1 1))

;; list1 = (1 1 0 1 0 1 1 0)
;; more-lists = ((1 1 0 1 1 1 1 1))
;; lists = ((1 1 0 1 0 1 1 0) (1 1 0 1 1 1 1 1))
;; clozure = #<procedure:...gos-DrRacket.scm:206:24>

;; . . kontinuation: undefined;
;;  cannot reference an identifier before its definition
;; > 
(define (function-map-with-escaping-by-continuation2 clozure list1 . more-lists)
  (call/cc (lambda (kontinuation)
	     (letrec (
		      (some? (lambda (fct list)
			       ;; returns #f if (function x) returns #t for 
			       ;; some x in the list
			       (and (pair? list)
				    (or (fct (car list))
					(some? fct (cdr list))))))
		      (inner-map
		       (lambda (clozure list1 . more-lists) ;; clozure is call like this : (clozure arg1 arg2 ...)
	     		
			 ;; variadic map implementation terminates
			 ;; when any of the argument lists is empty.
			 (let ((lists (cons list1 more-lists)))
			   
			   (if (some? null? lists)
			       '()
			       (let ((args (map car lists))
				     (rest-args (map cdr lists)))
				 ;; (dv list1)
				 ;; (dv more-lists)
				 ;; (dv lists)
				 ;; (dv args)
				 ;; (dv rest-args)
				 (cons (apply clozure args)
				       (apply inner-map clozure rest-args)))))))
		      
		       (lists (cons list1 more-lists))
		       )
	          (newline)
	          (dv list1)
	          (dv more-lists)
	          (dv lists)
		  (dv clozure)
	          (newline)
	       (apply inner-map clozure lists)))))








;; map-nil : a map version that exclude nil results
;; the same as map-with-lambdas but will exclude from the result list the '() result of function
;;
;; (map-nil + '(1 2 3) '(4 5 6)) -> '(5 7 9)
;; (map-nil (lambda (a b) (if (= a 2) '() (+ a b))) '() '(1 2 3) '(4 5 6)) -> '(5 9)
(define map-nil
  (lambda (function list1 . more-lists)
    (letrec ((some?
	      (lambda (fct list)
		;; returns #t if (function x) returns #t for 
		;; some x in the list
		(and (pair? list)
		     (or
		      (fct (car list))
		      (some? fct (cdr list)))))))
      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      (let ((lists (cons list1 more-lists)))
	(if (some? null? lists)
	    '()
	    (let ((funct-result (apply function (map car lists))))
	      (if (null? funct-result)
		  (apply map-nil function (map cdr lists))
		  (cons funct-result
			(apply map-nil function (map cdr lists))))))))))

;; map-nil-iter : a map version that exclude nil results
;; the same as map-with-lambdas but will exclude from the result list the '() result of function
;;
;; (map-nil-iter + '() '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter (lambda (a b) (if (= a 2) '() (+ a b))) '() '(1 2 3) '(4 5 6)) -> '(5 9)
(define map-nil-iter
  
  (lambda (function result list1 . more-lists)

    (letrec ((some?
	      (lambda (fct list)
		;; returns #t if (function x) returns #t for 
		;; some x in the list
		(and (pair? list)
		     (or
		      (fct (car list))
		      (some? fct (cdr list)))))))

      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      (let ((lists (cons list1 more-lists)))

	(if (some? null? lists)
	    result
	    (let* ((funct-result (apply function (map car lists))))

	      (if (null? funct-result)
		  (apply map-nil-iter function result (map cdr lists))
		  (apply
		   map-nil-iter
		   function
		   (append result (list funct-result))
		   (map cdr lists)))))))))


;; (map-nil-iter-call +  '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter-call (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3) '(4 5 6))  -> '(5 9)
(define map-nil-iter-call
  (lambda (function list1 . more-lists)
    (apply map-nil-iter function '() (cons list1 more-lists))))



;; map-nil-iter-optim : a map version that exclude nil results
;; the same as map-with-lambdas but will exclude from the result list the '() result of function
;;
;; (map-nil-iter-optim + '() '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter-optim (lambda (a b) (if (= a 2) '() (+ a b))) '() '(1 2 3) '(4 5 6)) -> '(5 9)
(define map-nil-iter-optim
  
  (lambda (function result list1 . more-lists)

    (letrec ((some?
	      (lambda (fct list)
		;; returns #t if (function x) returns #t for 
		;; some x in the list
		(and (pair? list)
		     (or
		      (fct (car list))
		      (some? fct (cdr list)))))))

      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      (let ((lists (cons list1 more-lists)))

	(if (some? null? lists)
	    result
	    (let* ((funct-result (apply function (map car lists))))

	      (if (null? funct-result)

		  (apply map-nil-iter-optim function result (map cdr lists))

		  (apply
		   map-nil-iter-optim
		   function
		   (cons funct-result result) ;; cons now create a reversed result list
		   (map cdr lists)))))))))



;; (map-nil-iter-optim-call +  '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter-optim-call (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3) '(4 5 6))  -> '(5 9)
(define map-nil-iter-optim-call
  (lambda (function list1 . more-lists)
    (reverse ;; cons had created a reversed result list
     (apply map-nil-iter-optim function '() (cons list1 more-lists)))))



;; map-nil-iter-optim-tail-calls : a map version that exclude nil results
;; the same as map-with-lambdas but will exclude from the result list the '() result of function
;;
;; (map-nil-iter-optim-tail-calls + '() '((1 2 3) (4 5 6)))  -> '(9 7 5)
;; (map-nil-iter-optim-tail-calls (lambda (a b) (if (= a 2) '() (+ a b))) '() '((1 2 3) (4 5 6))) -> '(9 5)
(define map-nil-iter-optim-tail-calls
  
  (lambda (function result lists)
    
    (letrec ((some?
	      (lambda (fct list)
		;; returns #t if (function x) returns #t for 
		;; some x in the list
		(and (pair? list)
		     (or
		      (fct (car list))
		      (some? fct (cdr list)))))))

      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      
      (if (some? null? lists)
	  
	  result
	  
	  (let* ((funct-result (apply function (map car lists))))
	    
	    (if (null? funct-result)

		(map-nil-iter-optim-tail-calls function result (map cdr lists))
		
		(map-nil-iter-optim-tail-calls
		 function
		 (cons funct-result result) ;; cons now create a reversed result list
		 (map cdr lists))))))))


;; (map-nil-iter-optim-tail-calls-call +  '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter-optim-tail-calls-call (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3) '(4 5 6))  -> '(5 9)
(define map-nil-iter-optim-tail-calls-call
  (lambda (function list1 . more-lists)
    (reverse ;; cons had created a reversed result list
     (apply map-nil-iter-optim-tail-calls
	    function
	    '()
	    (list
	     (cons list1 more-lists))))))


;; map-nil-iter-optim-tail-calls-fast : a map version that exclude nil results
;; the same as map-with-lambdas but will exclude from the result list the '() result of function
;;

(define map-nil-iter-optim-tail-calls-fast
  
  (lambda (function lists)
    
    (letrec ((some?
	      (lambda (fct list)
		;; returns #t if (function x) returns #t for 
		;; some x in the list
		(and (pair? list)
		     (or
		      (fct (car list))
		      (some? fct (cdr list)))))))

      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      
      (if (some? null? lists)
	  
	  '()
	  
	  (let ((funct-result (apply function (map car lists))))
	    
	    (if (null? funct-result)

		(map-nil-iter-optim-tail-calls-fast function (map cdr lists))
		
		(cons
		 funct-result
		 (map-nil-iter-optim-tail-calls-fast
		  function
		  (map cdr lists)))))))))


;; (map-nil-iter-optim-tail-calls-fast-call +  '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter-optim-tail-calls-fast-call (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3) '(4 5 6))  -> '(5 9)
;; (map-nil-iter-optim-tail-calls-fast-call (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3 7) '(4 5 6)) -> '(5 9)
(define map-nil-iter-optim-tail-calls-fast-call
  (lambda (function list1 . more-lists)
    (apply map-nil-iter-optim-tail-calls-fast
	   function
	   (list
	    (cons list1 more-lists)))))


;; map-nil-iter-optim-tail-calls-fast-no-nested-let : a map version that exclude nil results
;; the same as map-with-lambdas but will exclude from the result list the '() result of function
;;
(define map-nil-iter-optim-tail-calls-fast-no-nested-let
  
  (lambda (function lists)
          
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      
      (if (some? null? lists)
	  
	  '()
	    
	    (if (null? (apply function (map car lists)))

		(map-nil-iter-optim-tail-calls-fast-no-nested-let function (map cdr lists))
		
		(cons
		 (apply function (map car lists))
		 (map-nil-iter-optim-tail-calls-fast-no-nested-let
		  function
		  (map cdr lists)))))))


;; (map-nil-iter-optim-tail-calls-fast-no-nested-let-call +  '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter-optim-tail-calls-fast-no-nested-let-call (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3) '(4 5 6))  -> '(5 9)
;; (map-nil-iter-optim-tail-calls-fast-no-nested-let-call (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3 4) '(5 6 7)) -> '(6 10)
(define map-nil-iter-optim-tail-calls-fast-no-nested-let-call
  (lambda (function list1 . more-lists)
    (apply map-nil-iter-optim-tail-calls-fast-no-nested-let
	   function
	   (list
	    (cons list1 more-lists)))))




;; map-nil-iter-splice : a map version that exclude nil results
;; the same as map-with-lambdas but will exclude from the result list the '() result of function
;;
;; (map-nil-iter-splice + '() '(1 2 3) '(4 5 6))  -> '(5 7 9)
;; (map-nil-iter-splice (lambda (a b) (if (= a 2) '() (+ a b))) '() '(1 2 3) '(4 5 6)) -> '(5 9)
;;
;; WARNING: this version does not work!
(define map-nil-iter-splice
  
  (lambda (function result list1 . more-lists)

    (display-nl "map-nil-iter-splice : ")
    
    
    (letrec ((some?
	      (lambda (fct list)
		;; returns #t if (function x) returns #t for 
		;; some x in the list
		(and (pair? list)
		     (or
		      (fct (car list))
		      (some? fct (cdr list)))))))
      
      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      (let ((lists (cons list1 more-lists)))

	(dv lists)
	
	(if (some? null? lists)
	    result
	    (let* ((mcar-lists (map car lists))
		   (mcdr-lists (map cdr lists))
		   (ca-mcdr-lists (car mcdr-lists))
		   (cd-mcdr-lists (cdr mcdr-lists))
		   (funct-result 
		    (begin
		      (dv list1)
		      (dv more-lists)
		      (dv function)
		      (dv result)
		      (dv mcar-lists)
		      (dv mcdr-lists)
		      (dv ca-mcdr-lists)
		      (dv cd-mcdr-lists)
		      (apply function mcar-lists))))

	      (if (null? funct-result)
		  
		    (eval `(,map-nil-iter-splice
			    ,function
			    ,result
			    ,(car mcdr-lists) ,@(cdr mcdr-lists)))

		    (eval `(,map-nil-iter-splice
			    ,function
			    ,(append result (list funct-result))
			    ,(car mcdr-lists) ,@(cdr mcdr-lists))))))))))



;; Chaw definition of map-nil
;; (map/remove-nulls-1 (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3) '(4 5 6)) -> '(5 9)
;;  (map/remove-nulls-1 (lambda (a b) (if (= a 2) '() (+ a b))) '(1 2 3 7) '(4 5 6)) -> '(5 9)
(define (map/remove-nulls-1 proc . lsts)

  (define (f lsts result)

    (if (some? #;any null? lsts)

        (reverse result)

        (f (map cdr lsts)
           (let ((proc-result (apply proc
                                     (map car lsts))))
             (if (null? proc-result)
                 result
                 (cons proc-result
                       result))))))

  (f lsts '()))



;; commented because already exist in DrRacket
;; (define andmap
;;   (lambda (function list1 . more-lists)
;;     (letrec ((some? (lambda (fct list)
;; 		      ;; returns #f if (function x) returns #t for 
;; 		      ;; some x in the list
;; 		      (and (pair? list)
;; 			   (or (fct (car list))
;; 			       (some? fct (cdr list)))))))

;;       ;; variadic map implementation terminates
;;       ;; when any of the argument lists is empty.
;;       (let ((lists (cons list1 more-lists)))
;; 	(if (some? null? lists)
;; 	    #t
;; 	    (and (apply function (map car lists))
;; 		 (apply andmap function (map cdr lists))))))))

;; (my-map + '(1 2 3) '(4 5 6)) -> '(5 7 9)
;; (my-map + '(1 2 3) '(4 5 6) '(7 8 9)) -> '(12 15 18)
(define (my-map function list1 . list-of-lists) ; list of lists of arguments, ex: ((4 5 6) (7 8 9)) in the previous example

  (define (some? function list)
    ;; returns true if for at least one element x of list function(x) is true
    ;;  
    ;; returns #f if (function x) returns #f for 
    ;; some x in the list
    (and (pair? list)
         (or (function (car list))
             (some? function (cdr list)))))

  (define (map1 function list)
    ;; non-variadic map.  Returns a list whose elements
    ;; the result of calling function with corresponding
    ;; elements of list
    (if (null? list)
        '()
        (cons (function (car list))
              (map1 function (cdr list)))))
  
  ;; variadic map implementation terminates
  ;; when any of the argument lists is empty.
  (let ((lists (cons list1 list-of-lists))) ;; ex: (my-map + '(1 2 3) '(4 5 6) '(7 8 9)) 
    ;; '(1 2 3) '(4 5 6) '(7 8 9) ==> list1 = (1 2 3) , list-of-lists = ((4 5 6) (7 8 9))
    (if (some? null? lists) ;; lists = ((1 2 3) (4 5 6) (7 8 9))
        '()
        (cons (apply function (map1 car lists)) ;; ex: (apply + (1 4 7))
              (apply my-map function (map1 cdr lists)))))) ;; (apply my-map + ((2 3) (5 6) (8 9))) 



;; (my-map2 + '(1 2 3) '(4 5 6)) -> '(5 7 9)
(define (my-map2 function list1 . list-of-lists)

  (define (some? function list)
    ;; returns #f if (function x) returns #t for 
    ;; some x in the list
    (and (pair? list)
         (or (function (car list))
             (some? function (cdr list)))))

  (define (map1 function list)
    ;; non-variadic map.  Returns a list whose elements
    ;; the result of calling function with corresponding
    ;; elements of list
    (if (null? list)
        '()
        (cons (function (car list))
              (map1 function (cdr list)))))
  
  ;; variadic map implementation terminates
  ;; when any of the argument lists is empty.
  (let ((lists (cons list1 list-of-lists)))
    ;;(display-nl list1)
    ;;(display-nl list-of-lists)
    ;;(display-nl lists)
    (if (some? null? lists)
        '()
        (cons (apply function (map1 car lists))
              (apply my-map2 ;; the reason it works is given in the R4RS definition of apply:
		     ;; procedure:  (apply proc arg1 ... args) 
		     ;; Proc must be a procedure and args must be a list. Calls proc with the elements of the list (append (list arg1 ...) args) as the actual arguments.
		     (cons 
		      function
		      (map1 cdr lists)))))))



;; the same as my-map but defined with lambdas
;;
;; (my-map-with-lambdas + '(1 2 3) '(4 5 6)) -> '(5 7 9)
(define my-map-with-lambdas
  (lambda (function list1 . list-of-lists)
    (letrec (
	     (some? (lambda (fct list)
		      ;; returns #f if (function x) returns #t for 
		      ;; some x in the list
		      (and (pair? list)
			   (or (fct (car list))
			       (some? fct (cdr list))))))

	     (map1 (lambda (fct list)
		     ;; non-variadic map.  Returns a list whose elements
		     ;; the result of calling function with corresponding
		     ;; elements of list
		     (if (null? list)
			 '()
			 (cons (fct (car list))
			       (map1 fct (cdr list))))))
	     )
      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      (let ((lists (cons list1 list-of-lists)))
	(if (some? null? lists)
	    '()
	    (cons (apply function (map1 car lists))
		  (apply my-map-with-lambdas function (map1 cdr lists))))))))

;; the same as my-map-with-lambdas but defined with map instead of map1
;;
;; (map-with-lambdas + '(1 2 3) '(4 5 6)) -> '(5 7 9)
(define map-with-lambdas
  (lambda (function list1 . list-of-lists)
    (letrec (
	     (some? (lambda (fct list)
		      ;; returns #f if (function x) returns #t for 
		      ;; some x in the list
		      (and (pair? list)
			   (or (fct (car list))
			       (some? fct (cdr list))))))

	     ;; (map1 (lambda (fct list)
	     ;; 	     ;; non-variadic map.  Returns a list whose elements
	     ;; 	     ;; the result of calling function with corresponding
	     ;; 	     ;; elements of list
	     ;; 	     (if (null? list)
	     ;; 		 '()
	     ;; 		 (cons (fct (car list))
	     ;; 		       (map1 fct (cdr list))))))
	     )
      
      ;; variadic map implementation terminates
      ;; when any of the argument lists is empty.
      (let ((lists (cons list1 list-of-lists)))
	(if (some? null? lists)
	    '()
	    (cons (apply function (map car lists))
		  (apply map-with-lambdas function (map cdr lists))))))))


;; andmap (one argument version)
;; (define andmap
;;   (lambda (f l)
;;     (if (null? l)
;;         #t
;;         (and (f (car l))
;;              (andmap f (cdr l))))))
