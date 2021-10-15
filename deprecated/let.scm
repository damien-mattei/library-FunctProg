;; locals as in clojure ,no useless brackets




;; (let-rec* [x 1
;;            y (+ x 1)]
;;          y)
;; $1 = 2
;; scheme@(guile-user)> (let-rec* ( x 1
;;                                  y (+ x 1)
;;                                  z (+ 2 y) )
;;                               z)
;; $2 = 4

;; scheme@(guile-user)> (let-rec* (x 1)
;;                               x)
;; $2 = 1

;; (let-rec* () 7) -> 7

;; (let-rec* () )
;; DEPRECATED
(define-syntax let-rec*

  (syntax-rules ()

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ (var val) expr  ...) (letrec ((var val)) expr ...))
    
    ((_ (var1 val1 var2 val2 ...) expr  ...) (letrec ((var1 val1))
					       (let-rec* (var2 val2 ...) expr ...))) ))

;; (local (x 1 y (+ x 1) z (+ 2 y)) z y) -> 2

;; (local [ x 1
;; 	    y (+ x 1)
;; 	    z (+ 2 y) ]
;;        z y)
;; 2

(define-syntax local

  (syntax-rules ()

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ (var val) expr  ...) (let ((var val)) expr ...))
    
    ((_ (var1 val1 var2 val2 ...) expr  ...) (let ((var1 val1))
					       (local (var2 val2 ...) expr ...))) ))


;;  special forms with arrows

;; (let<-rec* (x <- 1
;;             y <- (+ x 1)
;;             z <- (+ 2 y))
;;            z) = 4
;; DEPRECATED
(define-syntax let<-rec*-deprecated

  (syntax-rules (<-)

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ (var <- val) expr  ...) (letrec ((var val)) expr ...))
    
    ((_ (var1 <- val1
	 var2 <- val2 ...)
	expr ...)
     (letrec ((var1 val1))
       (let<-rec*-deprecated (var2 <- val2 ...) expr ...))) ))


;;scheme@(guile-user)> (let<-rec* (x <- 1
;;                                 y <- (+ x 1)
;;                                 z <- (+ 2 y))
;;                               z)
;; 4
;;scheme@(guile-user)> (let<-rec* [x <- 1
;;                                 y <- (+ x 1)
;;                                 z <- (+ 2 y)]
;;                               z)
;; 4
(define-syntax let<-rec*

  (syntax-rules (<-)

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    ((_ (var1 <- val1) expr  ...) (letrec* ((var1 val1)) expr ...)) ;; case single binding
    ((_ (var1 <- val1 ;; multiple binding
	 var2 <- val2 ...)
	expr ...)
     (%parse-letrec-bindings-and-evaluate-expressions ((var1 val1))  (var2 <- val2 ...) expr ...))))

;; recursive macro with accumulator
(define-syntax %parse-letrec-bindings-and-evaluate-expressions

  (syntax-rules (<-)

    ((_ (bindings ...) (var1 <- val1) expr ...)  (letrec* (bindings ... (var1 val1)) ;; last binding
							  expr ...)) ;; we evaluate expressions
    ((_ (bindings ...) (var1 <- val1 ;; multiple binding
			var2 <- val2 ...) expr ...)
     ;; store (var1 val1) binding in accumulator and continue parsing
       (%parse-letrec-bindings-and-evaluate-expressions (bindings ... (var1 val1)) (var2 <- val2 ...) expr ...)))) 
    




;; (let-arrow* (x ← 1
;; 	        y ← {x + 1})
;;             y)
;; $1 = 2

;; (let-arrow* (x <- 1
;;              y <- (+ x 1)
;;              z <- (+ 2 y))
;;             z) = 4

;; (let-arrow* [ x 1 y (+ x 1) z (+ 2 y) ] z y)
;; = 2

;; scheme@(guile-user)>  (let-arrow* ({x ← 1}
;;         {y ← {x + 1}})
;;         x
;;         y)
;; $2 = 2
(define-syntax let-arrow*

  (syntax-rules (<- ->  ← →) ;;  ⟵ ⟶

    ((_ () expr ...) (begin expr ...)) ;;  case empty let

    
    ;; let with arrows
    ((_ (var <- val) expr  ...) (let ((var val)) expr ...))
    ((_ (val -> var) expr  ...) (let ((var val)) expr ...))
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (var2 <- val2 ...) expr ...)))
    ((_ (val1 -> var1 val2 -> var2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (var1 <- val1 val2 -> var2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (val1 -> var1 var2 <- val2 ...) expr  ...) (let ((var1 val1))
    						     (let-arrow* (var2 <- val2 ...) expr ...)))

    ((_ ((<- var val)) expr ...)    (let ((var val)) expr ...))
    ((_ ((<- var1 val1) (<- var2 val2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((<- var2 val2) ...) expr ...)))
    ((_ ((<- var1 val1) (-> val2 var2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val var)) expr ...)    (let ((var val)) expr ...))
    ((_ ((-> val1 var1) (-> val2 var2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val1 var1) (<- var2 val2) ...) expr  ...)     (let ((var1 val1))
							     (let-arrow* ((-> val2 var2) ...) expr ...)))
    
    ((_ (var ← val) expr  ...) (let ((var val)) expr ...))
    ((_ (val → var) expr  ...) (let ((var val)) expr ...))
    ((_ (var1 ← val1 var2 ← val2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (var2 ← val2 ...) expr ...)))
    ((_ (val1 → var1 val2 → var2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (val2 → var2 ...) expr ...)))
    ((_ (var1 ← val1 val2 → var2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (val2 → var2 ...) expr ...)))
    ((_ (val1 → var1 var2 ← val2 ...) expr  ...) (let ((var1 val1))
    						   (let-arrow* (var2 ← val2 ...) expr ...)))

    ((_ ((← var val)) expr ...)    (let ((var val)) expr ...))
    ((_ ((← var1 val1) (← var2 val2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((← var2 val2) ...) expr ...)))
    ((_ ((← var1 val1) (→ val2 var2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val var)) expr ...)    (let ((var val)) expr ...))
    ((_ ((→ val1 var1) (→ val2 var2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val1 var1) (← var2 val2) ...) expr  ...)     (let ((var1 val1))
							   (let-arrow* ((→ val2 var2) ...) expr ...)))
    

    
    ;; Warning : long arrow causes problems in Mac terminal at least
    ;; ((_ (val ⟶ var) expr  ...) (let ((var val)) expr ...))
    ;; ((_ (var ⟵ val) expr  ...) (let ((var val)) expr ...))
    ;; ((_ (val1 ⟶ var1 val2 ⟶ var2 ...) expr  ...) (let ((var1 val1))
    ;; 						   (let-arrow* (val2 ⟶ var2 ...) expr ...)))
    ;; ((_ (var1 ⟵ val1 var2 ⟵ val2 ...) expr  ...) (let ((var1 val1))
    ;; 						   (let-arrow* (var2 ⟵ val2 ...) expr ...)))
    ;; miss other definitions... for the long arrow

    ;; let with less brackets
    ((_ (var val) expr  ...) (let ((var val)) expr ...))
    ((_ (var1 val1 var2 val2 ...) expr  ...) (let ((var1 val1))
					       (let-arrow* (var2 val2 ...) expr ...)))
    
    ))

;; (letrec-arrow* [ fact ← (lambda (n)
;; 			  (if  {n = 1}
;; 			       1
;;                                {n * (fact {n - 1})}))
;;                           ]
;; 	       (fact 5))

;; = 120

(define-syntax letrec-arrow*

  (syntax-rules (<- ->  ← →) ;;  ⟵ ⟶

    ((_ () expr ...) (begin expr ...)) ;;  case empty letrec

    

    ;; letrec with arrows
    ((_ (var <- val) expr  ...) (letrec ((var val)) expr ...))
    ((_ (val -> var) expr  ...) (letrec ((var val)) expr ...))
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (var2 <- val2 ...) expr ...)))
    ((_ (val1 -> var1 val2 -> var2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (var1 <- val1 val2 -> var2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (val2 -> var2 ...) expr ...)))
    ((_ (val1 -> var1 var2 <- val2 ...) expr  ...) (letrec ((var1 val1))
    						     (letrec-arrow* (var2 <- val2 ...) expr ...)))

    ((_ ((<- var val)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((<- var1 val1) (<- var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((<- var2 val2) ...) expr ...)))
    ((_ ((<- var1 val1) (-> val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val var)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((-> val1 var1) (-> val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((-> val2 var2) ...) expr ...)))
    ((_ ((-> val1 var1) (<- var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							     (letrec-arrow* ((-> val2 var2) ...) expr ...)))
    
    ((_ (var ← val) expr  ...) (letrec ((var val)) expr ...))
    ((_ (val → var) expr  ...) (letrec ((var val)) expr ...))
    ((_ (var1 ← val1 var2 ← val2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (var2 ← val2 ...) expr ...)))
    ((_ (val1 → var1 val2 → var2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (val2 → var2 ...) expr ...)))
    ((_ (var1 ← val1 val2 → var2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (val2 → var2 ...) expr ...)))
    ((_ (val1 → var1 var2 ← val2 ...) expr  ...) (letrec ((var1 val1))
    						   (letrec-arrow* (var2 ← val2 ...) expr ...)))

    ((_ ((← var val)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((← var1 val1) (← var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((← var2 val2) ...) expr ...)))
    ((_ ((← var1 val1) (→ val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val var)) expr ...)    (letrec ((var val)) expr ...))
    ((_ ((→ val1 var1) (→ val2 var2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((→ val2 var2) ...) expr ...)))
    ((_ ((→ val1 var1) (← var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							   (letrec-arrow* ((→ val2 var2) ...) expr ...)))
    ;; letrec with less brackets
    ((_ (var val) expr  ...) (letrec ((var val)) expr ...))
    ((_ (var1 val1 var2 val2 ...) expr  ...) (letrec ((var1 val1))
    					       (letrec-arrow* (var2 val2 ...) expr ...)))
    ))


;; to be removed:
;; rename a special form with another
;; (define-syntax fir

;;   (syntax-rules ()

;;     ((_ (expr ...) stmt ...) (let-arrow* (expr ...) stmt ...))))





;; (let←* [x ← 1
;; 	   y ← {x + 1}]
;;        y)
;; $1 = 2

;; (let←* (x <- 1
;;         y <- (+ x 1)
;;         z <- (+ 2 y))
;;        z) = 4

;; (let←* ({x ← 1}
;;         {y ← {x + 1}})
;;        x
;;        y)
;; = 2
;; DEPRECATED
(define-syntax let←*

  (syntax-rules (<- -> ⟵ ⟶  ← →)

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ (var <- val) expr  ...) (let ((var val)) expr ...))
    ((_ (var ⟵ val) expr  ...) (let ((var val)) expr ...)) ;; Warning : long arrow causes problems in Mac terminal at least
    ((_ (var ← val) expr  ...) (let ((var val)) expr ...))
    
   
    
    ((_ (val -> var) expr  ...) (let ((var val)) expr ...))
    ((_ (val ⟶ var) expr  ...) (let ((var val)) expr ...))
    ((_ (val → var) expr  ...) (let ((var val)) expr ...))


    
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (let ((var1 val1))
						     (let←* (var2 <- val2 ...) expr ...)))
    ((_ (var1 ⟵ val1 var2 ⟵ val2 ...) expr  ...) (let ((var1 val1))
						   (let←* (var2 <- val2 ...) expr ...)))
    ((_ (var1 ← val1 var2 ← val2 ...) expr  ...) (let ((var1 val1))
						   (let←* (var2 ← val2 ...) expr ...)))
    
    
    ((_ (val1 -> var1 val2 -> var2 ...) expr  ...) (let ((var1 val1))
						     (let←* (val2 -> var2 ...) expr ...)))
    ((_ (val1 ⟶ var1 val2 ⟶ var2 ...) expr  ...) (let ((var1 val1))
						   (let←* (val2 -> var2 ...) expr ...)))
    ((_ (val1 → var1 val2 → var2 ...) expr  ...) (let ((var1 val1))
						   (let←* (val2 → var2 ...) expr ...)))

    ((_ ((← var val)) expr ...)    (let ((var val)) expr ...))

    ((_ ((← var1 val1) (← var2 val2) ...) expr  ...)     (let ((var1 val1))
							   (let←* ((← var2 val2) ...) expr ...)))

    ((_ ((→ var val)) expr ...)    (let ((var val)) expr ...))

    ((_ ((→ var1 val1) (→ var2 val2) ...) expr  ...)     (let ((var1 val1))
							   (let←* ((→ var2 val2) ...) expr ...))) ))

;; DEPRECATED
(define-syntax let←rec*

  (syntax-rules (<- -> ⟵ ⟶  ← →)

    ((_ () expr ...) (begin expr ...)) ;;  case empty letrec
    
    ((_ (var <- val) expr  ...) (letrec ((var val)) expr ...))
    ((_ (var ⟵ val) expr  ...) (letrec ((var val)) expr ...)) ;; Warning : long arrow causes problems in Mac terminal at least
    ((_ (var ← val) expr  ...) (letrec ((var val)) expr ...))
    
   
    
    ((_ (val -> var) expr  ...) (letrec ((var val)) expr ...))
    ((_ (val ⟶ var) expr  ...) (letrec ((var val)) expr ...))
    ((_ (val → var) expr  ...) (letrec ((var val)) expr ...))


    
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (letrec ((var1 val1))
						     (let←rec* (var2 <- val2 ...) expr ...)))
    ((_ (var1 ⟵ val1 var2 ⟵ val2 ...) expr  ...) (letrec ((var1 val1))
						   (let←rec* (var2 <- val2 ...) expr ...)))
    ((_ (var1 ← val1 var2 ← val2 ...) expr  ...) (letrec ((var1 val1))
						   (let←rec* (var2 ← val2 ...) expr ...)))
    
    
    ((_ (val1 -> var1 val2 -> var2 ...) expr  ...) (letrec ((var1 val1))
						     (let←rec* (val2 -> var2 ...) expr ...)))
    ((_ (val1 ⟶ var1 val2 ⟶ var2 ...) expr  ...) (letrec ((var1 val1))
						   (let←rec* (val2 -> var2 ...) expr ...)))
    ((_ (val1 → var1 val2 → var2 ...) expr  ...) (letrec ((var1 val1))
						   (let←rec* (val2 → var2 ...) expr ...)))

    ((_ ((← var val)) expr ...)    (letrec ((var val)) expr ...))

    ((_ ((← var1 val1) (← var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							   (let←rec* ((← var2 val2) ...) expr ...)))

    ((_ ((→ var val)) expr ...)    (letrec ((var val)) expr ...))

    ((_ ((→ var1 val1) (→ var2 val2) ...) expr  ...)     (letrec ((var1 val1))
							   (let←rec* ((→ var2 val2) ...) expr ...))) ))

;; DEPRECATED
;;(let-bracket←* ({x ← 1}
;;                {y ← {x + 1}})
;;               x y)
;; $1 = 2

(define-syntax let-bracket←*

  (syntax-rules (<- -> ⟵ ⟶  ← →)

    ((_ ()  expr ...) (begin expr ...)) ;;  case empty let

    ((_ ((← var val)) expr ...)    (let ((var val)) expr ...))

    ((_ ((← var1 val1) (← var2 val2) ...) expr  ...)     (let ((var1 val1))
							   (let-bracket←* ((← var2 val2) ...) expr ...)))    ))



;; DEPRECATED
;; scheme@(guile-user)> (local (x 1 y 2) x)
;; $1 = 1
;; DEPRECATED does not works inside function (arguments not evaluates by 'eval' in interaction-environment)
;; (define-syntax local

;;   (syntax-rules ()

;;     ((_ bindings body)  


;;      (let* ((bracketed-paired-bindings (pair-list-elements (quote bindings))))
	   
			    
;; 			 ;; (eval (quasiquote
;; 			 ;; 	(letrec* (unquote bracketed-paired-bindings)
;; 			 ;; 		 body))
;; 			 ;;       (interaction-environment))
			 
;; 			 ;; (eval (list 'letrec* bracketed-paired-bindings 'body)
;; 			 ;;       (interaction-environment))

;; 			 ((eval (lambda () (list 'letrec* bracketed-paired-bindings 'body))
;; 			       (interaction-environment)))
			 
;; 			 ))))




;; will never works ! :
;; (define-syntax local-let

;;   (syntax-rules ()

;;     ((_ (var val ...) body) (letrec* ((var val) ...)
;; 				     body))))

    ;; ((_ (var val) body) (letrec ((var val))
    ;; 			  body))))
			 

    
;; (define-syntax local-let2

;;   (syntax-rules ()

;;     ((_ (var val ...) expr ...) (letrec* ((var val))
;; 					 (local-let2 (...) expr ...)))

;;     ((_ (var val) expr ...) (letrec* ((var val))
;; 				     expr ...))))
			 
