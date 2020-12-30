;; locals as in clojure ,no useless brackets




;; (let-rec* (x 1
;;            y (+ x 1))
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

;; (let-rec* (x 1)
;;          x
;;          (+ x 1))
;; = 2

;; (let-rec* () 7) -> 7

;; (let-rec* () )

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
(define-syntax let<-rec*

  (syntax-rules (<-)

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ (var <- val) expr  ...) (letrec ((var val)) expr ...))
    
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (letrec ((var1 val1))
						     (let<-rec* (var2 <- val2 ...) expr ...))) ))

;; (let<-* (x <- 1
;;          y <- (+ x 1)
;;          z <- (+ 2 y))
;;       z) = 4
(define-syntax let<-*

  (syntax-rules (<-)

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ (var <- val) expr  ...) (let ((var val)) expr ...))
    
    ((_ (var1 <- val1 var2 <- val2 ...) expr  ...) (let ((var1 val1))
						     (let<-* (var2 <- val2 ...) expr ...))) ))





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
			 
