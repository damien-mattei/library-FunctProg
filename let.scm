;; local as in clojure ,no useless brackets

;; scheme@(guile-user)> (local (x 1 y 2) x)
;; $1 = 1
(define-syntax local

  (syntax-rules ()

    ((_ bindings body) (let ((bracketed-paired-bindings (pair-list-elements (quote bindings))))
			 ;; (eval (quasiquote
			 ;; 	(letrec* (unquote bracketed-paired-bindings)
			 ;; 		 body))
			 ;;       (interaction-environment))
			 
			 (eval (list 'letrec* bracketed-paired-bindings 'body)
			       (interaction-environment))
			 ))))


;; will never works ! :
;; (define-syntax local-let

;;   (syntax-rules ()

;;     ((_ (var val ...) body) (letrec* ((var val) ...)
;; 				     body))))

    ;; ((_ (var val) body) (letrec ((var val))
    ;; 			  body))))
			 

    
