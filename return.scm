
;; scheme@(guile-user)> (def (foo) (when #t (return "hello") "bye"))
;; scheme@(guile-user)> (foo)
;;  "hello"

;; (def x)

;; TODO: study def of a recursive function
(define-syntax def
  
    (lambda (stx)
      (syntax-case stx (declare)

	;; (def (declare x y z) 1 2 (list x y z))
	;; (() () ())

	((_ (declare var1 ...) <body> ...) #`(letrec ((var1 '()) ...)
					       <body> ...))
	
	;;  (def (foo) (when #t (return "hello") "bye"))
        ((_ (<name> <arg> ...) <body>  ...)
         (let ((ret-id (datum->syntax stx 'return)))
           #`(define (<name> <arg> ...)
               (call/cc (lambda (#,ret-id) <body>  ...)))))

	;; definition without a value assigned
	;; (def x)
	((_ var) #`(define var '()))

	;; (def x 7)
	((_ var expr) #`(define var expr))

	
	

	)))


;; definition and assignment
;; { x <+ 7 } is equivalent to : (<- x 7) or (define x 7)
(define-syntax <+
  (syntax-rules ()
    ((_ var expr) (define var expr))))

