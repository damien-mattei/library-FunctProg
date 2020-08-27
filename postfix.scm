


;; postfix notation


;; do (load "postfix.scm") , the last definitions loading must be done by hand, not well understanding why....

;; macro recursive version will definitely not work,why? perheaps recursive call cause infinite loop in this macro expansion context
(define-syntax postfix-bad
  (syntax-rules ()
    ((_ L)  (if (not-list? L)
		L
		(let* ((revL (reverse L))
		       (fct (first L))
		       (args (reverse (rest (revL))))
		       (post-fix-args (map (lambda (L2) (postfix-bad L2)) args)))

		  (apply fct post-fix-args))))))

		       


;; scheme@(guile-user)> (postfix (2 3 +))

;; L = (2 3 +)
;; revL = (+ 3 2)
;; args = (2 3)
;; $9 = (+ 2 3)


;; scheme@(guile-user)> (postfix (((x x *) 3 +) sin))

;; L = (((x x *) 3 +) sin)
;; revL = (sin ((x x *) 3 +))
;; args = (((x x *) 3 +))
;; L = ((x x *) 3 +)
;; revL = (+ 3 (x x *))
;; args = ((x x *) 3)
;; L = (x x *)
;; revL = (* x x)
;; args = (x x)
;; $10 = (sin (+ (* x x) 3))

(define-syntax postfix
  (syntax-rules ()
    ((_ L) (let* ((qL (quote L)) ;; quoted List expression
		  (pqL (postfix-rec qL)) ;; postfix quoted list expression
		 )
	     pqL))))
	     



;; scheme@(guile-user)> (postfix-rec '(2 3 +))
;; L = (2 3 +)
;; revL = (+ 3 2)
;; args = (2 3)
;; $1 = (+ 2 3)

;; examples are with calculus
;; basically i do not expect to use postfix with calculus ,rather with function composition

;; scheme@(guile-user)> (postfix-rec '(((x x *) 3 +) sin))
;; L = (((x x *) 3 +) sin)
;; revL = (sin ((x x *) 3 +))
;; args = (((x x *) 3 +))
;; L = ((x x *) 3 +)
;; revL = (+ 3 (x x *))
;; args = ((x x *) 3)
;; L = (x x *)
;; revL = (* x x)
;; args = (x x)
;; $2 = (sin (+ (* x x) 3))

(define (postfix-rec L)
  ;;(display L)
  ;;(newline)
  (if (not-list? L)
      L
      (let* ((revL (begin
		     (display-symb-nl L)
		     (reverse L)))
	     (fct (begin
		    (display-symb-nl revL)
		    (first revL)))
	     (args (reverse (rest revL)))
	     (post-fix-args
	      (begin
		(display-symb-nl args)
		(map postfix-rec args))))
	
	;;(display-symb-nl revL)
	
	(cons fct post-fix-args))))


;; scheme@(guile-user)> (postfix-evaluation (((x x *) 3 +) sin))
;; L = (((x x *) 3 +) sin)
;; revL = (sin ((x x *) 3 +))
;; args = (((x x *) 3 +))
;; L = ((x x *) 3 +)
;; revL = (+ 3 (x x *))
;; args = ((x x *) 3)
;; L = (x x *)
;; revL = (* x x)
;; args = (x x)
;; L = x
;; L = x
;; fct = *
;; post-fix-args = (2 2)
;; L = 3
;; fct = +
;; post-fix-args = (4 3)
;; fct = sin
;; post-fix-args = (7)
;; $17 = 0.6569865987187891

(define-syntax postfix-evaluation
  (syntax-rules ()
    ((_ L) (let* ((qL (quote L)) ;; quoted List expression
		  (pqL (postfix-rec-eval qL)) ;; postfix quoted list expression
		 )
	     pqL))))


(define-syntax postfix-eval
  (syntax-rules ()
    ((_ L) (let* ((qL (quote L)) ;; quoted List expression
		  (pqL (postfix-rec qL)) ;; postfix quoted list expression
		 )
	     pqL))))


;; scheme@(guile-user)> (postfix-rec-eval '(((2 5 *) 3 +) sin))
;; L = (((2 5 *) 3 +) sin)
;; revL = (sin ((2 5 *) 3 +))
;; args = (((2 5 *) 3 +))
;; L = ((2 5 *) 3 +)
;; revL = (+ 3 (2 5 *))
;; args = ((2 5 *) 3)
;; L = (2 5 *)
;; revL = (* 5 2)
;; args = (2 5)
;; L = 2
;; L = 5
;; fct = *
;; post-fix-args = (2 5)
;; L = 3
;; fct = +
;; post-fix-args = (10 3)
;; fct = sin
;; post-fix-args = (13)
;; $24 = 0.4201670368266409


;; scheme@(guile-user)> (let ((x 2) (y 3)) (postfix-rec-eval '(x y +)))
;; L = (x y +)
;; revL = (+ y x)
;; args = (x y)
;; L = x
;; L = y
;; fct = +
;; post-fix-args = (2 3)
;; $28 = 5


;; scheme@(guile-user)> (define x 2)
;; scheme@(guile-user)> (define y 3)
;; scheme@(guile-user)> (postfix-rec-eval '(((x x *) 3 +) sin))
;; L = (((x x *) 3 +) sin)
;; revL = (sin ((x x *) 3 +))
;; args = (((x x *) 3 +))
;; L = ((x x *) 3 +)
;; revL = (+ 3 (x x *))
;; args = ((x x *) 3)
;; L = (x x *)
;; revL = (* x x)
;; args = (x x)
;; L = x
;; L = x
;; fct = *
;; post-fix-args = (2 2)
;; L = 3
;; fct = +
;; post-fix-args = (4 3)
;; fct = sin
;; post-fix-args = (7)
;; $2 = 0.6569865987187891

(define (postfix-rec-eval L)
  ;;(display L)
  ;;(newline)
  (if (not-list? L)
      (begin
	(display-symb-nl L)
	;; using scheme-report-environment has no user definition !!!
	;;(eval L (scheme-report-environment 5)))
	(eval L (interaction-environment)))
      (let* ((revL (begin
		     (display-symb-nl L)
		     (reverse L)))
	     (fct (begin
		    (display-symb-nl revL)
		    (first revL)))
	     (args (reverse (rest revL)))
	     (post-fix-args
	      (begin
		(display-symb-nl args)
		(map postfix-rec-eval args))))
	
	(display-symb-nl fct)
	(display-symb-nl post-fix-args)

	;; (eval (cons fct post-fix-args) (scheme-report-environment 5))))) ;; not good

	;; (eval (cons fct post-fix-args) interaction-environment)))))

	(app-lst-mac fct post-fix-args))))

 	;;(apply fct post-fix-args)))) ;; not good

        ;; (apply (eval fct (interaction-environment))
	;;        (map (lambda (t) (eval t (interaction-environment)))
	;; 	    post-fix-args))))) 


;; scheme@(guile-user)> (app-lst sin '(0.7))
;; $12 = 0.644217687237691

;; scheme@(guile-user)> (app-lst + '(0.7 1))
;; $13 = 1.7

(define (app-lst f L)
  (apply f L))


;; scheme@(guile-user)> (app-lst-mac sin (list 0.7))
;; $8 = 0.644217687237691

;; scheme@(guile-user)> (app-lst-mac sin (list 'x))
;; $6 = 0.9092974268256817
;; scheme@(guile-user)> (app-lst-mac sin '(x))
;; $7 = 0.9092974268256817

(define-syntax app-lst-mac
  (syntax-rules ()
    ((_ f L) (apply (eval f (interaction-environment))
		    (map (lambda (t) (eval t (interaction-environment)))
			 L)))))



;; scheme@(guile-user)> (define y (list '+ x 5))
;; scheme@(guile-user)> x
;; $3 = 2
;; scheme@(guile-user)> y
;; $4 = (+ 2 5)
;; scheme@(guile-user)> y
;; $5 = (+ 2 5)
;; scheme@(guile-user)> (eval y)
;; ;;; <stdin>:13:0: warning: possibly wrong number of arguments to `eval'
;; ERROR: In procedure eval:
;; Wrong number of arguments to #<procedure eval (_ _)>

;; Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.
;; scheme@(guile-user) [1]> ,q
;; scheme@(guile-user)> (eval y (interaction-environment))
;; $6 = 7
;; scheme@(guile-user)> y
;; $7 = (+ 2 5)
;; scheme@(guile-user)> (define z (list '+ 'x 5))
;; scheme@(guile-user)> z
;; $8 = (+ x 5)
;; scheme@(guile-user)> (define q (list '+ x 5))
;; scheme@(guile-user)> q
;; $9 = (+ 2 5)
;; scheme@(guile-user)> (eval q (interaction-environment))
;; $10 = 7
;; scheme@(guile-user)> (eval z (interaction-environment))
;; $11 = 7
