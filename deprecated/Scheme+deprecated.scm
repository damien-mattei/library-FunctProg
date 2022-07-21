
(include "../array.scm")


;; scheme@(guile-user)> (load "SssDyna.scm")
;; scheme@(guile-user)> (defined-object? x)
;; $1 = #f
;; scheme@(guile-user)> (defined-object? {dyna[2 7]})
;; $2 = #t
;; scheme@(guile-user)> (define x 7)
;; scheme@(guile-user)> (defined-object? x)
;; $3 = #t
	
(define-syntax defined-object?
  (syntax-rules ()
    ((_ var)
     (begin (display "defined-object? : var=") (display (quote var)) (newline)
	    ;;{(not (symbol? (quote var))) or (defined? (quote var))})));;)
	    (or (not (symbol? (quote var))) (defined? (quote var)))))))



;; scheme@(guile-user)> ($  {x <- 7} {y <- 8} (+ x y))
;; ;;; <stdin>:26:0: warning: possibly unbound variable `x'
;; 15

;; scheme@(guile-user)> {{dyna[2 7]} <- 7}
;; ← : multidimensional vector or array set!
;; 7
;; scheme@(guile-user)> ($  {w <- {dyna[2 7]}} w)
;; ;;; <stdin>:14:0: warning: possibly unbound variable `w'
;; %parse-body-assignment :: undefined 
;; $bracket-apply$
;; %parse-body-assignment case 4 : new-expr =w
;; 7

;; ($  {y <- 7} {x <- 8} { z <- (+ z 7)} (+ x  y z))

(define-syntax $
  (syntax-rules ()
    ((_ one-thing-only)
     one-thing-only)
    ((_ stuff ...)
     (%parse-body-assignment () stuff ...)))) ;; at the beginning the accumulator () is empty


;; scheme@(guile-user)> ($  {x <- 7} {y <- 8} (+ x y))
;; ;;; <stdin>:80:0: warning: possibly unbound variable `x'
;; ;;; <stdin>:80:0: warning: possibly unbound variable `y'
;; %parse-body-assignment case 3: var=x expr =7
;; defined-object? : var=x
;; %parse-body-assignment case 3: undefined : var=x expr= 7
;; %parse-assignment : case 2 : name =y expr=8
;; defined-object? : var=y
;; %parse-assignment : case 2 : undefined 
;; %parse-body-assignment case 4 : new-expr=(+ x y)
;; %parse-body-assignment case 1
;; $81 = 15
;; scheme@(guile-user)> x
;; ;;; <stdin>:81:0: warning: possibly unbound variable `x'
;; ice-9/boot-9.scm:1685:16: In procedure raise-exception:
;; Unbound variable: x

;; Entering a new prompt.  Type `,bt' for a backtrace or `,q' to continue.
;; scheme@(guile-user) [1]> ,q
;; scheme@(guile-user)> (define dyna (make-array 0 {10} {20}))
;; scheme@(guile-user)> ($  {w <- {dyna[2 7]}} w)
;; ;;; <stdin>:84:0: warning: possibly unbound variable `w'
;; %parse-body-assignment case 3: var=w expr =($bracket-apply$ dyna 2 7)
;; defined-object? : var=w
;; %parse-body-assignment case 3: undefined : var=w expr= ($bracket-apply$ dyna 2 7)
;; $bracket-apply$ : array=dyna
;; %parse-body-assignment case 4 : new-expr=w
;; %parse-body-assignment case 1
;; $82 = 0
;; scheme@(guile-user)> {{dyna[2 7]} <- 17}
;; ← : multidimensional vector or array set!
;; $83 = 17
;; scheme@(guile-user)> ($  {w <- {dyna[2 7]}} w)
;; ;;; <stdin>:86:0: warning: possibly unbound variable `w'
;; %parse-body-assignment case 3: var=w expr =($bracket-apply$ dyna 2 7)
;; defined-object? : var=w
;; %parse-body-assignment case 3: undefined : var=w expr= ($bracket-apply$ dyna 2 7)
;; $bracket-apply$ : array=dyna
;; %parse-body-assignment case 4 : new-expr=w
;; %parse-body-assignment case 1
;; $84 = 17
;; scheme@(guile-user)> ($  {{dyna[2 7]} <- 3} 1)
;; %parse-body-assignment case 4 : new-expr=1
;; %parse-body-assignment case 1
;; $85 = 1
;; scheme@(guile-user)> {dyna[2 7]}
;; $bracket-apply$ : array=dyna
;; $86 = 3



;; scheme@(guile-user)> ($  {{dyna[2 7]} <- {dyna[1 2]}} {t <- {dyna[2 7]}} {t + 7})
;; ;;; <stdin>:91:0: warning: possibly unbound variable `t'
;; $bracket-apply$ : array=dyna
;; %parse-body-assignment case 3: var=t expr =($bracket-apply$ dyna 2 7)
;; defined-object? : var=t
;; %parse-body-assignment case 3: undefined : var=t expr= ($bracket-apply$ dyna 2 7)
;; $bracket-apply$ : array=dyna
;; %parse-body-assignment case 4 : new-expr=(+ t 7)
;; %parse-body-assignment case 1
;; $89 = 7


;; scheme@(guile-user)> (use-modules (guile define-guile-3))
;; scheme@(guile-user)> (begin {x <- 7} {y <- 8} (+ x y))
;; 15



;; scheme@(guile-user)> (define dyna (make-array 0 {10} {20}))
;; scheme@(guile-user)> {{dyna[2 7]} <- 7}
;; 7
;; scheme@(guile-user)> ($  {x <- 7} {y <- 8} {{dyna[2 7]} <- 7} (+ x y))
;; 15

;;(load "define-guile-3-load.scm")
;; scheme@(guile-user)> (define x 3)
;; scheme@(guile-user)> (new-begin  {x <- 7}  x)
;; 7
;; scheme@(guile-user)> (new-begin  {x <- 7}  x)
;; 7
;; scheme@(guile-user)> (new-begin  {x <- 7} {y <- 8} (+ x y))

;; (load "Scheme+.scm")
;;(define dyna (make-array 0 {10} {20}))
;;(define s 4)
;;($  {{dyna[2 7]} <- {dyna[1 2]}} {t <- {dyna[2 7]}} {x <- {t + 7}} {s <- 2} {x + s})
;; ...
;; 9


;; TODO: change letrec* with let* and use letrec for lambda only, and i must parse the body of lambda too :-(

;;(%parse-assignment (clauses ...) body ...)
;; we start from an empty (clauses ...) list of clauses and append one more clause (name expr) at end
;;
;; this is an accumulator of 'bindings' or clauses
;;
(define-syntax %parse-assignment
  (syntax-rules (<-);)

    ;; array:  {{dyna[2]} <- 7}
    ;; assign and go back to %parse-body-assignment
    ((_ (clauses ...) (<- (funct-or-macro array index) expr) rest ...)
     (letrec* (clauses ... (tmp expr))
	      
	      (display "%parse-assignment : case vector : defined ")
	      (display (quote name)) (display " expr=") (display (quote expr)) (newline)
	      ;;(set! name expr) ;; TODO must merge this macros file and macros of array.scm to make it work with arrays !!!
	      ;;(<- name expr)
	      (if (or (vector? array) (growable-vector? array))
		  (vector-set! array index tmp)
		  (array-set! array tmp index))
	      tmp
	      (%parse-body-assignment () rest ...))) ;; letrec* and assign and continue parsing the rest ...

    ;; multi dimensional array : {{dyna[2 7]} <- 7}
    ;; assign and go back to %parse-body-assignment
    ((_ (clauses ...) (<- (funct-or-macro array index ...) expr) rest ...)
     (letrec* (clauses ... (tmp expr))
	      
	      (display "%parse-assignment : case array : defined ")
	      (display (quote name)) (display " expr=") (display (quote expr)) (newline)
	      ;;(set! name expr) ;; TODO must merge this macros file and macros of array.scm to make it work with arrays !!!
	      ;;(<- name expr)
	      (array-set! array tmp index ...)
	      tmp
	      (%parse-body-assignment () rest ...))) ;; letrec* and assign and continue parsing the rest ...
    
     ;; new assignment : {x <- 7}
     ;; this one recursively call %parse-assignment
     ((_ (clauses ...) (<- name expr) rest ...)
      (begin   (display "%parse-assignment : case 2 : name =") (display (quote name)) (display " expr=") (display (quote expr)) (newline)
	       (if (defined-object? name)
		   (letrec* (clauses ...)
			    (display "%parse-assignment : case 2 : defined ")
			    (display (quote name)) (display " expr=") (display (quote expr)) (newline)
			    (set! name expr) ;; TODO must merge this macros file and macros of array.scm to make it work with arrays !!!
			    ;;(<- name expr)
			    (%parse-body-assignment () rest ...)) ;; letrec* and assign and continue parsing the rest ...
		   (begin (display "%parse-assignment : case 2 : undefined ")  (newline)
			  (%parse-assignment (clauses ... (name expr)) rest ...))))) ;; append one more clause (name expr) at end of clauses list
    
     ;; no more assignment: Exit
     ;; assign and go back to %parse-body-assignment
     ((_ clauses rest ...)
      (letrec* clauses ;; create local bindings :  TODO let do not return value of binding
	       ;; FORBIDDEN :(begin (display "%parse-assignment : case 2 : clauses=") (display clauses) (newline)
	       (%parse-body-assignment () rest ...)))));) ;; and parse-body the rest ...



;; A macro that transverses expressions
;; (%parse-body (seen-expressions ...) rest ...)
;; we store expressions gradually in the first argument list (exprs ...) called the accumulator
;;
;; this is an accumulator of expressions
;;
;; it is the starter %parse-assignment will be the engine....
(define-syntax %parse-body-assignment
  (syntax-rules (<-);) 
  
    ;; Found no definitions. Just exit
    ;; this case is called at the end when the accumulator is full of expressions to evaluate ! not at the start !
    ((_ (exprs ...))
     (begin (display "%parse-body-assignment case 1")  (newline)
       exprs ...)) ;; evaluate exprs ...

    ;; array:  {{dyna[2]} <- 7}
    ((_ (exprs ...) (<- (funct-or-macro array index) expr) rest ...)
     (begin
       exprs ...
       (let ((tmp expr)) ;; to avoid compute it twice
						 
	 ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
	
	 ;; normal case
	 ;; {T[2] ← 4}
	 ;; {T[3] ← T[2]}
	 (begin
	   ;;(display "← : vector or array set!") (newline)
	   (if (or (vector? array) (growable-vector? array))
	       (vector-set! array index tmp)
	       (array-set! array tmp index)))
	 
	 ;; ;; rare case  (to prevent any error)
	 ;; 						 (let ((var (funct-or-macro array index))) ;; MUST be in a variable , otherwise:
	 ;; ;; While compiling expression:
	 ;; ;; Syntax error:
	 ;; ;; unknown location: quote: bad syntax in form quote
	 ;; 						   (display "← : variable set! after creation") (newline)
	 ;; 						   (set! var tmp)))
	
	 tmp)
       (%parse-body-assignment () rest ...)
       
       ))

    ;; multi dimensional array : {{dyna[2 7]} <- 7}
    ((_ (exprs ...) (<- (funct-or-macro array index ...) expr) rest ...)
     (begin
       exprs ...
       (let ((tmp expr)) ;; to avoid compute it twice
						 
	 ;; (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
	 
	  (array-set! array tmp index ...)
	  ;; rare case (to prevent any error)
	  ;; (let ((var (funct-or-macro array index ...))) ;; MUST be in a variable
	  ;;   (display "← : variable set! after creation (multidimensional)") (newline)
	  ;;   (set! var tmp)))
	 tmp)
       (%parse-body-assignment () rest ...)
       
       ))
    
    ;; new assignment : {x <- 7}
    ;; An assignment, call %parse-assignment or set assignment immediately and parse the rest 
    ((_ (exprs ...) (<- var expr) rest ...)

     (begin
       (display "%parse-body-assignment case 3: var=") (display (quote var)) (display " expr =") (display (quote expr)) (newline)
       
       (if (defined-object? var)
	   
	   (begin (display "%parse-body-assignment case 3: defined : var=") (display (quote var)) (display " expr= ") (display (quote expr)) (newline)
		  exprs ...
		  (display (quote var)) (display " expr= ") (display (quote expr)) (newline)
		  ;; we must add a let to skip recomputation of expression and i must return expr
		  (set! var expr)
		  ;;(<- var expr)
		  (%parse-body-assignment () rest ...)
		  )
	   
	   (begin (display "%parse-body-assignment case 3: undefined : var=") (display (quote var)) (display " expr= ") (display (quote expr)) (newline)
		  exprs ...
		  (%parse-assignment ((var expr)) rest ... )))))
    
    ;; Just a new expression.
    ((_ (exprs ...) new-expr rest ...)
     (begin  (display "%parse-body-assignment case 4 : new-expr=") (display (quote new-expr)) (newline)
	     ;; add the new-expr to the accumulator
	     (%parse-body-assignment (exprs ... new-expr) rest ...))))) ;; shift, and go forward  in expression list and recursively call parse-body



 

;; scheme@(guile-user)> (def (foo) (when #t (return "hello") "bye"))
;; scheme@(guile-user)> (foo)
;;  "hello"


(define-syntax def
    (lambda (stx)
      (syntax-case stx ()
        ((_ (<name> <arg> ...) <body> <body>* ...)
         (let ((ret-id (datum->syntax stx 'return)))
           #`(define (<name> <arg> ...)
               (call/cc (lambda (#,ret-id) <body> <body>* ...))))))))




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
    


;; scratch:

;; (define x 1)
;; (define (foo) {x := 1})
;; {x := 7}
;; (define (foo)
;;   (define x 7)
;;   (let ((x 8))
;;     x))

;; (define (bar)
;;   (let ((x 8))
;;     (define x 7)
;;     x))

;; (define (bar2)
;;   (let ((x 8))
;;     (define x 7)
;;     x)
;;   x)
