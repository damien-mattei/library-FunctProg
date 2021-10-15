#lang Racket
;;(include "../let.scm")
(include "array-racket.scm")


(define-syntax letrec*

  (syntax-rules ()

    ((_ () expr ...) (begin expr ...)) ;;  case empty let
    
    ((_ ((var val)) expr  ...) (letrec ((var val)) expr ...))
    
    ((_ ((var1 val1) (var2 val2) ...) expr  ...) (letrec ((var1 val1))
					       (letrec* ((var2 val2) ...) expr ...))) ))


;> (defined-object? (toto 1 2))
;#t
;> (defined-object? (toto 1 2))
;#t
;> (if (defined? z) 'defined 'not-defined)
;'not-defined
;> (if (defined? x) 'defined 'not-defined)
;'not-defined
;> (define x 3)
;> (if (defined? x) 'defined 'not-defined)
;'defined
;> 
(define-syntax (defined? stx)
  (syntax-case stx ()
    [(_ id)
     (if (identifier? #'id)
         (with-syntax ([v (identifier-binding #'id)])
       #''v)
         #'#t)]))




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
    ;; (begin (display (quote var)) (newline)
	    ;;{(not (symbol? (quote var))) or (defined? (quote var))})));;)
	 ;;(or (not (symbol? (quote var))) (defined? (quote var))))))
     (or (not (symbol? (quote var))) (defined? var)))))

;; scheme@(guile-user)> ($  {x <- 7} {y <- 8} (+ x y))
;; ;;; <stdin>:26:0: warning: possibly unbound variable `x'
;; 15
(define-syntax $
  (syntax-rules ()
    ((_ one-thing-only)
     one-thing-only)
    ((_ stuff ...)
     (%parse-body-assignment () stuff ...)))) ;; at the beginning the accumulator () is empty



;; scheme@(guile-user)> (use-modules (guile define-guile-3))
;; scheme@(guile-user)> (begin {x <- 7} {y <- 8} (+ x y))
;; 15



;; scheme@(guile-user)> (define dyna (make-array 0 {10} {20}))
;; scheme@(guile-user)> {{dyna[2 7]} <- 7}
;; 7
;; scheme@(guile-user)> (new-begin  {x <- 7} {y <- 8} {{dyna[2 7]} <- 7} (+ x y))
;; While compiling expression:
;; Syntax error:
;; unknown file:6:0: letrec*: bad letrec* in form (letrec* ((($bracket-apply$ dyna 2 7) 7)) (%parse-body-assignment () (+ x y)))
;; scheme@(guile-user)> (defined-object? {dyna[2 7]})
;; #t

;;(load "define-guile-3-load.scm")
;; scheme@(guile-user)> (define x 3)
;; scheme@(guile-user)> (new-begin  {x <- 7}  x)
;; 7
;; scheme@(guile-user)> (new-begin  {x <- 7}  x)
;; 7
;; scheme@(guile-user)> (new-begin  {x <- 7} {y <- 8} (+ x y))


;;(%parse-assignment (clauses ...) body ...)
;; we start from an empty (clauses ...) list of clauses and append one more clause (name expr) at end
;;
;; this an accumulator of 'bindings' or clauses
;;
(define-syntax %parse-assignment
  (syntax-rules (<-);)
      
    ;; new assignment
    ((_ (clauses ...) (<- name expr) rest ...)
     ;;(begin
       ;;(display "%parse-assignment : case 1 : name =") (display (quote name)) (newline)
       (if (defined-object? name)
	   (letrec* (clauses ...)
		    ;;(set! name expr) ;; TODO must merge this macros file and macros of array.scm to make it work with arrays !!!
		    ;;(<- name expr)
		    (%parse-body-assignment () rest ...)) ;; letrec* and assign and continue parsing the rest ...
	   ;;(begin (display "undefined ")  (newline)
	   (%parse-assignment (clauses ... (name expr)) rest ...)));)) ;; append one more clause (name expr) at end of clauses list
    
    ;; Exit
    ((_ clauses rest ...)
     (letrec* clauses ;; create local bindings :  TODO let do not return value of binding
	      ;;(display "%parse-assignment : case 2 ") (newline)
	      (%parse-body-assignment () rest ...))))) ;; and parse-body the rest ...



;; A macro that transverses expressions
;; (%parse-body (seen-expressions ...) rest ...)
;; we store expressions gradually in the first argument list (exprs ...) called the accumulator
;;
;; this is an accumulator of expressions
;;
(define-syntax %parse-body-assignment
  (syntax-rules (<-);) 
  
    ;; Found no definitions. Just exit
    ;; this case is called at the end when the accumulator is full of expressions to evaluate ! not at the start !
    ((_ (exprs ...))
     (begin  ;(display "%parse-body-assignment case 1")  (newline)
       exprs ...)) ;; evaluate exprs ...
    
    ;; An assignment, exit to %parse-assignment or call assignment immediately and parse the rest 
    ((_ (exprs ...) (<- var expr) rest ...)

      ;;(defined-object? var))
     ;(begin
       ;(display "%parse-body-assignment case 3: expr =") (display (quote expr)) (newline)
	(if (defined-object? var)
	    (begin (display "%parse-body-assignment :: defined : var =") (display (quote var)) (display " expr=") (display expr) (newline)
	      exprs ...
	      ;;(set! var expr)
	      ;;(<- var expr)
	      (%parse-body-assignment () rest ...)
	      )
	    (begin (display "%parse-body-assignment :: undefined ") (newline)
	      exprs ... (%parse-assignment ((var expr)) rest ... ))));)
    
    ;; Just a new expression.
    ((_ (exprs ...) new-expr rest ...)
     (begin  (display "%parse-body-assignment case 4 : new-expr =") (display (quote new-expr)) (newline)
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
    
