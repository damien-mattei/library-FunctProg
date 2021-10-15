;; Copyright 2020 Linus Björnstam
;;
;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all source copies.
;; The software is provided "as is", without any express or implied warranties.
;;
;; Fix guile to have definitions in expression context in most common constructs.
;; nothing for srfi-11 so far. This is written using syntax-rules and should
;; be portable across implementation.

;; modified by Damien Mattei

;; use with with (use-modules (guile define-guile-3))


(define-module (guile define-guile-3)
  ;;#:use-module (ice-9 receive) ;; not used

  #:export (type-var? defined-object?)
  
  #:replace (;(new-lambda . lambda)
             (new-define . define)
             (new-begin . begin)
             ;; (new-let . let)
             ;; (new-let* . let*)
             ;; (let-letrec . letrec)
             ;; (new-letrec* . letrec*)
             ;; (new-case . case)
             ;; (new-cond . cond)
             )
  

  )



;; (define-syntax new-lambda
;;   (syntax-rules ()
;;     ((_ formals body body* ...)
;;      (lambda formals (new-begin body body* ...)))))

(define-syntax new-define
  (syntax-rules ()
    ((_ (name args ...) body body* ...)
     (define name (lambda (args ...) body body* ...)))
    ((_ name expr) (define name expr))))


(define-syntax new-begin
  (syntax-rules ()
    ((_ one-thing-only)
     one-thing-only)
    ((_ stuff ...)
     ;;(%parse-body () stuff ...))))
     (%parse-body-assignment () stuff ...))))

;; (define-syntax new-let
;;   (syntax-rules ()
;;     ((_ clauses body body* ...)
;;      (let clauses (new-begin body body* ...)))))

;; (define-syntax new-let*
;;   (syntax-rules ()
;;     ((_ clauses body body* ...)
;;      (let* clauses (new-begin body body* ...)))))

;; (define-syntax new-letrec
;;   (syntax-rules ()
;;     ((_ clauses body body* ...)
;;      (letrec clauses (new-begin body body* ...)))))

;; (define-syntax new-letrec*
;;   (syntax-rules ()
;;     ((_ clauses body body* ...)
;;      (letrec* clauses (new-begin body body* ...)))))

;; (define-syntax new-case
;;   (syntax-rules (else =>)
;;     ;; Special case for else with a lambda-clause.
;;     ((_ expr
;;         (test body ...)
;;         (else => else-body ...))
;;      (case expr
;;        (test (new-begin body ...))
;;        (else => (new-begin else-body ...))))
;;     ((_ expr
;;         (test body ...) ...)
;;      (case expr
;;        (test (new-begin body ...)) ...))))




;; ;; Exploits the fact that cond is just transformed into an if.
;; ;; This means we can, as with case, just re-use guile's own
;; ;; cond. Guile supports a non-standard guard-case. This should be removed
;; ;; if porting to any other scheme.
;; (define-syntax new-cond
;;   (syntax-rules (=> else)
;;     ;; Guile-specific guard case
;;     ((_ (test guard => body ...) rest ...)
;;      (receive vals test
;;        (if (apply guard vals)
;;            (apply (new-begin body ...) vals)
;;            (new-cond rest ...))))

;;     ;; Lambda case
;;     ((_ (test => body ...) rest ...)
;;      (let ((temp test))
;;         (if test
;;             ((new-begin body ...) temp)
;;             (internal-cond rest ...))))

;;     ;; Else case
;;     ((_ (else body ...))
;;      (new-begin body ...))

;;     ;; No clauses left and no else clause.
;;     ((_) (if #f #f))

;;     ;; Normal case
;;     ((_ (test body ...) rest ...)
;;      (if test
;;           (new-begin body ...)
;;           (new-cond rest ...)))))






;;(%parse-define (clauses ...) body ...)
;; we start from an empty (clauses ...) list of clauses and append one more clause (name expr) at end 
(define-syntax %parse-define
  (syntax-rules (new-define)
    ;; procedure definition
    ((_ (clauses ...) (new-define (name args ...) body body* ...) rest ... )
     ;;(%parse-define (clauses ... (name (new-lambda (args ...) body body* ...))) rest ... ))
     (%parse-define (clauses ... (name (lambda (args ...) body body* ...))) rest ... ))
    
    ;; Variable definition
    ((_ (clauses ...) (new-define name expr) rest ...)
     (%parse-define (clauses ... (name expr)) rest ...)) ;; append one more clause (name expr) at end of clauses list

    ;; Exit
    ((_ clauses rest ...)
     (letrec* clauses ;; create local bindings
       (%parse-body () rest ...))))) ;; and parse-body the rest ...

;; A macro that transverses expressions
;; (%parse-body (seen-exprssions ...) rest ...)
(define-syntax %parse-body
  (syntax-rules (new-define)
    ;; Found no definitions. Just exit
    ((_ (exprs ...))
     (begin exprs ...)) ;; evaluate exprs ...
    ;; A definition, exit to %parse-define
    ((_ (exprs ...) (new-define stuff ...) rest ...)
     ;; (begin (display "%parse-body case 2 : new-define") (newline) exprs ... (%parse-define () (new-define stuff ...) rest ... )))
     (begin exprs ... (%parse-define () (new-define stuff ...) rest ... ))) ;; evaluate exprs ... and other parse define result too

    ;; new assignment
    ((_ (exprs ...) (<- var expr) rest ...)
     (begin (display "%parse-body case 3 : assignment") (newline) exprs ... (<- var expr) rest ... ))
     
    ;; Just a new expression.
    ((_ (exprs ...) new-expr rest ...)
     (%parse-body (exprs ... new-expr) rest ...)))) ;; shift, and go forward  in expression list and recursively call parse-body



;; scheme@(guile-user)> (use-modules (guile define-guile-3))
;; scheme@(guile-user)> (begin {x <- 7} {y <- 8} (+ x y))
;; 15
;;(%parse-assignment (clauses ...) body ...)
;; we start from an empty (clauses ...) list of clauses and append one more clause (name expr) at end 
(define-syntax %parse-assignment
  (syntax-rules (<-)
      
    ;; new assignment
    ((_ (clauses ...) (<- name expr) rest ...)
     ;;(begin
       ;;(display "%parse-assignment : case 1 : name =") (display (quote name)) (newline)
       (if (defined-object? name)
	   (letrec* (clauses ...) (set! name expr) (%parse-body-assignment () rest ...)) ;; letrec* and assign and continue
	   ;;(begin (display "undefined ")  (newline)
	   (%parse-assignment (clauses ... (name expr)) rest ...)));)) ;; append one more clause (name expr) at end of clauses list
    
    ;; Exit
    ((_ clauses rest ...)
     (letrec* clauses ;; create local bindings :  TODO let do not return value of binding
	      ;;(display "%parse-assignment : case 2 ") (newline)
	      (%parse-body-assignment () rest ...))))) ;; and parse-body the rest ...



;; A macro that transverses expressions
;; (%parse-body (seen-exprssions ...) rest ...)
;; we store expressions gradually in the first argument list (exprs ...)
(define-syntax %parse-body-assignment
  (syntax-rules (<-) 
  
    ;; Found no definitions. Just exit
    ((_ (exprs ...))
     (begin  ;;(display "%parse-body-assignment case 1")  (newline)
       exprs ...)) ;; evaluate exprs ...
    
    ;; An assignment, exit to %parse-assignment or call assignment immediately
    ((_ (exprs ...) (<- var expr) rest ...)
     ;;(begin (display "%parse-body case 3 : assignment") (newline) exprs ... (<- var expr) rest ... ))
     ;;(begin
       ;;(display "%parse-body-assignment case 3: expr =") (display (quote expr)) (newline)
	(if (defined-object? var)
	    (begin ;;(display "defined") (newline)
	      exprs ... (set! var expr) rest ... )
	    (begin ;;(display "undefined") (newline)
	      exprs ... (%parse-assignment ((var expr)) rest ... ))));;))
    
    ;; Just a new expression.
    ((_ (exprs ...) new-expr rest ...)
    ;; (begin ;; (display "%parse-body-assignment case 4 : new-expr =") (display (quote new-expr)) (newline)
	     (%parse-body-assignment (exprs ... new-expr) rest ...))));) ;; shift, and go forward  in expression list and recursively call parse-body


;; scheme@(guile-user)> (type-var? x)
;; $1 = undefined
;; scheme@(guile-user)> (define x 7)
;; scheme@(guile-user)> (type-var? x)
;; $2 = defined
;;scheme@(guile-user)> (type-var? {dyna[2 7]})
;;$3 = array
;; deprecated

(define-syntax type-var?
  (syntax-rules ()
    ((_ var) (cond [(not (symbol? (quote var))) 'array]
		   [(defined? (quote var)) 'defined]
		   [else 'undefined]))))
	 
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
    ((_ var) {(not (symbol? (quote var))) or (defined? (quote var))})))
	 
