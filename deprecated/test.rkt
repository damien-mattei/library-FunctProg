#lang racket
(define resume-test-3 #f)

(define test-3 (lambda ()
   ; the let defines a variable i local to the lambda, and 
   ; sets its value to 0
   (let ((i 0))
     (display "Entering let") (newline)
     ;
     (call/cc (lambda (k) (set! resume-test-3 k)))
     ;
     ; The next time the-continuation is called, we start here.
     (display "I am back ")(newline)
     (set! i (+ i 1))
     ; and return the value i
     (display "Leaving let")(newline)
     i)))

(test-3)
(resume-test-3)
(resume-test-3)

(define-syntax create_def
  (syntax-rules ()
    ((_ var expr) (define var expr))))

(define-syntax ←
  (syntax-rules ()
    ((_ var expr)  (if (defined? var)
		       (begin
			 (display "← : variable set!") (newline)
			 (set! var expr))
			
		       (create_def var expr)))))