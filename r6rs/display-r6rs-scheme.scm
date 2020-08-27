;; macros or function to display a variable with a message and add a newline
(define-syntax display-msg-symb-nl 
  (syntax-rules ()
    ((_ msg symbl)   (begin
		       (display msg)
		       (display " ")
		       (display (symbol->string (quote symbl)))
		       (display " = ")
		       (display symbl)
		       (newline)))))

;; to be deleted
(define-syntax display-msg-symb-quote-nl 
  (syntax-rules ()
    ((_ msg symbl)   (begin
		       (display msg)
		       (display " ")
		       (display (symbol->string (quote symbl)))
		       (display " = ")
		       (display (quote symbl))
		       (newline)))))

(define-syntax display-symb-nl 
  (syntax-rules ()
    ((_ symbl)   (begin
		   (display (symbol->string (quote symbl)))
		   (display " = ")
		   (display symbl)
		   (newline)))))

(define-syntax display-expr-nl 
  (syntax-rules ()
    ((_ expr) (begin
		(display (quote expr))
		(display " = ")
		(display expr)
		(newline)))))


(define-syntax display-var-nl 
  (syntax-rules ()
    ((_ msg var)   (begin (display msg) (display var) (newline)))))

(define-syntax display-msg-var-nl 
  (syntax-rules ()
    ((_ msg var)   (begin (display msg) (display var) (newline)))))

(define (dvn msg var)
  (begin (display msg) (display var) (newline)))

(define-syntax display-nl 
  (syntax-rules ()
    ((_ msg)   (begin (display msg) (newline)))))


;; > (define _quai 34)
;; > (dv _quai)
;; _quai = 34
(define-syntax dv 
  (syntax-rules ()
    ((_ var) (begin
	       (display (symbol->string (quote var)))
	       (display " = ")
	       (display var)
	       (newline)))))

;; compatible with r6rs scheme
(define (fdv var)
  (begin (display (symbol->string var)) (display " = ") (display (eval var (interaction-environment))) (newline)))


;; > (define _quai 34)
;; > (de _quai)
;; _quai = 34
;; (de (array-2d-ref iepi 0 y))
;; (array-2d-ref iepi 0 y) = (1 x x 0)
(define-syntax de 
  (syntax-rules ()
    ((_ expr) (begin
	       (display (quote expr))
	       (display " = ")
	       (display expr)
	       (newline)))))


;; another definition that use a macro
;; note that there is no more need to quote the argument when using a macro 
;;
;;  (mac-cleaner (compact-display '(a ^ b ^ c))) -> a^b^c
;;  (mac-cleaner (enlight-dnf '(or (and c (not (or (and a (not b)) (and (not a) b)))) (and (not c) (or (and a (not b)) (and (not a) b)))))) -> (!a^!b^c)v(a^b^c)v(a^!b^!c)v(!a^b^!c)
(define-syntax mac-cleaner
  (syntax-rules ()
    ((mac-cleaner task) 
     (begin 
       task
       (display "")))))


;; display in red in unix terminal only
;; update: works in Guile REPL too
(define (display-red txt)
  (begin
    (display escape-char)
    (display "[31m")
    (display txt)
    (display escape-char)
    (display "[0m")))


;; return a red string for unix terminal
(define (string-red txt)
  (string-append (string escape-char)
		 "[31m"
		 txt
		 (string escape-char)
		 "[0m"))


(define (display-tail-in-red str n)
  (string-append
   (substring str 0 n)
   (string-red (substring str
			  n
			  (string-length str)))))

(define (display-tail-in-red-from-end str n)
  (string-append
   (substring str
	      0
	      (- (string-length str) n))
   (string-red (substring str
			  (- (string-length str) n)
			  (string-length str)))))

(define dtirfe display-tail-in-red-from-end)

