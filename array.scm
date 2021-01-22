;; arrays


;; the value v should be put before in a let to avoid multiple evaluation after macro expand
(define-syntax make-array-2d
  (syntax-rules ()
    ((_ sx sy) (let* ((array (make-vector sy)))
		 (for (i 0 (- sy 1))
		      (vector-set! array i (make-vector sx)))
		 array))
    ((_ sx sy v) (let* ((array (make-vector sy)))
		   (for (i 0 (- sy 1))
			(vector-set! array i (make-vector sx v)))
		   array))
    ((_ array sx sy) (begin
		       (set! (quote array) (make-vector sy))
		       (for (i 0 (- sy 1))
			    (vector-set! (quote array) i (make-vector sx)))))
    ((_ array sx sy v) (begin
		       (set! (quote array) (make-vector sy))
		       (for (i 0 (- sy 1))
			    (vector-set! (quote array) i (make-vector sx v)))))))


(define-syntax array-2d-ref
  (syntax-rules ()
    ((_ array x y) (vector-ref (vector-ref array y) x))))

(define-syntax array-2d-set!
  (syntax-rules ()
    ((_ array x y val) (vector-set! (vector-ref array y) x val))))

(define-syntax display-array-2d
  (syntax-rules ()
    ((_ array)
     (for (y 0 (- (vector-length array) 1))
	  (display-nl (vector-ref array y))))))


;; > (define _quai 34)
;; > (dv _quai)
;; _quai = 34
(define-syntax dv-2d 
  (syntax-rules ()
    ((_ var) (begin
	       ;;(display (symbol->string (quote var)))
	       (display (quote var))
	       (display-nl " = ")
	       (display-array-2d var)
	       (newline)))))


(define (funct-array-2d-set! array x y val) (vector-set! (vector-ref array y) x val))

(define (funct-array-2d-ref array x y) (vector-ref (vector-ref array y) x))

;; scheme@(guile-user)> (array-ref-set! dyna 7 3 4)
;; $4 = 7
;; scheme@(guile-user)> (array-ref dyna 3 4)
;; $5 = 7
(define-syntax array-ref-set!
  (syntax-rules ()
    ((_ array expr x y) (let ((v expr))
			  (array-set! array v x y)
			  v))))


;; SRFI 105 : Curly-infix-expressions allows a syntax like {Array[index]} with vectors
;; and arrays of any dimensions,size and shape

;; for guile
;; (define T (make-vector 5))
;; (vector-set! T 3 7)
;; scheme@(guile-user)> {T[3]}
;; $3 = 7
;; {T[3] <- 7}
;; 7

;; scheme@(guile-user)> (define a (make-array 999 '(1 2) '(3 4)))
;; scheme@(guile-user)> (array-ref a 2 4)
;; $3 = 999

;; scheme@(guile-user)> {a[2 4]}
;; $9 = 999

;; scheme@(guile-user)> (define b (make-array 'ho 3))
;; scheme@(guile-user)> (array-ref b 1)
;; $13 = ho

;; scheme@(guile-user)> {b[2]}
;; $15 = ho

;; scheme@(guile-user)> {a[2 4] <- 7}
;; scheme@(guile-user)> {a[2 4]}
;; $19 = 7
;; scheme@(guile-user)> {a[1 3] <- 5}
;; scheme@(guile-user)> {a[1 3] <- a[2 4]}
;; scheme@(guile-user)> {a[1 3]}
;; $20 = 7

(define-syntax $bracket-apply$
  (syntax-rules ()
    ((_ array index) (if (vector? array)
			 (vector-ref array index)
			 (array-ref array index)))
    ((_ array index ...) (array-ref array index ...)))) 



;; scheme@(guile-user)> '(<- {T[3]} {T[4]})
;; $14 = (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
;; scheme@(guile-user)> (<- {T[3]} {T[4]})
;; scheme@(guile-user)> {T[4]}
;; $15 = 7
;; scheme@(guile-user)> {T[3]}
;; $16 = 7

;; scheme@(guile-user)> '{T[3] <- T[4]}
;; $17 = (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
;; scheme@(guile-user)> {T[3] <- T[4]}

;; scheme@(guile-user)> {b[2] <- "toto"}
;; scheme@(guile-user)> {b[2]}
;; $17 = "toto"

;; scheme@(guile-user)> {b[2] <- b[1]}
;; scheme@(guile-user)> {b[1]}
;; $18 = ho


;; scheme@(guile-user)> {a[2 4] <- 7}
;; $1 = 7
;; (define-syntax <-
;;   (syntax-rules ()
;;     ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
;;     ((_ (funct-or-macro array index) expr) (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
;; 					       (let ((tmp expr))
;; 						 (if (vector? array)
;; 						     (vector-set! array index tmp)
;; 						     (array-set! array tmp index))
;; 						 tmp)
					       
;; 					       (funct-or-macro array index)))

;;     ((_ (funct-or-macro array index ...) expr) (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
;; 						   (let ((tmp expr))
;; 						     (array-set! array tmp index ...)
;; 						     tmp)
						   
;; 						   (funct-or-macro array index ...)))
    
;;     ;; (<- x 5)
;;     ((_ var expr) (let ((tmp expr))
;; 		    (set! var expr)
;; 		    tmp))))



(define-syntax <-
  (syntax-rules ()
    ;;  special form like : (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ((_ (funct-or-macro array index) expr) {array[index] ← expr} )

    ((_ (funct-or-macro array index ...) expr) {array[index ...] ← expr} )
    
    ;; (<- x 5)
    ((_ var expr) {var ← expr})))


;; ;; scheme@(guile-user)> {7 -> a[2 4]}
;; ;; $1 = 7
;; (define-syntax ->
;;   (syntax-rules ()
;;     ;;  special form like : (-> ($bracket-apply$ T 3) ($bracket-apply$ T 4))
;;     ((_ expr (funct-or-macro array index)) (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
;; 					       (let ((tmp expr))
;; 						 (if (vector? array)
;; 						     (vector-set! array index tmp)
;; 						     (array-set! array tmp index))
;; 						 tmp)
					       
;; 					       (funct-or-macro array index)))

;;     ((_ expr (funct-or-macro array index ...)) (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
;; 						   (let ((tmp expr))
;; 						     (array-set! array tmp index ...)
;; 						     tmp)
						   
;; 						   (funct-or-macro array index ...)))
    
;;     ;; (-> 5 x)
;;     ((_ expr var) (let ((tmp expr))
;; 		    (set! var expr)
;; 		    tmp))))


(define-syntax ->
  (syntax-rules ()
    ;;  special form like : (-> ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ((_ expr (funct-or-macro array index)) {expr → array[index]} )

    ((_ expr (funct-or-macro array index ...)) {expr → array[index ...]} )
    
    ;; (-> 5 x)
    ((_ expr var) {expr → var})))


;; scheme@(guile-user)> {a[2 4] ← 7}
;; $1 = 7

;; scheme@(guile-user)> {a[2 4]}
;; $1 = 999
;; scheme@(guile-user)> {a[2 4] ← 7}
;; $2 = 7
;; scheme@(guile-user)> {a[2 4]}
;; $3 = 7
;; scheme@(guile-user)> {1 → a[2 4]}
;; $4 = 1
;; scheme@(guile-user)> {a[2 4]}
;; $5 = 1
;; {x ← 2}

(define-syntax ←
  (syntax-rules ()
    ;;  special form like : (← ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    
    ;; one dimension array, example: {a[4] ← 7}
    ((_ (funct-or-macro array index) expr) (let ((tmp expr))
    					     (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
    						 ;; normal case
    						 (if (vector? array)
    						     (vector-set! array index tmp)
    						     (array-set! array tmp index))
						 
    						 ;; rare case (to prevent any error)
    						 (set! (funct-or-macro array index) tmp))
					     
    					     tmp))


    ;; multi dimensions array :  {a[2 4] ← 7}
    ((_ (funct-or-macro array index ...) expr) (let ((tmp expr)
						     ;; must be in variable
						     (var (funct-or-macro array index ...)))
						 (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
						     ;; normal case
						     (array-set! array tmp index ...)
						     
						     ;; rare case (to prevent any error)
						     (set! var tmp))
					     
						 tmp))

    ;; compact form but will display a warning: possibly wrong number of arguments to `vector-set!'
    ;; and if i remove ellipsis it is a severe error
    ;; ((_ (funct-or-macro array index ...) expr) (let ((tmp expr)
    ;; 						     (var (funct-or-macro array index ...)))
    ;; 						 (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$
    ;; 						     ;; normal case
    ;; 						     (if (vector? array)
    ;; 							 (vector-set! array index ... tmp)
    ;; 							 (array-set! array tmp index ...))
						     
    ;; 						     ;; rare case (to prevent any error)
    ;; 						     (set! var tmp))
					     
    ;; 						 tmp))
    
    ;; (← x 5)
    ((_ var expr) (let ((tmp expr))
		    (set! var tmp)
		    tmp))))


(define-syntax →
  (syntax-rules ()
    ;;  special form like : (→ ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ((_ expr (funct-or-macro array index)) {array[index] ← expr}  )
    ((_ expr (funct-or-macro array index ...)) {array[index ...] ← expr} )
    
    ;; (→ 5 x)
    ((_ expr var) {var ← expr})))


;; scheme@(guile-user)> {7 → a[2 4]}
;; $1 = 7
;; (define-syntax →
;;   (syntax-rules ()
;;     ;;  special form like : (→ ($bracket-apply$ T 3) ($bracket-apply$ T 4))
;;     ((_ expr (funct-or-macro array index)) (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test if funct-or-macro equal $bracket-apply$
;; 					       (let ((tmp expr))
;; 						 (if (vector? array)
;; 						     (vector-set! array index tmp)
;; 						     (array-set! array tmp index))
;; 						 tmp)
					       
;; 					       (funct-or-macro array index)))

;;     ((_ expr (funct-or-macro array index ...)) (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test if funct-or-macro equal $bracket-apply$
;; 						   (let ((tmp expr))
;; 						     (array-set! array tmp index ...)
;; 						     tmp)
						   
;; 						   (funct-or-macro array index ...)))
    
;;     ;; (→ 5 x)
;;     ((_ expr var) (let ((tmp expr))
;; 		    (set! var expr)
;; 		    tmp))))

;; DEPRECATED


;; scheme@(guile-user)> (define T (make-vector 5))
;; scheme@(guile-user)> (<=- (T 3) 7)

;; scheme@(guile-user)> {(T 3) <=- 10}
;; scheme@(guile-user)> {T[3]}
;; $4 = 10

;; scheme@(guile-user)> {T(3) <=- 12}
;; scheme@(guile-user)> {T[3]}
;; $5 = 12

;; scheme@(guile-user)> {T(4) <=- 7}
;; scheme@(guile-user)> {T(3) <=- T[4]}
;; scheme@(guile-user)> {T[3]}
;; $6 = 7
;; (define-syntax <=-
;;   (syntax-rules ()
;;     ((_ (array x) expr) (vector-set! array x expr))))
