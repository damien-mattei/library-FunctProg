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

;; for guile
;; (define T (make-vector 5))
;; (vector-set! T 3 7)
;; scheme@(guile-user)> {T[3]}
;; $3 = 7
(define-syntax $bracket-apply$
  (syntax-rules ()
    ((_ array x) (vector-ref array x)))) 

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


(define-syntax <-
  (syntax-rules ()
    ;;  (<- ($bracket-apply$ T 3) ($bracket-apply$ T 4))
    ((_ (funct-or-macro array x) expr) (if (equal? (quote $bracket-apply$) (quote funct-or-macro)) ;; test funct-or-macro equal $bracket-apply$ 
					   (vector-set! array x expr)
					   (funct-or-macro array x)))
    ;; (<- x 5)
    ((_ var expr) (set! var expr))))



