;; definition and assignment
;; { x <+ 7 } is equivalent to : (<- x 7) or (define x 7)
(define-syntax <+
  (syntax-rules ()
    ((_ var expr) (define var expr))))

;; definition and assignment
;; vintage version : Pascal,Algol,Simula,Ada,Smalltalk,Eiffel,... assignment operator
(define-syntax :=
  (syntax-rules ()
    ((_ var expr) (define var expr))))


;; definition without a value assigned
;;  (def x) 
(define-syntax def
  (syntax-rules ()
    ((_ var) (define var '()))))
