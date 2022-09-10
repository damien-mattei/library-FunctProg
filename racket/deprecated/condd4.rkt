#lang racket
(define-syntax condd4
  (syntax-rules ()
    ((_)
     (error 'condd4 "Fell through without else clause"))
    ((_ (else . e))
     (let () . e))
    ((_ #:do d . tail)
     (let () d (condd4 . tail)))
    ((_ (t . e) . tail)
     (if t
         (let () . e)
         (condd4 . tail)))))

;; condx: cond(itional) with optional execution of statements before
;
; example:
;(define x 1)
;(condx ((= x 7) 'never)
;        (exec
;          (define y 3)
;          (set! x 7))
;        ((= y 1) 'definitely_not)
;        (exec
;          (set! y 10)
;          (define z 2))
;        ((= x 7) (+ x y z)))
;
; 19

(define-syntax condx
  (syntax-rules (exec)
    ((_)
     (error 'condx "Fell through without else clause"))
    ((_ (else e ...))
     (let () e ...))
    ((_ (exec s ...) d1 ...)
     (let () s ... (condx d1 ...)))
    ((_ (t e ...) tail ...)
     (if t
         (let () e ...)
         (condx tail ...)))))

(define x 1)

;(condx ((= x 7) 'never)
;        (exec
;          (define y 3)
;          (set! x 7))
;        ((= y 1) 'definitely_not)
;        (exec
;          (set! y 10)
;          (define z 2))
;        ((= x 7) (+ x y z))
;        (else 'you_should_not_be_here))


(define-syntax condd4b
  (syntax-rules ()
    ((_)
     (error 'condd4b "Fell through without else clause"))
    ((_ (else e ...))
     (let () e ...))
    ((_ #:do d d1 ...)
     (let () d (condd4b d1 ...)))
    ((_ (t e ...) tail ...)
     (if t
         (let () e ...)
         (condd4b tail ...)))))




;(condd4b ((= x 7) 'never)
;        #:do
;        (begin (define y 3)
;               (set! x 7))
;        ((= y 1) 'definitely_not)
;        #:do
;        (begin
;          (set! y 10)
;          (define z 2))
;        ((= x 7) (+ x y z)))
;



; (cons 'f (cons 'arg1 'LrestArg))
;
;'(f arg1 . LrestArg)

(define (f-condd4 l)
  (condd4
   [(empty? l)
    empty]
   #:do (match-define (cons fst rst) l)
   [(zero? (modulo fst 3))
    (cons 3 (fst rst))]
   [(zero? (modulo fst 4))
    (cons (+ fst 4) rst)]
   [else (list fst)]))

(define-syntax condx2
  (syntax-rules (exec else)
    ((_)
     (error 'condx2 "No else clause"))
    ((_ (else e ...))
     (begin e ...))
    ((_ (exec s ...) d1 ...)
     (begin s ... (condx2 d1 ...)))
    ((_ (t e ...) tail ...)
     (if t
         (begin e ...)
         (condx2 tail ...)))))



(condx2 ((= x 7) 'never)
        (exec
          (define y 3)
          (set! x 7))
        ((= y 1) 'definitely_not)
        (exec
          (set! y 10)
          (define z 2))
        ((= x 7) (+ x y z))
        (else 'you_should_not_be_here))

(condx2 ((= x 7) 'never)
        (exec
          (set! y 3)
          (set! x 7))
        ((= y 1) 'definitely_not)
        (exec
          (set! y 10)
          (set! z 2))
        ((= x 7) (+ x y z))
        (else 'you_should_not_be_here))


