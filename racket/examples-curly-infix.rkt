#lang reader "SRFI-105.rkt"

(require "examples-curly-infix2.rkt")

(- (+ 3 3)
   {2 + 2})



{5 + 2}

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))
(fibonacci 7)

(define (fib n)
  (if {n < 2}
      n
      {(fib {n - 1}) + (fib {n - 2})} ))

(fib 11)


